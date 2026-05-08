#!/usr/bin/env python3
"""
Scrape BLS CES monthly jobs revisions (1979-present) from:
https://www.bls.gov/web/empsit/cesnaicsrev.htm

Outputs a tidy CSV with SA/NSA first/second/third prints and revision deltas.
Mike Konczal, but mostly ChatGPT
"""

import re
import sys
import time
from io import StringIO
from datetime import datetime, timezone

import pandas as pd
import requests
from requests.adapters import HTTPAdapter, Retry

SOURCE_URL = "https://www.bls.gov/web/empsit/cesnaicsrev.htm"

HEADERS = {
    # A realistic browser UA helps avoid 403s.
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/127.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
    "Referer": "https://www.bls.gov/",
    "Connection": "keep-alive",
}

MONTH_MAP = {
    "Jan.": 1, "Feb.": 2, "Mar.": 3, "Apr.": 4, "May": 5, "Jun.": 6,
    "Jul.": 7, "Aug.": 8, "Sep.": 9, "Oct.": 10, "Nov": 11, "Dec.": 12,
    "Jan": 1, "Feb": 2, "Mar": 3, "Apr": 4, "Jun": 6, "Jul": 7, "Aug": 8, "Sep": 9, "Oct": 10, "Nov": 11, "Dec": 12,
    "Nov.": 11, "May.": 5,
}

def _normalize_col(col):
    if isinstance(col, tuple):
        col = " ".join([str(c) for c in col if c and str(c) != "None"])
    return re.sub(r"\s+", " ", str(col)).strip()

def _to_num(s):
    cleaned = str(s).strip()
    cleaned = cleaned.replace("\u2014", "").replace("—", "")
    cleaned = re.sub(r"\s*\([A-Z]\)\s*$", "", cleaned)
    cleaned = cleaned.replace(",", "")
    return pd.to_numeric(cleaned, errors="coerce")

def _looks_like_year_table(df):
    cols = [_normalize_col(c) for c in df.columns]
    has_month = any(c.lower().startswith("month") for c in cols)
    has_year = any(c.lower().startswith("year") for c in cols)
    has_sa = any("Seasonally adjusted" in c for c in cols)
    has_nsa = any("Not seasonally adjusted" in c for c in cols)
    return has_month and has_year and has_sa and has_nsa

def _parse_year(val):
    m = re.search(r"\b(\d{4})\b", str(val))
    return int(m.group(1)) if m else pd.NA

def _extract_year_table(df):
    df = df.copy()
    df.columns = [_normalize_col(c) for c in df.columns]

    if "Month" not in df.columns:
        # first column usually holds months
        df = df.rename(columns={df.columns[0]: "Month"})
    if "Year" not in df.columns:
        # find a column with many 4-digit years
        for c in df.columns:
            if c == "Month":
                continue
            if df[c].astype(str).str.fullmatch(r"\s*\d{4}\s*(?:\([A-Z]\))?\s*").sum() >= 6:
                df = df.rename(columns={c: "Year"})
                break

    month_raw = df["Month"].astype(str).str.strip()
    month_clean = month_raw.str.replace(r"\s*\([A-Z]\)\s*$", "", regex=True)
    df = df[month_clean.isin(MONTH_MAP.keys())].copy()
    df["Month"] = month_clean
    if df.empty:
        return pd.DataFrame()

    def find_col(patterns, pool):
        for c in pool:
            if all(re.search(pat, c, re.I) for pat in patterns):
                return c
        return None

    targets = {
        "sa_1st":               [r"Seasonally adjusted", r"Over-the-month change", r"\b1st\b"],
        "sa_2nd":               [r"Seasonally adjusted", r"Over-the-month change", r"\b2nd\b"],
        "sa_3rd":               [r"Seasonally adjusted", r"Over-the-month change", r"\b3rd\b"],
        "sa_rev_2nd_minus_1st": [r"Seasonally adjusted", r"Revision", r"2nd\s*-\s*1st"],
        "sa_rev_3rd_minus_2nd": [r"Seasonally adjusted", r"Revision", r"3rd\s*-\s*2nd"],
        "sa_rev_3rd_minus_1st": [r"Seasonally adjusted", r"Revision", r"3rd\s*-\s*1st"],
        "nsa_1st":               [r"Not seasonally adjusted", r"Over-the-month change", r"\b1st\b"],
        "nsa_2nd":               [r"Not seasonally adjusted", r"Over-the-month change", r"\b2nd\b"],
        "nsa_3rd":               [r"Not seasonally adjusted", r"Over-the-month change", r"\b3rd\b"],
        "nsa_rev_2nd_minus_1st": [r"Not seasonally adjusted", r"Revision", r"2nd\s*-\s*1st"],
        "nsa_rev_3rd_minus_2nd": [r"Not seasonally adjusted", r"Revision", r"3rd\s*-\s*2nd"],
        "nsa_rev_3rd_minus_1st": [r"Not seasonally adjusted", r"Revision", r"3rd\s*-\s*1st"],
    }

    colmap = {}
    pools = df.columns
    for key, pats in targets.items():
        hit = find_col(pats, pools)
        if hit:
            colmap[key] = hit

    keep = ["Month", "Year"] + list(colmap.values())
    keep = [c for c in keep if c in df.columns]
    out = df[keep].copy()
    out = out.rename(columns={v: k for k, v in colmap.items()})

    for k in [k for k in targets.keys() if k in out.columns]:
        out[k] = out[k].apply(_to_num)

    out["month"] = out["Month"].astype(str).str.strip()
    out["month_num"] = out["month"].map(MONTH_MAP).astype("Int64")
    out["year"] = out["Year"].apply(_parse_year).astype("Int64")

    core_cols = [
        "year", "month", "month_num",
        "sa_1st", "sa_2nd", "sa_3rd",
        "sa_rev_2nd_minus_1st", "sa_rev_3rd_minus_2nd", "sa_rev_3rd_minus_1st",
        "nsa_1st", "nsa_2nd", "nsa_3rd",
        "nsa_rev_2nd_minus_1st", "nsa_rev_3rd_minus_2nd", "nsa_rev_3rd_minus_1st",
    ]
    for c in core_cols:
        if c not in out.columns:
            out[c] = pd.NA

    return out[core_cols].sort_values(["year", "month_num"]).reset_index(drop=True)

def _session_with_retries():
    retries = Retry(
        total=5,
        backoff_factor=0.8,
        status_forcelist=(408, 429, 500, 502, 503, 504),
        allowed_methods=("GET", "HEAD"),
        raise_on_status=False,
        respect_retry_after_header=True,
    )
    s = requests.Session()
    s.headers.update(HEADERS)
    s.mount("https://", HTTPAdapter(max_retries=retries))
    s.mount("http://", HTTPAdapter(max_retries=retries))
    return s

def _download_html(url: str) -> str:
    """Download HTML, using curl_cffi (Chrome TLS impersonation) to bypass Akamai bot detection."""
    try:
        from curl_cffi import requests as cffi_requests
        r = cffi_requests.get(url, impersonate="chrome120", timeout=30)
        r.raise_for_status()
        return r.text
    except ImportError:
        print("curl_cffi not installed; falling back to requests (may be blocked). Run: pip install curl-cffi", file=sys.stderr)

    sess = _session_with_retries()
    r = sess.get(url, timeout=30)
    r.raise_for_status()
    r.encoding = r.apparent_encoding or "utf-8"
    return r.text

def _parse_tables(html_text: str) -> list[pd.DataFrame]:
    # Try lxml first, then html5lib if lxml missing or fails
    try:
        return pd.read_html(StringIO(html_text), displayed_only=False, flavor="lxml")
    except Exception:
        return pd.read_html(StringIO(html_text), displayed_only=False, flavor="html5lib")

def scrape_bls_revisions(source_url: str = SOURCE_URL) -> pd.DataFrame:
    html_text = _download_html(source_url)
    tables = _parse_tables(html_text)

    year_tables = [t for t in tables if _looks_like_year_table(t)]
    pieces = []
    for t in year_tables:
        chunk = _extract_year_table(t)
        if not chunk.empty:
            pieces.append(chunk)

    if not pieces:
        raise RuntimeError("No monthly year tables found. The page structure may have changed.")

    df = pd.concat(pieces, ignore_index=True)

    df = df.dropna(subset=["year", "month_num"]).copy()
    numeric_cols = [c for c in df.columns if re.search(r"(^sa_|^nsa_)", c)]
    df[numeric_cols] = df[numeric_cols].apply(pd.to_numeric, errors="coerce")

    df["row_order"] = range(len(df))
    df["non_na_count"] = df[numeric_cols].notna().sum(axis=1)
    df = (
        df.sort_values(["year", "month_num", "non_na_count", "row_order"])
        .groupby(["year", "month_num"], as_index=False)
        .tail(1)
    )
    df = df.drop(columns=["row_order", "non_na_count"]).sort_values(["year", "month_num"]).reset_index(drop=True)

    # Drop rows where all SA/NSA value columns are NA (empty future months)
    numeric_cols2 = [c for c in df.columns if re.search(r"(^sa_|^nsa_)", c)]
    df = df.dropna(subset=numeric_cols2, how="all").reset_index(drop=True)

    # Add a proper date column (YYYY-MM-01)
    df["date"] = pd.to_datetime(
        dict(year=df["year"], month=df["month_num"], day=1),
        errors="coerce"
    )

    # Reorder so 'date' is the first column
    cols = ["date"] + [c for c in df.columns if c != "date"]
    df = df[cols]

    df["source_url"] = source_url
    df["scraped_at"] = datetime.now(timezone.utc).isoformat()

    return df

if __name__ == "__main__":
    try:
        df = scrape_bls_revisions(SOURCE_URL)
        print(df.tail(12))  # quick peek
        out_path = "data/bls_ces_monthly_revisions.csv"
        df.to_csv(out_path, index=False)
        print(f"\nSaved {len(df)} rows to {out_path}")
        print("Latest month is:", df.dropna(subset=["sa_1st"]).iloc[-1]["date"].strftime("%B, %Y"))

    except requests.HTTPError as e:
        print("HTTP error while fetching page:", e, file=sys.stderr)
        sys.exit(2)
    except Exception as e:
        print("Error while scraping:", e, file=sys.stderr)
        sys.exit(1)
