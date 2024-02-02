setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")

library(tidyverse)
library(ggtext)
library(lubridate)


#source(file = "1_b_load_bls_ces_jobs_data.R")

ces_data %>% filter(date > "2019-01-01") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap( ~ supersector_name)

table(ces_data$seasonal)

a$value


View(cps_jobs_data %>% filter(series_title=="(Seas) Unemployment Rate", periodicity_code == "M") %>% select(date, value) %>%
  arrange(value) %>% filter(value <= value[date==max(date)]))

U <- cps_jobs_data %>% filter(series_title=="(Seas) Unemployment Level",  periodicity_code == "M") %>% select(date, UL = value)
a <-cps_jobs_data %>% filter(series_title=="(Seas) Civilian Labor Force Level",  periodicity_code == "M") %>% left_join(U, by="date") %>%
  mutate(urate_full = UL/value) %>% select(date, urate_full) %>%
  arrange(urate_full)

b <-cps_jobs_data %>% filter(series_title=="(Seas) Civilian Labor Force Level",  periodicity_code == "M") %>% left_join(U, by="date") %>%
  mutate(urate_full = UL/value) %>% select(date, urate_full) %>% filter(urate_full <= urate_full[date==max(date)]) %>%
  arrange(urate_full) %>% mutate(year = year(date))

unique(b$year)



cps_jobs_data %>% filter(series_id == "LNS14000000") %>% select(date, urate = value) %>% mutate(diff_urate = log(urate)-log(lag(urate,12))) %>%
  ggplot(aes(date,diff_urate)) + geom_line() + theme_classic() + geom_hline(yintercept = -0.1, color="red") +
  labs(title="Do you know why I pulled you over? You were way over the speed limit.",
       subtitle="log of unemployment rate minus the log of unemployment rate lagged 12 months. Red line is -0.1.")

ggsave("graphics/speed_limit.png",  width = 12, height=6.75, dpi="retina")



on_temporary_layoff <- cps_jobs_data %>% filter(series_id %in% c("LNS13023653","LNS11000000")) %>%
  group_by(date) %>%
  summarize(temp_layoff = value[series_id == "LNS13023653"]/value[series_id == "LNS11000000"]) %>%
  filter(!is.na(temp_layoff))

cps_jobs_data %>% filter(series_id == "LNS14000000") %>% select(date, urate = value) %>%
  mutate(urate = urate/100) %>%
  left_join(on_temporary_layoff, by="date") %>%
  mutate(u_excluding_temp = urate - temp_layoff) %>%
  mutate(diff_urate_excluding = log(u_excluding_temp)-log(lag(u_excluding_temp,12))) %>%
  mutate(diff_urate = log(urate)-log(lag(urate,12))) %>%
  ggplot(aes(date,diff_urate)) + geom_line() + geom_line(aes(date,diff_urate_excluding),color="green") + theme_classic() + geom_hline(yintercept = -0.1, color="red") +
  labs(title="Do you know why I pulled you over? You were way over the speed limit.",
       subtitle="log of unemployment rate minus the log of unemployment rate lagged 12 months. Red line is -0.1.\nGreen line is excluding 'Job losers and persons who completed temporary jobs - On temporary layoff'")

ggsave("graphics/speed_limit2.png",  width = 12, height=6.75, dpi="retina")



recession_starts <- c("1945-02-01",
                      "1948-11-01",
                      "1953-07-01",
                      "1958-08-01",
                      "1960-04-01",
                      "1969-12-01",
                      "1973-11-01",
                      "1980-01-01",
                      "1981-07-01",
                      "1990-07-01",
                      "2001-03-01",
                      "2007-12-01",
                      "2020-02-01")
recession_starts <- as.Date(recession_starts)



data <- cps_jobs_data %>% filter(series_id == "LNS12300060") %>%
  select(date,value = value)


# Import the data

# Define a function that takes in the data and the start date of a recession, and returns the number of months it takes to return to the pre-recession employment to population ratio
months_to_recovery <- function(data, recession_start_date){
  pre_recession_erp <- data %>%
    filter(date < recession_start_date) %>%
    filter(date == max(date))
  pre_recession_erp <- pre_recession_erp$erp
  
  post_recession_data <- data %>%
    filter(date >= recession_start_date)
  
  recovery_date <- post_recession_data %>%
    filter(erp >= pre_recession_erp) %>%
    filter(date == min(date))
  recovery_date <- recovery_date$date
  
  if(is.na(recovery_date)){
    return(NA)
  } else {
    x=interval(ymd(recession_start_date),ymd(recovery_date))
    x= x %/% months(1)
    return(x)
  }
}

months_to_recovery(data,"2007-12-01")

# Apply the function to every recession start date and store the results in a data frame
recession_dates <- c("1945-02-01", "1948-11-01", "1953-07-01", "1958-08-01", "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")
recession_dates <- as.Date(recession_dates)
recovery_times <- data.frame(recession_start_date = ymd(recession_dates), months_to_recovery = NA)
for(i in 1:nrow(recovery_times)){
  recovery_times$months_to_recovery[i] <- months_to_recovery(data, recovery_times$recession_start_date[i])
}

# Print the results
recession_dates_all <- read_csv("data/recession_dates.csv")
recession_dates <- recession_dates_all$end_date

#unemployment rate
data <- cps_jobs_data %>% filter(series_id == "LNS14000000") %>%
  select(date,value = value)


a <- data %>% mutate(at_period = date %in% recession_dates) %>%
  mutate(period_title = as.character(year(date))) %>%
  mutate(period_title = if_else(at_period, period_title, as.character(NA)))

data$within_period <- as.Date(NA)
for(i in 1:length(recession_dates)){
  data <- data %>%
    mutate(within_period_holder = date >= recession_dates[i] & date < recession_dates[i] %m+% months(36)) %>%
    mutate(within_period = if_else(within_period_holder,recession_dates[i],within_period))
  
}

data$within_period <- as.factor(year(data$within_period))
data <- data[!is.na(within_period)]

data <- data %>% group_by(within_period) %>%
  mutate(x=interval(ymd(min(date)),date)) %>%
  mutate(x= x %/% months(1)) %>%
  mutate(diff_value = value-value[date == min(date)]) %>%
  ungroup()



data %>% ggplot(aes(x,diff_value, color=within_period)) + geom_line() + facet_wrap(~within_period)

# Example list of dates
date_list <- c("2022-05-10", "2022-06-15", "2022-07-20")

# Example array of all dates
all_dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")

# Create a variable with the last date in the list for each date in the array
last_dates <- sapply(all_dates, function(date) {
  subset_dates <- as.Date(date_list)[as.Date(date_list) <= date]
  if (length(subset_dates) > 0) {
    as.Date(max(subset_dates))
  } else {
    NA
  }
})

# Format the last dates as character strings
formatted_dates <- format(last_dates, "%Y-%m-%d")

# Print the last dates
print(formatted_dates)

##### CES BY RACE ####
LNS12000003
b <- cps_jobs_data %>% filter(date == "2023-01-01", seasonal == "U")
LNU02000003\
LNU12000003

a <-
  cps_jobs_data %>% filter(series_id %in% c("LNS12000003","LNU02000003", "LNU02000006")) %>%
  filter(date >= "2020-01-01") %>%
  group_by(series_title) %>%
  summarize(date = date, diff = value - value[date == "2020-01-01"]) %>%
  ungroup() %>%
  ggplot(aes(date, diff, color=series_title)) + geom_line() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -900) +
  theme_classic() +
  theme(legend.position=c(0.7,0.3)) + labs(subtitle="Difference Between Month and Feburary 2020. Mike Konczal, Roosevelt Institute.")

ggsave("graphics/race_recovery.png",  width = 12, height=6.75, dpi="retina")

b <- 
  cps_jobs_data %>% filter(series_id %in% c("LNS12000000","LNU02000000")) %>%
  filter(date > "2020-01-01") %>%
  group_by(series_title) %>%
  summarize(date = date, diff = value - value[date == "2020-02-01"]) %>%
  ungroup()



compare_2019 <- ces_data %>% filter(seasonal == "S", industry_name == "Newspaper, periodical, book, and directory publishers", date == "2022-01-01")

View(as_tibble(unique(compare_2019)))


ces_data %>% filter(seasonal == "S", data_type_code == 1, industry_name == "Newspaper, periodical, book, and directory publishers") %>%
  ggplot(aes(date,value)) + geom_line() + labs(subtitle="All employment, 'Newspaper, periodical, book, and directory publishers,' in thousands. BLS, CES.") + theme_classic()

programmers <- cps_jobs_data %>% filter(series_id == "LNU02038230")

news <- cps_jobs_data %>% filter(occupation_text == "News analysts, reporters, and journalists")

programmers %>%
  ggplot(aes(date, value)) + geom_line() + geom_point() + labs(subtitle="All employment, annual, occupation: computer programmers, BLS CPS.") + theme_classic()

cps_jobs_data %>% filter(series_id == "LNU02038313") %>%
  ggplot(aes(date, value)) + geom_line() + geom_point() + labs(subtitle="All employment, annual, occupation: News analysts, reporters, and journalists, BLS CPS.") + theme_classic()

unique(cps_jobs_data$series_title)[grep("omputer", unique(cps_jobs_data$series_title))]

a <- cps_jobs_data %>% filter(str_detect(series_title, "Employed - Computer programmers"))

%>% group_by(industry_name) %>%
  summarize(before = value[date == "2019-12-01"],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before,
         months_category = "Since End of 2019",
         difference_label = round(difference),
         positive = difference_label >= 0) %>%
  ungroup()