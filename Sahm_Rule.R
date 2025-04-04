library(scales)
library(zoo)
library(tidyverse)
library(govMacroTools)
library(lubridate)
library(viridis)



# Generate 100 dates, starting from the maximum date in the input vector and going back X months at a time
generate_dates <- function(dates, X) {
  max_date <- max(dates)
  generated_dates <- seq(max_date, length.out = 100, by = paste0("-", X, " months"))
  return(generated_dates)
}

make_Sahm <- function(df) {
  # Make sure your data are sorted by date
  df <- df[order(df$date), ]
  
  # 1. Compute the 3-month moving average of the unemployment rate.
  #    This moving average is aligned to the right (i.e. the average of the current and previous two months).
  df$unemp_ma3 <- rollapply(
    df$unemployment,
    width = 3,
    FUN = mean,
    align = "right",
    fill = NA
  )
  
  # 2. For each month, compute the minimum 3-month moving average over the past 12 months.
  #    Again, aligned to the right (so it considers the current month and the previous 11 months).
  df <- df %>%
    mutate(
      unemp_ma3_lag1 = lag(unemp_ma3, 1),
      min_ma12 = rollapply(
        unemp_ma3_lag1,
        width = 12,
        FUN = min,
        align = "right",
        fill = NA
      )
    )
  
  # 3. Calculate the difference between the current 3-month moving average and the minimum of the last 12 months.
  df$diff <- df$unemp_ma3 - df$min_ma12
  
  # 4. Check whether the Sahm Rule is triggered: diff >= 0.5
  df$sahm_trigger <- df$diff >= 0.5
  
  # 5. Optionally, calculate how many percentage points are still needed to trigger the rule.
  #    (If the value is negative, the threshold hasn't been reached yet.)
  df$gap_to_trigger <- 0.5 - df$diff
  
  return(df)
}

##### TRUMP LAYOFFS ####
df <- getFRED(c("CLF16OV","UNEMPLOY"), rename_variables = c("lf_level","ur_level"))
df <- df %>% mutate(unemployment = ur_level/lf_level) 

df <- make_Sahm(df)
df$regular_Sahm <- df$diff

df <- df %>% mutate(ur_level = if_else(date == max(date), ur_level + 200, ur_level))
df <- df %>% mutate(unemployment = ur_level/lf_level)
df <- make_Sahm(df)
df$layoff_Sahm <- df$diff


df %>%
  select(date, regular_Sahm:layoff_Sahm) %>%
  pivot_longer(regular_Sahm:layoff_Sahm, names_to = "type", values_to = "value") %>%
  filter(year(date)> 2022) %>%
# mutate(type = case_when(
#    type == "regular_Sahm"    ~ "a. Current Sahm Difference",
#    type == "layoff_Sahm"   ~ "b. 200K More Unemployed",
#    TRUE                            ~ type  # fallback in case of unexpected values
#  )) %>%
  ggplot(aes(date, value, color=type)) + geom_line(size = 1.5) +
  geom_hline(yintercept = 0.005) +
  scale_y_continuous(labels = percent) +
  theme_classic(base_size = 18) +
  scale_color_brewer(palette="Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size=14)) +
  labs(
    title    = "Progress But Still Near a Threshold",
    subtitle = "3-Month Average of Unemployment Minus Lowest Unemployment Over Past Year; Recession Prediction at 0.5%",
    x        = "",
    y        = "",
    caption  = "Dividing labor force and uemployed levels for extra decimals. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(df$date, 12),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle          = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.3,0.8)
  )




##### REVISIONS CODE #####

df <- getFRED(c("CLF16OV","UNEMPLOY"), rename_variables = c("lf_level","ur_level"))
df <- df %>% mutate(unemployment = ur_level/lf_level) #%>% select(date, unemployment)

df <- read_csv("sahm_rule.csv")

df <- df %>%
  mutate(unemployment_old = UNEMPLOY_20241206/CLF16OV_20241206,
         unemployment_new = UNEMPLOY_20250206/CLF16OV_20250206) %>%
  rename(date = observation_date)

df$unemployment <- df$unemployment_old
df <- make_Sahm(df)
df$old_Sahm <- df$diff
df$unemployment <- df$unemployment_new
df <- make_Sahm(df)
df$new_Sahm <- df$diff

df <- df %>%
  select(date, old_Sahm, new_Sahm) %>%
  pivot_longer(old_Sahm:new_Sahm, values_to = "value", names_to = "type") %>%
  filter(!is.na(value))


df %>%
  filter(year(date)> 2022) %>%
  mutate(type = case_when(
    type == "new_Sahm"    ~ "Current Sahm Difference",
    type == "old_Sahm"   ~ "Dec 2024 Sahm Difference",
    TRUE                            ~ type  # fallback in case of unexpected values
  )) %>%
  ggplot(aes(date, value, color=type)) + geom_line(size = 1.5) +
  geom_hline(yintercept = 0.005) +
  scale_y_continuous(labels = percent) +
  theme_classic(base_size = 18) +
  scale_color_brewer(palette="Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size=14)) +
  labs(
    title    = "Was Triggering the Sahm Rule Just a Dream?",
    subtitle = "3-Month Average of Unemployment Minus Lowest Unemployment Over Past Year; Recession Prediction at 0.5%",
    x        = "",
    y        = "",
    caption  = "Dividing labor force and uemployed levels for extra decimals. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(df$date, 12),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle          = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.8,0.5)
  )

ggsave("tester2.png", width = 12, height = 6, units = "in", dpi = "retina")



####### Just regular ######

df <- getFRED(c("CLF16OV","UNEMPLOY","UNRATE"), rename_variables = c("lf_level","ur_level", "unrate"))
df <- df %>% mutate(unemployment = ur_level/lf_level) %>% select(date, unemployment = unrate)

df <- make_Sahm(df)
tail(df,1)

df %>%
  filter(year(date)> 2022) %>%
  ggplot(aes(date, diff)) + geom_line(size = 1.5) +
  geom_hline(yintercept = 0.005) +
  scale_y_continuous(labels = percent) +
  theme_classic(base_size = 18) +
  scale_color_brewer(palette="Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size=14)) +
  labs(
    title    = "Was Triggering the Sahm Rule Just a Dream?",
    subtitle = "3-Month Average of Unemployment Minus Lowest Value Over Past Year; Recession Prediction at 0.5%",
    x        = "",
    y        = "",
    caption  = "Dividing labor force and uemployed levels for extra decimals. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(df$date, 12),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle          = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.8,0.5)
  )

ggsave("tester3.png", width = 12, height = 6, units = "in", dpi = "retina")


df <- getFRED(c("CLF16OV","UNEMPLOY","UNRATE"), rename_variables = c("lf_level","ur_level", "unrate"))
df <- df %>% mutate(unemployment = ur_level/lf_level)

df$unemployment <- df$unrate



a <- df
a$ur <- a$unemployment

a <- a %>%
  mutate(last_three_months = (ur + lag(ur,1) + lag(ur,2))/3) %>%
  mutate(lowest_previous = rollapply(last_three_months, width = 13, FUN = min, align = "right", fill = NA)) %>%
  mutate(diff = last_three_months - lowest_previous)
tail(a)
