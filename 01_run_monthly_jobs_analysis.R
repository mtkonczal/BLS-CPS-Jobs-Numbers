library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
source("scripts/03_graphic_scripts.R")

make_date <- function(x){
  x$date <- as.Date(paste(x$month, "01", x$year, sep="/"), "%m/%d/%Y")
  return(x)
}

ces_data <- getBLSFiles("ces", "rortybomb@gmail.com")
cps_jobs_data <- getBLSFiles("cps", "rortybomb@gmail.com")

ces_data$data_type_code_org <- ces_data$data_type_code
ces_data$data_type_code <- as.numeric(ces_data$data_type_code)


source("scripts/03_graphic_scripts.R")

three_six_wages_title <- "Average Hourly Earnings Remain Volatile"
three_six_wages(ces_data, three_six_wages_title)
ggsave("graphics/wages_3_6.png", dpi = "retina", width = 12, height = 8, units = "in")

total_title <- "Job Growth In Recent Months"
total_jobs_graphic(ces_data, total_title)
ggsave("graphics/total_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")

make_jobs_chart(ces_data)



unemployment_rate_by_type(cps_jobs_data, graphic_title = "Job Losers and New Entrants Drive Unemployment Increase", start_date = "2022-01-01", axis_months = 6)
ggsave("graphics/u_by_type.png", dpi = "retina", width = 12, height = 6.75, units = "in")