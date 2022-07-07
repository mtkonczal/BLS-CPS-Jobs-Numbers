setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")

library(tidyverse)
library(ggtext)

#source(file = "1_b_load_bls_ces_jobs_data.R")

ces_data %>% filter(date > "2019-01-01") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap( ~ supersector_name)

table(ces_data$seasonal)

a$value