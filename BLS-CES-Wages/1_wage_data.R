# Mike Konczal

setwd("/Users/mkonczal/Documents/GitHub/BLS-CES-Wages/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)

#### CES EMPLOYMENT ########


AllEmployeeList <- c(
"ce.data.05b.TotalPrivate.AllEmployeeHoursAndEarnings",
"ce.data.10b.MiningAndLogging.AllEmployeeHoursAndEarnings",
"ce.data.20b.Construction.AllEmployeeHoursAndEarnings",
"ce.data.30b.Manufacturing.AllEmployeeHoursAndEarnings",
"ce.data.31b.ManufacturingDurableGoods.AllEmployeeHoursAndEarnings",
"ce.data.32b.ManufacturingNondurableGoods.AllEmployeeHoursAndEarnings",
"ce.data.40b.TradeTransportationAndUtilities.AllEmployeeHoursAndEarnings",
"ce.data.41b.WholesaleTrade.AllEmployeeHoursAndEarnings",
"ce.data.42b.RetailTrade.AllEmployeeHoursAndEarnings",
"ce.data.43b.TransportationAndWarehousingAndUtilities.AllEmployeeHoursAndEarnings",
"ce.data.50b.Information.AllEmployeeHoursAndEarnings",
"ce.data.55b.FinancialActivities.AllEmployeeHoursAndEarnings",
"ce.data.60b.ProfessionalBusinessServices.AllEmployeeHoursAndEarnings",
"ce.data.65b.EducationAndHealthCare.AllEmployeeHoursAndEarnings",
"ce.data.70b.LeisureAndHospitality.AllEmployeeHoursAndEarnings",
"ce.data.80b.OtherServices.AllEmployeeHoursAndEarnings")

loadAllEmployeeList <- function(base_data, addition_name){
    location <- paste("https://download.bls.gov/pub/time.series/ce/", addition_name, sep="")
    addition_data <- read_delim(location)
    if(is.na(base_data))
      return(addition_data)
    base_data <- rbind(base_data, addition_data)
    return(base_data)
}

ces_wages_data <- NA
for (x in 1:length(AllEmployeeList)) {
  ces_wages_data <- loadAllEmployeeList(ces_wages_data, AllEmployeeList[x])
}

ces_wages_data <- ces_wages_data %>%
  clean_names()
ces_wages_data$series_id <- str_trim(ces_wages_data$series_id)
ces_wages_data$value <- as.numeric(ces_wages_data$value)
ces_wages_data$date <- paste(substr(ces_wages_data$period, 2,3), "01", ces_wages_data$year, sep="/")
ces_wages_data$date <- as.Date(ces_wages_data$date, "%m/%d/%Y")

ces_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.series")
ces_series <- ces_series %>% 
  clean_names()
ces_series$series_id <- str_trim(ces_series$series_id)

ces_wages_data_type <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.datatype")
ces_super_sector <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.supersector")
ces_industry_code <- read_delim(file = "https://download.bls.gov/pub/time.series/ce/ce.industry")
ces_series <- inner_join(ces_series, ces_wages_data_type, by = "data_type_code")
ces_series <- inner_join(ces_series, ces_super_sector, by = "supersector_code")
ces_series <- inner_join(ces_series, ces_industry_code, by = "industry_code")

ces_wages_data <- inner_join(ces_wages_data, ces_series, by = c("series_id"))

#save(ces_wages_data, file = "data/ces_wages_data.RData")
######################################################################

ces_wages_data %>% filter(date == "2022-05-01", data_type_code=="03") %>% group_by(display_level) %>% summarize(count = n())

ces_wages_data %>% filter(display_level==1, data_type_code=="11", industry_name=="Total private", seasonal=="S") %>%
  mutate(value = value/lag(value,3)-1) %>%
  ggplot(aes(date,value)) + geom_line()

# STARTER GRAPHIC - PROOF OF CONCEPT

months_GR <- interval(ymd("2016-01-01"), "2020-01-01")
months_GR = months_GR %/% months(1)

months_2021 <- interval(ymd("2020-12-01"), ymd("2021-12-01"))
months_2021 = months_2021 %/% months(1)

months_2022 <- interval(ymd("2021-12-01"), max("2022-05-01"))
months_2022 = months_2022 %/% months(1)

months_recovery <- interval(ymd("2020-12-01"), max("2022-05-01"))
months_recovery = months_recovery %/% months(1)

a <- ces_wages_data %>% filter(seasonal == "S", data_type_code == "03", display_level == 5)  %>%
  group_by(industry_code) %>%
  mutate(start_GR = value[date=="2016-01-01"]) %>%
  mutate(end_GR = value[date=="2020-01-01"]) %>%
  mutate(start_2021 = value[date=="2020-12-01"]) %>%
  mutate(end_2021 = value[date=="2021-11-01"]) %>%
  mutate(end_2022 = value[date=="2022-05-01"]) %>%
  mutate(rate_GR = (end_GR/start_GR)^(1/(months_GR/12)) - 1) %>%
  mutate(rate_2021 = (end_2021/start_2021)^(1/(months_2021/12)) - 1) %>%
  mutate(rate_2022 = (end_2022/end_2021)^(1/(months_2022/12)) - 1) %>%
  mutate(rate_recovery = (end_2022/end_2021)^(1/(months_recovery/12)) - 1) %>%
  ungroup()

a %>% arrange(industry_name, date) %>% filter(date > "2015-12-01") %>% write.csv("tester.csv")

ggplot(a, aes(rate_2021, rate_GR)) + geom_point() + theme_classic()  + geom_abline(slope=1, intercept=0) + geom_smooth(method="lm")


cc <- a %>% filter(date == "2022-05-01") %>% mutate(decreased_2022 = rate_2022 - rate_2021) %>% arrange(decreased_2022)


ces_wages_data %>% filter(series_id == "CES6056174003", year> 2019) %>%
  ggplot(aes(date, value)) + geom_line()

b <- a %>% mutate(increased_2021 = rate_2021 - rate_GR, decreased_2022 = rate_2022 - rate_2021) %>% filter(date == "2022-05-01")

a %>% mutate(increased_2021 = rate_2021 - rate_GR, decreased_2022 = rate_2022 - rate_2021) %>% filter(date > "2021-01-01") %>%
  filter(increased_2021 > quantile(increased_2021, 0.97)) %>%
  ggplot(aes(date, value, color=industry_name)) + theme_classic() + geom_line()


b %>% filter(date == "2022-05-01") %>%
ggplot(aes(increased_2021, decreased_2022)) + geom_point(size=2.5, color="darkblue") + theme_classic() + geom_smooth(method="lm") +
  labs(x = "Difference in Earnings Growth Rate between 2021 and 2016-2019",
       y = "Difference between 2022 and 2021",
       title = "Sub-industries With Faster 2021 Hourly Wage Increases Saw Larger Decelerations in 2022",
       subtitle = "Difference between annualized growth rate of average hourly earnings, 2021 vs 2016-2019 (x-axis) and 2022 vs 2021 (y-axis)",
       caption ="Average hourly earnings of all employees, 187 sub-industry values with display rank 5 or greater.
       BLS, CES, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme(title = element_text(face="plain", size=26),
        plot.subtitle = element_text(size=23, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=15, lineheight = 1.05, margin=margin(9,0,15,0)),
        axis.text=element_text(size=20, margin=margin(9,0,15,0)))

ggsave("wage1.png", width = 19, height=10.68, dpi="retina")

b %>% filter(date == "2022-05-01") %>%
  ggplot(aes(increased_2021, decreased_2022, size=end_2022)) + geom_point(color="darkblue") + theme_classic() + geom_smooth(method="lm") +
  labs(x = "Difference in Earnings Growth Rate between 2021 and 2016-2019",
       y = "Difference between 2022 and 2021",
       title = "Sub-industries With Faster 2021 Hourly Wage Incrases Saw Larger Decelerations in 2022",
       subtitle = "Difference between annualized growth rate of average hourly earnings, 2021 vs 2016-2019 (x-axis) and 2022 vs 2021 (y-axis)",
       caption ="Average hourly earnings of all employees, 187 sub-industry values with display rank 5 or greater.
       BLS, CES, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme(title = element_text(face="plain", size=26),
        plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=15, lineheight = 1.05), axis.text=element_text(size=20)) +
  theme(legend.position='none')

ggsave("wage2.png", width = 19, height=10.68, dpi="retina")


ces_wages_data %>% filter(industry_name %in% c("Automobile dealers", "Total private"), seasonal == "S", data_type_code == "03") %>%
ggplot(aes(date,value, color=industry_name)) + geom_line()



ccc <- as.list(cc)
b %>% filter(industry_name %in% ccc)


ggsave("reversal.png", dpi="retina", width = 12, height=6.75, units = "in")
b
bb <- lm(decreased_2022 ~ increased_2021, data=b)
summary(bb)

bb <- lm(decreased_2022 ~ increased_2021, weights = end_2022, data=b)
summary(bb)
ggplot(b, aes(rate_GR, end_GR)) + geom_point() + geom_smooth(method="lm")

c <- b %>% mutate(increased_2021 = increased_2021*start_2021, decreased_2022 = decreased_2022*end_2021)
ggplot(c, aes(increased_2021, decreased_2022)) + geom_point() + theme_classic() + geom_smooth(method="lm")
cc <- lm(decreased_2022 ~ increased_2021, data=c)
summary(cc)


ggplot(a, aes(log(end_GR), log(end_2022))) + geom_point() + theme_classic()  + geom_abline(slope=1, intercept=0) + geom_smooth(method="lm")
ggplot(a, aes(rate_2022, rate_2021)) + geom_point() + theme_classic() + geom_abline(slope=1, intercept=0)
ggplot(a, aes(rate_recovery*end_2022, rate_GR*end_GR)) + geom_point() + theme_classic() + geom_smooth(method="lm")


a %>% summarize(smaller = sum(rate_2022 < rate_2021)/n())

b <- lm(rate_recovery ~ rate_GR, data=a)
summary(b)

c <- lm(rate_2021 ~ rate_GR, data=a)
summary(c)

c <- lm(rate_2022 ~ rate_2021, data=a)
summary(c)


a <- a %>% mutate(v1 = rate_recovery*end_2022, v2 = rate_GR*end_GR, v3 = rate_2021*end_2021)


a %>% arrange(rate_2022)


tester <- ces_wages_data %>% filter(seasonal == "S", data_type_code == "03", display_level == 1)  %>%
  group_by(industry_code) %>%
  mutate(start_GR = value[date=="2016-01-01"]) %>%
  mutate(end_GR = value[date=="2020-01-01"]) %>%
  mutate(start_2021 = value[date=="2020-12-01"]) %>%
  mutate(end_2021 = value[date=="2021-11-01"]) %>%
  mutate(end_2022 = value[date=="2022-05-01"]) %>%
  mutate(rate_GR = (end_GR/start_GR)^(1/(months_GR/12)) - 1) %>%
  mutate(rate_2021 = (end_2021/start_2021)^(1/(months_2021/12)) - 1) %>%
  mutate(rate_2022 = (end_2022/end_2021)^(1/(months_2022/12)) - 1) %>%
  mutate(rate_recovery = (end_2022/end_2021)^(1/(months_recovery/12)) - 1) %>%
  ungroup()


##### TRY AGAIN
max_date <- ces_wages_data %>% filter(!is.na(date)) %>% select(date)
max_date <- max(max_date$date) %m-% months(1)

months_2021 <- interval(ymd("2021-01-01"), ymd("2022-07-01"))
months_2021 = months_2021 %/% months(1)

months_2022 <- interval(ymd("2022-07-01"), max_date)
months_2022 = months_2022 %/% months(1)

a <- ces_wages_data %>% filter(seasonal == "S", data_type_code == "03", display_level == 5)  %>%
  group_by(industry_name) %>%
  summarize(start_2021 = value[date=="2021-01-01"], end_2021 = value[date=="2022-07-01"], end_2022 = value[date==max_date], supersector_code = supersector_code) %>%
  mutate(rate_2021 = (end_2021/start_2021)^(1/(months_2021/12)) - 1) %>%
  mutate(rate_2022 = (end_2022/end_2021)^(1/(months_2022/12)) - 1) %>%
  ungroup()


a %>%
  ggplot(aes(rate_2021, rate_2022)) + geom_point() + theme_classic() + geom_smooth(method = "lm") + facet_wrap(~super)
a

b <- lm(rate_2022 ~ rate_2021 + supersector_code, data=a)
summary(b)