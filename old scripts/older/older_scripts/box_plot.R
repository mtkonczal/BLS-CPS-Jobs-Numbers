
#source("../BLS-CPS-Jobs-Numbers/1_b_load_bls_ces_jobs_data.R")
ces_bp <- ces_data %>% filter(series_id == "CES0000000001") %>% mutate(job_growth = 1000*(value - lag(value,1))) %>%
  #mutate(Great_Recession = date > "2009-01-01" & date < "2019-01-01") %>% mutate(covid_recovery = date >= "2021-01-01") %>%
  filter(date > "2009-12-01", date < "2017-01-01" | date == max(date)) %>% mutate(is_pandemic = (date > "2020-12-01"))

ces_bp$is_pandemicT <- as.character(ces_bp$is_pandemic)
ces_bp$is_pandemicT <- str_replace(ces_bp$is_pandemicT, "FALSE", "Great Recession,\n2010-2016")
ces_bp$is_pandemicT <- str_replace(ces_bp$is_pandemicT, "TRUE", "July, 2022")
ces_bp$is_pandemicT <- factor(ces_bp$is_pandemicT, levels=c("Great Recession,\n2010-2016","July, 2022"))

ggplot(ces_bp, aes(x=is_pandemicT, job_growth)) + geom_boxplot(fill="skyblue") + theme_classic() +
  theme(plot.title.position = "plot", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15), plot.title = element_text(size=28),
        plot.subtitle = element_text(size=15), plot.caption = element_text(size=10)) +
  labs(x="", y="", subtitle="The Single Large Outlier During the Great Recession's Recovery is Just Above Last Month's Job Number",
       title="Boxplot of Monthly Payroll Jobs Number Increases", caption="CES, seasonally adjusted. Author's Calculation. Mike Konczal, Boxplots Guy, Roosevelt Institute") +
  scale_y_continuous(labels = comma) +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())

ggsave("graphics/boxplots.png", width = 19, height=10.68, dpi="retina")


#  scale_y_continuous(labels = scales::comma()) +
ces_bp %>% filter(is_pandemic) %>% summarize(median(job_growth))
ces_bp %>% filter(!is_pandemic) %>% summarize(max(job_growth))

tail(ces_bp$job_growth)

ces_bp %>% group_by(is_pandemic) %>% summarize(max(job_growth))