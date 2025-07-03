# Script to learn how to start our BLS routine

# SET THIS NEXT LINE TO YOUR WORKING DIRECTORY
setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")

library(tidyverse)
library(ggtext)
library(lubridate)
library(scales)
library(ggrepel)

### RUN THE NEXT LINE ONCE TO LOAD THE DATA, IT WILL TAKE A FEW MINUTES
#source(file = "1_a_load_bls_cps_jobs_data.R")


### HERE IS A LIST OF SERIES IDs FOR THINGS YOU ARE INTERESTED IN
### I FOUND THESE BY GOING INTO THE FILE CREATED ABOVE
#(Seas) Unemployment Rate LNS14000000
#(Seas) Unemployment Rate - Men LNS14000001
#(Seas) Unemployment Rate - Women LNS14000002
#(Seas) Unemployment Rate - White LNS14000003
#(Seas) Unemployment Rate - White Men LNS14000004
#(Seas) Unemployment Rate - White Women LNS14000005
#(Seas) Unemployment Rate - Black or African American LNS14000006
#(Seas) Unemployment Rate - Black or African American Men LNS14000007
#(Seas) Unemployment Rate - Black or African American Women LNS14000008
#(Seas) Unemployment Rate - Hispanic or Latino LNS14000009

# PICKED SOME TO EXAMINE IN A GRAPHIC
urate_categories <- c("LNS14000004", "LNS14000005", "LNS14000007", "LNS14000008", "LNS14000009")
# PICKED A DATE TO START THE GRAPHIC FROM
start_date <- "2021-01-01"

# MAKE OUR DATA SET
# U is THE CPS DATA SET, FIRST FILTERED FOR SEASONAL, MONTHLY DATA
U <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>%
  # THEN FILTERED FOR DATA THAT IS IN THE CATEGORIES YOU SET ABOVE
  filter(series_id %in% urate_categories) %>%
  # THEN FILTERED FOR DATES AFTER YOUR START DATE
  filter(date >= start_date) %>%
  # THEN, SINCE THE UNEMPLOYMENT RATE IS TREATED AS AN INTERGER, DIVIDED BY 100 TO GET THE PERCENTAGE
  mutate(value_percent = value/100)

# WE CLEAN UP THE SERIES TITLE SO IT WILL BE IN THE GRAPHIC BETTER
U$series_title <- str_replace_all(U$series_title, "\\(Seas\\) Unemployment Rate - ", "")

# LET'S LOOK AT SOME ANALYSIS
# WHAT STORIES IS THIS TELLING US?
U %>% group_by(series_title) %>% summarize(start_value = value[date==start_date],
                                           end_value = value[date==max(date)],
                                           change = value[date == start_date] - value[date==max(date)],
                                           own_percent_change = value[date == start_date]/value[date==max(date)]) %>%
  arrange(desc(start_value))


# WE ARE GOING TO MAKE THIS GRAPHIC IN THREE STEPS. FIRST IS THE CORE, WE CHOOSE THE DATA,
# THEN THE AESTHETIC MAPPING (aes) OF DATE TO X AXIS AND VALUE TO Y,
# AND THEN GIVE EACH SERIES WE PICKED IT'S OWN COLOR AND OWN LINE
# THEN WE ASSIGN A LINE GEOMETRY (geom) AND IT MAKES THE GRAPHIC.
# THE NEXT LINE TAKES YOUR PLOT OBJECT AND MOVES THE LEGEND, THE NEXT LINE DISPLAYS IT
your_plot <- ggplot(U, aes(x = date, y = value_percent, color=series_title)) + geom_line(size = 1.2) + theme_classic()
your_plot <- your_plot + theme(legend.title = element_blank(), legend.position = "bottom") + scale_y_continuous(labels = scales::percent)

your_plot

# WE THEN ADD CAPTIONS
your_plot <- your_plot + labs(x="",
                 y="",
                 title="The Unemployment Rate is Going Down, But Who Benefits Most?",
                 subtitle="We must examine this closely",
                 caption = "Data: BLS, CPS. Seasonally adjusted. Author's calculations") +
              theme(plot.title = element_markdown(size = 30, face="bold"),
                    plot.subtitle = element_markdown(size = 20, margin=margin(9,0,15,0)))
your_plot

# WE THEN SAVE IT WITH PROPORTIONS FOR SOCIAL MEDIA
ggsave("your_graphic.png", width = 19, height=10.68, dpi="retina")

# I add a new column "last_value" which is the percent value times a logical statement
# that is 1 if it's the maximum date or April 21, and 0 if it's any other date
# So this column only has a non-zero value if it's the maximum date (see what it looks like)
# You can change the logical statement to be whatever dates you want
U <- U %>% mutate(last_value = value_percent*(date == max(date) | date == "2021-04-01")) %>%
  mutate(last_value = round(last_value,))
# I then run a function that replaces 0s with "NA" so ggplot will ignore those values
U$last_value <- na_if(U$last_value, 0)

U$last_value


your_plot <- ggplot(U, aes(x = date, y = value_percent, color=series_title)) + geom_line(size = 1.2)
your_plot <- your_plot + theme(legend.title = element_blank(), legend.position = "bottom") + scale_y_continuous(labels = scales::percent) + scale_x_date(date_labels = "%b-%y", breaks = "1 month") + geom_point (color = "black", size = 1)
# To the call that adds the labels, have it add "last value" instead, so it adds only where it's dates you want.
your_plot <- your_plot + geom_label_repel (aes (y = last_value, label = scales::percent (value_percent,accuracy=0.1)), show.legend = FALSE)
your_plot


# WE THEN ADD CAPTIONS
your_plot <- your_plot + labs(x=“”,
                              y=“”,
                              title=“The Unemployment Rate is Going Down - Black and Latino persons benefit the most”,
                              subtitle=“Black and Latino persons also experience the highest rate of unemployment during the recession”,
                              caption = “Data: BLS, CPS. Seasonally adjusted. Author’s calculations”) +
  theme(plot.title = element_markdown(size = 30, face=“bold”),
        plot.subtitle = element_markdown(size = 20, margin=margin(9,0,15,0)))
your_plot



