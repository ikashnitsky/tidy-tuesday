#===============================================================================
# 2019-07-12 -- TidyTuesday
# Data on UFO sightings ,2019-06-25
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# load required packages
library(tidyverse)
library(janitor)
library(sf)
library(lubridate)
library(magrittr)
library(hrbrthemes); import_roboto_condensed()


# the data
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# create time variables using lubridate
df <- ufo_sightings %>% 
        mutate(
                date_time = date_time %>% parse_date_time('mdy_HM'),
                month = date_time %>% month(label = TRUE) %>% as.factor(),
                year = date_time %>% year(),
                day = date_time %>% day(),
                week = date_time %>% week(),
                wday = date_time %>% wday(week_start = 1, label = TRUE),
                date_documented = date_documented %>% parse_date_time('mdy'),
                gap_doc = as.duration(date_documented - date_time) / 
                        as.duration(years(1))
        )


# calendar plot of sighting tolls by day

df %>% 
        filter(year %in% 2000:2013) %>% 
        group_by(year, day) %>% 
        mutate(cases = n()) %>% 
        ggplot(aes(week, wday, fill = cases))+
        geom_tile()+
        scale_fill_viridis_c("# UFO\nsightings\nper day",option = "B")+
        scale_x_continuous(breaks = c(1:9, 20, 30, 52))+
        facet_grid(year~month, scales = "free")+
        theme_minimal(base_family = font_rc)+
        theme(panel.grid = element_blank())+
        labs(x = "week of the year", y = NULL,
             title = "Numbers of reported UFO sightings in the World",
             caption = "Data: The National UFO Reporting Center, #TidyTuesday | Dataviz: @ikashnitsky")+
        theme(axis.text.y = element_text(size = 7),
              plot.title = element_text(size = 30, face = 2))

gg <- last_plot()

# save
ggsave("2019-06-25-ufo-sightings/tidytuesday-ikashnitsky-ufo-sightings.pdf", width = 10, height = 10, device = cairo_pdf)
ggsave("2019-06-25-ufo-sightings/tidytuesday-ikashnitsky-ufo-sightings.png",  gg, width = 10, height = 10, type  = "cairo")
