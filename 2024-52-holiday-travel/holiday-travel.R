#===============================================================================
# 2024-12-29 -- TidyTuesday
# week 52 - Holiday Travel
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# Lai S, Sorichetta A, Steele J, Ruktanonchai CW, Cunningham AD, Rogers G, Koper P, Woods D, Bondarenko M, Ruktanonchai NW, et al. 2022. Global holiday datasets for understanding seasonal human mobility and population dynamics. Scientific Data 9: 17 DOI: 10.1038/s41597-022-01120-z

# My objective is to try using hrbrmstr/imago to represent the world in pseudo AuthaGraph projection

library(tidyverse)
library(sf)
# pak::pak("hrbrmstr/imago")
library(imago)
library(countrycode)

# get data for the week
tuesdata <- tidytuesdayR::tt_load(2024, week = 52)

global_holidays <- tuesdata$global_holidays  |> 
    janitor::clean_names()

# calculate the number of public holidays by country
n_holidays <- global_holidays |> 
    # get for one year only, say 2018
    mutate(year = date |> year()) |> 
    # filter only public hilidays
    filter(
        type == "Public holiday",
        year == 2018
    ) |> 
    # tally holidays
    group_by(iso3c = iso3) |> 
    summarise(n_pub_holidays = n()) |> 
    ungroup()

# get country outlines
geo <- imago()

# unite 
df <- geo |> left_join(n_holidays) 

# custom theming
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
theme_set(theme_ik(base_family = "ah", base_size = 11))

# plot
df |> 
    ggplot(aes(fill = n_pub_holidays))+
    geom_sf(color = "#ffffff", linewidth = .01)+ # for whatever reason color = NA gives an error
    scale_fill_viridis_b(
        breaks = c(5, 10, 15, 20), 
        guide = guide_colorsteps(direction = "horizontal", barwidth = 10),
        na.value = "#badada"
    )+
    coord_sf(datum = NA)+
    theme(
        legend.position = c(.65, .25),
        legend.title.position = "top"
    )+
    labs(
        title = "Unequally distributed bonus days of rest",
        fill = "Number of public holidays per year",
        caption = "#TidyTuesday 2024-52 | @ikashnitsky.phd"
    )

ggsave(
    filename = "2024-52-holiday-travel/n-public-holidays.png",
    width = 7, 
    height = 4
)
