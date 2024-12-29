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
    geom_sf(color = "#ffffff", size = 0.01, linewidth = .01)+ # for whatever reason color = NA gives an error
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

# check the distribution
df |> 
    ggplot(aes(n_pub_holidays))+
    geom_density(size = 1, color = "#044444", lineend = "round")+
    geom_hline(yintercept = 0, color = "#077777", size = .5)


# the canonical Robinson distribution -------------------------------------

# get world map outline (you might need to install the package)
world_outline <- spData::world |> 
    st_as_sf()

# let's use a fancy projection
world_outline_robinson <- world_outline |> 
    st_transform(crs = "ESRI:54030")

# country borders
country_borders <- world_outline_robinson |> 
    rmapshaper::ms_innerlines()

# get ISO3c codes
library(countrycode)

world_outline_robinson <- world_outline_robinson |> 
    filter(!iso_a2 == "AQ") |> # get rid of Antarctica
    mutate(
        iso3c = countrycode(iso_a2, origin = "iso2c", destination = "iso3c")
    )
    
# unify
df_rob <- world_outline_robinson |> left_join(n_holidays) 

# map!
df_rob |> 
    ggplot()+
    geom_sf(aes(fill = n_pub_holidays), color = NA)+
    geom_sf(data = country_borders, size = .1, color = "#269999" |> prismatic::clr_lighten())+
    scale_fill_viridis_b(
        breaks = c(5, 10, 15, 20), 
        guide = guide_colorsteps(direction = "horizontal", barwidth = 20),
        na.value = "#badada"
    )+
    coord_sf(datum = NA)+
    theme(
        legend.position = "top",
        legend.title.position = "top",
        plot.title = element_text(hjust = .5)
    )+
    labs(
        fill = NULL,
        title = "Number of public holidays per year",
        caption = "#TidyTuesday 2024-52 | @ikashnitsky.phd"
    )

ggsave(
    filename = "2024-52-holiday-travel/n-public-holidays-robinson.png",
    width = 7, 
    height = 4
)
