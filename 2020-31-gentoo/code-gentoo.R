#===============================================================================
# 2020-07-31 -- TidyTuesday
# Gentoo penguins
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# load required packages
library(tidyverse) 
library(palmerpenguins)


# data on human birth weight
# http://data.un.org/Data.aspx?q=birth+weight&d=POP&f=tableCode%3a60

hum <- read_csv("2020-31-gentoo/UNdata_Export_20200731_154711593.zip") %>% 
    janitor::clean_names()



hum %>% 
    filter(!birth_weight=="Total") %>% 
    mutate(bw_3_4 = birth_weight %in% c("3000 - 3499", "3500 - 3999")) %>% 
    group_by(bw_3_4) %>% 
    summarise(prop = value %>% sum) %>% 
    pull(prop)


hum %>% 
    filter(!birth_weight=="Total") %>% 
    separate(birth_weight, into = c("lower", "upper"), sep = " - ") %>% 
    drop_na() %>% 
    mutate(est = (as.numeric(lower)+as.numeric(upper))/2) %>% 
    group_by(est) %>% 
    summarise(prop = value %>% sum) %>% 
    mutate(
        weight_group = c(
            "1,500 and less", "1,500 and less",
            "1,500—2,000", "1,500—2,000",
            "2,000—2,500", "2,500—3,000",
            "3,000—3,500", "3,500 and more",
            "3,500 and more"
        )
    ) %>% 
    group_by(weight_group) %>% 
    summarise(prop = prop %>% sum) %>% 
    ungroup() %>% 
    mutate(prop = prop %>% prop.table()) %>% 
    ggplot(aes(prop, weight_group))+
    geom_col(color = NA, fill = 5)+
    hrbrthemes::scale_x_percent()+
    ggdark::dark_theme_minimal(base_size = 14)+
    labs(
        title = "Weight of human newborns",
        subtitle = "United Nations data, pooled across countries and years",
        caption = "@ikashnitsky",
        y = "Weight, grams",
        x = NULL
    )+
    theme(text = element_text(family = "mono"),
          plot.title = element_text(size = 30, face = 2))

ggsave("2020-31-gentoo/human-newborns.png", width = 9, height = 5 )


# penguins ----------------------------------------------------------------

peng <- penguins %>% 
    mutate(weight_group = body_mass_g %>% cut(c(0, 3e3, 4e3, Inf))) %>% 
    group_by(species, weight_group) %>% 
    summarise(n = n()) %>% 
    drop_na() %>% 
    group_by(species) %>% 
    mutate(prop = prop.table(n)) %>% 
    pivot_wider(names_from = weight_group, values_from = prop)

    