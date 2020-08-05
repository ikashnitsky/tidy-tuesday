#===============================================================================
# 2020-08-04 -- TidyTuesday
# Energy consumprion
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(ggdark)
library(ggtern)
library(tricolore)
library(geofacet)
library(hrbrthemes)
library(cowplot)

# own options for ternary theme
own_tern_theme <- dark_theme_minimal(base_family = font_rc) +
    theme(
        legend.position = "none",
        axis.title = element_blank(),
        tern.axis.arrow.show = FALSE, 
        tern.axis.ticks.length.major = unit(12, "pt"),
        tern.axis.text.R =  element_text(size = 12, colour = 5),
        tern.axis.text.L = element_text(size = 12, colour = 6),
        tern.axis.text.T = element_text(size = 12, colour = 7),
        tern.axis.ticks.major.R =  element_line(colour = 5, size = 1),
        tern.axis.ticks.major.L = element_line(colour = 6, size = 1),
        tern.axis.ticks.major.T = element_line(colour = 7, size = 1),
        tern.axis.title.T = element_text(
            hjust = -.1,
            vjust = -2,
            angle = -60,
            family = font_rc,
            face = 2,
            color = 7
        ),
        tern.axis.title.L = element_text(
            hjust = -1,
            vjust = -2.5,
            angle = 60,
            family = font_rc,
            face = 2,
            color = 6
        ),
        tern.axis.title.R = element_text(
            hjust = 1.5,
            vjust = 3,
            family = font_rc,
            face = 2,
            color = 5
        ),
        text = element_text(
            family = font_rc,
            face = 2,
            size = 14,
            color = "grey20"
        )
    )

## Load in data
raw <- tidytuesdayR::tt_load('2020-08-04')


# totals dataset
totals <- raw$country_totals %>% 
    filter(type == "Total net production") %>% 
    pivot_longer(cols = c(`2016`,`2017`,`2018`), names_to = "year")

# types -- calculate ternary proportions
types <- raw$energy_types %>% 
    pivot_longer(cols = c(`2016`,`2017`,`2018`), names_to = "year") %>% 
    mutate(
        type_tern = type %>% 
            as_factor %>% 
            fct_other(
                keep = c("Conventional thermal", "Nuclear"),
                other_level = "Renewable"
            ) %>% 
            fct_recode(c("Conventional" = "Conventional thermal"))
    ) %>% 
    # turn to just three categories
    group_by(country, year, type_tern) %>% 
    summarise(value = value %>% sum(na.rm = T)) %>% 
    # calculate proportions
    group_by(country, year) %>% 
    mutate(value = value %>% prop.table()) %>% 
    # to wider
    pivot_wider(names_from = type_tern, values_from = value)

# calculate ternary colors
tern <- Tricolore(
    types, p1 = "Conventional", p2 = "Nuclear", p3 = "Renewable",
    show_data = F,
    contrast = .5, lightness = 1, chroma = 1, hue = 10/12
)

# add hex colors to the dataset
types$color <- tern$rgb

# attach totals to the dataframe
df <- types %>% 
    left_join(totals) %>% 
    right_join(europe_countries_grid1, by = c("country" = "code")) %>% 
    # reverse arrange to ensure largest labels are plotted first
    group_by(year) %>% 
    arrange(value %>% desc) %>% 
    ungroup() 


# plotting ----------------------------------------------------------------

# color legend
tern$key +
    tern_limit(T = 1.1, L = 1.1, R = 1.1)+
    geom_Tline(Tintercept = 0, color = 5)+
    geom_Rline(Rintercept = 0, color = 6)+
    geom_Lline(Lintercept = 0, color = 7)+
    geom_point(
        data = df %>%  filter(year == 2018),
        aes(Conventional, Nuclear, z = Renewable)
    )+
    own_tern_theme

gg_legend <- last_plot()


# ggtern with ternary colorcoding
df %>% 
    filter(year == 2018) %>% 
    ggtern(
        aes(
            Conventional, Nuclear, z = Renewable, 
            group = country, size = value, color = color, alpha = value
        )
    )+
    tern_limit(T = 1.1, L = 1.1, R = 1.1)+
    geom_Tline(Tintercept = 0, color = 5, alpha = .25)+
    geom_Rline(Rintercept = 0, color = 6, alpha = .25)+
    geom_Lline(Lintercept = 0, color = 7, alpha = .25)+
    geom_text(aes(label = country), fontface = 2)+
    scale_color_identity()+
    scale_size_continuous(range = c(2, 8))+
    scale_alpha_continuous(range = c(.5, 1), trans = "reverse")+
    own_tern_theme

gg_tern <- last_plot()

# geofaceted bubbles 
df %>% 
    filter(year == 2018) %>% 
    ggplot(aes(col, 9 - row, color = color, size = value, z = Renewable))+
    geom_point()+
    geom_text(aes(label = country, size = value/5), 
              color = "#ffffff", fontface = 2)+
    scale_color_identity()+
    scale_size_continuous(range = c(5, 25))+
    dark_theme_void(base_family = font_rc)+
    theme(legend.position = "none")

gg_bubl <- last_plot()

cowplot::plot_grid(gg_legend, gg_tern, gg_bubl) # error trying to arrange

# exporting ---------------------------------------------------------------

# showtext to save fonts in PDF
library(showtext)
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
showtext_auto()

# save in PDF to finalize in Inkscape
ggsave("2020-32-energy/gg_tern.pdf", gg_tern, width = 8, height = 7)
ggsave("2020-32-energy/gg_legend.pdf", gg_legend, width = 6, height = 5)
ggsave("2020-32-energy/gg_bubl.pdf", gg_bubl, width = 6, height = 5)

# Follow in Inkscape
