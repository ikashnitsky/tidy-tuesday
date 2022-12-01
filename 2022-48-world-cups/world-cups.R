#===============================================================================
# 2022-12-01 -- TidyTuesay
# 2022-48 World Cups
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(sysfonts)
library(showtext)


sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()

#dowbload data
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')


brk <- seq(1930, 2018, 4)
lbl <-  tibble(brk = brk %>% paste) %>%
    mutate(
        lbl = case_when(
            str_sub(brk, 4, 4) == "0" ~ brk %>% paste0("\n", .),
            TRUE ~ str_sub(brk, 3, 4) %>% paste0("'", .)
        )
    ) %>%
    pull(lbl)

worldcups %>%
    ggplot(aes(year, goals_scored/games))+
    geom_line(color = "#444444")+
    geom_point(color = "#444444")+
    scale_y_continuous(limits = c(0, 6), expand = c(0, 0))+
    scale_x_continuous(breaks = brk, labels = lbl)+
    theme_minimal()+
    theme(
        panel.grid.minor = element_blank(),
        text = element_text(family = "ah"),
        plot.subtitle = element_text(
            size = 14, color = "#444444", face = 2
        ),
        plot.caption = element_text(color = "#444444")
    )+
    labs(
        subtitle = "Average goals scored per game played at the World Cups\n",
        y = NULL,
        x = NULL,
        caption = "\n#TidyTuiesday 2022-48 / @ikashnitsky@fosstodon.org"
    )

ggsave("2022-48-world-cups//world-cups.pdf",
       width = 6.4, height = 3.6,
       bg = "#dadada")
