#===============================================================================
# 2019-09-24 -- TidyTuesday
# School diversity -- ternary dataviz
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# load required packages
library(tidyverse) 
library(ggtern)
library(ggrepel)
library(extrafont); font_rs = "Roboto Slab"
library(hrbrthemes); import_roboto_condensed()


# fetch the data
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv") 

clean <- df %>% 
        transmute(
                state = ST,
                year = SCHOOL_YEAR,
                diverse,
                nstud = Total
        )


# calculate ternary proportions ---------------------------------

tern <- clean %>% 
        group_by(state, year, diverse) %>% 
        summarise(nstud = nstud %>% sum(na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(state, year) %>% 
        mutate(prop = prop.table(nstud)) %>% 
        ungroup() %>% 
        select(-nstud) %>% 
        spread(diverse, prop) %>% 
        replace_na(list(Diverse = 0, `Extremely undiverse` = 0, Undiverse = 0))

# calculate total number of students by state and year
total_stud <- clean %>% 
        group_by(state, year) %>% 
        summarise(nstud = nstud %>% sum(na.rm = TRUE)) %>% 
        ungroup() 

df_plot <- tern %>% 
        left_join(total_stud) %>% 
        # reverse arrange to ensure largest labelas are plotted first
        group_by(year) %>% 
        arrange(nstud %>% desc) %>% 
        ungroup() %>% 
        mutate(year_lab = year %>% str_replace("-", "–"))



# plot 
df_plot %>% 
        ggtern(aes(`Extremely undiverse`, Undiverse, z = Diverse, group = state))+
        geom_text(
                aes(label = state, size = nstud, color = nstud, alpha = nstud), 
                family = font_rc, fontface = 2
        )+
        scale_size_continuous(range = c(1, 10))+
        scale_alpha_continuous(range = c(.7, 1), trans = "reverse")+
        scale_color_viridis_c(
                limits = c(0, 6e6), 
                breaks = 1:5*1e6,
                labels = paste0(1:5, "M")
        )+
        
        scale_L_continuous("Extremely undiverse")+
        facet_wrap(~year_lab)+
        guides(
                size = FALSE,
                alpha = FALSE,
                color = guide_colorbar(
                        barwidth = 20, barheight = .5, 
                        title.position = "top", 
                )
        )+
        labs(
                title = "Racial diversity in the US schools",
                subtitle = "Proportions of students attending the three categories of schools by state",
                caption = "Data: The Washington Post, #TidyTuesday | Dataviz: @ikashnitsky",
                color = "Total number of students"
        )+
        theme_gray(base_family = font_rc, base_size = 14) +
        theme(
                legend.position = "bottom",
                axis.title = element_blank(),
                panel.border = element_rect(colour = NA, fill = NA),
                strip.background = element_rect(fill = "grey90", 
                                                colour = NA),
                strip.text = element_text(colour = "#35978e", face = 2, size = 20),
                plot.background = element_rect(fill = NA, colour = NA),
                tern.axis.arrow.show = FALSE, 
                tern.axis.ticks.length.major = unit(12, "pt"),
                tern.axis.text = element_text(size = 12, colour = "grey20"),
                tern.axis.title.T = element_text(hjust = -.1, vjust = -2, angle = -60, family = font_rc, face = 2),
                tern.axis.title.L = element_text(hjust = -.7, vjust = -2.5, angle = 60, family = font_rc, face = 2),
                tern.axis.title.R = element_text(hjust = 1.5, vjust = 3, family = font_rc, face = 2),
                text = element_text(family = font_rc, size = 14, face = 2, color = "grey20"),
                plot.title = element_text(size = 30, family = font_rs),
                plot.subtitle = element_text(size = 20),
                plot.caption  = element_text(size = 12)
        )
        

gg <- last_plot()

ggsave("2019-09-24-school-diversity/tidytuesday-ikashnitsky-school-diversity.png",  gg, width = 10, height = 7, type  = "cairo")
ggsave("2019-09-24-school-diversity/tidytuesday-ikashnitsky-school-diversity.pdf",  gg, width = 10, height = 7, device = cairo_pdf)


# plot change with arrows -------------------------------------------------

df_plot %>% 
        ggtern(aes(`Extremely undiverse`, Undiverse, z = Diverse, group = state))+
        geom_path(size = .75,  alpha = .5, aes(color = nstud))+
        geom_text(
                data = . %>% filter(year == "1994-1995"),
                aes(label = state, size = nstud, color = nstud, alpha = nstud), 
                family = font_rc, fontface = 2
        )+
        geom_point(
                data = . %>% filter(year == "2016-2017"),
                aes(color = nstud), 
                size = .5
        )+
        scale_size_continuous(range = c(1, 10))+
        scale_alpha_continuous(range = c(.7, 1), trans = "reverse")+
        scale_color_viridis_c(
                limits = c(0, 6e6), 
                breaks = 1:5*1e6,
                labels = paste0(1:5, "M")
        )+
        
        scale_L_continuous("Extremely undiverse")+

        guides(
                size = FALSE,
                alpha = FALSE,
                color = guide_colorbar(
                        barwidth = 20, barheight = .5, 
                        title.position = "top", 
                )
        )+
        labs(
                title = "Racial diversity in the US schools",
                subtitle = "Proportions of students attending the three categories of schools by state\nLabels represent values for 1994-95, dots 2016-17; lines show change over time",
                caption = "Data: The Washington Post, #TidyTuesday | Dataviz: @ikashnitsky",
                color = "Total number of students"
        )+
        theme_gray(base_family = font_rc, base_size = 14) +
        theme(
                legend.position = "bottom",
                axis.title = element_blank(),
                panel.border = element_rect(colour = NA, fill = NA),
                legend.margin = margin(),
                legend.box.margin = margin(), 
                strip.background = element_rect(fill = "grey90", 
                                                colour = NA),
                strip.text = element_text(colour = "#35978e", face = 2, size = 20),
                plot.background = element_rect(fill = NA, colour = NA),
                tern.axis.arrow.show = FALSE, 
                tern.axis.ticks.length.major = unit(12, "pt"),
                tern.axis.text = element_text(size = 12, colour = "grey20"),
                tern.axis.title.T = element_text(hjust = -.1, vjust = -2, angle = -60, family = font_rc, face = 2),
                tern.axis.title.L = element_text(hjust = -.7, vjust = -2.5, angle = 60, family = font_rc, face = 2),
                tern.axis.title.R = element_text(hjust = 1.5, vjust = 3, family = font_rc, face = 2),
                text = element_text(family = font_rc, size = 14, face = 2, color = "grey20"),
                plot.title = element_text(size = 24, family = font_rs),
                plot.subtitle = element_text(size = 14, face = 1),
                plot.caption  = element_text(size = 10)
        )


gg <- last_plot()

ggsave("2019-09-24-school-diversity/tidytuesday-ikashnitsky-school-diversity-change.png",  gg, width = 8, height = 9, type  = "cairo")
ggsave("2019-09-24-school-diversity/tidytuesday-ikashnitsky-school-diversity-change.pdf",  gg, width = 8, height = 9, device = cairo_pdf)

