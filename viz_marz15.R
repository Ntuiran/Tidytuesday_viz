# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2022-03-15')

cr <- tuesdata$cran


library(tidyverse)
library(lubridate)
library(dplyr)
library("png")       
library(data.table)
data <- data.table(cr)
tidypack <- c("dplyr", "forcats", "ggplot2", "purrr", "readr", "stringr", "tibble", "tidyr")
data <- data %>% filter(package %in% tidypack, rmd > 0) #Count more than 0
data$Year <- format(as.Date(data$date), format = "%Y")
data <- data %>%
  group_by(package,Year) %>%
  summarise(rmd = sum(rmd))

library(ggthemes)

p1<-data %>%
  ggplot(aes(Year, 100, 
             size = rmd,
             fill = package)) +
  facet_grid(vars(package)) +
  geom_point(shape = 21) +
  scale_size_area(max_size = 15) +
  coord_cartesian(ylim = c(50, 150)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(guide = "none") +
  theme_void()+
  labs(x = "", 
       y = "Number of vignettes (rmd)", 
       title = "Tidyverse package vignettes", 
       subtitle = "Each dot represents the number of vignettes according to each version release year for the open source tidyverse package. \n\n Data: https://github.com/rmflight/vignette_analysis | Plot: @Neiltuirant \n\n")+
  theme(plot.background = element_rect(colour = "#d2d2de", fill = "#d2d2de"), 
        panel.background = element_rect(colour = "#d2d2de", fill = "#d2d2de"),
        legend.background = element_rect(colour = "#d2d2de", fill = "#d2d2de"),
        legend.key = element_rect(colour = "#d2d2de", fill = "#d2d2de"),
        plot.title = element_text(colour = "#080842", family = "source", face = "bold", hjust = 0, size = 16),
        plot.subtitle=element_text(size=10, face="italic", color="#080842"),
        legend.text = element_text(color = "#080842"),
        legend.title = element_text(color = "#080842"),
        axis.text.x = element_text(color = "#080842"),
        text=element_text(size=10),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(colour = "#3A3B3C", fill = "#3A3B3C"),
        strip.text = element_text(margin = margin(20,5,20,5)),
        panel.grid.major = element_line(colour = "#b9b9dd"),
        panel.grid.minor = element_line(colour = "#b9b9dd"))

dplyr <- readPNG("dplyr.png")
forcats <- readPNG("forcats.png")
ggplot2 <- readPNG("ggplot2.png")
purrr <- readPNG("purrr.png")
readr <- readPNG("readr.png")
stringr <- readPNG("stringr.png")
tibble <- readPNG("tibble.png")
tidyr <- readPNG("tidyr.png")

g <- ggplot_gtable(ggplot_build(p1))
strips <- grep("strip", g$layout$name)
library(grid)
new_grobs <- list(rasterGrob(dplyr),
                  rasterGrob(forcats), 
                  rasterGrob(ggplot2), 
                  rasterGrob(purrr), 
                  rasterGrob(readr), 
                  rasterGrob(stringr), 
                  rasterGrob(tibble), 
                  rasterGrob(tidyr))
g$grobs[strips] <- new_grobs
  
grid.draw(g)

