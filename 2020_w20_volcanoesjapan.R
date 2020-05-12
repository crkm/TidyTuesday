# get the data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

# load packages

library(tidyverse)
library(ggplot2)
library(maps)
library(ggrepel)
library(viridis)

# volcano number per country

as.factor(volcano$country)

countryfreq <- volcano %>% 
  group_by(country) %>%
  summarise(no_rows = length(country))

# isolate data i want to use

japanvolcano <- volcano %>% filter(country == "Japan") %>%
  arrange(desc(elevation))

# bubble map

japan <- map_data("world") %>% filter(region =="Japan")

p <-
  ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_point(data = japanvolcano, aes(x = longitude, y = latitude, size = elevation, color = elevation, alpha = 0.9)) +
  scale_size_continuous(range = c(0.5,7), name = "") +
  scale_color_viridis(trans = "log", name = "Elevation") +
  geom_text_repel(data = japanvolcano %>% arrange(elevation) %>% tail(10), aes(x=longitude, y=latitude, label=volcano_name), size=3, box.padding = 1.5, segment.alpha = 0.5) +
  theme_void()

p +
  ggtitle("Elevation of Japanese Volcanoes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_text(vjust = 3)) +
  theme(legend.position = c(0.9, 0.3)) +
  guides(alpha = FALSE)
p