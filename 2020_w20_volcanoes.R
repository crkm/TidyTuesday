# get the Data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


# load packages

library(tidyverse)
library(ggplot2)

# isolate data I want to use

volcanoid <- as.factor(volcano$volcano_number)
mainregion <- as.factor(volcano$region)
subregion <- as.factor(volcano$subregion)
rock <- as.factor(volcano$major_rock_1)
elevation <- volcano$elevation

mydata <- tibble(volcanoid, mainregion, subregion, elevation, rock)
 
# rock type and elevation

library(ggridges)
p <- ggplot(mydata, aes(x = elevation, y = rock, fill = rock)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_ridges() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=16, face="bold")) +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_brewer(palette="Set3") +
  labs(x = "Elevation", y = "Rock Type") + 
  ggtitle("Association Between Elevation and Major Volcanic Rock Types")
p

# rock type and elevation by main region

p2 <- p +
  facet_wrap(~ mainregion, labeller = label_wrap_gen()) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size=8, angle=45),
        axis.text.y = element_text(size=8),
        plot.title = element_text(size=16, face="bold")) +
  scale_x_continuous(n.breaks = 5) +
  scale_fill_brewer(palette="Set3") +
  labs(x = "Elevation", y = "Rock Type") + 
  ggtitle("Association Between Elevation and Major Volcanic \nRock Types Found In Different Global Regions")
p2