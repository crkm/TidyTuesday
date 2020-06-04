# Tidy Tuesday Week 23
# Marble Racing
# @curiekphd

# import data

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# load libraries

library(tidyverse)
library(directlabels)

# isolate the data i want to use

marbles[order(as.Date(marbles$date, format="%d/%m/%Y")),]

data <- marbles %>%
  filter(!is.na(points)) %>%
  group_by(team_name) %>%
  mutate(pointstime = cumsum(points))

data <- select(data, date, race, site, team_name, points, pointstime)


# plot time!

p <- data %>%
  ggplot(aes(x = fct_inorder(date), y = pointstime, group = team_name, color = team_name)) +
  geom_line(size = 2, alpha = 0.7) +
  labs(title = "The Most Point-iful Marble Racing Teams \n",
       caption = "@curiekphd | Source: Jelle's Marble Runs",
       x = "Date of Race",
       y = "Cumulative Points") +
  geom_dl(aes(label = team_name), method = list(dl.trans(x = x + .2), "last.qp")) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"),
        plot.background = element_rect(fill= "#444B5A", color = "#444B5A"),
        panel.background = element_rect(fill = "#444B5A", color = "transparent"),
        axis.title = element_text(size = 14, color = '#969696'),
        axis.text = element_text(color = "#969696"),
        panel.grid = element_line(color = "#666583"),
        axis.ticks = element_line(color = "#666583"),
        plot.title = element_text(hjust = 0.5, size = 20, color = "#bdbdc9", face = "bold"),
        plot.caption = element_text(color = "#bdbdc9"),
        axis.title.x = element_text(vjust=-1))

p
