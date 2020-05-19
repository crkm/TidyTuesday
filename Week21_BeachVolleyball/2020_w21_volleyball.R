# get the data

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# load packages

library(tidyverse)
library(skimr)
library(ggdark)
library(patchwork)

# summary of data

skim(vb_matches)

# organise data

women_vb <- vb_matches %>% filter(gender == "W") %>%
  mutate(w_height = w_p1_hgt + w_p2_hgt) %>%
  mutate(l_height = l_p1_hgt + l_p2_hgt)

women <- 
women_vb %>%
  mutate(id = row_number()) %>%
  select(id, w_height, l_height) %>%
  pivot_longer(cols = c(w_height, l_height),
names_to = "player", values_to = "height") %>%
  mutate(status = case_when(player == "w_height"~ "Winners",
                            TRUE ~ "Losers"))

men_vb <- vb_matches %>% filter(gender == "M") %>%
  mutate(w_height = w_p1_hgt + w_p2_hgt) %>%
  mutate(l_height = l_p1_hgt + l_p2_hgt)

men <-
men_vb %>%
  mutate(id = row_number()) %>%
  select(id, w_height, l_height) %>%
  pivot_longer(cols = c(w_height, l_height), names_to = "player", values_to = "height") %>%
  mutate(status = case_when(player == "w_height"~ "Winners",
                            TRUE ~ "Losers"))

# check distribution

w_w_hist <- ggplot(women_vb, aes(w_height)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
w_w_hist

w_l_hist <- ggplot(women_vb, aes(l_height)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
w_l_hist

m_w_hist <- ggplot(men_vb, aes(w_height)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
m_w_hist

m_l_hist <- ggplot(men_vb, aes(l_height)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
m_l_hist

# stats

t.test(women_vb$w_height, women_vb$l_height)
t.test(men_vb$w_height, men_vb$l_height)

# plots

p <- ggplot(women, aes(x = status, y = height, fill = status)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "white", alpha = 0.2) +
  ylim(125, 165) +
  scale_fill_manual(values=c("lightslategrey", "rosybrown1")) +
  dark_theme_gray() +
  labs(title = " \nHeight Differences Between Winning and Losing Teams in Women's Beach Volleyball",
      subtitle = " \nAverage combined height of winning teams: 141.8 inches \nAverage combined height of losing teams: 141.4 inches",
      caption = "t(69294) = 20.2, P < 0.000****\n ",
      x = " ",
      y = "Combined height (inches)") +
  theme(
    legend.position = "none")
p            

p2 <- ggplot(men, aes(x = status, y = height, fill = status)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "white", alpha = 0.2) +
  ylim(125, 165) +
  scale_fill_manual(values=c("slategrey", "rosybrown1")) +
  dark_theme_gray() +
  labs(title = " \nHeight Differences Between Winning and Losing Teams in Men's Beach Volleyball",
       subtitle = " \nAverage combined height of winning teams: 152.7 inches \nAverage combined height of losing teams: 152.3 inches",
       caption = "t(69294) = 20.2, P < 0.000****\n ",
       x = " ",
       y = "Combined height (inches)") +
  theme(
    legend.position = "none")
p2           

p + p2
