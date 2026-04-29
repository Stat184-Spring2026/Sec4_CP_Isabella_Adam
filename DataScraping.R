library(tidyverse)
library(rvest)
library(knitr)

china_games <- read_html("https://www.esportsearnings.com/countries/cn") %>%
  html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
  mutate(Country = "China")

usa_games <- read_html("https://www.esportsearnings.com/countries/us") %>%
  html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
  mutate(Country = "USA")

korea_games <- read_html("https://www.esportsearnings.com/countries/kr") %>%
  html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
  mutate(Country = "Korea")

china_top10 <- china_games %>% 
  rename(Rank = 1, Game = `Game Name`, Earnings = `Total (Game)`) %>% 
  mutate(Earnings = parse_number(Earnings)) %>% 
  slice_max(Earnings, n = 10) %>% 
  mutate(Pct = Earnings / sum(Earnings) * 100)

usa_top10 <- usa_games %>% 
  rename(Rank = 1, Game = `Game Name`, Earnings = `Total (Game)`) %>% 
  mutate(Earnings = parse_number(Earnings)) %>% 
  slice_max(Earnings, n = 10) %>% 
  mutate(Pct = Earnings / sum(Earnings) * 100)

korea_top10 <- korea_games %>% 
  rename(Rank = 1, Game = `Game Name`, Earnings = `Total (Game)`) %>% 
  mutate(Earnings = parse_number(Earnings)) %>% 
  slice_max(Earnings, n = 10) %>% 
  mutate(Pct = Earnings / sum(Earnings) * 100)

View(china_top10 %>% select(Game, Pct) %>% arrange(desc(Pct)))
View(korea_top10 %>% select(Game, Pct) %>% arrange(desc(Pct)))
View(usa_top10 %>% select(Game, Pct) %>% arrange(desc(Pct)))

game_colors <- c(
  "Dota 2" = "red",
  "Arena of Valor" = "orange",
  "PLAYERUNKNOWN'S BATTLEGROUNDS Mobile" = "tan",
  "League of Legends" = "green",
  "CrossFire" = "darkred",
  "PLAYERUNKNOWNâ\u0080\u0099S BATTLEGROUNDS" = "pink",
  "CrossFire Mobile" = "seagreen",
  "League of Legends: Wild Rift" = "blueviolet",
  "Naraka: Bladepoint" = "turquoise4",
  "Identity V" = "deepskyblue",
  "Fortnite" = "blue",
  "Rocket League" = "purple",
  "Counter-Strike: Global Offensive" = "cyan",
  "Apex Legends" = "deeppink",
  "Rainbow Six Siege" = "grey",
  "Call of Duty: Warzone" = "yellow",
  "VALORANT" = "springgreen",
  "Halo Infinite" = "magenta3",
  "StarCraft II" = "royalblue",
  "Overwatch" = "tomato",
  "Overwatch 2" = "wheat4",
  "StarCraft: Brood War" = "paleturquoise",
  "StarCraft: Remastered" = "skyblue2",
  "Heroes of the Storm" = "lightsalmon3"
)
china_top10 <- china_top10 %>% mutate(Pct = Earnings / sum(Earnings) * 100)
usa_top10 <- usa_top10 %>% mutate(Pct = Earnings / sum(Earnings) * 100)
korea_top10 <- korea_top10 %>% mutate(Pct = Earnings / sum(Earnings) * 100)

ggplot(china_top10, aes(x = "", y = Pct, fill = reorder(Game, Pct))) +
  geom_col() +
  scale_fill_manual(values = game_colors) +
  labs(title = "China - Top 10 Games by Earnings", x = NULL, y = "% of Top 10 Earnings", fill = "Game") +
  theme_minimal()

ggplot(usa_top10, aes(x = "", y = Pct, fill = reorder(Game, Pct))) +
  geom_col() +
  scale_fill_manual(values = game_colors) +
  labs(title = "USA - Top 10 Games by Earnings", x = NULL, y = "% of Top 10 Earnings", fill = "Game") +
  theme_minimal()

ggplot(korea_top10, aes(x = "", y = Pct, fill = reorder(Game, Pct))) +
  geom_col() +
  scale_fill_manual(values = game_colors) +
  labs(title = "Korea - Top 10 Games by Earnings", x = NULL, y = "% of Top 10 Earnings", fill = "Game") +
  theme_minimal()