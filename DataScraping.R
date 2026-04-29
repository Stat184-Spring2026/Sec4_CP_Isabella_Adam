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
  "CrossFire Mobile" = "#009900",
  "League of Legends: Wild Rift" = "#9900FF",
  "Naraka: Bladepoint" = "#00CCCC",
  "Identity V" = "#0099CC",
  "Fortnite" = "#0000FF",
  "Rocket League" = "#6600FF",
  "Counter-Strike: Global Offensive" = "#00FFFF",
  "Apex Legends" = "#FF00FF",
  "Rainbow Six Siege" = "grey",
  "Call of Duty: Warzone" = "yellow",
  "VALORANT" = "darkorchid",
  "Halo Infinite" = "#CC0033",
  "StarCraft II" = "#3366FF",
  "Overwatch" = "#FF6633",
  "Overwatch 2" = "#FF9966",
  "StarCraft: Brood War" = "#99CCFF",
  "StarCraft: Remastered" = "#6699FF",
  "Heroes of the Storm" = "#996633"
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