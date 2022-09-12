## Generar top de jugadores según métrica específica + viz

## Librerias ---------------------------------------------

library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(lubridate)
library(proxy)
library(ggplot2) 
library(forcats) 
library(ggrepel)

## Cargar CSV      ----------------------
premier_2122 <- read_csv("instat_players_stats_season_21_22_england_premier_league.csv",col_types = list(.default = "c"))
premier_2122 <- clean_names(premier_2122)
names(premier_2122)

glimpse(premier_2122)

## editar columnas  -----------------
names(premier_2122)
premier_2122 <- clean_names(premier_2122)
glimpse(premier_2122)


## Limpieza y conversión de datos (NA - Formatos)
# Formato numérico
premier_2122$expected_assists <- as.numeric(premier_2122$expected_assists)
premier_2122$x_g_expected_goals <- as.numeric(premier_2122$x_g_expected_goals)


## Premier League 21/22  ==  xG + xA  Top 20 players 

premier_xGxA90_2122 <- premier_2122 %>% 
  rename("xG" = "x_g_expected_goals","xA" = "expected_assists") %>% 
  select(player_name,team,position,minutes_played,matches_played,xG,xA) %>% 
  mutate(across("xA", ~as.numeric(str_replace(.x, "-", "0")))) %>% 
  mutate(across("xG", ~as.numeric(str_replace(.x, "-", "0")))) %>%
  mutate(minutes_played = as.numeric(minutes_played)) %>% 
  mutate(xGxA_p90 = round((xG+xA)/minutes_played*90,2)) %>% 
  filter(minutes_played > 900) %>% 
  arrange(desc(xGxA_p90)) %>% 
  head(20)


## Guardar archivo  --------------------
write_csv(premier_xGxA90_2122,"Top_xGxA_players_premier_2122.csv")   

## Cargar archivo
teams_euro_xA_xGA <- read_csv("teams_euro_xA_xGA_p90.csv",col_types = list(.default = "c"))

glimpse(teams_euro_xA_xGA)

##  EDA  ----------------------------------------------------
plot_euro_xGA_xGA <- teams_euro_xA_xGA %>% 
  select(team_name,xG_p90,xGA_p90) %>% 
  mutate(xG_p90 = round(as.numeric(xG_p90),3),
         xGA_p90 = round(as.numeric(xGA_p90),3),
         mean_xGp90 = round(mean(xG_p90),3),
         mean_xGAp90 = round(mean(xGA_p90),3))


## Scatterplot del total de xGF y xGA c/90 minutos por equipos - Euro 2020

p <- ggplot(plot_euro_xGA_xGA, aes(x = xG_p90, y = xGA_p90)) +
  geom_point(size = 3, shape = 21, fill = "darkblue", alpha = 0.7) +
  annotate("text", x = 2.75, y = 1.40, label = "Mean xGA", color = "darkred", size = 3) +
  annotate("text", x = 1.25, y = 3, label = "Mean xG", color = "darkred", size = 3) +
  geom_vline(aes(xintercept = mean_xGp90, color = "darkred", linetype = "dashed")) +
  geom_hline(aes(yintercept = mean_xGAp90, color = "darkred", linetype = "dashed")) +
  scale_x_continuous(breaks = seq(0.25, 3.25, 0.25), labels = seq(0.25, 3.25, 0.25), limits = c(0.25, 3.25)) +
  scale_y_continuous(breaks = seq(0.25, 3, 0.25), labels = seq(0.25, 3, 0.25), limits = c(0.25, 3)) +
  geom_text_repel(aes(label = team_name)) +
  labs(x = "\nxG a Favor por 90 minutos", 
       y = "xG en Contra por 90 minutos\n",
       title = "Total xGF vs xGA por equipos cada 90 minutos",
       subtitle = "Euro 2020",
       caption = "Data: Statsbomb / Made By: @csarleonardo") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.text = element_blank(),legend.position = "none")

p
p + facet_wrap(~team_name)

## Guardar gráfico en formato .png
ggsave("total_p90_xGF_xGA_teams_euro2020.png", width = 12, height = 8)


