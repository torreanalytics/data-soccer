## Gráfico xG Timeline con autogol
## Euro 2020


## librerias      -------------------------------------------

library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2) 
library(forcats) 
library(ggrepel) 
library(tidyr)
library(glue)
library(ggtext)
library(RColorBrewer)
library(cowplot)
library(forcats)
library(ggbeeswarm)
library(ggforce)
library(gridExtra)

## Gráfico xG Timeline para partido de la Euro 2020 con autogol.

## Cargar

eventing_data_euro <- read_csv("statsbomb_eventing_data_euro_2020.csv",col_types = list(.default = "c"))

matchs_data_euro <- read_csv("statsbomb_info_partidos_euro_2020.csv",col_types = list(.default = "c"))

table(eventing_data_euro$type.name)
table(eventing_data_euro$shot.outcome.name)
glimpse(eventing_data_euro)
names(eventing_data_euro)
table(eventing_data_euro$shot.type.name)

## EDA
matchs_owngoal <- eventing_data_euro %>% 
  select(team.name,match_id, type.name) %>% 
  filter(type.name == "Own Goal For")

matchs_owngoal_a <- eventing_data_euro %>% 
  select(team.name,match_id, type.name) %>% 
  filter(type.name == "Own Goal Against")


## match_id 3794686 | Partido con autogol 
## 
target_game <- eventing_data_euro %>% 
  filter(match_id == "3794686" & type.name == "Shot" & shot.outcome.name != "Blocked") %>% 
  select(match_id, ElapsedTime, type.name, team = team.name, player.name, xG = shot.statsbomb_xg, shot.outcome.name) 

target_game_2 <-  eventing_data_euro %>% 
  filter(match_id == "3794686" & type.name == "Own Goal For") %>% 
  select(match_id, ElapsedTime, type.name, team = team.name, player.name, xG = shot.statsbomb_xg, shot.outcome.name) %>% 
  mutate(shot.outcome.name = "Own Goal",
         player.name = "Pedro González López")

glimpse(target_game_euro)
names(target_game_euro)

## Transformación de datos 
target_game_euro <- bind_rows(target_game,target_game_2) %>%
  mutate(xG = as.numeric(xG),
         ElapsedTime = as.numeric(ElapsedTime))

target_game_euro[is.na(target_game_euro)] <- 0

target_game_euro <- target_game_euro %>% 
  mutate(Time_min = round(ElapsedTime/60, 0)) %>% 
  arrange(Time_min)

write.csv(target_game_euro, "target_game_euro.csv")  

## xG Acumulado

target_xG_acum <- target_game_euro %>% 
  arrange(ElapsedTime) %>% 
  group_by(team) %>% 
  mutate(xG_acum = cumsum(xG),
         Goles_acum = cumsum(ifelse(shot.outcome.name %in% c("Goal","Own Goal"),1,0)))

target_xG_acum = target_xG_acum %>% 
  bind_rows(target_xG_acum %>% filter(team == "Spain") %>% head(1) %>% mutate(Time_min = 0, xG_acum = 0)) %>%
  bind_rows(target_xG_acum %>% filter(team == "Croatia") %>% head(1) %>% mutate(Time_min = 0, xG_acum = 0)) 

Goles <- target_xG_acum %>% filter(shot.outcome.name %in% c("Goal","Own Goal")) %>% 
  head(8)

## Creación de Gráfico
names(target_xG_acum)

brewer.pal(2, "Dark2")
#1B9E77 . Verde | #D95F02 . naranja

croatia <- target_xG_acum %>% filter(team == "Croatia")
spain <- target_xG_acum %>% filter(team == "Spain")

# Crear titulo personalizado
title_text = glue("<b style = 'color: #1B9E77'>Croacia {max(croatia$Goles_acum)} (xG = {round(max(croatia$xG_acum), 2)}) <br> <b style='color: #D95F02'> España {max(spain$Goles_acum)} (xG = {round(max(spain$xG_acum), 2)})<br>")

t1 <- ggplot(target_xG_acum, aes(x = Time_min, y = xG_acum, col = team)) +
  geom_step(size = 1) +
  geom_point(data = Goles, aes(fill = team), size = 3, shape = 21, col = "black") +
  theme_bw() +
  geom_vline(xintercept = 45, linetype = "dotted", size = 2,color = "#49525E") +
  geom_vline(xintercept = 90, linetype = "dotted", size = 2,color = "#49525E") +
  geom_vline(xintercept = 105, linetype = "dotted", size = 2,color = "#49525E") +
  geom_label_repel(data = Goles, aes(label = player.name), size = 3) +
  scale_y_continuous(breaks = seq(0, 3, 0.2), expand = c(0.01, 0.01), limits = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 125, 15), expand = c(0.01, 0.01), limits = c(0, 125)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "\nTiempo (minutos)", 
       y = "xG Acumulado\n",
       title = title_text,
       subtitle = "Euro 2020",
       caption = "Data: Statsbomb / Made By: @csarleonardo") +
  theme(text = element_text(family = "serif"),
        plot.title = element_markdown(size = 18),
        plot.subtitle = element_text(face="bold", size=12, color = "white"),
        legend.position = "none",
        plot.margin = margin(1.2, 1, 0.5, 0.5, "cm"),
        panel.background = element_rect(fill = "#0B2130"),
        plot.background = element_rect(fill = "#0B2130"),
        panel.grid.major = element_line(linetype = "dotted", color = "#49525E"),
        panel.grid.minor = element_line(linetype = "dotted", color = "#49525E"),
        axis.text = element_text(size=10, color = "white"),
        axis.title.x = element_text(size=14, color = "white"),
        axis.title.y = element_text(size=14, color = "white"),
        plot.caption = element_text(color = "white"),
        axis.line = element_line(color = "#49525E"))

t1

t2 <- ggdraw() +
  draw_plot(t1) +
  draw_image("logo_euro_2020.png",  x = 0.4, y = 0.42, scale = 0.1)
t2

## Guardar gráfico

ggsave("Match_Euro_xG_cum_plot.png", width = 12, height = 8)




