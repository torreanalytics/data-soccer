## Gráfico de dispersión con variables xG vs xGA por jugador
## Premier League 2021-2022
## Viz

## librerias   ---------------------------------------------

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

#  ------------------------------------
## Haz un gráfico de dispersión mapeando al menos 2 variables adicionales a las que se incluyen en los ejes X e Y. 
## Para ello puedes utilizar el color de relleno, la forma y/o el tamaño del punto, por ejemplo. 

## Cargar archivos

player_premier_xG_xA <- read_csv("Top_xGxA_players_premier_2122.csv")
glimpse(player_premier_xG_xA)

## Top 10 de jugadores con mejores métricas en xG + xA  
top_players_xG_xA <- player_premier_xG_xA %>% 
  mutate(umbral_MJ = max(minutes_played)*0.3) %>% 
  filter(minutes_played >= umbral_MJ) %>% 
  mutate(xG_xA = (xG+xA)) %>% 
  arrange(desc(xG_xA)) %>% 
  head(10)
names(top_players_xG_xA)

## Scatterplot relacionado con el xG vs xA por jugador / top 10 players premier 2122 

t <- ggplot(top_players_xG_xA,aes(xG,xA,)) +
  geom_point(aes(shape = position, fill = xGxA_p90), size = 4, alpha = 0.8) +
  scale_shape_manual(values = c(21:24,25)) +
  scale_fill_distiller(palette = "Spectral", direction = 1)+
  geom_text_repel(aes(label = player_name),box.padding = 0.5) +
  scale_x_continuous(breaks = seq(0, 25, 2.5), labels = seq(0, 25, 2.5), limits = c(2.5, 22.5)) +
  scale_y_continuous(breaks = seq(0, 25, 2.5), labels = seq(0, 25, 2.5), limits = c(2.5, 25)) +
  labs(x = "\nTotal xG", 
       y = "Total xA\n",
       title = "Total xG vs xA per player - Top 10",
       subtitle = "Premier League - Season 21-22",
       caption = "Data: Statsbomb / Made By: @csarleonardo",
       shape = "Position") +
  theme_bw() +
  theme(plot.title = element_text(face="bold.italic", size=16, color = "#68001D"),
        plot.subtitle = element_text(face="italic", size=12, color = "black"),
        panel.background = element_rect(fill = "#D5D5D5"),
        panel.grid.major = element_line(linetype = "dashed", color = "#B0BEC5"),
        panel.grid.minor = element_line(linetype = "dashed", color = "#B0BEC5"),
        axis.text = element_text(face="italic", size=10, color = "black"),
        axis.title.x = element_text(face="italic", size=14, color = "#68001D"),
        axis.title.y = element_text(face="italic", size=14, color = "#68001D"),
        plot.caption = element_text(face="italic", color = "#68001D")
  )
t

ggsave("players_best_xG_xA_premier2122.png", width = 12, height = 8)

##############################################################################