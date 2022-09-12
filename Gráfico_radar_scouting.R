## Gráfico de radar para similitud de jugadores
## Scouting
## Premier League 2021/2022

## librerias    ---------------------------------------------

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


## Genera un gráfico de radar de Heung-min Son y otro para el jugador más similar que te dio en el resultado 
## 
## Cargar

data_simil <- read_csv("simil_Heung_Min_Son_top5.csv")

data_premier <- read_csv("instat_players_stats_season_21_22_england_premier_league.csv",col_types = list(.default = "c"))
names(data_premier)
data_premier_clean <- clean_names(data_premier)
glimpse(data_premier_clean)


# Preparación de data
columnas_de_texto <- c("player_num", "player_name", "position", "nationality", "team", "national_team", "foot")

data_premier_clean2 <- data_premier_clean %>%
  mutate(across(everything(), ~gsub("[%]", "", .x))) %>%
  mutate(across(-c(columnas_de_texto), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>%
  filter(minutes_played >= max(minutes_played)*0.3) %>%
  mutate(label = paste0(player_name, "\n(", team, ")"))

## Selección de métricas
metricas_LM <- c("expected_assists","x_g_expected_goals","attacking_challenges_won",
                 "air_challenges_won","dribbles_successful","crosses_accurate",
                 "accurate_passes", "lost_balls_in_own_half","ball_interceptions",
                 "ball_recoveries_in_opponents_half")

## métricas por 90 min
players_p90 <- data_premier_clean2 %>% 
  select(columnas_de_texto,minutes_played,metricas_LM,label) %>%
  mutate(across(metricas_LM, ~(.x/minutes_played*90), .names = "{.col}_p90"))

## métricas por 90 min en percentil
players_percentile <- players_p90 %>% 
  group_by(position) %>% 
  mutate(across(ends_with("p90"), ~round(percent_rank(.),2), .names = "{.col}_percentile")) %>% 
  ungroup()

## Cambiar a formato largo las variables
metricas_p90 <- players_p90 %>% 
  select(ends_with("p90")) %>% 
  names()

players_p90_long <- players_p90 %>% 
  pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90")

metricas_percentile <- players_percentile %>% 
  select(ends_with("percentile")) %>% 
  names()

players_percentile_long <- players_percentile %>% 
  pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentile")

## Combinar DF base para la selección de jugadores 
# DATA Heung-Min Son ---------------------------------------

names(players_p90_long)

df_Son <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>%
  filter(player_name == "Heung-Min Son") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "crosses_accurate_p90" ~ "Centros precisos",
                            metric == "accurate_passes_p90" ~ "Pases precisos",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "ball_interceptions_p90" ~ "Intercepciones")) %>% 
  select(player_name, team,label, metric,p90,percentile)

## Estructura del plot

# Radar Heung-Min Son
n_metrics_Son <- length(df_Son$metric)
temp_S <- 360/n_metrics_Son/2
myAng_S <- seq(-temp_S, -360 + temp_S, length.out = n_metrics_Son)
ang_S <- ifelse(myAng_S < -90, myAng_S+180, myAng_S)                 
ang <- ifelse(ang_S < -90, ang_S+180, ang_S)


df_Son$metric <- factor(df_Son$metric,
                        levels = c("Intercepciones","Recuperaciones\ncampo rival","Duelos\naéreos\nganados",
                                   "Pases precisos","Centros precisos","xA",
                                   "Duelos\nofensivos\nganados","Duelos\naéreos\nganados","Regates \nexitosos","xG",
                                   "Pérdidas\ncampo propio"))

# Plot
s1 <- ggplot(df_Son, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#2ec4b6", stat = "identity",
           width = 1, colour = "white", linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#148076", colour = "white") +
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 3, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(data_premier_clean2$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_Son$player_name[1]} ({df_Son$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang, color = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 16,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(size = 10, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 

s1
## Guardar gráfico  de Son

ggsave("radar_plot_Heung_Min_Son.png", width = 12, height = 8)


# DATA Philippe Coutinho ------------------------------------
df_Coutinho <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>%
  filter(player_name == "Philippe Coutinho") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "crosses_accurate_p90" ~ "Centros precisos",
                            metric == "accurate_passes_p90" ~ "Pases precisos",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "ball_interceptions_p90" ~ "Intercepciones")) %>% 
  select(player_name, team,label, metric,p90,percentile)

## Estructura del plot

# Radar Philippe Coutinho
n_metrics_Cou <- length(df_Coutinho$metric)
temp_C <- 360/n_metrics_Cou/2
myAng_C <- seq(-temp_C, -360 + temp_C, length.out = n_metrics_Cou)
ang_C <- ifelse(myAng_C < -90, myAng_C+180, myAng_C)                 
ang_C <- ifelse(ang_C < -90, ang_C+180, ang_C)


df_Coutinho$metric <- factor(df_Coutinho$metric,
                             levels = c("Intercepciones","Recuperaciones\ncampo rival","Duelos\naéreos\nganados",
                                        "Pases precisos","Centros precisos","xA",
                                        "Duelos\nofensivos\nganados","Duelos\naéreos\nganados","Regates \nexitosos","xG",
                                        "Pérdidas\ncampo propio"))

# Plot
c1 <- ggplot(df_Coutinho, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#E8BFB6", stat = "identity",
           width = 1, colour = "white", linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#CF2F40", colour = "white", alpha = 0.8) +
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1,    colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 3, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles respecto a jugadores de la misma posición con al menos {round(max(data_premier_clean2$minutes_played, na.rm = T)*0.3, 0)} min. jugados\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_Coutinho$player_name[1]} ({df_Coutinho$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang, color = "white"),
        plot.title = element_markdown(hjust = 0.5, size = 16,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(size = 10, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 
c1

## Guardar gráfico de Philippe Coutinho

ggsave("radar_plot_Philippe Coutinho.png", width = 12, height = 8)

# Combinar gráficos ----------------------------------------
radar_LM <- grid.arrange(s1,c1,nrow = 1)

######################################################################################





