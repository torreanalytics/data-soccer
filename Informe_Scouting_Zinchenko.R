## Curso de Big Data Basico - La Pizarra del DT
## Módulo 7 - Actividad Trabajo Final


## load libraries  ---------------------------------------------

library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)

library(ggplot2) 
library(forcats) 
library(ggrepel) 
library(proxy)
library(tidyr)
library(glue)
library(ggtext)
library(RColorBrewer)
library(cowplot)
library(forcats)
library(ggbeeswarm)
library(ggforce)
library(gridExtra)


# Load files

data_premier_2122 <- read_csv("instat_players_stats_season_21_22_england_premier_league.csv",col_types = list(.default = "c"))

names(data_premier_2122)
table(data_premier_2122$position)

data_premier_2122 <- clean_names(data_premier_2122)

#########################################################
## Selección de jugador objetivo "Olexandr Zinchenko"
## position = LD
## team = Man City
## foot = Left
#########################################################

player_target <-data_premier_2122 %>% 
  filter(player_name == "Olexandr Zinchenko")

## Limpieza de datos
columnas_de_texto <- c("player_num", "player_name", "position", "nationality", "team", "national_team", "foot")

data_premier_2122$minutes_played <- as.numeric(data_premier_2122$minutes_played)

data_premier_LD <- data_premier_2122 %>%
  mutate(across(everything(), ~gsub("[%]", "", .x))) %>%
  mutate(across(-c(columnas_de_texto), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>%
  mutate(umbral_MJ = max(minutes_played)*0.25) %>% 
  filter(minutes_played >= umbral_MJ)  %>%
  filter(position == "LD" & age >= 23, age <= 28) %>% 
  mutate(label = paste0(player_name, "\n(", team, ")")) %>% 
  relocate(label, .before = expected_assists)

data_premier_umbral <- data_premier_2122 %>%
  mutate(across(-c(columnas_de_texto), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>%
  mutate(umbral_MJ = max(minutes_played)*0.25) %>% 
  filter(minutes_played >= umbral_MJ)  %>%

names(data_premier_LD)
glimpse(data_premier_clean)

####################################################
## Establecimiento de métricas según posición de LD
###################################################

metricas_LD <-  c("x_g_expected_goals","expected_assists","assists","shots_on_target","key_passes_accurate",
                  "crosses_accurate","dribbles_successful","attacking_challenges_won",
                  "defensive_challenges_won","air_challenges_won","ball_recoveries_in_opponents_half",
                  "tackles_successful","ball_interceptions","lost_balls_in_own_half")

## Conversión de métricas a 90 minutos
glimpse(data_premier_LD)

players_LD_p90 <- data_premier_LD %>% 
  select(columnas_de_texto,minutes_played,metricas_LD,label) %>% 
  mutate(across(metricas_LD, ~(.x/minutes_played*90), .names = "{.col}_p90"))

## Conversión de métricas p90 a percentil 

players_LD_percentile <- players_LD_p90 %>% 
  mutate(across(ends_with("p90"), ~round(percent_rank(.),2), .names = "{.col}_percentile"))

###############################################################
## Definición de métricas de similitud con target y Data base
##############################################################
names(players_LD_percentile)

metricas_simil <-  c("x_g_expected_goals_p90_percentile",               
                     "expected_assists_p90_percentile",
                     "assists_p90_percentile",                          
                     "shots_on_target_p90_percentile",                  
                     "key_passes_accurate_p90_percentile",
                     "crosses_accurate_p90_percentile",                 
                     "dribbles_successful_p90_percentile",              
                     "attacking_challenges_won_p90_percentile",
                     "defensive_challenges_won_p90_percentile",         
                     "air_challenges_won_p90_percentile",
                     "ball_recoveries_in_opponents_half_p90_percentile",
                     "tackles_successful_p90_percentile",
                     "ball_interceptions_p90_percentile",               
                     "lost_balls_in_own_half_p90_percentile")

## Definir jugador objetivo

target_player <-players_LD_percentile %>% 
  filter(player_name == "Olexandr Zinchenko") 
  

# Data  
data_LD <- players_LD_percentile %>% 
  select(player_name,team,metricas_simil)


# Variable similitud 
sim = simil(x = data_LD %>% select(-c(player_name,team)),
            y = target_player %>% select(metricas_simil),
            method = "cosine")

# Generar similitud por posición y métricas en percentiles con Olexandr Zinchenko en jugadores de la premier temporada 21/22

scouting_percentile <- data_LD %>% 
  mutate(sim_percentil_cosine = as.numeric(sim)) %>% 
  arrange(desc(sim_percentil_cosine)) %>% 
  relocate(sim_percentil_cosine, .before = x_g_expected_goals_p90_percentile)

# Guardar DF de scouting de LD premier 2122
write.csv(scouting_percentile,"scouting_premier_2122.csv")

################################################################################
## Establecer variables para contextualizar xG defensivo y acciones defensivas 
## por equipo durante la temporada 21/22
################################################################################

defensive_context <- data_premier_2122 %>% 
  select(team,x_g_expected_goals,
         defensive_x_g_x_g_of_shots_made_by_guarded_player,
         defensive_x_g_per_shot,
         defensive_challenges,
         tackles,
         ball_interceptions) %>%
  mutate(across(-c(team), as.numeric)) %>%
  mutate(across(where(is.double), ~replace_na(.x, 0))) %>% 
  group_by(team) %>% 
  filter(team %in% c("Liverpool","Man City","Chelsea","Arsenal","West Ham","Tottenham","Man United","Brentford","Brighton","Southampton",
                     "Newcastle Utd.","Crystal Palace","Everton","Leeds United","Leicester","Aston Villa","Watford","Burnley",
                     "Wolverhampton","Norwich")) %>% 
  summarise(xG = sum(x_g_expected_goals),
            xG_def = sum(defensive_x_g_x_g_of_shots_made_by_guarded_player),
            defensive_challenges = sum(defensive_challenges),
            tackles = sum(tackles),
            ball_interceptions = sum(ball_interceptions)) %>% 
  mutate(defensive_actions = defensive_challenges + tackles + ball_interceptions) %>% 
  select(team,xG,xG_def,defensive_actions) %>% 
  arrange(xG_def)

# Guardar varaible establecida como contexto defensivo  / premier 2122
write.csv(defensive_context,"def_context_premier_2122.csv")
defensive_context <- read_csv("def_context_premier_2122.csv")

##################################################################
### Creación de gráficos de radar sobre candidatos seleccionados 
##################################################################

# Ajuste de data a formato largo de p90 y percentiles

metricas_p90 <- players_LD_p90 %>% 
  select(ends_with("p90")) %>% 
  names()

players_p90_long <- players_LD_p90 %>% 
  pivot_longer(cols = metricas_p90, names_to = "metric", values_to = "p90")


metricas_percentile <- players_LD_percentile %>% 
  select(ends_with("percentile")) %>% 
  names()

players_percentile_long <- players_LD_percentile %>% 
  pivot_longer(cols = metricas_percentile, names_to = "metric", values_to = "percentile")


# DF de target player = Olexandr Zinchenko y estructura del radar plot
######################################################################

table(players_p90_long$metric)

df_zin <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>% 
  filter(player_name == "Olexandr Zinchenko") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "assists_p90" ~ "Asistencias",
                            metric == "shots_on_target_p90" ~ "Disparos \nal arco",
                            metric == "key_passes_accurate_p90" ~ "Pases \nprecisos",
                            metric == "crosses_accurate_p90" ~ "Centros \nprecisos",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "defensive_challenges_won_p90" ~ "Duelos \ndefensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "tackles_successful_p90" ~ "Entradas\nexitosas",
                            metric == "ball_interceptions_p90" ~ "Intercepciones",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",)) %>% 
  select(player_name,team,label,metric,p90,percentile)

## Estructura del plot

# Radar plot Olexandr Zinchenko

n_metrics_zin <- length(df_zin$metric)
temp_zin <- 360/n_metrics_zin/2
myAng_zin <- seq(-temp_zin, -360 + temp_zin, length.out = n_metrics_zin)
ang_zin <- ifelse(myAng_zin < -90, myAng_zin+180, myAng_zin)                 
ang_zin <- ifelse(ang_zin < -90, ang_zin+180, ang_zin)


df_zin$metric <- factor(df_zin$metric,
                        levels = c("xG","xA","Asistencias",
                                   "Disparos \nal arco","Pases \nprecisos","Centros \nprecisos",
                                   "Regates \nexitosos","Duelos \nofensivos\nganados",
                                   "Duelos \ndefensivos\nganados","Duelos \naéreos\nganados","Recuperaciones\ncampo rival",
                                   "Entradas\nexitosas","Intercepciones","Pérdidas \ncampo propio"))
# Radar plot

z1 <- ggplot(df_zin, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#263238", stat = "identity",
           width = 1, colour = "white", alpha = 0.5,linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#73A4CA", colour = "white") +
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1, colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 2, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles que relaciona a laterales izquierdos con más de {round(max(data_premier_2122$minutes_played, na.rm = T)*0.25, 0)} min. jugados\n\n Rango de edad entre 23 y 28 años\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_zin$player_name[1]} ({df_zin$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang_zin, color = "white"),
        plot.title = element_markdown(face="bold",hjust = 0.5, size = 18,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(hjust = 0.5, size = 8, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 
z1

## Guardamos gráfico de Olexandr Zinchenko

ggsave("radar_plot_Olexandr_Zinchenko.png", width = 12, height = 8)


# DF de Konstantinos Tsimikas (Candidato 1) y estructura del radar plot
###################################################################

df_kons <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>% 
  filter(player_name == "K. Tsimikas") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "assists_p90" ~ "Asistencias",
                            metric == "shots_on_target_p90" ~ "Disparos \nal arco",
                            metric == "key_passes_accurate_p90" ~ "Pases \nprecisos",
                            metric == "crosses_accurate_p90" ~ "Centros \nprecisos",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "defensive_challenges_won_p90" ~ "Duelos \ndefensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "tackles_successful_p90" ~ "Entradas\nexitosas",
                            metric == "ball_interceptions_p90" ~ "Intercepciones",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",)) %>% 
  select(player_name,team,label,metric,p90,percentile)

## Estructura del plot

# Radar plot Konstantinos Tsimikas

n_metrics_kons <- length(df_kons$metric)
temp_kons <- 360/n_metrics_kons/2
myAng_kons <- seq(-temp_kons, -360 + temp_kons, length.out = n_metrics_kons)
ang_kons <- ifelse(myAng_kons < -90, myAng_kons+180, myAng_kons)                 
ang_kons <- ifelse(ang_kons < -90, ang_kons+180, ang_kons)


df_kons$metric <- factor(df_kons$metric,
                        levels = c("xG","xA","Asistencias",
                                   "Disparos \nal arco","Pases \nprecisos","Centros \nprecisos",
                                   "Regates \nexitosos","Duelos \nofensivos\nganados",
                                   "Duelos \ndefensivos\nganados","Duelos \naéreos\nganados","Recuperaciones\ncampo rival",
                                   "Entradas\nexitosas","Intercepciones","Pérdidas \ncampo propio"))

# Radar plot

k1 <- ggplot(df_kons, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#263238", stat = "identity",
           width = 1, colour = "white", alpha = 0.5,linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#C62828", colour = "white", alpha = 0.6) +
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1, colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 2, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles que relaciona a laterales izquierdos con más de {round(max(data_premier_2122$minutes_played, na.rm = T)*0.25, 0)} min. jugados\n\n Rango de edad entre 23 y 28 años\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_kons$player_name[1]} ({df_kons$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang_kons, color = "white"),
        plot.title = element_markdown(face="bold",hjust = 0.5, size = 18,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(hjust = 0.5, size = 8, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 
k1

## Guardamos gráfico de Konstantinos Tsimikas

ggsave("radar_plot_Konstantinos_Tsimikas.png", width = 12, height = 8)

# DF de Matt Targett (Candidato 2) y estructura del radar plot
###################################################################

df_matt <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>% 
  filter(player_name == "Matt Targett") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "assists_p90" ~ "Asistencias",
                            metric == "shots_on_target_p90" ~ "Disparos \nal arco",
                            metric == "key_passes_accurate_p90" ~ "Pases \nprecisos",
                            metric == "crosses_accurate_p90" ~ "Centros \nprecisos",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "defensive_challenges_won_p90" ~ "Duelos \ndefensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "tackles_successful_p90" ~ "Entradas\nexitosas",
                            metric == "ball_interceptions_p90" ~ "Intercepciones",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",)) %>% 
  select(player_name,team,label,metric,p90,percentile)

## Estructura del plot

# Radar plot Matt Targett

n_metrics_matt <- length(df_matt$metric)
temp_matt <- 360/n_metrics_matt/2
myAng_matt <- seq(-temp_matt, -360 + temp_matt, length.out = n_metrics_matt)
ang_matt <- ifelse(myAng_matt < -90, myAng_matt+180, myAng_matt)                 
ang_matt <- ifelse(ang_matt < -90, ang_matt+180, ang_matt)


df_matt$metric <- factor(df_matt$metric,
                         levels = c("xG","xA","Asistencias",
                                    "Disparos \nal arco","Pases \nprecisos","Centros \nprecisos",
                                    "Regates \nexitosos","Duelos \nofensivos\nganados",
                                    "Duelos \ndefensivos\nganados","Duelos \naéreos\nganados","Recuperaciones\ncampo rival",
                                    "Entradas\nexitosas","Intercepciones","Pérdidas \ncampo propio"))

# Radar plot

m1 <- ggplot(df_matt, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#263238", stat = "identity",
           width = 1, colour = "white", alpha = 0.5,linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#FFE93F", colour = "white", alpha = 0.8) + 
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1, colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 2, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles que relaciona a laterales izquierdos con más de {round(max(data_premier_2122$minutes_played, na.rm = T)*0.25, 0)} min. jugados\n\n Rango de edad entre 23 y 28 años\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_matt$player_name[1]} ({df_matt$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang_matt, color = "white"),
        plot.title = element_markdown(face="bold",hjust = 0.5, size = 18,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(hjust = 0.5, size = 8, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 
m1

## Guardamos gráfico de Matt Targett

ggsave("radar_plot_Matt_Targett.png", width = 12, height = 8)


# DF de Junior Firpo (Candidato 3) y estructura del radar plot
###################################################################

df_jun <- players_p90_long %>% 
  bind_cols(players_percentile_long %>% select(percentile)) %>% 
  filter(player_name == "Junior Firpo") %>% 
  mutate(metric = case_when(metric == "x_g_expected_goals_p90" ~ "xG",
                            metric == "expected_assists_p90" ~ "xA",
                            metric == "assists_p90" ~ "Asistencias",
                            metric == "shots_on_target_p90" ~ "Disparos \nal arco",
                            metric == "key_passes_accurate_p90" ~ "Pases \nprecisos",
                            metric == "crosses_accurate_p90" ~ "Centros \nprecisos",
                            metric == "dribbles_successful_p90" ~ "Regates \nexitosos",
                            metric == "attacking_challenges_won_p90" ~ "Duelos \nofensivos\nganados",
                            metric == "defensive_challenges_won_p90" ~ "Duelos \ndefensivos\nganados",
                            metric == "air_challenges_won_p90" ~ "Duelos \naéreos\nganados",
                            metric == "ball_recoveries_in_opponents_half_p90" ~ "Recuperaciones\ncampo rival",
                            metric == "tackles_successful_p90" ~ "Entradas\nexitosas",
                            metric == "ball_interceptions_p90" ~ "Intercepciones",
                            metric == "lost_balls_in_own_half_p90" ~ "Pérdidas \ncampo propio",)) %>% 
  select(player_name,team,label,metric,p90,percentile)

## Estructura del plot

# Radar plot Junior Firpo

n_metrics_jun <- length(df_jun$metric)
temp_jun <- 360/n_metrics_jun/2
myAng_jun <- seq(-temp_jun, -360 + temp_jun, length.out = n_metrics_jun)
ang_jun <- ifelse(myAng_jun < -90, myAng_jun+180, myAng_jun)                 
ang_jun <- ifelse(ang_jun < -90, ang_jun+180, ang_jun)


df_jun$metric <- factor(df_jun$metric,
                         levels = c("xG","xA","Asistencias",
                                    "Disparos \nal arco","Pases \nprecisos","Centros \nprecisos",
                                    "Regates \nexitosos","Duelos \nofensivos\nganados",
                                    "Duelos \ndefensivos\nganados","Duelos \naéreos\nganados","Recuperaciones\ncampo rival",
                                    "Entradas\nexitosas","Intercepciones","Pérdidas \ncampo propio"))
# Radar plot

j1 <- ggplot(df_jun, aes(x = metric, y = percentile)) +
  geom_bar(aes(y = 1), fill = "#263238", stat = "identity",
           width = 1, colour = "white", alpha = 0.5,linetype = "dashed") +
  geom_bar(stat = "identity", width = 1, fill = "#347828", colour = "white", alpha = 0.8) +
  geom_hline(yintercept = 0.25, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.50, colour = "white", linetype = "longdash", alpha = 0.5)+
  geom_hline(yintercept = 0.75, colour = "white", linetype = "longdash", alpha = 0.5)+ 
  geom_hline(yintercept = 1, colour = "white", alpha = 0.5) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  geom_label(aes(label = round(p90, 2)), fill = "white", size = 2, color = "black", show.legend = FALSE) +
  labs(fill = "",   
       caption = glue("Percentiles que relaciona a laterales izquierdos con más de {round(max(data_premier_2122$minutes_played, na.rm = T)*0.25, 0)} min. jugados\n\n Rango de edad entre 23 y 28 años\n\nViz: @csarleonardo  |  Data: Instat"),     
       title = glue("{df_jun$player_name[1]} ({df_jun$team[1]})"),
       subtitle = glue("Premier League 21/22 | Métricas cada 90 min.")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#011627", color = "white"),
        panel.background = element_rect(fill = "#011627", color = "white"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = ang_jun, color = "white"),
        plot.title = element_markdown(face="bold",hjust = 0.5, size = 18,color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12,color = "white"),
        plot.caption = element_text(hjust = 0.5, size = 8, color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 2, 2, 2)) 
j1

## Guardamos gráfico de Junior Firpo

ggsave("radar_plot_Junior_Firpo.png", width = 12, height = 8)






