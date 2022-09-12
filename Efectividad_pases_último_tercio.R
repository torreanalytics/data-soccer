## Precisión de pases ultimo tercio 
## Euro 2020 
## Viz


## librerias  ---------------------------------------------

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

## Cargar archivos
euro2020_events <- read_csv("statsbomb_eventing_data_euro_2020.csv")


## editar columnas ------------------
names(euro2020_events)
glimpse(euro2020_events)

table(euro2020_events$type.name, useNA = "ifany")
table(euro2020_events$pass.outcome.name, useNA = "ifany")
summary(euro2020_events$pass.end_location.x)
table(euro2020_events$pass.end_location.x, useNA = "ifany")

## euro 2020 Top 10 Teams pass to final third

top_team_pass_final_third <- euro2020_events %>% 
  filter(type.name == "Pass") %>% 
  mutate(pass_zone = ifelse(pass.end_location.x >= 80, "pass_final_third", "regular_pass")) %>% 
  group_by(team.name,pass_zone) %>% 
  summarise(n_pass = n(),
            n_pass_successful = sum(ifelse(is.na(pass.outcome.name),1,0))) %>% 
  mutate(precision_pass = round(n_pass_successful / n_pass*100,1)) %>% 
  filter(pass_zone == "pass_final_third") %>% 
  select(team.name,n_pass,n_pass_successful,pass_zone,precision_pass) %>% 
  arrange(desc(precision_pass)) %>% 
  head(10)

## Guardar archivo  --------------------
write_csv(top_team_pass_final_third,"teams_pass_final_third_euro2020_edit.csv")     

pass_final_third <- read_csv("teams_pass_final_third_euro2020_edit.csv")

## EDA

pass_final_third <- pass_final_third %>% 
  mutate(team.name = as.factor(team.name)) 

names(pass_final_third)
glimpse(pass_final_third)

## Gráfico de barras horizontales del top 10 de equipos con mejor precisión en pases en ultimo tercio - Euro 2020
f <- ggplot(pass_final_third, aes(x = fct_reorder(team.name, n_pass_successful), y = n_pass_successful)) +
  geom_bar(stat = "identity", fill = "#008346", col = "white", alpha = 0.4) +
  geom_label(aes(label = paste(precision_pass,"%"))) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 1350,100),labels = seq(0, 1350,100),limits = c(0,1350)) +
  theme_bw() +
  labs(x = "Teams\n",
       y = "\nPass Successful_p90",
       title = "Passing Effectiveness Last Third",
       subtitle = "Top 10 Teams / Euro 2020",
       caption = "Data: Statsbomb / Made By: @csarleonardo") +
  theme(plot.title = element_text(face="bold.italic", size=16, color = "#008346"),
        plot.subtitle = element_text(face="italic", size=12, color = "white"),
        panel.background = element_rect(fill = "#0B2130"),
        plot.background = element_rect(fill = "#0B2130"),
        panel.grid.major = element_line(linetype = "dashed", color = "#49525E"),
        panel.grid.minor = element_line(linetype = "dashed", color = "#49525E"),
        axis.text = element_text(face="bold.italic", size=10, color = "white"),
        axis.title.x = element_text(face="italic", size=14, color = "white"),
        axis.title.y = element_text(face="italic", size=14, color = "white"),
        plot.caption = element_text(face="italic", color = "white"))

f 

ggsave("passing_effect_last_third.png", width = 12, height = 8)
