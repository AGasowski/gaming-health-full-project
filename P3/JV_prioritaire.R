rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")
install.packages("ggthemes")


library(vcd)
library(dplyr)
library(ggplot2)
library(ggthemes)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


# Pratique du JV prioritaire

data <- data %>%
  mutate(QB06B = case_when(
    QB06B == 1 ~ "Jamais",
    QB06B == 2 |
      QB06B == 3 ~ "Parfois",
    QB06B == 4 ~ "Assez souvent",
    QB06B == 5 ~ "Très souvent"
  )) %>% 
  mutate(QB06B = factor(QB06B,
                      levels = c("Jamais", "Parfois", "Assez souvent", "Très souvent"),
                      ordered = TRUE))



#ADRS
data <- data %>%
  mutate(ADRS_cat= case_when(
    ADRS_cat == 0 ~ "Sans risque",
    ADRS_cat == 1 ~ "Risque modéré",
    ADRS_cat == 2 ~ "Risque élevé"
  )) %>% 
  mutate(ADRS_cat = factor(ADRS_cat,
                      levels = c("Sans risque", "Risque modéré", "Risque élevé"),
                      ordered = TRUE))


#Graphique
data_clean <- data %>% filter(!is.na(ADRS_cat) & !is.na(QB06B))

# Calculer les pourcentages par groupe de santé physique
pourc <- data_clean %>%
  count(ADRS_cat, QB06B) %>%
  group_by(ADRS_cat) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(pourc, aes(x = ADRS_cat, y = pct, fill = QB06B)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_few() +
  labs(x = "Catégorie d'ADRS",
       y = "Pourcentage",
       fill = "Pratique du jeu vidéo prioritaire") +
  theme_minimal()



