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
  mutate(QB06A = case_when(
    QB06A == 1 ~ "Jamais",
    QB06A == 2 |
      QB06A == 3 ~ "Parfois",
    QB06A == 4 |
      QB06A == 5 ~ "Souvent"
  )) %>% 
  mutate(QB06A = factor(QB06A,
                        levels = c("Jamais", "Parfois", "Souvent"),
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
data_clean <- data %>% filter(!is.na(ADRS_cat) & !is.na(QB06A))

# Calculer les pourcentages par groupe de santé physique
pourc <- data_clean %>%
  count(ADRS_cat, QB06A) %>%
  group_by(ADRS_cat) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(pourc, aes(x = ADRS_cat, y = pct, fill = QB06A)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_few() +
  labs(x = "Catégorie d'ADRS",
       y = "Pourcentage",
       fill = "Pratique du jeu vidéo prioritaire") +
  theme_minimal()



