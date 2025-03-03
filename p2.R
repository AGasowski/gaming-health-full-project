rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")

library(vcd)
library(dplyr)
library(ggplot2)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))



###SANTE


data <- data %>% 
  mutate(santephysique = case_when(
    q18imc < 18.5 ~ "insuffisance pondérale",
    q18imc > 30 ~ "obésité",
    q18imc > 25 ~ "surpoids",
    TRUE ~ "corpulence normale"
  ))




##Calcul IMC moyen en fonction de la fréquence de jeux d'argents
aggregate( q18imc ~ qb07abcdef1, data = data, FUN = mean, na.rm = TRUE)

#On enlève la valeur aberrante
data_clean <- data[data$q18imc <= 100, ]

summary(data_clean$q18imc)

aggregate( q18imc ~ qb07abcdef1, data = data_clean, FUN = mean, na.rm = TRUE)


##Calcul IMC moyen en fonction de la fréquence de jeux vidéos
aggregate( q18imc ~ QB02, data = data_clean, FUN = mean, na.rm = TRUE)


##
# Calcul des proportions pour chaque catégorie
data_prop <- data %>%
  group_by(Q20, QB02) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Q20) %>%
  mutate(percentage = count / sum(count) * 100)


# Convertir en facteurs avec des labels explicites
labels_sport <- c("Jamais", "Rarement", "1x/sem", "2-3x/sem", "4-5x/sem", "Quotidien")
labels_jeux <- c("Jamais", "Rarement", "1-2h/sem", "3-5h/sem", "6-10h/sem", "Quotidien"

colors <- c( 1 = "#E69F00", 2 = "#56B4E9", 3 = "#009E73", 
            4 = "#F0E442", 5 = "#0072B2", 6 = "#D55E00")


# Diagramme amélioré en barres empilées
ggplot(data_prop, aes(x = Q20, y = percentage, fill = QB02)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Fréquence de pratique sportive",
       y = "Proportion (%)",
       fill = "Fréquence de jeux vidéo",
       title = "Comparaison entre pratique sportive et jeux vidéo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = data$Q20, fill = data$QB02)) +
  geom_bar(position = "dodge") +
  labs(x = "Fréquence de pratique sportive", 
       y = "Nombre d’individus",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal()
