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


# Création d'un vecteur de correspondance
labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")

labels_etatsante <- c("Pas du tout satisfaisant", "Peu satisfaisant",
                      "Plutôt satisfaisant", "Très satisfaisant")


# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)
data$Q17 <- factor(labels_etatsante[data$Q17], levels = labels_etatsante)












# GRAPHIQUE 1
data_clean_etatsanteJV <- data %>% filter(!is.na(Q17) & !is.na(QB02))
data_etatsanteJV <- data_clean_etatsanteJV %>% 
  count(QB02, Q17) %>% 
  group_by(QB02) %>% 
  mutate(pct = n / sum(n) * 100)


data_etatsantetot <- data_clean_etatsanteJV %>% 
  count(Q17) %>% 
  mutate(pct = n / sum(n) * 100)


ggplot(data_etatsanteJV, aes(x = QB02, y = pct, fill = Q17)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Qualification de son état de santé en fonction de la catégorie de joueurs de jeux vidéos",
       x = "Fréquence de jeux vidéo",
       y = "Pourcentage",
       fill = "Etat de santé") +
  theme_minimal()
