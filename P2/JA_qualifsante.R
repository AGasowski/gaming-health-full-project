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
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)
data$QB07C1 <- factor(labels_frequencejeu[data$QB07C1], levels = labels_frequencejeu)

data$Q17 <- factor(labels_etatsante[data$Q17], levels = labels_etatsante)



# GRAPHIQUE 1


data_clean_etatsanteJA <- data %>% filter(!is.na(Q17) & !is.na(qb07abcdef1))
data_etatsanteJA <- data_clean_etatsanteJA %>% 
  count(qb07abcdef1, Q17) %>% 
  group_by(qb07abcdef1) %>% 
  mutate(pct = n / sum(n) * 100)


data_cleantot <- data %>% filter(!is.na(Q17))
data_etatsantetot <- data_cleantot %>% 
  count(Q17) %>% 
  mutate(pct = n / sum(n) * 100)



ggplot(data_etatsanteJA, aes(x = qb07abcdef1, y = pct, fill = Q17)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Qualification de son état de santé en fonction de la catégorie de joueurs de jeux d'argent",
       x = "Fréquence de jeux d'argent",
       y = "Pourcentage",
       fill = "Etat de santé") +
  theme_minimal()
