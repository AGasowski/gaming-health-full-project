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


labels_non_oui <- c("Non", "Oui")


# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)


data <- data %>% 
  mutate(Q21A = case_when(
    Q21A == 1 ~ "Non",
    Q21A == 2 ~ "Oui"
  ))










# GRAPHIQUE 1 : 
data_clean_medecin_JV <- data %>% filter(!is.na(Q21A) & !is.na(QB02))
# Calculer les pourcentages par groupe de joueurs
data_pourc_medJV <- data_clean_medecin_JV %>%
  count(QB02, Q21A) %>%
  group_by(QB02) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_pourc_medJV, aes(x = QB02, y = pct, fill = Q21A)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Part des individus ayant été chez le médecin au cours de l'année pour chaque catégorie de joueur",
       x = "Fréquence de jeux vidéo",
       y = "Pourcentage",
       fill = "A été chez le médecin") +
  theme_minimal()
