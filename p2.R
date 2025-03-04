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
  mutate(IMC = case_when(
    q18imc < 18.5 ~ "Insuffisance pondérale",
    q18imc > 30 ~ "Obésité",
    q18imc > 25 ~ "Surpoids",
    TRUE ~ "Corpulence normale"
  ))


# Création d'un vecteur de correspondance
labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")

labels_sport <- c("Chaque jour", "4 à 6 fois par semaine", "3 fois par semaine",
                  "2 fois par semaine", "Une fois par semaine", "Une fois par mois",
                  "Moins d'une fois par mois", "Jamais")


# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)

data$Q20 <- factor(labels_sport[data$Q20], levels = labels_sport)



ordre_IMC <- c("Insuffisance pondérale", "Corpulence normale", 
               "Surpoids", "Obésité")
data$IMC <- factor(data$IMC, levels = ordre_IMC)



print(data$Q20)
data <- data %>% 
  mutate(Q20 = case_when(
    Q20 == "4 à 6 fois par semaine" |
      Q20 == "3 fois par semaine" |
      Q20 == "2 fois par semaine" ~ "Plusieurs fois par semaine",
    Q20 == "Moins d'une fois par mois" |
      Q20 == "Jamais" ~ "Jamais ou presque jamais" |
      Q20 == "Une fois par mois",
    Q20 == "Chaque jour" ~ "Chaque jour",
    Q20 == "Une fois par semaine" ~ "Une fois par semaine"
  ))









##Calcul IMC moyen en fonction de la fréquence de jeux d'argents (inutile)
aggregate( q18imc ~ qb07abcdef1, data = data, FUN = mean, na.rm = TRUE)

#On enlève la valeur aberrante
data_clean <- data[data$q18imc <= 100, ]

aggregate( q18imc ~ qb07abcdef1, data = data_clean, FUN = mean, na.rm = TRUE)

##Calcul IMC moyen en fonction de la fréquence de jeux vidéos
aggregate( q18imc ~ QB02, data = data_clean, FUN = mean, na.rm = TRUE)


#Fréquence de jeu et Etat de santé
tab <- table(data$santephysique, data$QB02)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct




#GRAPHIQUE1

data_clean_IMC_freqJV <- data %>% filter(!is.na(QB02) & !is.na(q18imc))

# Calculer les pourcentages par groupe de santé physique
data_pourc_etatsante_freqJV <- data_clean_IMC_freqJV %>%
  count(IMC, QB02) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_etatsante_freqJV, aes(x = IMC, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéos") +
  theme_minimal()


#GRAPHIQUE2

data_clean_IMC_freqJA <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

# Calculer les pourcentages par groupe de santé physique
data_pourc_etatsante_freqJA <- data_clean_IMC_freqJA %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_etatsante_freqJA, aes(x = IMC, y = pct, fill = ifelse(qb07abcdef1 == "Jamais", NA, qb07abcdef1))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()








data_clean_sport_freq <- data %>% filter(!is.na(Q20) & !is.na(qb07abcdef1))
# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJA <- data_clean_sport_freq %>%
  count(Q20, qb07abcdef1) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJA, aes(x = Q20, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par catégorie d'IMC",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()
