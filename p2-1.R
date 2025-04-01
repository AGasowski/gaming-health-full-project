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

labels_sportred <- c("Jamais ou presque jamais", "Une fois par semaine",
                     "Plusieurs fois par semaine", "Chaque jour")

labels_non_oui <- c("Non", "Oui")



# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)
data$QB07C1 <- factor(labels_frequencejeu[data$QB07C1], levels = labels_frequencejeu)

data$Q20 <- factor(labels_sport[data$Q20], levels = labels_sport)

ordre_IMC <- c("Insuffisance pondérale", "Corpulence normale", 
               "Surpoids", "Obésité")
data$IMC <- factor(data$IMC, levels = ordre_IMC)

data <- data %>% 
  mutate(Q20 = case_when(
    Q20 == "4 à 6 fois par semaine" |
      Q20 == "3 fois par semaine" |
      Q20 == "2 fois par semaine" ~ "Plusieurs fois par semaine",
    Q20 == "Moins d'une fois par mois" |
      Q20 == "Jamais" |
      Q20 == "Une fois par mois" ~ "Jamais ou presque jamais" ,
    Q20 == "Chaque jour" ~ "Chaque jour",
    Q20 == "Une fois par semaine" ~ "Une fois par semaine"
  ))


# GRAPHIQUE 1

#qui prend en compte seulement les joueurs, pour avoir un total à 100%
data_clean_IMC_freqJA_100 <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc) & qb07abcdef1!="Jamais")

data_pourc_etatsante_freqJA_100 <- data_clean_IMC_freqJA_100 %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_pourc_etatsante_freqJA_100, aes(x = IMC, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent des joueurs par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()


# GRAPHIQUE 2

data_clean_IMC_freqJV_100 <- data %>% filter(!is.na(QB02) & !is.na(q18imc) & QB02!="Jamais")

# Calculer les pourcentages par groupe de santé physique
data_pourc_etatsante_freqJV_100 <- data_clean_IMC_freqJV_100 %>%
  count(IMC, QB02) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_etatsante_freqJV_100, aes(x = IMC, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo des joueurs par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéos") +
  theme_minimal()



