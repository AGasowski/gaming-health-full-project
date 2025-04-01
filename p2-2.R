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



print(data$Q20)



# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)
data$QB07C1 <- factor(labels_frequencejeu[data$QB07C1], levels = labels_frequencejeu)


data <- data %>% 
  mutate(Q20 = case_when(
    Q20 == 2 |
      Q20 == 3 |
      Q20 == 4 ~ 3,
    Q20 == 7 |
      Q20 == 8 |
      Q20 == 5 ~ 1 ,
    Q20 == 1 ~ 4,
    Q20 == 6 ~ 2
  ))

print(data$Q20)
data$Q20 <- factor(labels_sportred[data$QB07C1], levels = labels_sportred)




# GRAPHIQUE 1 (inutile)
data_clean_sport_freq_100 <- data %>% filter(!is.na(Q20) & !is.na(qb07abcdef1) & qb07abcdef1!="Jamais")
# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJA_100 <- data_clean_sport_freq_100 %>%
  count(Q20, qb07abcdef1) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJA_100, aes(x = Q20, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent des joueurs par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()


# GRAPHIQUE 1 (utile)
qb07abcdef1 <- data %>% filter (!is.na(qb07abcdef1))
data$qb07simp <- ifelse(data$qb07abcdef1 == "Jamais", "Non joueur", "Joueur")

data_clean_sport_freq_100 <- data %>% filter(!is.na(Q20) & !is.na(qb07abcdef1))
# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJA_100 <- data_clean_sport_freq_100 %>%
  count(Q20, qb07simp) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)


# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJA_100, aes(x = Q20, y = pct, fill = qb07simp)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent des joueurs par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()


# GRAPHIQUE 4

data_clean_sport_freqJV <- data %>% filter(!is.na(Q20) & !is.na(QB02))
# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJV <- data_clean_sport_freqJV %>%
  count(Q20, QB02) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)


# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJV, aes(x = Q20, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal()


data_filter_sportJV <- data_pourc_sport_freqJV %>%
  filter(QB02 != "Jamais")
ggplot(data_filter_sportJV, aes(x = Q20, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal()