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

labels_sport <- c("Chaque jour", "4 à 6 fois par semaine", "3 fois par semaine",
                  "2 fois par semaine", "Une fois par semaine", "Une fois par mois",
                  "Moins d'une fois par mois", "Jamais")

labels_sportred <- c("Jamais ou presque jamais", "Une fois par semaine",
                     "Plusieurs fois par semaine", "Chaque jour")


# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)

data$Q20 <- factor(labels_sport[data$Q20], levels = labels_sport)



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

data$Q20 <- factor(data$Q20, 
                levels = c("Jamais ou presque jamais", "Une fois par semaine", "Plusieurs fois par semaine", "Chaque jour"))







# GRAPHIQUE 1 : Freq JV en fonction du sport
data <- data %>%
  mutate(QB02 = case_when(
    QB02 == "Jamais" ~ "Pas joueur",  # Si c'est "Jamais", on laisse "Jamais"
    TRUE ~ "Joueur"  # Sinon, on met "Joueur"
  ))

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
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99")) +
  labs(title = "Répartition de la fréquence de jeux vidéo par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre









# GRAPHIQUE 2 : Freq JV simplifiée en fonction du sport
data_clean_sport_freqJV <- data %>% filter(!is.na(Q20) & !is.na(QB02) & QB02!="Jamais")

data_clean_sport_freqJV <- data_clean_sport_freqJV %>% 
  mutate(QB02 = case_when(
    QB02 == "Une fois par mois ou moins" |
      QB02 == "2-3 fois par mois" ~ "Moins d'une fois par semaine",
    QB02 == "Une fois par semaine" |
      QB02 == "Plusieurs fois par semaine" ~ "Au moins une fois par semaine",
    QB02 == "Tous les jours ou presque" ~ "Au quotidien" ,
  ))

# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJV <- data_clean_sport_freqJV %>%
  count(Q20, QB02) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)


data_pourc_sport_freqJV$QB02 <- factor(data_pourc_sport_freqJV$QB02, 
                                               levels = c("Moins d'une fois par semaine", "Au moins une fois par semaine", "Au quotidien"))



# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJV, aes(x = Q20, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99")) +
  labs(title = "Répartition de la fréquence de jeux vidéo par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre








# GRAPHIQUE 3 : Prop de JV en fonction de sport
data_clean_sport_freqJV <- data %>% filter(!is.na(Q20) & !is.na(QB02))

data_clean_sport_freqJV <- data_clean_sport_freqJV %>%
  mutate(QB02simp = ifelse(QB02 == "Jamais", "Non joueur", "Joueur"))

# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqJV <- data_clean_sport_freqJV %>%
  count(Q20, QB02simp) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)


# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqJV, aes(x = Q20, y = pct, fill = fct_rev(QB02simp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99")) +
  labs(title = "Répartition de la fréquence de jeux vidéo par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre

