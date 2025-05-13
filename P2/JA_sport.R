rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")


library(vcd)
library(dplyr)
library(ggplot2)
library(ggthemes)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


data <- data %>%
  mutate(JAsansPS = pmax(QB07A1, QB07B1, QB07D1, QB07E1, QB07F1, na.rm = TRUE))




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




# Remplacement des valeurs numériques par les catégories
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


data$Q20 <- factor(labels_sportred[data$Q20], levels = labels_sportred)









# GRAPHIQUE 1 : GRAPHE DOUBLE Sport en fonction de JA vs PS
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
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5, reverse = TRUE)) +
  labs(title = "Proportion de joueurs de jeux d'argent en fonction de la fréquence de pratique sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Jeux d'argent") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) + # Inverser l'ordre dans la légende
  theme_minimal()

# Diagramme double
data_clean_sport_freq_100 <- data %>%
  filter(!is.na(Q20), !is.na(qb07abcdef1), !is.na(QB07C1))  # Éliminer les NA dès le début


# Création des variables binaires
data_clean_sport_freq_100 <- data_clean_sport_freq_100 %>%
  mutate(qb07simp = ifelse(qb07abcdef1 == "Jamais", "Non joueur", "Joueur"),
         QB07C1simp = ifelse(QB07C1 == "Jamais", "Non joueur", "Joueur"),
         Q20 = ifelse(Q20 == "Jamais ou presque jamais", "Pas sportif", "Sportif"))


data_pourc_sport_freq_100_JA <- data_clean_sport_freq_100 %>%
  count(qb07simp, Q20) %>%
  group_by(qb07simp) %>%
  mutate(pct = n / sum(n) * 100) %>%
  mutate(Type = "Jeux d'argent")  # Ajouter une colonne pour distinguer
print(data_pourc_sport_freq_100_JA)

data_pourc_sport_freq_100_PS <- data_clean_sport_freq_100 %>%
  count(QB07C1simp, Q20) %>%
  group_by(QB07C1simp) %>%
  mutate(pct = n / sum(n) * 100) %>%
  rename(qb07simp = QB07C1simp) %>%  # Harmoniser les noms
  mutate(Type = "Paris sportifs")  # Ajouter une colonne pour distinguer

# Fusionner les deux ensembles de données
data_pourc_sport_freq_100_total <- bind_rows(data_pourc_sport_freq_100_JA, data_pourc_sport_freq_100_PS)

# Création du graphique avec barres côte à côte
ggplot(data_pourc_sport_freq_100_total, aes(x = qb07simp, y = pct, fill = Q20)) +
  geom_bar(stat = "identity", position = "fill") +  # Empilement normalisé à 100%
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_fill(vjust = 0.5)) +  # Texte centré sur chaque barre
  facet_wrap(~Type) +  # Séparer jeux d'argent et paris sportifs
  labs(title = "Répartition des sportifs et non-sportifs parmi les joueurs et non-joueurs",
       x = "Jeux d'argent / de paris sportifs",
       y = "Pourcentage",
       fill = "Pratique sportive") +
  scale_y_continuous(labels = scales::percent) +  # Afficher en pourcentage
  theme_minimal()









# GRAPHIQUE 2 : GRAPHE TRIPLE Sport en fonction de JA vs PS vs JAsansPS

# Création du graphique
data_clean_sport_freq_100 <- data %>%
  filter(!is.na(Q20), !is.na(qb07abcdef1), !is.na(QB07C1), !is.na(JAsansPS))  # Éliminer les NA dès le début


# Création des variables binaires
data_clean_sport_freq_100 <- data_clean_sport_freq_100 %>%
  mutate(qb07simp = ifelse(qb07abcdef1 == "Jamais", "Non joueur", "Joueur"),
         QB07C1simp = ifelse(QB07C1 == "Jamais", "Non joueur", "Joueur"),
         Q20 = ifelse(Q20 == "Jamais ou presque jamais", "Pas sportif", "Sportif"),
         JAsansPS = ifelse(JAsansPS == 1, "Non joueur", "Joueur"))


# Calcul pourcentage
data_pourc_sport_freq_100_JAsansPS <- data_clean_sport_freq_100 %>%
  count(JAsansPS, Q20) %>%
  group_by(JAsansPS) %>%
  mutate(pct = n / sum(n) * 100) %>%
  mutate(Type = "Jeux d'argent hors paris sportifs")  # Ajouter une colonne pour distinguer

data_pourc_sport_freq_100_JA <- data_clean_sport_freq_100 %>%
  count(qb07simp, Q20) %>%
  group_by(qb07simp) %>%
  mutate(pct = n / sum(n) * 100) %>%
  rename(JAsansPS = qb07simp) %>%  # Harmoniser les noms
  mutate(Type = "Jeux d'argent")  # Ajouter une colonne pour distinguer
print(data_pourc_sport_freq_100_JA)

data_pourc_sport_freq_100_PS <- data_clean_sport_freq_100 %>%
  count(QB07C1simp, Q20) %>%
  group_by(QB07C1simp) %>%
  mutate(pct = n / sum(n) * 100) %>%
  rename(JAsansPS = QB07C1simp) %>%  # Harmoniser les noms
  mutate(Type = "Paris sportifs")  # Ajouter une colonne pour distinguer


# Fusionner les deux ensembles de données
data_pourc_sport_freq_100_total <- bind_rows(data_pourc_sport_freq_100_JAsansPS, data_pourc_sport_freq_100_JA, data_pourc_sport_freq_100_PS)


# Ordre dans les barres
data_pourc_sport_freq_100_total <- data_pourc_sport_freq_100_total %>%
  mutate(Type = factor(Type, levels = c("Jeux d'argent hors paris sportifs", "Jeux d'argent", "Paris sportifs")))


# Création du graphique avec barres côte à côte
ggplot(data_pourc_sport_freq_100_total, aes(x = JAsansPS, y = pct, fill = Q20)) +
  geom_bar(stat = "identity", position = "fill") +  # Empilement normalisé à 100%
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_fill(vjust = 0.5)) +  # Texte centré sur chaque barre
  scale_fill_few() +
  facet_wrap(~Type) +  # Séparer jeux d'argent et paris sportifs
  labs(x = "Jeux d'argent",
       y = "Pourcentage",
       fill = "Pratique sportive") +
  scale_y_continuous(labels = scales::percent) +  # Afficher en pourcentage
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre









# GRAPHIQUE 3 : Fréquence précise JA en fonction du sport (que les joueurs)
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









# GRAPHIQUE 4 : Freq de JA en fonction du sport (pop totale)
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
  labs(title = "Répartition de la fréquence de jeux d'argent par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()


# Création du graphique à barres empilées sans les non joueurs (inutile)
data_filter_sportJA <- data_pourc_sport_freqJA %>%
  filter(qb07abcdef1 != "Jamais")

ggplot(data_filter_sportJA, aes(x = Q20, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()









# GRAPHIQUE 5 : Freq PS en fonction de sport (pop totale)
data_clean_sport_freqPS <- data %>% filter(!is.na(Q20) & !is.na(QB07C1))
# Calculer les pourcentages par groupe de sportifs
data_pourc_sport_freqPS <- data_clean_sport_freqPS %>%
  count(Q20, QB07C1) %>%
  group_by(Q20) %>%
  mutate(pct = n / sum(n) * 100)


# Création du graphique à barres empilées (100%)
ggplot(data_pourc_sport_freqPS, aes(x = Q20, y = pct, fill = QB07C1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de paris sportifs par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de paris sportifs") +
  theme_minimal()

# Création du graphique à barres empilées sans les non joueurs (inutile)
data_filter_sportPS <- data_pourc_sport_freqPS %>%
  filter(QB07C1 != "Jamais")

ggplot(data_filter_sportPS, aes(x = Q20, y = pct, fill = QB07C1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de paris sportifs par fréquence d'activité sportive",
       x = "Fréquence de pratique sportive",
       y = "Pourcentage",
       fill = "Fréquence de paris sportifs") +
  theme_minimal()

