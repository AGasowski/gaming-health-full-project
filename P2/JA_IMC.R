rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")


library(vcd)
library(dplyr)
library(ggplot2)
library(forcats)

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



# Remplacement des valeurs numériques par les catégories
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)
data$QB07C1 <- factor(labels_frequencejeu[data$QB07C1], levels = labels_frequencejeu)



ordre_IMC <- c("Insuffisance pondérale", "Corpulence normale", 
               "Surpoids", "Obésité")
data$IMC <- factor(data$IMC, levels = ordre_IMC)




# GRAPHIQUE 1 : Prop de joueurs et Non joueurs par IMC

#qui prend en compte seulement les joueurs, pour avoir un total à 100%
data_clean <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

data_clean <- data_clean %>%
  mutate(qb07simp = ifelse(qb07abcdef1 == "Jamais", "Non joueur", "Joueur"))

data_100 <- data_clean %>%
  count(IMC, qb07simp) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_100, aes(x = IMC, y = pct, fill = fct_rev(qb07simp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion de joueurs de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Jeux d'argent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre



# GRAPHIQUE 2 : Fréquence JA par IMC (seulement les joueurs)

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



# GRAPHIQUE 3 : Fréquence de JA simplifiée (total population)
data_clean <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

data_clean <- data_clean %>% 
  mutate(qb07abcdef1 = case_when(
    qb07abcdef1 == "Jamais" ~ "Jamais",
    qb07abcdef1 == "Une fois par mois ou moins" |
      qb07abcdef1 == "2-3 fois par mois" ~ "Rarement",
    qb07abcdef1 == "Une fois par semaine" |
      qb07abcdef1 == "Plusieurs fois par semaine" |
      qb07abcdef1 == "Tous les jours ou presque" ~ "Au moins une fois par semaine" ,
  ))


data_pourc_etatsante_freqJA_100 <- data_clean %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

data_pourc_etatsante_freqJA_100$qb07abcdef1 <- factor(data_pourc_etatsante_freqJA_100$qb07abcdef1, 
                                                      levels = c("Jamais", "Rarement", "Au moins une fois par semaine"))


ggplot(data_pourc_etatsante_freqJA_100, aes(x = IMC, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent des joueurs par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()











# GRAPHIQUE 3 (inutile)

data_clean_IMC_freqJA <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

# Calculer les pourcentages par groupe de santé physique
data_pourc_etatsante_freqJA <- data_clean_IMC_freqJA %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_etatsante_freqJA, aes(x = IMC, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()

# Création du graphique à barres empilées sans les non joueurs
data_filter_etatsanteJA <- data_pourc_etatsante_freqJA %>%
  filter(qb07abcdef1 != "Jamais")

ggplot(data_filter_etatsanteJA, aes(x = IMC, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()

#Graphique qui prend en compte seulement les joueurs, pour avoir un total à 100%
data_clean_IMC_freqJA_100 <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc) & qb07abcdef1!="Jamais")

data_pourc_etatsante_freqJA_100 <- data_clean_IMC_freqJA_100 %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_pourc_etatsante_freqJA_100, aes(x = IMC, y = pct, fill = qb07abcdef1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux d'argent") +
  theme_minimal()



