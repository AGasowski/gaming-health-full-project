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



# Remplacement des valeurs numériques par les catégories
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)

ordre_IMC <- c("Insuffisance pondérale", "Corpulence normale", 
               "Surpoids", "Obésité")
data$IMC <- factor(data$IMC, levels = ordre_IMC)










# GRAPHIQUE 1 : Fréquence JV par IMC (seulement les joueurs)

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





# GRAPHIQUE 2 (inutile)

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

data_filter_etatsanteJV <- data_pourc_etatsante_freqJV %>%
  filter(QB02 != "Jamais")
ggplot(data_filter_etatsanteJV, aes(x = IMC, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéos") +
  theme_minimal()









# GRAPHIQUE 3 : Prop de joueur par IMC

#qui prend en compte seulement les joueurs, pour avoir un total à 100%
data_clean <- data %>% filter(!is.na(QB02) & !is.na(q18imc))

data_clean <- data_clean %>%
  mutate(QB02simp = ifelse(QB02 == "Jamais", "Non joueur", "Joueur"))

data_100 <- data_clean %>%
  count(IMC, QB02simp) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_100, aes(x = IMC, y = pct, fill = fct_rev(QB02simp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99")) +
  labs(title = "Proportion de joueurs de jeux vidéo par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Jeux vidéo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre










# GRAPHIQUE 3 : Fréquence de JV simplifiée (total population)
data_clean <- data %>% filter(!is.na(QB02) & !is.na(q18imc))

data_clean <- data_clean %>% 
  mutate(QB02 = case_when(
    QB02 == "Jamais" ~ "Jamais",
    QB02 == "Une fois par mois ou moins" |
      QB02 == "2-3 fois par mois" ~ "Rarement",
    QB02 == "Une fois par semaine" |
      QB02 == "Plusieurs fois par semaine" ~ "Chaque semaine",
    QB02 == "Tous les jours ou presque" ~ "Tous les jours ou presque" ,
  ))


data_pourc_etatsante_freqJA_100 <- data_clean %>%
  count(IMC, QB02) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

data_pourc_etatsante_freqJA_100$QB02 <- factor(data_pourc_etatsante_freqJA_100$QB02, 
                                                      levels = c("Jamais", "Rarement", "Chaque semaine", "Tous les jours ou presque"))


ggplot(data_pourc_etatsante_freqJA_100, aes(x = IMC, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition de la fréquence de jeux vidéo des joueurs par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal()










# GRAPHIQUE 4 : Fréquence de JV simplifiée (joueurs seulement)
data_clean <- data %>% filter(!is.na(QB02) & !is.na(q18imc) & QB02!="Jamais")

print(data_clean$QB02)

data_clean <- data_clean %>% 
  mutate(QB02 = case_when(
    QB02 == "Une fois par mois ou moins" |
      QB02 == "2-3 fois par mois" ~ "Moins d'une fois par semaine",
    QB02 == "Une fois par semaine" |
      QB02 == "Plusieurs fois par semaine" ~ "Au moins une fois par semaine",
    QB02 == "Tous les jours ou presque" ~ "Au quotidien" ,
  ))


data_pourc_etatsante_freqJA_100 <- data_clean %>%
  count(IMC, QB02) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

data_pourc_etatsante_freqJA_100$QB02 <- factor(data_pourc_etatsante_freqJA_100$QB02, 
                                               levels = c("Moins d'une fois par semaine", "Au moins une fois par semaine", "Au quotidien"))


ggplot(data_pourc_etatsante_freqJA_100, aes(x = IMC, y = pct, fill = QB02)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99")) +
  labs(title = "Répartition de la fréquence de jeux vidéo des joueurs par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Fréquence de jeux vidéo") +
  theme_minimal()
