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



print(data$Q20)
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

data <- data %>% 
  mutate(Q21A = case_when(
    Q21A == 1 ~ "Non",
    Q21A == 2 ~ "Oui"
  ))

data$Q20 <- factor(data$Q20, levels = labels_sportred)
print(data$Q21A)









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



#GRAPHIQUE2

data_clean_IMC_freqJA <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

# Calculer les pourcentages par groupe de santé physique
data_pourc_etatsante_freqJA <- data_clean_IMC_freqJA %>%
  count(IMC, qb07abcdef1) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

# Création du graphique à barres empilées (100%)
ggplot(data_pourc_etatsante_freqJA, aes(x = IMC, y = pct, fill = qb07abcdef1) +
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



#Graphique 3
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






#Graphique 4
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

# Création du graphique à barres empilées sans les non joueurs
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



#Graphique 5
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

# Création du graphique à barres empilées sans les non joueurs
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



#Graphique 6
data_clean_medecinJA <- data %>% filter(!is.na(Q21A) & !is.na(qb07abcdef1))
# Calculer les pourcentages par groupe de joueurs
data_pourc_med <- data_clean_medecinJA %>%
  count(qb07abcdef1, Q21A) %>%
  group_by(qb07abcdef1) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_pourc_med, aes(x = qb07abcdef1, y = pct, fill = Q21A)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Part des individus ayant été chez le médecin au cours de l'année pour chaque catégorie de joueur",
       x = "Fréquence de jeux d'argent",
       y = "Pourcentage",
       fill = "A été chez le médecin") +
  theme_minimal()


#Graphique 7
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

data_clean_medtot <- data %>% filter(!is.na(Q21A))
data_medecin_tot <- data_clean_medtot %>% 
  count(Q21A) %>% 
  mutate(pct = n / sum(n) * 100)




#Graphique 8

labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")
data$QB02 <- factor(labels_frequencejeu[data$QB02], levels = labels_frequencejeu)

labels_etatsante <- c("Pas du tout satisfaisant", "Peu satisfaisant",
                      "Plutôt satisfaisant", "Très satisfaisant")

data$Q17 <- factor(labels_etatsante[data$Q17], levels = labels_etatsante)

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




##Graphique 9

labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)

labels_etatsante <- c("Pas du tout satisfaisant", "Peu satisfaisant",
                      "Plutôt satisfaisant", "Très satisfaisant")

data$Q17 <- factor(labels_etatsante[data$Q17], levels = labels_etatsante)

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
