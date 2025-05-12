rm(list=ls())

install.packages("dplyr")
install.packages("writexl")

library(dplyr)
library(writexl)
library(tidyverse)
library(readr)

setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


# Création de variables 

# Catégories d'IMC
data <- data %>% 
  mutate(IMC = case_when(
    q18imc < 18.5 ~ 1,
    q18imc > 30 ~ 2,
    q18imc > 25 ~ 3,
    TRUE ~ 4
  ))

#Catégorie de parieur (en mise maximale)
table(data$QB10B)
summary(data$QB10B)
print(data$QB10B)
sum(is.na(data$QB10B))
max(data$QB10B)
data$QB10B[!is.na(data$QB10B)]

data <- data %>%
  mutate(mise_cat = case_when(
    QB10B > 0 & QB10B <= 10 ~ 1,           # 1 pour 0-10
    QB10B > 10 & QB10B <= 25 ~ 2,          # 2 pour 10-25
    QB10B > 25 & QB10B <= 50 ~ 3,          # 3 pour 25-50
    QB10B > 50 ~ 4,                        # 4 pour + de 50
  ))




# Réduction jeux d'argent
data <- data %>%
  mutate(qb07abcdef1 = case_when(
    qb07abcdef1 == 1 ~ 1,           # 1 pour Jamais, 4 pour Au quotidien
    qb07abcdef1 == 2 |
      qb07abcdef1 == 3 ~ 2,
    qb07abcdef1 == 4 |
      qb07abcdef1 == 5 ~ 3,
    qb07abcdef1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07A1 = case_when(
    QB07A1 == 1 ~ 1,              # 1 pour Jamais, 4 pour Au quotidien
    QB07A1 == 2 |
      QB07A1 == 3 ~ 2,
    QB07A1 == 4 |
      QB07A1 == 5 ~ 3,
    QB07A1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07B1 = case_when(
    QB07B1 == 1 ~ 1,           # 1 pour Jamais, 4 pour Au quotidien
    QB07B1 == 2 |
      QB07B1 == 3 ~ 2,
    QB07B1 == 4 |
      QB07B1 == 5 ~ 3,
    QB07B1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07C1 = case_when(
    QB07C1 == 1 ~ 1,          # 1 pour Jamais, 4 pour Au quotidien
    QB07C1 == 2 |
      QB07C1 == 3 ~ 2,
    QB07C1 == 4 |
      QB07C1 == 5 ~ 3,
    QB07C1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07D1 = case_when(
    QB07D1 == 1 ~ 1,          # 1 pour Jamais, 4 pour Au quotidien
    QB07D1 == 2 |
      QB07D1 == 3 ~ 2,
    QB07D1 == 4 |
      QB07D1 == 5 ~ 3,
    QB07D1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07E1 = case_when(
    QB07E1 == 1 ~ 4,             # 1 pour Jamais, 4 pour Au quotidien
    QB07E1 == 2 |
      QB07E1 == 3 ~ 2,
    QB07E1 == 4 |
      QB07E1 == 5 ~ 3,
    QB07E1 == 6 ~ 4
  ))

data <- data %>%
  mutate(QB07F1 = case_when(
    QB07F1 == 1 ~ 1,           # 1 pour Jamais, 4 pour Au quotidien
    QB07F1 == 2 |
      QB07F1 == 3 ~ 2,
    QB07F1 == 4 |
      QB07F1 == 5 ~ 3,
    QB07F1 == 6 ~ 4
  ))





# Réduction jeux vidéos
data <- data %>%
  mutate(QB02 = case_when(
    QB02 == 1 ~ 1,            # 1 pour Jamais, 4 pour Au quotidien
    QB02 == 2 |
      QB02 == 3 ~ 2,
    QB02 == 4 |
      QB02 == 5 ~ 3,
    QB02 == 6 ~ 4
  ))




# Réduction sport
data <- data %>%
  mutate(Q20 = case_when(
    Q20 == 1 ~ "Au quotidien",
    Q20 == 2 |
      Q20 == 3 |
      Q20 == 4 ~ "Plusieurs fois par semaine",
    Q20 == 5 ~ "Une fois par semaine",
    Q20 == 6 |
      Q20 == 7 |
      Q20 == 8 ~ "Jamais ou presque jamais"
  ))

data <- data %>%
  mutate(Q20 = case_when(
    Q20 == "Au quotidien" ~ 4,                # 1 pour Jamais, 4 pour Au quotidien
    Q20 == "Plusieurs fois par semaine" ~ 3,
    Q20 == "Une fois par semaine" ~ 2,
    Q20 == "Jamais ou presque jamais" ~ 1
  ))



# Ajout de variables Joueur/Non joueur
data <- data %>%
  mutate(JoueurJA = ifelse(qb07abcdef1 == 1, 0, 1))
data <- data %>%
  mutate(JoueurJV = ifelse(QB02 == 1, 0, 1))



# Renommer les variables pour l'ACM
data <- data %>%
  rename(freqJA = qb07abcdef1, freqJV = QB02, freqSport = Q20, freqMedecin = Q21A,
         jeuPbSante = QB08F, critique = QB08G, pbArgent = QB08H, culpabilite= QB08I)




write_csv(data, "data_simplifiee_facto.csv")

