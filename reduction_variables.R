rm(list=ls())

install.packages("dplyr")
install.packages("writexl")

library(dplyr)
library(writexl)
library(tidyverse)
library(readr)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


data <- data %>% 
  mutate(IMC = case_when(
    q18imc < 18.5 ~ "Insuffisance pondérale",
    q18imc > 30 ~ "Obésité",
    q18imc > 25 ~ "Surpoids",
    TRUE ~ "Corpulence normale"
  ))


# Réduction jeux d'argent
data <- data %>%
  mutate(qb07abcdef1 = case_when(
    qb07abcdef1 == 1 ~ "Jamais",
    qb07abcdef1 == 2 |
      qb07abcdef1 == 3 ~ "Moins d'une fois par semaine",
    qb07abcdef1 == 4 |
      qb07abcdef1 == 5 ~ "Au moins une fois par semaine",
    qb07abcdef1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07A1 = case_when(
    QB07A1 == 1 ~ "Jamais",
    QB07A1 == 2 |
      QB07A1 == 3 ~ "Moins d'une fois par semaine",
    QB07A1 == 4 |
      QB07A1 == 5 ~ "Au moins une fois par semaine",
    QB07A1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07B1 = case_when(
    QB07B1 == 1 ~ "Jamais",
    QB07B1 == 2 |
      QB07B1 == 3 ~ "Moins d'une fois par semaine",
    QB07B1 == 4 |
      QB07B1 == 5 ~ "Au moins une fois par semaine",
    QB07B1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07C1 = case_when(
    QB07C1 == 1 ~ "Jamais",
    QB07C1 == 2 |
      QB07C1 == 3 ~ "Moins d'une fois par semaine",
    QB07C1 == 4 |
      QB07C1 == 5 ~ "Au moins une fois par semaine",
    QB07C1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07D1 = case_when(
    QB07D1 == 1 ~ "Jamais",
    QB07D1 == 2 |
      QB07D1 == 3 ~ "Moins d'une fois par semaine",
    QB07D1 == 4 |
      QB07D1 == 5 ~ "Au moins une fois par semaine",
    QB07D1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07E1 = case_when(
    QB07E1 == 1 ~ "Jamais",
    QB07E1 == 2 |
      QB07E1 == 3 ~ "Moins d'une fois par semaine",
    QB07E1 == 4 |
      QB07E1 == 5 ~ "Au moins une fois par semaine",
    QB07E1 == 6 ~ "Au quotidien"
  ))

data <- data %>%
  mutate(QB07F1 = case_when(
    QB07F1 == 1 ~ "Jamais",
    QB07F1 == 2 |
      QB07F1 == 3 ~ "Moins d'une fois par semaine",
    QB07F1 == 4 |
      QB07F1 == 5 ~ "Au moins une fois par semaine",
    QB07F1 == 6 ~ "Au quotidien"
  ))





# Réduction jeux vidéos
data <- data %>%
  mutate(QB02 = case_when(
    QB02 == 1 ~ "Jamais",
    QB02 == 2 |
      QB02 == 3 ~ "Moins d'une fois par semaine",
    QB02 == 4 |
      QB02 == 5 ~ "Au moins une fois par semaine",
    QB02 == 6 ~ "Au quotidien"
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



# Ajout de variables Joueur/Non joueur
data <- data %>%
  mutate(JoueurJA = ifelse(qb07abcdef1 == "Jamais", "Non joueur", "Joueur"))
data <- data %>%
  mutate(JoueurJV = ifelse(QB02 == "Jamais", "Non joueur", "Joueur"))



# Renommer les variables pour l'ACM
data <- data %>%
  rename(freqJA = qb07abcdef1, freqJV = QB02, freqSport = Q20, freqMedecin = Q21A)




write_csv(data, "data_simplifiee.csv")

