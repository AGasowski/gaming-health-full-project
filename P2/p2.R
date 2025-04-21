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
