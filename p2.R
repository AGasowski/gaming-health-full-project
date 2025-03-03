rm(list=ls())

install.packages("dplyr")
install.packages("vcd")

library(vcd)
library(dplyr)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))



###SANTE


data <- data %>% 
  mutate(santephysique = case_when(
    q18imc < 18.5 ~ "insuffisance pondérale",
    q18imc > 30 ~ "obésité",
    q18imc > 25 ~ "surpoids",
    TRUE ~ "corpulence normale"
  ))




##Calcul IMC moyen en fonction de la fréquence de jeu
aggregate( q18imc ~ qb07abcdef1, data = data, FUN = mean, na.rm = TRUE)

#On enlève la valeur aberrante
data_clean <- data[data$q18imc <= 100, ]

summary(data_clean$q18imc)

aggregate( q18imc ~ qb07abcdef1, data = data_clean, FUN = mean, na.rm = TRUE)