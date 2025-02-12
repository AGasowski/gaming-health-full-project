setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat")

install.packages("dplyr")
library(dplyr)

data <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

#datautiles contient tous les individus qui ont répondu au questionnaire sur les jeux
datautiles <- data %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))