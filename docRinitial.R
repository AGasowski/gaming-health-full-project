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

# catégories IMC
data <- data %>% 
  mutate(santephysique = case_when(
    q18imc < 18.5 ~ "insuffisance pondérale",
    q18imc > 30 ~ "obésité",
    q18imc > 25 ~ "surpoids",
    TRUE ~ "corpulence normale"
  ))

# Création de deux variables IMC et IMC_categorie
data$Q18 <- as.numeric(data$Q18)
data$Q19 <- as.numeric(data$Q19)
data$IMC <- data$Q19 / (data$Q18 / 100)^2
data$IMC[data$IMC < 8 | data$IMC > 50] <- NA
data$IMC_categorie <- cut(data$IMC,
                          breaks = c(0, 18.5, 25, 30, Inf),
                          labels = c("Maigreur", "Normal", "Surpoids", "Obésité"))

# Création variable Etat_de_santé
categories_Q17 <- c("Pas du tout satisfaisant", "Peu satisfaisant", "Plutôt satisfaisant", "Très satisfaisant")
data$Etat_de_santé <- factor(data$Q17, levels = 1:4, labels = categories_Q17)

# Création variables score_ADRS et score_ADRS_categorie
variable_ADRS <- c("Q22A", "Q22B", "Q22C", "Q22D", "Q22E", "Q22F", "Q22G", "Q22H", "Q22I", "Q22J")
data$score_ADRS <- rowSums(data[, variable_ADRS] == "1")
data$score_ADRS_categorie <- cut(data$score_ADRS,
                                 breaks = c(-1, 3, 6, 11),
                                 labels = c("Peu de risque d’EDC", "Risque d’EDC modéré", "Risque d’EDC important"))

# EDC = Épisode dépressif caractérisé

#Création data_réduit qui contient uniquement les variables crées et modifiées
data_reduit <- data[, c("score_ADRS", "score_ADRS_categorie", "Etat_de_santé", "IMC", "IMC_categorie")]