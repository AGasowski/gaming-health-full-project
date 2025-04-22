rm(list=ls())

library(writexl)
library(dplyr)

setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv("data_simplifiee.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D) & !is.na(mise_habituelle))

# Exemple avec deux variables catégorielles : var1 et var2
table_cont <- table(data$freqJA, data$mise_habituelle)

chi_result <- chisq.test(table_cont)

# Affichage des résultats
chi_result

if (chi_result$p.value < 0.05) {
  cat("✅ Il existe une association significative entre les deux variables (p < 0.05).\n")
} else {
  cat("❌ Aucune association significative détectée (p ≥ 0.05).\n")
}


# Nécessite le package 'lsr'
install.packages("lsr")  # à faire une seule fois
library(lsr)

cramersV_result <- cramersV(table_cont)
cramersV_result


interpretation <- if (cramersV_result < 0.1) {
  "Très faible"
} else if (cramersV_result < 0.2) {
  "Faible"
} else if (cramersV_result < 0.4) {
  "Modérée"
} else if (cramersV_result < 0.6) {
  "Forte"
} else {
  "Très forte"
}

cat("🔍 Force de l'association (V de Cramér) :", interpretation, "\n")
















# Filtrer les individus ayant répondu aux questions
data2 <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D) & !is.na(mise_maximale))

# Exemple avec deux variables catégorielles : var1 et var2
table_cont <- table(data2$freqJA, data2$mise_habituelle)

chi_result <- chisq.test(table_cont)

# Affichage des résultats
chi_result

if (chi_result$p.value < 0.05) {
  cat("✅ Il existe une association significative entre les deux variables (p < 0.05).\n")
} else {
  cat("❌ Aucune association significative détectée (p ≥ 0.05).\n")
}



cramersV_result <- cramersV(table_cont)
cramersV_result


interpretation <- if (cramersV_result < 0.1) {
  "Très faible"
} else if (cramersV_result < 0.2) {
  "Faible"
} else if (cramersV_result < 0.4) {
  "Modérée"
} else if (cramersV_result < 0.6) {
  "Forte"
} else {
  "Très forte"
}

cat("🔍 Force de l'association (V de Cramér) :", interpretation, "\n")

