rm(list=ls())

install.packages("writexl")   # pour écrire dans un fichier Excel

library(writexl)
library(dplyr)

setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv("data_simplifiee.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))

datasansNA <- datainitial %>% 
  filter(!is.na(IMC) & !is.na(Etat_sante) & !is.na(Sportif) & !is.na(freqSport) & !is.na(freqMedecin) & 
           !is.na(JoueurJA) & !is.na(JoueurJV) & !is.na(freqJA) & !is.na(freqJV) & !is.na(JoueurPS))

# Définir les variables à mettre en lignes et en colonnes
vars_lignes <- c("JoueurJA", "JoueurJV", "freqJA", "freqJV", "JoueurPS")

vars_colonnes <- c("freqSport", "freqMedecin", "Sportif", "Etat_sante", "IMC")


# Initialiser une liste pour stocker les résultats
resultats <- list()

# Boucle sur toutes les combinaisons ligne x colonne
for (ligne in vars_lignes) {
  for (colonne in vars_colonnes) {
    # Créer une table de contingence
    tab <- table(datasansNA[[ligne]], datasansNA[[colonne]])
    
    # Faire le test du chi²
    test <- chisq.test(tab)
    
    # Calcul du V de Cramer
    N <- sum(tab)  # Taille de l'échantillon
    k <- min(nrow(tab), ncol(tab))  # Plus petit nombre de catégories
    V_Cramer <- sqrt(test$statistic / (N * (k - 1)))
    
    # Déterminer l'interprétation du test
    interpretation <- ifelse(test$p.value <= 0.05, "Il y a un lien", "Il y a indépendance")
    
    # Déterminer la force du lien
    force_lien <- case_when(
      V_Cramer < 0.1 ~ "Très faible",
      V_Cramer < 0.3 ~ "Faible",
      V_Cramer < 0.5 ~ "Modéré",
      TRUE ~ "Fort"
    )
    
    # Sauvegarder les résultats
    nom <- paste(ligne, "vs", colonne)
    resultats[[nom]] <- data.frame(
      Variable_Ligne = ligne,
      Variable_Colonne = colonne,
      Chi2 = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      Degre_liberté = as.numeric(test$parameter),
      Interpretation = interpretation,
      V_Cramer = as.numeric(V_Cramer),
      Force_Lien = force_lien
    )
  }
}

# Combiner tous les résultats dans un seul data frame
tableau_final <- do.call(rbind, resultats)

# Écrire dans un fichier Excel
write_xlsx(tableau_final, "resultats_corr_P2.xlsx")

