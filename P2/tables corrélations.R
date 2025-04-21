rm(list=ls())

install.packages("writexl")   # pour écrire dans un fichier Excel

library(writexl)
library(dplyr)

setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))

# Définir les variables à mettre en lignes et en colonnes
vars_lignes <- c("QB07A1", "QB07B1", "QB07C1", "QB07D1", "QB07E1", "QB07F1", "qb07abcdef1",  
                 "QB08A", "QB08B", "QB08C", "QB08D", "QB08E", "QB08F", "QB08G", "QB08H", "QB08I")

vars_colonnes <- c("Q20", "Q21A", "ADRS_cat", "Q21C", "Q21E", "Q22A", "Q22B", "Q22C",  
                   "Q22D", "Q22E", "Q22F", "Q22G", "Q22H", "Q22I", "Q22J", "Q23", "Q24")


# Initialiser une liste pour stocker les résultats
resultats <- list()

# Boucle sur toutes les combinaisons ligne x colonne
for (ligne in vars_lignes) {
  for (colonne in vars_colonnes) {
    # Créer une table de contingence
    tab <- table(data[[ligne]], data[[colonne]])
    
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
write_xlsx(tableau_final, "resultats_chi2.xlsx")

