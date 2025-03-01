rm(list=ls())

install.packages("dplyr")
install.packages("vcd")

library(vcd)
library(dplyr)

# Lire le fichier CSV
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Créer des vecteurs des variables de santé et de jeux vidéo (JV)
variables_sante <- c("Q17")
variable_ADRS <- c("Q22A", "Q22B", "Q22C", "Q22D", "Q22E", "Q22F", "Q22G", "Q22H", "Q22I", "Q22J")
variables_JV <- c("QB01A")

# Combiner les vecteurs
variable <- c(variables_sante, variable_ADRS, variables_JV)

# Filtrer les colonnes utiles
data <- datainitial[, variable]

# Créer un vecteur de catégories pour QB01A
categories_QBO1A <- c("Aucun", "1-2 jours", "3-5 jours", "6-9 jours", "10-19 jours", "20-29 jours", "Tous les jours ou presque")

# Convertir la colonne QB01A en facteur avec les nouvelles catégories
data$QB01A <- factor(data$QB01A, levels = 1:7, labels = categories_QBO1A)

# Créer un vecteur de catégories pour Q17
categories_Q17 <- c("Pas du tout satisfaisant", "Peu satisfaisant", "Plutôt satisfaisant", "Très satisfaisant")

# Créer la colonne Etat_de_santé d'après la Q17
data$Etat_de_santé <- factor(data$Q17, levels = 1:4, labels = categories_Q17)

# Renommer la colonne QB01A en "joue à des jeux vidéo"
colnames(data)[colnames(data) == "QB01A"] <- "joue à des jeux vidéo"

# Calculer le score ADRS
data$score_ADRS <- rowSums(data[, variable_ADRS] == "1")

# Catégoriser le score ADRS
data$score_ADRS_categorie <- cut(data$score_ADRS,
                                 breaks = c(-1, 3, 6, 11),
                                 labels = c("Peu de risque d’EDC", "Risque d’EDC modéré", "Risque d’EDC important"))




# Convertir les colonnes de type factor en chaînes de caractères
data[] <- lapply(data, function(x) if(is.factor(x)) as.character(x) else x)

# Remplacer les NA par la chaîne de caractères "NA"
data[is.na(data)] <- "NA"
