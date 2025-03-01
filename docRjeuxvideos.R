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
    q18imc < 17 | q18imc > 30 ~ "très mauvaise",
    q18imc < 18.5 | q18imc > 25 ~ "plutôt mauvaise",
    TRUE ~ "bonne"
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
data_reduit <- data[, c("score_ADRS", "score_ADRS_categorie", "IMC", "IMC_categorie")]


# Création variable jours_joué_par_mois
categories_QBO1A <- c("Aucun", "1-2 jours", "3-5 jours", "6-9 jours", "10-19 jours", "20-29 jours", "Tous les jours ou presque")
data$QB01A <- factor(data$QB01A, levels = 1:7, labels = categories_QBO1A)
data_reduit$jours_joué_par_mois <- data$QB01A

# Création de la variable dépense_JV_par_année avec la somme ou 0
data$dépense_JV_par_année <- ifelse(data$QB03A == 2, as.numeric(data$QB03B), 0)
data_reduit$dépense_JV_par_année <- data$dépense_JV_par_année

# Création de la variable dépense_catégorie
data$dépense_catégorie <- case_when(
  data$dépense_JV_par_année == 0 ~ "aucune dépense",
  data$dépense_JV_par_année < 60 ~ "faible dépense",
  data$dépense_JV_par_année >= 60 & data$dépense_JV_par_année < 180 ~ "dépense modérée",
  data$dépense_JV_par_année >= 180 & data$dépense_JV_par_année < 600 ~ "forte dépense",
  data$dépense_JV_par_année >= 600 ~ "dépense conséquente"
)
data_reduit$dépense_catégorie <- data$dépense_catégorie

print(table(data$dépense_catégorie))

# Création de la variable difficulté_controle_JV avec des libellés
data$difficulté_controle_JV <- factor(data$QB06A, 
                                  levels = 1:5, 
                                  labels = c("Jamais", "Rarement", "De temps en temps", "Assez souvent", "Très souvent"))
data_reduit$difficulté_controle_JV <- data$difficulté_controle_JV

print(table(data$difficulté_controle_JV))

# Création de la variable difficulté_controle_JV avec des libellés
data$pratique_JV_prioritaire <- factor(data$QB06B, 
                                      levels = 1:5, 
                                      labels = c("Jamais", "Rarement", "De temps en temps", "Assez souvent", "Très souvent"))
data_reduit$pratique_JV_prioritaire <- data$pratique_JV_prioritaire

print(table(data$pratique_JV_prioritaire))

# Création de la variable joue_malgré_conséquences_négatives avec des libellés
data$joue_malgré_conséquences_négatives <- factor(data$QB06C, 
                                       levels = 1:5, 
                                       labels = c("Jamais", "Rarement", "De temps en temps", "Assez souvent", "Très souvent"))
data_reduit$joue_malgré_conséquences_négatives <- data$joue_malgré_conséquences_négatives

print(table(data$joue_malgré_conséquences_négatives))

# Création de la variable problèmes_pratique_JV avec des libellés
data$problèmes_pratique_JV <- factor(data$QB06D, 
                                                  levels = 1:5, 
                                                  labels = c("Jamais", "Rarement", "De temps en temps", "Assez souvent", "Très souvent"))
data_reduit$problèmes_pratique_JV <- data$problèmes_pratique_JV

print(table(data$problèmes_pratique_JV))

# Convertir les colonnes de type factor en chaînes de caractères
data_reduit[] <- lapply(data_reduit, function(x) if(is.factor(x)) as.character(x) else x)

# Remplacer les NA par la chaîne de caractères "NA"
data_reduit[is.na(data_reduit)] <- "NA"
