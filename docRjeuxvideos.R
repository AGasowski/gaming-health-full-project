rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")

library(ggplot2)
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

# Visualisation distribution IMC
data$IMC <- as.numeric(as.character(data$IMC))
ggplot(data, aes(x = IMC)) +
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black') +
  labs(title = 'Distribution de l\'IMC')


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
# Visualisation de cette variable

# Calculer les proportions de chaque score_ADRS
data_ADRS <- data %>%
  group_by(score_ADRS) %>%
  summarise(proportion = n() / nrow(data))

# Créer le graphique
ggplot(data_ADRS, aes(x = factor(score_ADRS), y = proportion, fill = cut(score_ADRS, breaks = c(-1, 3, 6, 11), labels = c("Peu de risque d’EDC", "Risque d’EDC modéré", "Risque d’EDC important")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(proportion * 100), "%")), vjust = -0.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("lightgreen", "yellow", "orange")) +
  labs(title = "Proportions de chaque score ADRS",
       x = "Score ADRS",
       y = "Proportion",
       fill = "Catégorie de risque") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Création data_réduit qui contient uniquement les variables crées et modifiées
data_reduit <- data[, c("score_ADRS", "score_ADRS_categorie", "Etat_de_santé", "IMC", "IMC_categorie")]


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


#
# Code pour 4 différents graphiques qui semblent pertinents
#
 

# Définir l'ordre des catégories
ordre_score_ADRS <- c("Peu de risque d’EDC", "Risque d’EDC modéré", "Risque d’EDC important")
ordre_frequence <- c("Très souvent", "Assez souvent", "De temps en temps", "Rarement", "Jamais")
ordre_etat_sante <-  c("Pas du tout satisfaisant", "Peu satisfaisant", "Plutôt satisfaisant", "Très satisfaisant")
ordre_IMC <- c("Maigreur", "Normal", "Surpoids", "Obésité")
ordre_jours_joue <- c("Aucun", "1-2 jours", "3-5 jours", "6-9 jours", "10-19 jours", "20-29 jours", "Tous les jours ou presque")


# Filtrer les données pour enlever les lignes contenant "NA"
data_filtre <- data_reduit %>%
  filter(score_ADRS_categorie != "NA" &
           difficulté_controle_JV != "NA" &
           pratique_JV_prioritaire != "NA" &
           Etat_de_santé != "NA" &
           IMC_categorie != "NA" &
           jours_joué_par_mois != "NA")


# Convertir en facteurs avec les niveaux définis
data_filtre$score_ADRS_categorie <- factor(data_filtre$score_ADRS_categorie, levels = ordre_score_ADRS)
data_filtre$difficulté_controle_JV <- factor(data_filtre$difficulté_controle_JV, levels = ordre_frequence)
data_filtre$pratique_JV_prioritaire <- factor(data_filtre$pratique_JV_prioritaire, levels = ordre_frequence)
data_filtre$Etat_de_santé <- factor(data_filtre$Etat_de_santé, levels = ordre_etat_sante)
data_filtre$IMC_categorie <- factor(data_filtre$IMC_categorie, levels = ordre_IMC)
data_filtre$jours_joué_par_mois <- factor(data_filtre$jours_joué_par_mois, levels = ordre_jours_joue)


# Calculer les pourcentages
data_pourcentage_1 <- data_filtre %>%
  count(score_ADRS_categorie, difficulté_controle_JV) %>%
  group_by(score_ADRS_categorie) %>%
  mutate(pct = n / sum(n) * 100)

data_pourcentage_2 <- data_filtre %>%
  count(score_ADRS_categorie, pratique_JV_prioritaire) %>%
  group_by(score_ADRS_categorie) %>%
  mutate(pct = n / sum(n) * 100)

data_pourcentage_3 <- data_filtre %>%
  count(Etat_de_santé, pratique_JV_prioritaire) %>%
  group_by(Etat_de_santé) %>%
  mutate(pct = n / sum(n) * 100)

data_pourcentage_4 <- data_filtre %>%
  count(IMC_categorie, jours_joué_par_mois) %>%
  group_by(IMC_categorie) %>%
  mutate(pct = n / sum(n) * 100)


# Créer les graphiques
ggplot(data_pourcentage_1, aes(x = score_ADRS_categorie, y = pct, fill = difficulté_controle_JV)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des individus selon la difficultés à contrôler mon activité de jeu vidéo par score ADRS",
       x = "Score ADRS Catégorie",
       y = "Pourcentage",
       fill = "Difficultés à contrôler mon activité de jeu vidéo") +
  theme_minimal()

ggplot(data_pourcentage_2, aes(x = score_ADRS_categorie, y = pct, fill = pratique_JV_prioritaire)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des individus selon la pratique du jeu vidéo prioritaire par score ADRS",
       x = "Score ADRS Catégorie",
       y = "Pourcentage",
       fill = "Pratique du jeu vidéo prioritaire") +
  theme_minimal()

ggplot(data_pourcentage_3, aes(x = Etat_de_santé, y = pct, fill = pratique_JV_prioritaire)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des individus selon la pratique du jeu vidéo prioritaire par état de santé",
       x = "État de Santé",
       y = "Pourcentage",
       fill = "Pratique du jeu vidéo prioritaire") +
  theme_minimal()

ggplot(data_pourcentage_4, aes(x = IMC_categorie, y = pct, fill = jours_joué_par_mois)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des individus selon le nombre de jours passés à jouer aux JV par mois, par catégorie d'IMC",
       x = "IMC Catégorie",
       y = "Pourcentage",
       fill = "Nombre de jours passés à jouer aux JV par mois") +
  theme_minimal()

#
#
#
