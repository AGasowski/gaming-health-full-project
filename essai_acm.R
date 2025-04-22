library(dplyr)


variable_ADRS <- c("Q22A", "Q22B", "Q22C", "Q22D", "Q22E", "Q22F", "Q22G", "Q22H", "Q22I", "Q22J")
data$score_ADRS <- rowSums(data[, variable_ADRS] == "1")
data$score_ADRS_categorie <- cut(data$score_ADRS,
                                 breaks = c(-1, 3, 6, 11),
                                 labels = c("Peu de risque d’EDC", "Risque d’EDC modéré", "Risque d’EDC important"))

###Score santé mentale des questions QB06

data <- data %>%
  rowwise() %>%
  mutate(somme_sante_mentale = sum(QB06A, QB06B, QB06C, QB06D, na.rm = TRUE)) %>%
  ungroup()
data <- data %>%
  mutate(groupe_sante_mentale = cut(somme_sante_mentale, breaks = c(-Inf, 4, 8, 12, 16, 20),
                                    labels = c("1", "2", "3", "4", "5"),
                                    include.lowest = TRUE))
library(FactoMineR)
library(factoextra)
library(dplyr)

# Préparer les données pour l'ACM
data_acm <- data %>%
  select(groupe_sante_mentale, score_ADRS_categorie)

# Convertir les variables en facteurs
data_acm$groupe_sante_mentale <- as.factor(data_acm$groupe_sante_mentale)
data_acm$score_ADRS_categorie <- as.factor(data_acm$score_ADRS_categorie)

# Effectuer l'ACM
acm_result <- MCA(data_acm, graph = FALSE)

# Visualiser les résultats
# Biplot des individus
fviz_mca_ind(acm_result, repel = TRUE, col.ind = "black") +
  ggtitle("Biplot des individus")

# Biplot des variables
fviz_mca_var(acm_result, repel = TRUE, col.var = "blue") +
  ggtitle("Biplot des variables")
