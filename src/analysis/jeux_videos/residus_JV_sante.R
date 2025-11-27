rm(list=ls())


install.packages("corrplot")

library(writexl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)


setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat/")
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB02) & !is.na(Q17))

data <- data %>%
  mutate(QB02 = case_when(
    QB02 == 1 ~ "Jamais",            # 1 pour Jamais, 4 pour Au quotidien
    QB02 == 2 ~ "Moins d'une fois par semaine",
    QB02 == 3 ~ "Au moins une fois par semaine",  
    QB02 == 4 ~ "Au quotidien", 
  ))

data <- data %>%
  mutate(Q17 = case_when(
    Q17 == 1 |            
      Q17 == 2 ~ "Peu ou pas du tout satisfaisant",
    Q17 == 3 | 
      Q17 == 4 ~ "Très ou plutôt satisfaisant", 
  ))




data$QB02 <- factor(data$QB02,
                      levels = c("Jamais",
                                 "Moins d'une fois par semaine",
                                 "Au moins une fois par semaine", 
                                 "Au quotidien"))

data$Q17 <- factor(data$Q17,
                      levels = c("Peu ou pas du tout satisfaisant",
                                          "Très ou plutôt satisfaisant"))


# Exemple avec deux variables catégorielles : var1 et var2
table_cont <- table(data$QB02, data$Q17)
table_cont

chi_result <- chisq.test(table_cont)




#Résidus
residus <- chi_result$stdres
print(residus)
corrplot(residus, is.cor = FALSE, method = "color", addCoef.col = "black")


df <- as.data.frame(as.table(residus))
df
names(df) <- c("Frequence", "Etat", "Residual")

# Graphique des résidus avec ggplot2
ggplot(df, aes(x = Etat, y = Frequence, fill = Residual)) +
  geom_tile(color = "white") +  # Carreaux avec contour blanc
  # Couleur du texte en fonction de la valeur du résidu
  geom_text(aes(label = round(Residual, 2), 
                color = ifelse(abs(Residual) > 2, "white", "black"))) +  
  scale_fill_gradient2(low = "#E6550D", mid = "white", high = "#3182BD", midpoint = 0) +  # Gradient pour les résidus
  scale_color_identity() +  # Pour utiliser les couleurs définies dynamiquement
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 12, face = "bold"), # Modalités en noir
        axis.text.x = element_text(color = "black", size = 12, face = "bold")) + # Modalités en noir
  labs(x = "Etat de santé",
       y = "Fréquence des jeux d'argent")




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

