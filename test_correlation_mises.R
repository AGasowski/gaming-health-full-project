rm(list=ls())


install.packages("corrplot")

library(writexl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)


setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv("data_simplifiee.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D) & !is.na(mise_habituelle))




data <- data %>%
  mutate(mise_habituelle = case_when(
    mise_habituelle == 1 ~ "0-10€",
    mise_habituelle == 2 ~ "10-25€",
    mise_habituelle == 3 ~ "25-50€",
    mise_habituelle == 4 ~ "+ de 50€",
  ))


data <- data %>% 
  filter(freqJA != "Jamais")
data$freqJA <- factor(data$freqJA,
                           levels = c("Moins d'une fois par semaine",
                                      "Au moins une fois par semaine", 
                                      "Au quotidien"))

data$mise_habituelle <- factor(data$mise_habituelle,
                          levels = c("0-10€", "10-25€", "25-50€", "+ de 50€"))


# Exemple avec deux variables catégorielles : var1 et var2
table_cont <- table(data$freqJA, data$mise_habituelle)
table_cont

chi_result <- chisq.test(table_cont)




#Résidus
residus <- chi_result$stdres
print(residus)
corrplot(residus, is.cor = FALSE, method = "color", addCoef.col = "black")


df <- as.data.frame(as.table(residus))
names(df) <- c("Frequence", "Mise", "Residual")

# Graphique des résidus avec ggplot2
ggplot(df, aes(x = Mise, y = Frequence, fill = Residual)) +
  geom_tile(color = "white") +  # Carreaux avec contour blanc
  # Couleur du texte en fonction de la valeur du résidu
  geom_text(aes(label = round(Residual, 2), 
                color = ifelse(abs(Residual) > 2, "white", "black"))) +  
  scale_fill_gradient2(low = "#E6550D", mid = "white", high = "#3182BD", midpoint = 0) +  # Gradient pour les résidus
  scale_color_identity() +  # Pour utiliser les couleurs définies dynamiquement
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 12, face = "bold"), # Modalités en noir
        axis.text.x = element_text(color = "black", size = 12, face = "bold")) + # Modalités en noir
  labs(x = "Mise habituelle",
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

