rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
library(dplyr)
library(vcd)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))

# Tests de correlations entre toutes les variables intéressantes
# Ici methode de spearman car les deux sont quantitatives
correlation1 <- cor(data$ADRS_score, data$QB08A, method="spearman", use="complete.obs")
print(correlation1)

# V de Cramer quand les deux sont qualitatives
cramer_v <- assocstats(table(data$ADRS_cat, data$QB08I))$cramer

