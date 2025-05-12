#ACM P3
rm(list=ls())

install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)
library(dplyr)



setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv("data_simplifiee_facto.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


data_selec <- data %>%
  select(ADRS_cat, jeuPbSante, critique, pbArgent, culpabilite)




data_selec <- na.omit(data_selec)

data_selec[] <- lapply(data_selec, as.factor)




acm <- MCA(data_selec, graph = FALSE)

#Graphique des valeurs propres
fviz_screeplot(acm, addlabels = TRUE, ylim = c(0, 25))


#Afficher les valeurs propres
eig <- get_eigenvalue(acm)
print(eig)

#Afficher l'ACM
fviz_mca_var(acm, repel = TRUE)

fviz_mca_var(acm, axes = c(1, 3), repel = TRUE, title = "Dim 1 vs Dim 3")
