rm(list=ls())

install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)
library(dplyr)



setwd("C:/Users/Alexandre/Desktop/ENSAI/Projet_Stat/projetstat")
datainitial <- read.csv("data_simplifiee.csv", header = TRUE)

# Filtrer les individus ayant répondu aux questions
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))


data_selec <- data %>%
  select(freqJA, ADRS_cat)




data_selec <- na.omit(data_selec)
data_selec$ID <- paste("ID", 1:nrow(data_selec), sep = "_")  # Crée une colonne "ID"

acm <- MCA(data_selec, graph = FALSE)  # graph=FALSE évite les graphes automatiques

print(acm)

fviz_mca_var(acm, repel = TRUE)




