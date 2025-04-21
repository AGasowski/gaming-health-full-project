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
  select(freqJA, freqJV, ADRS_cat, freqMedecin, freqSport)




data_selec <- na.omit(data_selec)

data_selec[] <- lapply(data_selec, as.factor)




acm <- MCA(data_selec, graph = FALSE)
fviz_mca_var(acm, repel = TRUE)
