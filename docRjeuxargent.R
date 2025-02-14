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

# RECHERCHE DE CORRELATIONS ENTRE VARIABLES: V DE CRAMER

t <- table(data$Q17, data$QB08D)
cramer_v <- assocstats(t)
cramer_v
