rm(list=ls())

install.packages("dplyr")
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
