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



###SANTE


data <- data %>% 
  mutate(santephysique = case_when(
    q18imc < 17 | q18imc > 30 ~ "très mauvaise",
    q18imc < 18.5 | q18imc > 25 ~ "plutôt mauvaise",
    TRUE ~ "bonne"
  ))

# RECHERCHE DE CORRELATIONS ENTRE VARIABLES: V DE CRAMER

t <- table(data$Q22I, data$qb07abcdef1)
cramer_v <- assocstats(t)
cramer_v

summary(data$qb07_heb)

tab <- table(data$QB07C1, data$Q21E)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##ceux qui font des paris sportifs vont moins chez le psychologue



tab <- table(data$Q20, data$qb07_ann)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les plus sportifs semblent + jouer aux jeux d'argent dans l'année



tab <- table(data$Q20, data$qb07_heb)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les plus sportifs semblent + jouer aux jeux d'argent dans la semaine


tab <- table(data$Q20, data$qb07_quo)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les plus sportifs semblent + jouer aux jeux d'argent au quotidien



tab <- table(data$Q20, data$QB07C1)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les plus sportifs semblent + jouer aux paris sportifs



tab <- table(data$Q20, data$qb07_quo)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les sportifs jouent + aux jeux d'argent au quotidien que les non sportifs



tab <- table(data$QB07C1, data$ADRS_cat)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#résultat difficile à interprêter
#un joueur sera en meilleure santé mentale sauf les plus gros joueurs



tab <- table(data$QB07C1, data$Q22C)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#ceux qui ne jouent jamais aux paris sportifs ont une tristesse débrodante, en 
#plus grande proportion que le reste de la population



tab <- table(data$QB07E1, data$Q22C)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#??



tab <- table(data$QB08G, data$Q22D)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#les individus qui reçoivent beaucoup de critiques sur leurs habitudes de jeu
#ont en plus grande proportion moins d'intérêt (plus rien ne les intéresse, plus
#rien ne les amuse)




tab <- table(data$QB07A1, data$ADRS_cat)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#plus grosse proportion des ADRS de catégorie 2 parmi les individus qui jouent 
#aux jeux de tirage 1 fois et plusieurs fois par semaine


tab <- table(data$QB07C1, data$Q22H)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct




summary(data$qb07_casino_web)

summary(data$QB08G)

