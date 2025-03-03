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
    q18imc < 18.5 ~ "insuffisance pondérale",
    q18imc > 30 ~ "obésité",
    q18imc > 25 ~ "surpoids",
    TRUE ~ "corpulence normale"
  ))

# RECHERCHE DE CORRELATIONS ENTRE VARIABLES: V DE CRAMER

t <- table(data$santephysique, data$QB08I)
cramer_v <- assocstats(t)
cramer_v

summary(data$QB08F)





###               OBSERVATIONS INTERESSANTES

###   ETAT DE SANTE ET JEU
tab <- table(data$qb07abcdef1, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
## On remarque que ceux qui jouent quotidiennement aux
##jeux d'argent (tout confondu) déclarent en plus grande
##proportion avoir un niveau de santé peu satisfaisant


tab <- table(data$QB08E, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui ont senti avoir un problème avec le jeu se disent 
##en moins bon état de santé que les autres


tab <- table(data$QB08F, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##On remarque que ceux qui disent que le jeu a causé des problèmes
##de santé disent avoir un état de santé beaucoup moins satisfaisant
##(logique, inutile de travailler dessus)


tab <- table(data$QB08G, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui disent que leurx habitudes de jeu ont été critiquées sont
##en moins bon état de santé que les autres


tab <- table(data$QB08I, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui se sont senti coupable de leurs habitudes de jeu se disent
##en moins bonne santé



###   SPORT ET JEU

tab <- table(data$Q20, data$QB07C1)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Les plus sportifs jouent + aux paris sportifs


tab <- table(data$Q20, data$qb07abcdef1)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Les plus sportifs jouent + aux jeux d'argent (tout confondu)


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



###   ALLER CHEZ UN MEDECIN ET JEU

tab <- table(data$qb07abcdef1, data$Q21A)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui jouent chaque jour sont moins allé chez le médecin que
##les autres


###   RISQUE DE DEPRESSION ET JEU

tab <- table(data$qb07abcdef1, data$ADRS_cat)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Le risque de dépression est globalement le même pour ceux qui jouent
##et ceux qui ne jouent pas


###     PROBLEME DE SANTE ET JEU

tab <- table(data$qb07abcdef1, data$Q21C)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Autant de problèmes de santé chez les joueurs et non joueurs


###     CONSULTER PSY ET JEU

tab <- table(data$QB07C1, data$Q21E)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui font des paris sportifs vont moins chez le psychologue


tab <- table(data$qb07abcdef1, data$Q21E)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui jouent plusieurs fois par semaines vont moins chez le 
##psychologue



###     IMC ET JEU
##On remarque que l'imc et le jeu ne sont pas corrélés



###   TRISTESSE ET JEU 

tab <- table(data$QB07C1, data$Q22C)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#Ceux qui ne jouent jamais aux paris sportifs ont une tristesse débordante en 
#plus grande proportion que le reste de la population

tab <- table(data$qb07abcdef1, data$Q22C)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui jouent aux jeux d'argent sont moins tristes que les autres



###     PLUS D'INTERET ET JEU

tab <- table(data$QB08F, data$Q22D)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux chez qui le jeu a causé des problèmes de santé ont moins d'intérêt


tab <- table(data$QB08G, data$Q22D)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
#Les individus qui reçoivent beaucoup de critiques sur leurs habitudes de jeu
#ont en plus grande proportion moins d'intérêt (plus rien ne les intéresse, plus
#rien ne les amuse)



###     DECOURAGEMENT ET JEU

tab <- table(data$qb07abcdef1, data$Q22H)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux qui jouent aux jeux d'argent sont moins découragés que les autres


tab <- table(data$QB07C1, data$Q22H)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Le résultat ci-dessus est encore plus flagrant chez ceux qui jouent aux 
##paris sportifs



###       QUALITE DE SOMMEIL ET JEU

##Pas de corrélation visible



###       PAS DE REUSSITE AU BOULOT/ECOLE ET JEU

tab <- table(data$QB08G, data$Q22J)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Ceux dont on critique les habitudes de jeux y arrivent moins pour travailler



###     TENTATIVE DE SUICIDE ET JEU

##Trop peu de personnes ont fait une tentative pour étudier cette variable



###     PENSEES SUICIDAIRES DANS L'ANNEE ET JEU

tab <- table(data$qb07abcdef1, data$Q24)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
##Les très gros joueurs ont moins de pensées suicidaires que les autres
