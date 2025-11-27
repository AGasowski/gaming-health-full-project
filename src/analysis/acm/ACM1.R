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



# Extraire les modalités
var <- get_mca_var(acm)
modalites <- rownames(var$coord)


# Définir les couleurs
# Recréer proprement le vecteur couleurs aligné avec les modalités
couleurs <- sapply(modalites, function(mod) {
  if (grepl("^ADRS_cat_", mod)) {
    return(case_when(
      mod == "ADRS_cat_0" ~ "#66BB6A",
      mod == "ADRS_cat_1" ~ "#FB8C00",
      mod == "ADRS_cat_2" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^jeuPbSante_", mod)) {
    return(case_when(
      mod == "jeuPbSante_1" ~ "#66BB6A",
      mod == "jeuPbSante_2" ~ "#C0CA33",
      mod == "jeuPbSante_3" ~ "#FB8C00",
      mod == "jeuPbSante_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^critique_", mod)) {
    return(case_when(
      mod == "critique_1" ~ "#66BB6A",
      mod == "critique_2" ~ "#C0CA33",
      mod == "critique_3" ~ "#FB8C00",
      mod == "critique_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^pbArgent_", mod)) {
    return(case_when(
      mod == "pbArgent_1" ~ "#66BB6A",
      mod == "pbArgent_2" ~ "#C0CA33",
      mod == "pbArgent_3" ~ "#FB8C00",
      mod == "pbArgent_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^culpabilite_", mod)) {
    return(case_when(
      mod == "culpabilite_1" ~ "#66BB6A",
      mod == "culpabilite_2" ~ "#C0CA33",
      mod == "culpabilite_3" ~ "#FB8C00",
      mod == "culpabilite_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else {
    return("black")
  }
})


# Extraire les coordonnées
var <- get_mca_var(acm)

# Juste pour vérif : que les couleurs sont dans le bon ordre
stopifnot(length(couleurs) == length(modalites))


# Créer un data.frame temporaire pour forcer le mapping
groups <- as.factor(modalites)  # chaque modalité devient son propre "groupe"

# Replot en forçant chaque modalité comme groupe distinct
p <- fviz_mca_var(acm,
                  repel = TRUE,
                  col.var = groups) +  # on triche ici : un groupe = une modalité
  scale_color_manual(values = couleurs) +
  theme_minimal()

print(p)


p2 <- fviz_mca_var(acm,
                   axes = c(1, 3),
                   repel = TRUE,
                   col.var = groups) +
  scale_color_manual(values = couleurs) +   # ← cette ligne est essentielle
  theme_minimal()

print(p2)
  
  

