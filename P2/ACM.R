#ACM P2
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
  } else if (grepl("^freqMedecin_", mod)) {
    return(case_when(
      mod == "freqMedecin_1" ~ "#42A5F5",
      mod == "freqMedecin_2" ~ "#42A5F5",
      TRUE ~ "black"
    ))
  } else if (grepl("^freqJA_", mod)) {
    return(case_when(
      mod == "freqJA_1" ~ "#66BB6A",
      mod == "freqJA_2" ~ "#C0CA33",
      mod == "freqJA_3" ~ "#FB8C00",
      mod == "freqJA_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^freqJV_", mod)) {
    return(case_when(
      mod == "freqJV_1" ~ "#66BB6A",
      mod == "freqJV_2" ~ "#C0CA33",
      mod == "freqJV_3" ~ "#FB8C00",
      mod == "freqJV_4" ~ "#E53935",
      TRUE ~ "black"
    ))
  } else if (grepl("^freqSport_", mod)) {
    return(case_when(
      mod == "freqSport_1" ~ "#66BB6A",
      mod == "freqSport_2" ~ "#C0CA33",
      mod == "freqSport_3" ~ "#FB8C00",
      mod == "freqSport_4" ~ "#E53935",
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
