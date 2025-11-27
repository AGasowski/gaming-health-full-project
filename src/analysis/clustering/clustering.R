rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")


library(vcd)
library(dplyr)
library(ggplot2)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))






#EXEMPLE TD OPTIMISATION

#### Exemple d’AFCM: Preception des OGM
rm(list=ls())
require(FactoMineR)

ogm <- read.table(
  "~/Documents/enseignements/ENSAI/1A/SEM/exemples/data/ogm.csv",
  header = TRUE,
  sep=";"
)
dim(ogm)

levels(ogm$Position.Al.H)[4] <- levels(ogm$Position.Al.H)[1]
levels(ogm$Position.Culture) <-
  c("Favorable", "Pas Favorable du Tout", "Plutot Defavorable", "Favorable")
dataActif <- ogm[,1:16]
dataToCluster <- tab.disjonctif(ogm)
Kmax <- 10
results <- list()
criterion <- rep(NA, Kmax)
for (k in 1:Kmax){
  results[[k]] <- kmeans(dataToCluster, k)
  criterion[k] <- results[[k]]$tot.withinss
}
plot(1:Kmax, criterion, xlab="Number of clusters", ylab="Criterion")


Kselec <- 3
by(ogm[,1:16], results[[Kselec]]$cluster, summary)










# TEST CLUSTERING
# Charger les bibliothèques nécessaires
rm(list=ls())
library(tidyverse)    # Pour la manipulation des données
library(cluster)      # Pour le clustering
library(factoextra)   # Pour la visualisation des clusters

# Charger la base de données (remplace 'data.csv' par ton fichier)
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Suppression des valeurs manquantes
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))

# Sélection des variables pertinentes (remplace par les colonnes intéressantes)
data_selected <- data[, c("Q20", "Q21A", "Q17", "qb07abcdef1", "QB02")]
data_selected <- na.omit(data_selected)


data_selected <- data_selected[, apply(data_selected, 2, function(x) length(unique(x))) > 1]




# Normalisation des données (recommandé pour éviter que certaines variables dominent)
data_scaled <- scale(data_selected)


data_scaled[is.na(data_scaled)] <- 0
data_scaled[is.nan(data_scaled)] <- 0
data_scaled[is.infinite(data_scaled)] <- 0

# Dendogramme
# Calculer la matrice de distance
dist_matrix <- dist(data_scaled, method = "euclidean")




# Appliquer le clustering hiérarchique (méthode Ward pour optimiser l'inertie intra-classe)
hc <- hclust(dist_matrix, method = "ward.D2")

# Visualiser le dendrogramme
plot(hc, labels = FALSE, main = "Dendrogramme du clustering hiérarchique")


#Déterminer le nombre de clusters optimal
# Méthode du coefficient d'agglomération (Elbow Method)
fviz_nbclust(data_scaled, hcut, method = "wss") + 
  ggtitle("Méthode du coude")

# Indice de silhouette pour chaque nombre de clusters
fviz_nbclust(data_scaled, hcut, method = "silhouette") + 
  ggtitle("Indice de silhouette")


#Découper l'arbre en k-cluster et afficher les résultats
k <- 6  # Choisis le bon nombre de clusters d’après les graphiques précédents
clusters <- cutree(hc, k)

# Ajouter les clusters aux données originales
data$Cluster <- as.factor(clusters)

# Afficher les individus de chaque cluster
head(data)


#Visualiser les clusters
# Colorer le dendrogramme selon les clusters
fviz_dend(hc, k = k, rect = TRUE, rect_fill = TRUE, show_labels = FALSE)



cluster_summary <- aggregate(data_selected, by = list(Cluster = data$Cluster), FUN = mean)
print(cluster_summary)






# TEST 2
# TEST CLUSTERING
rm(list=ls())  # Nettoyage de l'environnement

# Chargement des bibliothèques
library(tidyverse)    # Manipulation de données
library(cluster)      # Clustering
library(factoextra)   # Visualisation des clusters
library(FactoMineR)   # Analyse en Composantes Principales (PCA)

# Charger la base de données
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Suppression des individus avec des NA dans les variables essentielles
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B) & !is.na(QB01C) & !is.na(QB01D))

# Sélection des variables pertinentes
data_selected <- data[, c("Q20", "Q21A", "Q17", "qb07abcdef1", "QB02")]

# Suppression des colonnes ayant plus de 50% de valeurs manquantes (optionnel)
data_selected <- data_selected[, colSums(is.na(data_selected)) / nrow(data_selected) < 0.5]

# Remplacement des NA restants par la médiane de chaque colonne
data_selected <- data_selected %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Suppression des variables constantes (qui n'ont qu'une seule valeur unique)
data_selected <- data_selected[, apply(data_selected, 2, function(x) length(unique(x)) > 1)]

# Normalisation des données
data_scaled <- scale(data_selected)

# Suppression des valeurs aberrantes (NaN, Inf)
data_scaled[is.na(data_scaled)] <- 0
data_scaled[is.nan(data_scaled)] <- 0
data_scaled[is.infinite(data_scaled)] <- 0

# Dendrogramme
if (nrow(data_scaled) > 1) {
  # Calculer la matrice de distance
  dist_matrix <- dist(data_scaled, method = "euclidean")
  
  # Appliquer le clustering hiérarchique (méthode Ward)
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Visualiser le dendrogramme
  plot(hc, labels = FALSE, main = "Dendrogramme du clustering hiérarchique")
  
  # Déterminer le nombre optimal de clusters
  fviz_nbclust(data_scaled, FUN = hcut, method = "wss") + ggtitle("Méthode du coude")
  fviz_nbclust(data_scaled, FUN = hcut, method = "silhouette") + ggtitle("Indice de silhouette")
  
  # Choisir le bon nombre de clusters (à ajuster selon les résultats du graphe)
  k <- 6  
  
  # Découper l'arbre en k clusters
  clusters <- cutree(hc, k)
  
  # Ajouter les clusters aux données originales
  data$Cluster <- as.factor(clusters)
  
  # Afficher les individus de chaque cluster
  print(head(data))
  
  # Visualisation du clustering sur le dendrogramme
  fviz_dend(hc, k = k, rect = TRUE, rect_fill = TRUE, show_labels = FALSE)
  
  # Résumé des valeurs moyennes par cluster
  cluster_summary <- aggregate(data_selected, by = list(Cluster = data$Cluster), FUN = mean)
  print(cluster_summary)
  
  # Visualisation des clusters avec ACP
  res.pca <- PCA(data_selected, scale.unit = TRUE, graph = FALSE)
  fviz_pca_ind(res.pca, geom.ind = "point", col.ind = data$Cluster, 
               palette = "jco", addEllipses = TRUE, legend.title = "Clusters")
} else {
  print("Erreur : Pas assez de données après nettoyage.")
}

table(data$Cluster)










# TEST 3 (JA, JV, ETATSANTE)
# TEST CLUSTERING
rm(list=ls())  # Nettoyage de l'environnement

# Chargement des bibliothèques
library(tidyverse)    # Manipulation de données
library(cluster)      # Clustering
library(factoextra)   # Visualisation des clusters
library(FactoMineR)   # Analyse en Composantes Principales (PCA)

# Charger la base de données
datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# Suppression des individus avec des NA dans les variables essentielles
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B) & !is.na(QB01C) & !is.na(QB01D))

# Sélection des variables pertinentes
data_selected <- data[, c("Q17", "qb07abcdef1", "QB02")]

# Suppression des colonnes ayant plus de 50% de valeurs manquantes (optionnel)
data_selected <- data_selected[, colSums(is.na(data_selected)) / nrow(data_selected) < 0.5]

# Remplacement des NA restants par la médiane de chaque colonne
data_selected <- data_selected %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Suppression des variables constantes (qui n'ont qu'une seule valeur unique)
data_selected <- data_selected[, apply(data_selected, 2, function(x) length(unique(x)) > 1)]

# Normalisation des données
data_scaled <- scale(data_selected)

# Suppression des valeurs aberrantes (NaN, Inf)
data_scaled[is.na(data_scaled)] <- 0
data_scaled[is.nan(data_scaled)] <- 0
data_scaled[is.infinite(data_scaled)] <- 0

# Dendrogramme
if (nrow(data_scaled) > 1) {
  # Calculer la matrice de distance
  dist_matrix <- dist(data_scaled, method = "euclidean")
  
  # Appliquer le clustering hiérarchique (méthode Ward)
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Visualiser le dendrogramme
  plot(hc, labels = FALSE, main = "Dendrogramme du clustering hiérarchique")
  
  # Déterminer le nombre optimal de clusters
  fviz_nbclust(data_scaled, FUN = hcut, method = "wss") + ggtitle("Méthode du coude")
  fviz_nbclust(data_scaled, FUN = hcut, method = "silhouette") + ggtitle("Indice de silhouette")
  
  # Choisir le bon nombre de clusters (à ajuster selon les résultats du graphe)
  k <- 6  
  
  # Découper l'arbre en k clusters
  clusters <- cutree(hc, k)
  
  # Ajouter les clusters aux données originales
  data$Cluster <- as.factor(clusters)
  
  # Afficher les individus de chaque cluster
  print(head(data))
  
  # Visualisation du clustering sur le dendrogramme
  fviz_dend(hc, k = k, rect = TRUE, rect_fill = TRUE, show_labels = FALSE)
  
  # Résumé des valeurs moyennes par cluster
  cluster_summary <- aggregate(data_selected, by = list(Cluster = data$Cluster), FUN = mean)
  print(cluster_summary)
  
  # Visualisation des clusters avec ACP
  res.pca <- PCA(data_selected, scale.unit = TRUE, graph = FALSE)
  fviz_pca_ind(res.pca, geom.ind = "point", col.ind = data$Cluster, 
               palette = "jco", addEllipses = TRUE, legend.title = "Clusters")
} else {
  print("Erreur : Pas assez de données après nettoyage.")
}

table(data$Cluster)



