rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("tidyr")

library(vcd)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
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

summary(data$santephysique)
table(data$santephysique)



# RECHERCHE DE CORRELATIONS ENTRE VARIABLES: V DE CRAMER

t <- table(data$Q21A, data$qb07abcdef1)
cramer_v <- assocstats(t)
cramer_v

summary(data$QB08F)





###               OBSERVATIONS INTERESSANTES

###   ETAT DE SANTE ET JEU
tab <- table(data$qb07abcdef1, data$Q17)
tab
tab_pct <- prop.table(tab, margin = 1) * 100
tab_pct
print(tab_pct)
## On remarque que ceux qui jouent quotidiennement aux
##jeux d'argent (tout confondu) déclarent en plus grande
##proportion avoir un niveau de santé peu satisfaisant


# Votre table de données
tab_pct <- data.frame(
  Ligne = 1:6,
  `Pas du tout satisfaisant` = c(0.9978048, 1.7871018, 0.8797654, 1.6129032, 1.6393443, 2.9411765),
  `Peu satisfaisant` = c(6.8249850, 7.6923077, 10.5571848, 8.8709677, 12.2950820, 26.4705882),
  `Plutôt satisfaisant` = c(43.5442028, 46.2315462, 51.3196481, 45.9677419, 42.6229508, 23.5294118),
  `Très satisfaisant` = c(48.6330074, 44.2890443, 37.2434018, 43.5483871, 43.4426230, 47.0588235)
)
colnames(tab_pct)<- c("Ligne", "Pas du tout satisfaisant", "Peu satisfaisant",
                      "Plutôt satisfaisant", "Très satisfaisant")
# Remplacer les valeurs dans la colonne Ligne par des noms plus parlants
tab_pct <- tab_pct %>%
  mutate(Ligne = recode(Ligne, `1` = "jamais",
                        `2` = "1 fois par\nmois ou moins",
                        `3` = "2 à 3 fois par\nmois",
                        `4` = "1 fois par\nsemaine",
                        `5` = "plusieurs fois par\nsemaine",
                        `6` = "tous les jours"))
# Transformer les données en format long
tab_pct_long <- tab_pct %>%
  pivot_longer(cols = c(`Pas du tout satisfaisant`, `Peu satisfaisant`,
                        `Plutôt satisfaisant`, `Très satisfaisant`),
               names_to = "Estimation de l'état de santé",
               values_to = "pourcentage")


# Créer le graphique à barres empilées
ggplot(tab_pct_long, aes(x = Ligne, y = pourcentage, fill = `Estimation de l'état de santé`)) +
  geom_bar(stat = "identity") +
  labs(title =  "Auto-détermination de l'état de santé par rapport à la fréquence de jeux d'argent et de hasard",
       x = "Fréquence de jeu d'argent et de hasard",
       y = "Pourcentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## j'ai pas réussi à mettre les colonnes du graphique dans l'ordre et 
##j'arrive pas à aller à la ligne pour le titre
  


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


### quel jeu d'argent a le plus de "jamais"
# Liste des variables à comparer
variables <- c("QB07A1", "QB07B1", "QB07C1", "QB07D1", "QB07E1", "QB07F1")

# Compter les occurrences de "Jamais" pour chaque variable en ignorant les NA
occurrences_jamais <- sapply(variables, function(var) sum(data[[var]] == 1, na.rm = TRUE))

# Afficher les résultats
occurrences_jamais

total_reponses <- sapply(variables, function(var) sum(!is.na(data[[var]])))

# Calculer le pourcentage de réponses "jamais" par rapport au total des réponses valides
pourcentage_jamais <- (occurrences_jamais / total_reponses) * 100

# Afficher les résultats
list(
  occurrences_jamais = occurrences_jamais,
  total_reponses = total_reponses,
  pourcentage_jamais = pourcentage_jamais
)
## 80,9% n'ont jamais joué à des jeux de grattages, c'est le taux le plus bas,
## 19,1% ont déjà joué à des jeux de grattage contre 2,7 pour machines à sous



# Compter les occurrences des modalités 1 et 2 pour chaque variable en ignorant les NA
occurrences_1_ou_2 <- sapply(variables, function(var) sum(data[[var]] %in% c(1, 2), na.rm = TRUE))

# Calculer le nombre total de réponses valides (non NA) pour chaque variable
total_reponses <- sapply(variables, function(var) sum(!is.na(data[[var]])))

# Calculer le pourcentage de réponses 1 ou 2 par rapport au total des réponses valides
pourcentage_1_ou_2 <- (occurrences_1_ou_2 / total_reponses) * 100

# Afficher les résultats
list(
  occurrences_1_ou_2 = occurrences_1_ou_2,
  total_reponses = total_reponses,
  pourcentage_1_ou_2 = pourcentage_1_ou_2
)
## 5,7% des sondés jouent plus d'une fois par mois à des paris sportifs, 
##c'est 4,1% pour les tickets à gratter

## jeu d'argent internet

tab <- sum(data$qb07_ps_web == 0, na.rm = T)
tab
### 289 parient en ligne contre 266 pas en ligne 

## mise habituelle paris
tab <- summary (data$QB10B[data$QB10B != 0 & !is.na(data$QB10B)])
tab
## en moyenne les mises habituelles sont de 252 € dans l'année, médianne = 10 et 3ème quartile = 20


## plus grosse mise
tab <- summary(data$QB11[data$QB11 != 0 & !is.na(data$QB11)])
tab
## plus grosse mise médianne 20€ et moyenne 412€ BIZARRE plus gros que total mises sur l'année

# Calculer le premier quartile (Q1) et le troisième quartile (Q3)
Q1 <- quantile(data$QB10B, 0.25, na.rm = TRUE)
Q3 <- quantile(data$QB10B, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Définir les limites pour les valeurs aberrantes
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtrer les données pour enlever les valeurs aberrantes
filtered_data <- data[data$QB10B >= lower_bound & data$QB10B <= upper_bound, ]

# Afficher le nombre de lignes avant et après le filtrage
cat("Nombre de lignes avant filtrage :", nrow(data), "\n")
cat("Nombre de lignes après filtrage :", nrow(filtered_data), "\n")
summary( filtered_data$QB10B)



# Filtrer les valeurs NA et 0 dans QB10B
filtered_data <- filtered_data %>%
  filter(!is.na(QB10B), QB10B != 0)

# Calculer les moyennes de QB10B pour chaque catégorie de joueurs
moyennes <- filtered_data %>%
  summarise(
    `Joueurs occasionnels` = mean(QB10B[qb07_ps_ann == 1], na.rm = TRUE),
    `Joueurs hebdomadaires` = mean(QB10B[qb07_ps_heb == 1], na.rm = TRUE),
    `Joueurs quotidiens` = mean(QB10B[qb07_ps_quo == 1], na.rm = TRUE)
  )

# Afficher le tableau
print


### graphique fréquence de jeu argent
# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Créer un dataframe avec les intitulés des modalités
labels <- c("jamais", "1 fois par mois ou moins", "2-3 fois par mois", "1 fois par semaine", "plusieurs fois par semaine", "tous les jours ou presque")

# Filtrer les valeurs NA dans qb07abcdef1
filtered_data <- data %>%
  filter(!is.na(qb07abcdef1))

# Calculer le nombre total de réponses valides
total_responses <- nrow(filtered_data)

# Calculer les proportions pour chaque catégorie
proportions <- filtered_data %>%
  count(qb07abcdef1) %>%
  mutate(proportion = n / total_responses * 100)

# Créer le graphique en barres avec les pourcentages sur les barres
ggplot(proportions, aes(x = factor(qb07abcdef1, levels = 1:6, labels = labels), y = proportion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(proportion, 1)), vjust = -0.3, color = "black") + # Ajouter les pourcentages
  labs(
    title = "Répartition de la population selon sa consommation de jeux d'argent",
    x = "Fréquence de consommation",
    y = "Pourcentage"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )



# Créer un dataframe avec les intitulés des modalités
modalites <- c("jamais", "1 fois par mois ou moins", "2-3 fois par mois",
               "1 fois par semaine", "plusieurs fois par semaine", "tous les jours ou presque")

# Regrouper les données pour qb07abcdef1 et QB02, en excluant les NA
data_long <- data %>%
  select(qb07abcdef1, QB02, Q03) %>%
  pivot_longer(cols = c(qb07abcdef1, QB02), names_to = "variable", values_to = "modalite") %>%
  filter(!is.na(modalite) & !is.na(Q03)) %>%
  mutate(modalite = factor(modalite, levels = 1:6, labels = modalites))

# Remplacer les étiquettes des variables
data_long <- data_long %>%
  mutate(variable = recode(variable, qb07abcdef1 = "Jeux d'argent", QB02 = "Jeux vidéo"))

# Calculer le nombre total d'hommes et de femmes pour chaque variable
total_counts <- data_long %>%
  group_by(variable, Q03) %>%
  summarise(total = n(), .groups = 'drop')

# Calculer les proportions pour chaque catégorie en fonction du sexe
proportions <- data_long %>%
  group_by(variable, modalite, Q03) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(total_counts, by = c("variable", "Q03")) %>%
  mutate(proportion = count / total * 100)

# Créer le graphique en barres empilées combiné
ggplot(proportions, aes(x = modalite, y = proportion, fill = factor(Q03, labels = c("Homme", "Femme")))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Fréquence des consommation de jeu par sexe",
    x = "Modalité",
    y = "Pourcentage des joueurs par sexe",
    fill = "Sexe"
  ) +
  scale_fill_few() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

#### PCS du père et jeu d'argent
# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)

# Créer un dataframe avec les intitulés des modalités pour q15a_br
csp_labels <- c("Agriculteur exploitant", "Artisan, commerçant", "Chef d’entreprise",
                "Cadre", "Profession intermédiaire", "Employé", "Ouvrier", "Sans profession")

# Créer un dataframe avec les intitulés des modalités pour qb07abcdef1
modalites_qb07 <- c("jamais", "une  mois", "2-3 ",
                    "semaine", "plusieurs  semaine", "tous les jours")

# Filtrer les valeurs NA et créer le tableau croisé
data_filtered <- data %>%
  filter(!is.na(q15a_br) & !is.na(qb07abcdef1)) %>%
  mutate(
    q15a_br = factor(q15a_br, levels = 1:8, labels = csp_labels),
    qb07abcdef1 = factor(qb07abcdef1, levels = 1:6, labels = modalites_qb07)
  )

# Créer le tableau croisé avec les proportions en pourcentage
tableau_croise <- data_filtered %>%
  group_by(q15a_br, qb07abcdef1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(qb07abcdef1, count, fill = 0) %>%
  mutate(across(where(is.numeric), ~ . / rowSums(across(where(is.numeric))) * 100))

# Afficher le tableau croisé
print(tableau_croise)

#### graphique joueur et non joueurs de poker paris sportif et ticket
# Supprimer les valeurs NA
data_filtered <- data_filtered %>%
  filter(!is.na(QB07B1) & !is.na(QB07C1) & !is.na(QB07E1))

# Créer une fonction pour transformer les données
transform_data <- function(data, var_name) {
  data %>%
    select(!!sym(var_name)) %>%
    mutate(category = case_when(
      .data[[var_name]] == 1 ~ "Non joueur",
      .data[[var_name]] %in% 2:6 ~ "Joueur"
    )) %>%
    group_by(category) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
}

# Appliquer la transformation aux trois variables
tickets <- transform_data(data_filtered, "QB07B1")
paris_sportifs <- transform_data(data_filtered, "QB07C1")
poker <- transform_data(data_filtered, "QB07E1")

# Combiner les données
combined_data <- bind_rows(
  tickets %>% mutate(variable = "tickets à gratter"),
  paris_sportifs %>% mutate(variable = "paris sportifs"),
  poker %>% mutate(variable = "poker")
)

ggplot(combined_data, aes(x = variable, y = percentage, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_few() +
  labs(title = "Pratique des individus selon le jeu d'argent",
       x = "Jeux d'argent",
       y = "Part des individus",
       fill = "Catégorie") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#### Pour ces 3 jeux à quelle fréquence
# Créer un dataframe avec les intitulés des modalités
modalites <- c("jamais", "1 fois par mois ou moins", "2-3 fois par mois",
               "une fois par semaine", "plusieurs fois par semaine", "tous les jours ou presque")

# Exemple de données (à remplacer par vos données réelles)
data_filtered <- data %>%
  filter(!is.na(QB07C1) & !is.na(QB07B1) & !is.na(QB07E1)) %>%
  mutate(
    ticket = factor(QB07B1, levels = 1:6, labels = c(modalites)),
    paris_sportif = factor(QB07C1, levels = 1:6, labels = c(modalites)),
    poker = factor(QB07E1, levels = 1:6, labels = c(modalites))
  )

# Transformer les données en format long
data_long <- data_filtered %>%
  pivot_longer(cols = c(ticket, paris_sportif, poker), names_to = "variable", values_to = "modalite")

# Filtrer pour exclure la modalité "jamais" (1)
data_long <- data_long %>%
  filter(modalite != "jamais")

# Calculer le nombre d'occurrences pour chaque modalité
data_counts <- data_long %>%
  count(variable, modalite)

# Calculer le nombre total d'occurrences pour chaque variable (pour les modalités 2 à 6)
total_counts <- data_counts %>%
  group_by(variable) %>%
  summarise(total = sum(n))

# Calculer le pourcentage d'occurrences pour chaque modalité
data_percentages <- data_counts %>%
  left_join(total_counts, by = "variable") %>%
  mutate(percentage = (n / total) * 100)

# Renommer les variables pour un affichage plus lisible
data_percentages <- data_percentages %>%
  mutate(variable = recode(variable, "paris_sportif" = "paris sportifs"))%>%
  mutate(variable = recode(variable, "ticket" = "tickets à gratter"))
         

# Créer le graphique en barres empilées avec les pourcentages
ggplot(data_percentages, aes(x = modalite, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Fréquence de jeu pour les joueurs de poker,\n paris sportifs et tickets à gratter",
    x = "Fréquence de jeu",
    y = "Part des joueurs pour chaque jeu",
    fill = "Variable"
  ) +
  scale_fill_few() +  # Utiliser une palette de couleurs
  geom_text(aes(label = round(percentage, 1)), position = position_dodge(width = 0.9), vjust = -0.3, color = "black") + # Ajouter les pourcentages
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

