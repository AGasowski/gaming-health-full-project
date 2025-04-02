rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")


library(vcd)
library(dplyr)
library(ggplot2)
library(forcats)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))



###SANTE

data <- data %>% 
  mutate(IMC = case_when(
    q18imc < 18.5 ~ "Insuffisance pondérale",
    q18imc > 30 ~ "Obésité",
    q18imc > 25 ~ "Surpoids",
    TRUE ~ "Corpulence normale"
  ))



# Création d'un vecteur de correspondance
labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")



# Remplacement des valeurs numériques par les catégories
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)


ordre_IMC <- c("Insuffisance pondérale", "Corpulence normale", 
               "Surpoids", "Obésité")
data$IMC <- factor(data$IMC, levels = ordre_IMC)




# GRAPHIQUE 1

#qui prend en compte seulement les joueurs, pour avoir un total à 100%
data_clean <- data %>% filter(!is.na(qb07abcdef1) & !is.na(q18imc))

data_clean <- data_clean %>%
  mutate(qb07simp = ifelse(qb07abcdef1 == "Jamais", "Non joueur", "Joueur"))

data_100 <- data_clean %>%
  count(IMC, qb07simp) %>%
  group_by(IMC) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_100, aes(x = IMC, y = pct, fill = fct_rev(qb07simp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion de joueurs de jeux d'argent par catégorie d'IMC",
       x = "Catégorie d'IMC",
       y = "Pourcentage",
       fill = "Jeux d'argent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre

