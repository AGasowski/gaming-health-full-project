rm(list=ls())

install.packages("dplyr")
install.packages("vcd")
install.packages("ggplot2")


library(vcd)
library(dplyr)
library(ggplot2)
library(ggthemes)

datainitial <- read.csv2("Enquête 2022-20250104/bdd_2022.csv", header = TRUE)

# data contient tous les individus qui ont répondu
# au questionnaire sur les jeux
data <- datainitial %>% 
  filter(!is.na(QB01A) & !is.na(QB01B)  & !is.na(QB01C)  & !is.na(QB01D))



# Création d'un vecteur de correspondance
labels_frequencejeu <- c("Jamais", "Une fois par mois ou moins", 
                         "2-3 fois par mois", "Une fois par semaine", 
                         "Plusieurs fois par semaine",
                         "Tous les jours ou presque")

labels_non_oui <- c("Non", "Oui")


# Remplacement des valeurs numériques par les catégories
data$qb07abcdef1 <- factor(labels_frequencejeu[data$qb07abcdef1], levels = labels_frequencejeu)
data$QB07C1 <- factor(labels_frequencejeu[data$QB07C1], levels = labels_frequencejeu)


data <- data %>% 
  mutate(Q21A = case_when(
    Q21A == 1 ~ "Non",
    Q21A == 2 ~ "Oui"
  ))









# GRAPHIQUE 1 : Part qui a vu un médecin en fonction JA
data_clean_medecinJA <- data %>% filter(!is.na(Q21A) & !is.na(qb07abcdef1))


table_cont <- table(data_clean_medecinJA$Q21A, data_clean_medecinJA$qb07abcdef1)
table_cont
chisq.test(table_cont)

assocstats(table_cont)

# Calculer les pourcentages par groupe de joueurs
data_pourc_med <- data_clean_medecinJA %>%
  count(qb07abcdef1, Q21A) %>%
  group_by(qb07abcdef1) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(data_pourc_med, aes(x = qb07abcdef1, y = pct, fill = Q21A)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_few() +
  labs(x = "Fréquence de jeux d'argent",
       y = "Pourcentage",
       fill = "A été chez le médecin") +
  theme_minimal()

