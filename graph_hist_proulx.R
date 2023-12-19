
# Load package ------------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------

data <- readRDS("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/data/merged_v1.rds")


# Year --------------------------------------------------------------------

gg <- ggplot(data, aes(x = as.factor(year))) +  # Convertir 'year' en facteur
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable années des sondages") +
  xlab("Années") +
  ylab("Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation du texte sur l'axe x
print(gg)
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_year.png", gg)


# Age --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_age)) +
  geom_bar(fill = "skyblue", color = "black", width = 1) +
  ggtitle("Histogramme de la variable âge") +
  xlab("Age") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_age.png", gg)

# gender --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_gender)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable genre") +
  xlab("genre") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_gender.png", gg)

# lang --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_lang.1)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable langue") +
  xlab("genre") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_lang.png", gg)

# educ --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_educ)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable educ") +
  xlab("educ") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_educ.png", gg)

# ses_family income --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_family_income_centile_cat)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable family income") +
  xlab("family income") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_family_income.png", gg)

# ses_origine_from_canada --------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_origin_from_canada.1)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5) +
  ggtitle("Histogramme de la variable origine from canada") +
  xlab("origine from can") +
  ylab("Fréquence")
print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_ses_origine_from_canada.png", gg)

# ses_year_canada--------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_year_canada)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.7, alpha = 0.7) +  # Ajustements visuels
  ggtitle("Distribution des années au Canada") +
  xlab("Année au Canada") +
  ylab("Fréquence") +
  theme_minimal() +  # Choix du thème
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotation du texte sur l'axe x
        plot.title = element_text(hjust = 0.5),  # Centrer le titre du graphique
        legend.position = "none")  # Supprimer la légende si elle n'est pas nécessaire

print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_year_canada.png", gg)


# ses_religiosity--------------------------------------------------------------------

gg <- ggplot(data, aes(x = ses_religiosity)) +
  geom_bar(fill = "skyblue", color = "black", width = 0.1, alpha = 0.7) +
  ggtitle("Distribution de religiosity") +
  xlab("Religiosity") +
  ylab("Fréquence") 


print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_religiousity.png", gg)

# int_pol --------------------------------------------------------------------

gg <- ggplot(data, aes(x = factor(int_pol))) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5, alpha = 1) +
  ggtitle("Distribution de l'intérêt politique") +
  xlab("Intérêt politique (0 = Non, 1 = Oui)") +
  ylab("Fréquence") +
  theme_minimal()


print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_int_pol.png", gg)



# iss_souv --------------------------------------------------------------------

gg <- ggplot(data, aes(x = factor(iss_souv))) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5, alpha = 1) +
  ggtitle("Distribution de iss_souv") +
  xlab("iss_souv") +
  ylab("Fréquence") +
  theme_minimal()


print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_iss_souv.png", gg)


# iss_idcan --------------------------------------------------------------------
gg <- ggplot(data, aes(x = factor(iss_idcan))) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5, alpha = 1) +
  ggtitle("Distribution de id can") +
  xlab("id can") +
  ylab("Fréquence") +
  theme_minimal()


print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_idcan.png", gg)


# party_id --------------------------------------------------------------------
gg <- ggplot(na.omit(data), aes(x = factor(party_id_prov))) +
  geom_bar(fill = "skyblue", color = "black", width = 0.5, alpha = 1) +
  ggtitle("Distribution party_id_prov") +
  xlab("party_id_prov") +
  ylab("Fréquence") +
  theme_minimal()


print(gg)
# Enregistrer le graphique avec un nom de fichier
ggsave("C:/Users/USAGER-01/Dropbox/Travail/CLESSN/Publications/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/graph/hist_party_id.png", gg)







