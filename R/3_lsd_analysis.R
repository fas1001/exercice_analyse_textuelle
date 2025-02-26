###############################################################################
# SCRIPT D'ANALYSE DE SENTIMENT SUR DES TEXTES CONCERNANT LES RÉFUGIÉS
# FAS1001 - Introduction aux mégadonnées en sciences sociales
###############################################################################

# SECTION 1: CHARGEMENT DES BIBLIOTHÈQUES ET FONCTIONS
###############################################################################
# Chargement des bibliothèques nécessaires
library(dplyr)         # Pour la manipulation de données
library(quanteda)      # Pour l'analyse de texte
library(ggplot2)       # Pour la visualisation
library(tidytext)      # Pour l'analyse de texte par phrase
library(stringr)       # Pour la manipulation de chaînes de caractères

# Chargement des fonctions personnalisées pour le prétraitement des textes
source("R/9_lsd_prep_functions.R")

# SECTION 2: IMPORTATION ET PRÉPARATION DES DONNÉES
###############################################################################
# Lecture du fichier de données brutes contenant les articles
articles_raw <- readRDS("data/raw/data.rds")    

# Préparation des données pour l'analyse
# 1. Division des articles en phrases
# 2. Filtrage des phrases contenant des mots-clés sur les réfugiés ou migrants
# 3. Création d'identifiants uniques pour chaque phrase
articles_preprocessed <- articles_raw %>%
  # Division du texte en phrases tout en conservant la structure du document
  tidytext::unnest_sentences(
    output = body,  # Colonne de sortie pour les phrases
    input = text,            # Colonne contenant le texte original
    drop = FALSE             # Conserver les colonnes originales
  ) %>%
  # Filtrer uniquement les phrases contenant les mots-clés d'intérêt
  filter(grepl('\\brefugee|\\bmigrant', body, ignore.case = TRUE)) %>%
  # Créer une numérotation cohérente des phrases au sein de chaque document
  group_by(doc_id) %>%       # Regrouper par identifiant de document
  mutate(
    sentence_id = paste(doc_id, row_number(), sep = "_")  # Créer un ID unique pour chaque phrase
  ) %>%
  ungroup() %>%
  # Renommer et sélectionner les colonnes pertinentes
  select(doc_id, sentence_id, source, date, body, everything())

# SECTION 3: PRÉTRAITEMENT DU TEXTE
###############################################################################
# Initialisation de la colonne qui contiendra le texte prétraité
articles_preprocessed$body_prepped <- NA

# Application séquentielle des fonctions de prétraitement avec barre de progression
# Note: chaque fonction ci-dessous effectue une transformation spécifique du texte
# pour préparer l'analyse de sentiment
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body, LSDprep_contr)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, LSDprep_dict_punct)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, remove_punctuation_from_acronyms)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, remove_punctuation_from_abbreviations)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, LSDprep_punctspace)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, LSDprep_negation)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, LSDprep_dict)
articles_preprocessed$body_prepped <- pbapply::pbsapply(articles_preprocessed$body_prepped, mark_proper_nouns)

# Sauvegarde des données prétraitées pour éviter de refaire le prétraitement
saveRDS(articles_preprocessed, "data/tmp/lsd_data.rds")

# Rechargement des données prétraitées (en pratique, on peut sauter cette étape si on continue le script)
articles_preprocessed <- readRDS("data/tmp/lsd_data.rds")
articles_df <- articles_preprocessed %>% as.data.frame()

# SECTION 4: ANALYSE DE SENTIMENT
###############################################################################
# Création du corpus de texte à partir des phrases prétraitées
corpus_texte <- quanteda::tokens(articles_df$body_prepped)

# Application du dictionnaire de sentiment (Lexicoder Sentiment Dictionary)
# pour compter les termes positifs et négatifs
matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(corpus_texte, data_dictionary_LSD2015, nested_scope = "dictionary")
)

# Conversion de la matrice de fréquence des termes en dataframe
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "id")

# Combinaison des résultats de sentiment avec les données originales
articles_sentiment <- cbind(articles_df, resultats_sentiment) %>%
  select(-id)  # Supprimer la colonne d'identifiant redondante

# SECTION 5: CALCUL DES MÉTRIQUES DE SENTIMENT
###############################################################################
articles_sentiment <- articles_sentiment %>%
  mutate(
    total_words = str_count(body_prepped, "\\S+"),  # Compter le nombre total de mots
    # Calcul des proportions de termes positifs et négatifs
    proportion_positive = (positive + neg_negative) / total_words,  # Les termes positifs et négations de négatifs
    proportion_negative = (negative + neg_positive) / total_words,  # Les termes négatifs et négations de positifs
    tone_index = proportion_positive - proportion_negative  # Indice de ton global (positif vs négatif)
  )

# SECTION 6: AGRÉGATION PAR DOCUMENT ET PAYS
###############################################################################
# Regroupement des données par document et pays pour obtenir des métriques globales
articles_par_pays <- articles_sentiment %>%
  group_by(doc_id, country) %>%
  summarise(
    total_positive = sum(positive),              # Somme des termes positifs
    total_negative = sum(negative),              # Somme des termes négatifs
    total_neg_positive = sum(neg_positive),      # Somme des négations de termes positifs
    total_neg_negative = sum(neg_negative),      # Somme des négations de termes négatifs
    total_words = sum(total_words),              # Nombre total de mots
    # Calcul des proportions globales
    proportion_positive = sum(positive) / sum(total_words),
    proportion_negative = sum(negative) / sum(total_words),
    # Indice de ton global normalisé
    tone_index = (sum(positive) - sum(negative)) / (sum(positive) + sum(negative)),
    .groups = "drop"
  )

# SECTION 7: MODÉLISATION STATISTIQUE
###############################################################################
# Régression linéaire pour tester l'effet du pays sur l'indice de ton
modele_regression <- lm(tone_index ~ country, data = articles_par_pays)

# Affichage des résultats du modèle
summary(modele_regression)

# SECTION 8: VISUALISATION DES RÉSULTATS
###############################################################################
# Préparation des données pour la visualisation
donnees_graphique <- articles_par_pays %>%
  group_by(country) %>%
  summarise(
    mean_tone_index = mean(tone_index, na.rm = TRUE),  # Moyenne de l'indice de ton par pays
    n = n(),                                           # Nombre d'articles par pays
    .groups = 'drop'
  )

# Création du graphique
graphique_sentiment <- ggplot(donnees_graphique, aes(x = country, y = mean_tone_index, fill = country)) + 
  # Barres représentant la moyenne de l'indice de ton par pays
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  # Couleurs personnalisées pour chaque pays
  scale_fill_manual(values = c(
    "Iraq" = "#007A3D",    # Vert
    "Syrie" = "#CE1126",   # Rouge
    "Ukraine" = "#0057B7"  # Bleu
  )) +
  # Ligne de référence à zéro (neutre)
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1, alpha = 0.7) +
  # Thème minimaliste
  theme_minimal() +
  # Personnalisation de l'apparence
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(hjust = 0.5, size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  # Étiquettes du graphique
  labs(
    x = "Pays", 
    y = "Score moyen de sentiment",
    title = "Sentiment moyen envers les réfugiés par pays",
    subtitle = paste("Basé sur", sum(donnees_graphique$n), "articles contenant des mentions de réfugiés/migrants")
  )

# Affichage du graphique
print(graphique_sentiment)

