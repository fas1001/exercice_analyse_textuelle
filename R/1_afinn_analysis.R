# Chargement des bibliothèques nécessaires
library(ggplot2)    # Pour la visualisation des données
library(dplyr)      # Pour la manipulation des données (filtrage, agrégation, etc.)
library(tidyr)      # Pour le nettoyage et la restructuration des données
library(tidytext)   # Pour l'analyse de texte selon l'approche "tidy"
library(purrr)      # Pour la programmation fonctionnelle
library(stringr)    # Pour la manipulation des chaînes de caractères

# Chargement des données
df <- readRDS("data/raw/data.rds")    # Lecture du fichier RDS contenant les articles

# Étape 1: Diviser les articles en phrases et filtrer UNIQUEMENT pour les phrases liées aux réfugiés
refugee_sentences <- df %>%
  # Diviser le texte en phrases en utilisant une expression régulière qui détecte les fins de phrases
  # (?<=\\.|\\?|\\!) recherche un point, point d'interrogation ou d'exclamation suivi d'un espace
  mutate(sentences = str_split(text, "(?<=\\.|\\?|\\!)\\s+")) %>%
  # Dérouler la liste de phrases pour avoir une ligne par phrase
  unnest(sentences) %>%
  # Regrouper par article pour créer des numéros de phrases
  group_by(doc_id) %>%
  # Ajouter un numéro séquentiel à chaque phrase de l'article
  mutate(sentence_number = row_number()) %>%
  ungroup() %>%
  # Créer un identifiant unique pour chaque phrase (combinaison de l'ID du document et du numéro de phrase)
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  # UNIQUEMENT conserver les phrases qui mentionnent les réfugiés ou les migrants
  # grepl cherche un motif dans un texte (ignore.case=TRUE rend la recherche insensible à la casse)
  filter(grepl('refugee|refugees|migrant|migrants', sentences, ignore.case = TRUE)) %>%
  # Ne garder que les documents qui ont au moins une phrase concernant les réfugiés
  group_by(doc_id) %>%
  # n() compte le nombre de lignes dans chaque groupe; filter(n() > 0) garde les groupes avec au moins une ligne
  filter(n() > 0) %>%
  ungroup()

# Étape 2: Nettoyer les phrases pour l'analyse de sentiment
clean_sentences <- refugee_sentences %>%
  mutate(
    # Création d'une nouvelle colonne contenant les phrases nettoyées
    sentences_clean = sentences %>%
      str_to_lower() %>%                # Conversion en minuscules
      str_remove_all("!") %>%           # Suppression des points d'exclamation
      str_remove_all("\\.") %>%         # Suppression des points
      str_remove_all(",")               # Suppression des virgules
  )

# Étape 3: Tokenisation des phrases nettoyées (division en mots individuels)
tokens <- clean_sentences %>%
  # unnest_tokens divise le texte en mots individuels
  # output = word: nom de la colonne de sortie pour les mots
  # input = sentences_clean: colonne contenant le texte à diviser
  unnest_tokens(
    output = word,
    input = sentences_clean
  )

# Étape 4: Suppression des mots vides (stop words)
# Les mots vides sont des mots très fréquents qui n'apportent généralement pas de sens (le, la, et, etc.)
stop_words <- get_stopwords(language = "en")    # Récupération de la liste des mots vides en anglais
tokens_clean <- anti_join(
  tokens,       # Table principale contenant tous les mots
  stop_words,   # Table de référence contenant les mots vides
  by = "word"   # Jointure sur la colonne "word"
)                # anti_join garde uniquement les lignes de tokens qui n'ont PAS de correspondance dans stop_words

# Étape 5: Application de l'analyse de sentiment AFINN
# AFINN est un lexique qui attribue des scores de -5 (très négatif) à +5 (très positif) à des mots
afinn <- get_sentiments("afinn")    # Chargement du lexique AFINN
sentiment_scores <- inner_join(
  tokens_clean,    # Table principale contenant les mots nettoyés
  afinn,           # Table de référence contenant les scores de sentiment
  by = "word"      # Jointure sur la colonne "word"
)                  # inner_join garde uniquement les mots qui ont un score dans le lexique AFINN

# Étape 6: Calcul du sentiment par phrase
sentence_sentiment <- sentiment_scores %>%
  # Grouper par document, ID de phrase, texte de la phrase, date et pays
  group_by(doc_id, sentence_id, sentences, date, country) %>%
  summarise(
    # Calculer le sentiment de la phrase en additionnant les scores des mots
    # na.rm=TRUE ignore les valeurs manquantes
    sentence_sentiment = sum(value, na.rm = TRUE),
    .groups = "drop"    # Supprimer le groupement après l'opération
  )

# Étape 7: Calcul du sentiment par article
article_sentiment <- sentence_sentiment %>%
  # Grouper par document
  group_by(doc_id) %>%
  summarise(
    # Calculer le sentiment moyen de toutes les phrases concernant les réfugiés dans l'article
    article_sentiment = mean(sentence_sentiment, na.rm = TRUE),
    # Compter le nombre de phrases concernant les réfugiés
    num_refugee_sentences = n(),
    .groups = "drop"    # Supprimer le groupement après l'opération
  ) %>%
  # Joindre les informations originales du document (date, pays)
  left_join(
    # Sélectionner uniquement les colonnes nécessaires et éliminer les doublons
    df %>% select(doc_id, date, country) %>% distinct(),
    by = "doc_id"    # Jointure sur la colonne doc_id
  )

# Étape 8: Joindre les métriques de sentiment aux articles originaux
# Inclure uniquement les articles qui mentionnent les réfugiés/migrants
final_df <- df %>%
  # Joindre les données de sentiment aux articles originaux
  left_join(article_sentiment, by = c("doc_id", "date", "country")) %>%
  # Ne garder que les lignes où article_sentiment n'est pas NA
  # (c'est-à-dire, uniquement les articles qui parlent des réfugiés)
  filter(!is.na(article_sentiment))

# Analyse statistique: création d'un modèle linéaire pour examiner 
# l'influence du pays sur le sentiment des articles
m1 <- lm(article_sentiment ~ country, data = final_df)
# Affichage du résumé du modèle (coefficients, significativité, etc.)
summary(m1)

# Utilisation de final_df de notre analyse de sentiment AFINN
# S'assurer que les noms de pays correspondent exactement à ceux dans les données
# (Iraq, Syrie, Ukraine)
final_df$country <- as.character(final_df$country)

# Calculer le sentiment moyen par pays
df_plot <- final_df %>%
  group_by(country) %>%
  summarise(
    mean_sentiment = mean(article_sentiment, na.rm = TRUE),
    se_sentiment = sd(article_sentiment, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

# Créer le graphique à barres
p <- ggplot(df_plot, aes(x = country, y = mean_sentiment, fill = country)) + 
  # Barres pour les moyennes
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  
  # Lignes d'erreur standard
  geom_errorbar(
    aes(ymin = mean_sentiment - se_sentiment, 
        ymax = mean_sentiment + se_sentiment,
        color = country),
    position = position_dodge(width = 0.9), 
    width = 0.25, 
    size = 1
  ) + 
  
  # Personnalisation des couleurs pour Iraq, Syrie et Ukraine
  scale_fill_manual(values = c(
    "Iraq" = "#007A3D",    # Vert (couleur du drapeau irakien)
    "Syrie" = "#CE1126",   # Rouge (couleur du drapeau syrien)
    "Ukraine" = "#0057B7"  # Bleu (couleur du drapeau ukrainien)
  )) +
  
  scale_color_manual(values = c(
    "Iraq" = "#007A3D",    # Vert
    "Syrie" = "#CE1126",   # Rouge
    "Ukraine" = "#0057B7"  # Bleu
  )) + 
  
  # Utiliser les noms de pays exacts
  scale_x_discrete(labels = c("Iraq" = "Iraq", "Syrie" = "Syrie", "Ukraine" = "Ukraine")) + 
  
  # Ligne horizontale à 0
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1, alpha = 0.7) +
  
  # Thème propre et épuré (similaire à theme_clean_light)
  theme_minimal() +
  
  # Personnalisation du thème exactement comme dans votre exemple
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(hjust = 0.5, size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  
  # Étiquettes des axes
  labs(
    x = "Pays", 
    y = "Score moyen de sentiment",
    title = "Sentiment moyen envers les réfugiés par pays",
    subtitle = paste("Basé sur", sum(df_plot$n), "articles contenant des mentions de réfugiés/migrants")
  )

# Afficher le graphique
print(p)
