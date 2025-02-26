# Exercice d'Analyse Textuelle

Ce dépôt contient des scripts d'analyse textuelle pour le cours FAS1001 - Introduction aux mégadonnées en sciences sociales.

## Comment utiliser ce dépôt

1. **Explorer les exemples**:
   - Commencez par ouvrir et exécuter le script `R/1_afinn_analysis.R` pour comprendre l'analyse de sentiment de base
   - Explorez ensuite les scripts `R/2_bing_analysis.R` et `R/3_lsd_analysis.R` pour voir d'autres approches
   - Chaque script contient des commentaires détaillés expliquant ce que fait le code

2. **Travailler sur votre exercice**:
   - Ouvrez le script `R/4_student_practice.R`
   - Suivez les instructions pour répondre à la question de recherche
   - Vous pouvez vous inspirer des exemples fournis dans les scripts 1, 2 et 3

### Conseils d'utilisation
- **Comprendre le code**: Les scripts sont abondamment commentés pour faciliter la compréhension
- **Expérimentation**: N'hésitez pas à modifier les paramètres et à observer les résultats

## Structure du dépôt

### Scripts d'analyse
- `R/1_afinn_analysis.R` - Exemple d'analyse de sentiment avec le lexique AFINN
- `R/2_bing_analysis.R` - Exemple d'analyse de sentiment avec le lexique Bing
- `R/3_lsd_analysis.R` - Exemple d'analyse utilisant la méthodologie LSD (Lexicoder Sentiment Dictionary)
- `R/9_lsd_prep_functions.R` - Fonctions utilitaires pour l'analyse LSD

### Script pour les étudiants
- `R/4_student_practice.R` - Script de pratique pour les étudiants contenant une question de recherche à explorer:
  > "Est-ce que les articles biaisés à gauche ont un ton plus positif que les articles biaisés à droite?"

### Données
- `data/raw/` - Données brutes pour les analyses
- `data/tmp/` - Données temporaires générées lors des analyses LSD
