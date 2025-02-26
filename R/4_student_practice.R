###############################################################################
# SCRIPT DE PRATIQUE D'ANALYSE DE TEXTE POUR LES ÉTUDIANTS
# FAS1001 - Introduction aux mégadonnées en sciences sociales
###############################################################################
library(dplyr)

df <- readRDS("data/raw/data_political_bias.rds")

# Défi: Essayez de répondre à la question: 
# « Est-ce que les articles biaisés à gauche ont un ton plus positif que les articles biaisés à droite? »

# Ëtapes:
# 1. Créer une variable binaire pour les articles biaisés à gauche (1) et à droite (0)
# 2. Faire l'analyse textuelle pour tous les articles 
# 3. Faire une régression lm(tone ~ left_bias, data = df) pour voir si le biais à gauche est associé à un ton plus positif
