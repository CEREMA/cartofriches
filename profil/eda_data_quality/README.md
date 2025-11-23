# 🧹 Audit Qualité des Données Cartofriches – Phase EDA & Nettoyage

**Auteur :** Rova Ralaimidona  
**Contact :** irova@me.com  
🧭 Le rapport complet d’audit qualité est consultable en ligne ici :  
👉 [Lire le rapport interactif](https://hagatiana.github.io/cartofriches/data_quality_cartofriches.html)

---

## 🗺️ Contexte

Cartofriches est une initiative publique portée par le CEREMA visant à recenser les friches industrielles, urbaines et rurales présentes sur le territoire français. Elle s’appuie sur une plateforme interactive en ligne, combinant visualisation cartographique et base de données structurée.

Toutefois, les jeux de données fournis – bien que riches – présentent de nombreuses limitations en matière de qualité :
- données manquantes massives,
- doublons,
- hétérogénéité des formats,
- désalignement entre les différentes sources (tableaux, carte interactive, observatoires).

Ces limites compromettent à la fois la lisibilité pour l’utilisateur et la fiabilité des analyses pour les décideurs publics.

---

## 🎯 Objectifs du notebook

Ce travail s’inscrit dans une démarche exploratoire et critique, visant à :
- auditer la qualité des données disponibles,
- structurer les variables par domaines cohérents (foncier, pollution, urbanisme...),
- détecter les incohérences majeures (valeurs manquantes, doublons, conflits),
- produire des indicateurs de qualité synthétiques pour faciliter la prise de décision,
- poser les bases techniques pour une future modélisation ou un scoring décisionnel.

---

## 🧠 Contribution individuelle – Rova Ralaimidona

Ce notebook a été réalisé dans le cadre d’un projet de groupe, où j’ai assuré la phase initiale de traitement des données. Mes apports principaux :

- Audit de la structure des données et organisation par domaines fonctionnels,
- Production d’une série de **6 KPI qualité des données**, permettant de quantifier précisément :
  - la complétude,
  - les doublons,
  - la validité,
  - les modalités dominantes,
  - la clarté typologique,
  - et la cohérence métier,
- Proposition d’une variable cible alternative (`site_reconv_type`), plus alignée avec les critères de mutabilité utilisés par les acteurs publics (EPF),
- Alerte sur les risques de biais liés à un enrichissement excessif ou mal maîtrisé,
- Amélioration du score modèle de 0.43 à 0.56 après traitement (base sur laquelle le modèle final a atteint 0.63).

➡️ Ces apports ont permis à l’équipe de clarifier ses choix stratégiques et d’ancrer le travail de modélisation sur des bases plus solides, mieux alignées avec les enjeux métiers du projet.

---

## 🔧 Étapes réalisées

- Analyse exploratoire (EDA) par domaines thématiques,
- Construction d’une fonction d’audit automatisé pour chaque groupe de variables,
- Calcul et agrégation de **6 KPI de qualité des données** par domaine :
  - Taux de complétude,
  - Taux de doublons,
  - Taux de validité (selon dictionnaire CNIG),
  - Modalité dominante et taux de dominance,
  - Typologie des colonnes,
  - Taux de valeurs non valides,
- Détection de corrélations fortes entre variables critiques,
- Premiers arbitrages sur la sélection des variables pertinentes pour la modélisation.

---

## ⏱️ Répartition du temps par type d’effort

| Étape                                                                  | Importance | Pourquoi cela a pris du temps                                                         | Temps estimé |
|------------------------------------------------------------------------|------------|----------------------------------------------------------------------------------------|--------------|
| 🔍 Définir un cadre d’analyse                                          | ✅          | Clarification du périmètre (friches, domaines, typologies, objectifs d’usage)         | 8%           |
| 📖 Formaliser le dictionnaire des variables                            | ✅          | Documentation détaillée des champs selon CNIG et observations terrain                 | 6%           |
| 🧠 Construire une méthode d’audit qualité                              | ✅✅         | Formalisation des règles de validation, organisation des domaines, process itératif   | 12%          |
| ⚙️ Écrire les règles de validation par champ                           | ✅✅         | Codage fin des formats valides, regex, typologies, correspondances CNIG               | 10%          |
| 📊 Construire un système de KPI qualité par domaine                    | ✅✅✅       | Sélection, formulation et calcul de 6 indicateurs pertinents pour le scoring qualité  | 12%          |
| 🧰 Produire un template de rapport généralisable                       | ✅✅✅       | Création d’un rapport clair, synthétique, réutilisable sur tous domaines              | 15%          |
| 🗂️ Comparer et comprendre les origines multiples des jeux de données  | ✅          | Analyse des écarts entre la carte, les tableaux, et les observatoires                 | 5%           |
| 🧪 Tester les premières hypothèses de modélisation (prémodèle, Cramér) | ✅          | Sélection des variables, essais de ciblage, équilibrage, sandbox de features          | 7%           |
| 🤔 Prendre des décisions critiques (cible, enrichissement, biais)      | ✅          | Choix de `site_reconv_type`, réflexion sur le lien entre qualité et usage modèle      | 10%          |
| 🧾 Rédiger la synthèse finale sur la qualité des données               | ✅          | Compilation des résultats, structuration des domaines, conclusion                     | 5%           |
| 🔧 Prise en main des outils collaboratifs (Git, GitHub, conventions)   | ✅          | Apprentissage de GitHub, logique des PR, structure du repo, markdown, push/pull       | 6%           |
| 💻 Génération et automatisation du code                                | –          | Script rapide via méthodes éprouvées, fonctions générées efficacement                 | 4%           |

**Total estimé :** ~110 heures sur 14 jours

---

## 📦 Données utilisées

Les données traitées proviennent du dépôt open source [CEREMA/cartofriches](https://github.com/CEREMA/cartofriches), mis à disposition sous licence MIT.

---

## 📝 Licence

Ce projet dérive du dépôt original `cartofriches`, publié par le CEREMA sous licence MIT.  
Les travaux de structuration, nettoyage et audit qualité des données ont été réalisés par Rova Ralaimidona à des fins exploratoires et pédagogiques.

© 2025 Rova Ralaimidona – Licence MIT

