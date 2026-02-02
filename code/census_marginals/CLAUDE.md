# Census Marginals

Ce dossier contient les scripts pour extraire les distributions marginales des recensements canadiens (Québec seulement) afin de créer des poids de sondage via raking.

## Objectif

Créer un fichier `marginals_all.rds` combinant les distributions marginales de tous les recensements disponibles (1961-2021) pour ensuite appliquer du raking au dataset `merged_v1.rds`.

## Sources de données

| Années | Source | Méthode |
|--------|--------|---------|
| 1961-1991 | [UToronto MDL](https://mdl.library.utoronto.ca/collections/numeric-data/census-canada) | Téléchargement manuel |
| 1996-2021 | Statistique Canada | Package `cancensus` |

## Variables à extraire

Toutes les distributions sont croisées par **genre** (homme/femme).

| Variable | Categories | Notes |
|----------|------------|-------|
| `age_group` | Groupes d'âge du recensement (ex: 15-24, 25-34, etc.) | Conversion en génération au moment du raking |
| `language` | french, english, other | Langue maternelle |
| `education` | 0, 1 | 0 = sans diplôme post-secondaire, 1 = post-secondaire+ |

## Pipeline par année

Chaque année de recensement (1961-1991) a deux scripts:

1. **`fetch_YYYY.R`**: Télécharge et extrait les fichiers ZIP
   - Crée le dossier `raw/YYYY/`
   - Télécharge les ZIP depuis UToronto MDL
   - Extrait les .sav avec préfixes M_/F_ pour différencier male/female
   - Crée les codebooks automatiques via sondr::sav_to_codebook() (voir code/census_marginals/fetch_1961.R)

2. **`structure_YYYY.R`**: Lit les .sav et crée les marginals
   - Lit les fichiers .sav extraits
   - Filtre pour le Québec
   - Extrait age_group, language, education par gender
   - Produit `marginals_YYYY.rds`

Pour 1996-2021, un seul script `fetch_YYYY.R` suffit (utilise `cancensus`).

## Structure de sortie

Chaque script `structure_YYYY.R` (ou `fetch_YYYY.R` pour 1996+) produit un fichier `marginals_YYYY.rds` avec la structure suivante:

```r
# data.frame avec colonnes:
tibble(
  census_year = integer(),    # Année du recensement (1971, 1976, ...)
  variable = character(),     # "age_group" | "language" | "education"
  category = character(),     # Valeur de la variable (ex: "25-34", "french", "1")
  gender = character(),       # "male" | "female"
  n = numeric(),              # Population (count)
  prop = numeric()            # Proportion du total Quebec
)
```

**Note**: Les proportions (`prop`) sont calculées sur le total de la population québécoise 18+ (ou 15+ selon disponibilité).

## Fichiers

```
code/census_marginals/
  # 1961-1991: deux scripts par année (UToronto MDL)
  fetch_1961.R          # Download + extract ZIP
  structure_1961.R      # Lire .sav → marginals_1961.rds
  fetch_1971.R
  structure_1971.R
  ...
  fetch_1991.R
  structure_1991.R

  # 1996-2021: un script par année (cancensus)
  fetch_1996.R
  fetch_2001.R
  fetch_2006.R
  fetch_2011.R
  fetch_2016.R
  fetch_2021.R

  combine_marginals.R   # Combine tous les marginals_YYYY.rds

SharedFolder_spsa_article_nationalisme/data/census/
  marginals/
    raw/
      1961/             # M_*.sav, F_*.sav extraits
      1971/
      ...
    marginals_1961.rds
    marginals_1971.rds
    ...
    marginals_2021.rds
    marginals_all.rds   # Toutes les années combinées
```

## Utilisation pour le raking

Le script `../generate_weights.R` utilisera `marginals_all.rds` pour:

1. Associer chaque observation de sondage à l'année de recensement appropriée
2. Calculer les poids via raking (iterative proportional fitting)
3. Gérer les valeurs manquantes avec des fonctions de raking par combinaisons de variables disponibles

## Notes importantes

- **Géographie**: Québec total seulement (pas de breakdown par région)
- **Population cible**: 18 ans et plus (électeurs potentiels)
- **Cohérence**: Les catégories doivent être harmonisées entre années pour permettre le raking
- Chaque année de recensement peut avoir une structure différente - explorer au cas par cas
