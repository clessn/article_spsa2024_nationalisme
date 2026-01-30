# Interprétation des analyses supplémentaires: Event Study DiD et HAPC

Ce document présente les résultats des analyses complémentaires réalisées pour l'article "Zooming in on Zoomers" et leur interprétation dans le contexte de l'argument principal.

---

## 1. Event Study Difference-in-Differences

### Objectif

Fournir des preuves quasi-expérimentales que les événements constitutionnels *causent* des changements d'attitudes envers la souveraineté, en comparant des événements "traitement" (crises constitutionnelles réelles) à des événements "placebo" (périodes sans crise majeure).

### Design

- **Événements traitement**: Référendum 1980, Meech Lake 1990, Référendum 1995, Commandites 2005
- **Événements placebo**: 2012 et 2020
- **Modèle**: `iss_souv ~ post_event * generation + ses_lang.1`
- **Méthode**: Effets marginaux moyens par génération via `marginaleffects`

### Résultats: Événements traitement

| Événement | Génération | Effet post-événement | Erreur-type | p-value | Significatif |
|-----------|------------|---------------------|-------------|---------|--------------|
| **Référendum 1980** | Preboomer | +0.172 | 0.023 | <0.001 | Oui |
| | Boomer | +0.137 | 0.029 | <0.001 | Oui |
| **Meech Lake 1990** | Preboomer | +0.082 | 0.030 | 0.007 | Oui |
| | Boomer | +0.071 | 0.030 | 0.018 | Oui |
| | X | +0.057 | 0.049 | 0.249 | Non |
| **Référendum 1995** | Preboomer | +0.053 | 0.029 | 0.066 | Marginal |
| | Boomer | -0.025 | 0.027 | 0.357 | Non |
| | X | -0.021 | 0.036 | 0.562 | Non |
| **Commandites 2005** | Preboomer | +0.012 | 0.029 | 0.686 | Non |
| | Boomer | +0.010 | 0.027 | 0.726 | Non |
| | X | -0.002 | 0.030 | 0.944 | Non |
| | Y | -0.043 | 0.046 | 0.353 | Non |

**Constats clés:**
1. Seul le **référendum de 1980** montre des effets forts et significatifs (+0.14 à +0.17 sur l'échelle 0-1)
2. **Meech Lake 1990** montre des effets modérés pour les générations plus âgées uniquement
3. Le **référendum de 1995** ne montre pas d'effet positif significatif
4. Le **scandale des commandites** ne montre aucun effet détectable

### Résultats: Événements placebo

| Événement | Génération | Effet post-événement | Erreur-type | p-value | Significatif |
|-----------|------------|---------------------|-------------|---------|--------------|
| **Placebo 2012** | Preboomer | +0.033 | 0.027 | 0.234 | Non |
| | Boomer | -0.005 | 0.021 | 0.808 | Non |
| | X | -0.058 | 0.023 | 0.010 | Oui (négatif) |
| | Y | -0.097 | 0.027 | <0.001 | Oui (négatif) |
| | Z | +0.085 | 0.119 | 0.476 | Non |
| **Placebo 2020** | Preboomer | +0.049 | 0.022 | 0.028 | Marginal |
| | Boomer | +0.036 | 0.011 | <0.001 | Oui |
| | X | -0.037 | 0.012 | 0.001 | Oui (négatif) |
| | Y | -0.085 | 0.013 | <0.001 | Oui (négatif) |
| | Z | -0.116 | 0.018 | <0.001 | Oui (négatif) |

**Constats clés:**
1. Les placebos ne sont **pas neutres** comme attendu
2. Les générations X, Y et Z montrent des **déclins significatifs** pendant ces périodes
3. La Gen Z montre le déclin le plus prononcé (-0.116) lors du placebo 2020
4. Les Boomers montrent une légère hausse (+0.036) pendant la même période

### Test des Parallel Trends

| Événement | Coefficient pré-événement | Erreur-type | p-value | Parallel trends? |
|-----------|---------------------------|-------------|---------|------------------|
| Référendum 1980 | +0.019 | 0.028 | 0.491 | Oui |
| Meech Lake 1990 | -0.015 | 0.027 | 0.594 | Oui |
| Référendum 1995 | -0.068 | 0.028 | 0.016 | **Non** |
| Commandites 2005 | -0.063 | 0.025 | 0.011 | **Non** |

**2 sur 4 événements violent l'hypothèse de parallel trends**, ce qui affaiblit l'interprétation causale pour 1995 et 2005.

---

## 2. Analyse HAPC-CCREM

### Objectif

Résoudre le "problème d'identification APC" en séparant formellement les effets d'âge, de période et de cohorte via un modèle à effets aléatoires croisés (Yang & Land, 2008).

### Modèle

```r
glmer(iss_souv ~ age_scaled + I(age_scaled^2) + (1|generation) + (1|year_factor),
      data = data, family = binomial)
```

- **Effets fixes**: Âge (quadratique)
- **Effets aléatoires croisés**: Génération (cohorte) et Année (période)
- **N** = 36,102 observations
- **Groupes**: 5 générations × 18 années

### Décomposition de la variance

| Source | Variance | % du total |
|--------|----------|------------|
| Cohorte (génération) | 0.099 | **2.7%** |
| Période (année) | 0.240 | **6.6%** |
| Résiduel (π²/3) | 3.290 | 90.7% |

**Interprétation:**
- La période explique **2.4 fois plus** de variance que la cohorte
- Les deux effets restent modestes par rapport à la variance inexpliquée
- La majorité de la variation (90.7%) est attribuable à des facteurs individuels non modélisés

### Tests de rapport de vraisemblance

| Comparaison | χ² | df | p-value |
|-------------|-----|----|---------|
| Modèle complet vs. sans cohorte | 381.59 | 1 | <2.2e-16 |
| Modèle complet vs. sans période | 827.12 | 1 | <2.2e-16 |

Les deux effets sont **hautement significatifs**, mais l'effet période contribue davantage au modèle.

### Effets aléatoires de cohorte (génération)

| Génération | Effet aléatoire | Interprétation |
|------------|-----------------|----------------|
| Preboomer | -0.246 | Moins souverainiste que la moyenne |
| **Boomer** | **+0.433** | **Plus souverainiste** |
| X | +0.206 | Modérément souverainiste |
| Y | +0.009 | Essentiellement neutre |
| **Z** | **-0.398** | **Plus fédéraliste** |

**Interprétation:**
- Les Boomers sont la génération la plus souverainiste, contrôlant pour l'âge et la période
- La Gen Z est la plus fédéraliste, avec un effet comparable en magnitude mais de signe opposé aux Boomers
- La Gen Y est essentiellement neutre
- L'écart Boomer-Gen Z est de **0.83 points** sur l'échelle logit

### Effets aléatoires de période (année)

| Année | Effet | Contexte historique |
|-------|-------|---------------------|
| 1968 | -0.936 | Pré-mouvement souverainiste moderne |
| 1974 | -0.808 | Élection PQ 1976 à venir |
| 1979 | -0.912 | Avant référendum 1980 |
| 1984 | -0.054 | Post-référendum 1980 |
| 1988 | -0.039 | Avant Meech Lake |
| **1993** | **+0.164** | Post-échec Meech/Charlottetown |
| **1997** | **+0.366** | Post-référendum 1995 |
| 2000 | +0.098 | Début scandale commandites |
| 2004 | +0.298 | Commission Gomery |
| 2006 | +0.172 | Post-Gomery |
| 2008 | +0.310 | Maintien du soutien |
| 2011 | +0.120 | Déclin progressif |
| 2015 | +0.139 | Légère stabilisation |
| 2019 | -0.068 | Gouvernement CAQ |
| 2021 | +0.230 | Hausse post-pandémie |
| 2022 | -0.180 | Déclin |
| **2023** | **+1.051** | **Forte hausse** |

**Constats clés:**
1. Les années 1990 (post-Meech, post-1995) montrent les effets positifs les plus forts de la période pré-2023
2. L'effet 2023 (+1.05) est exceptionnellement élevé - à investiguer (contexte politique CAQ? débats linguistiques?)
3. Les années récentes (2019-2022) montrent une volatilité accrue

### Effet de l'âge (fixe)

| Paramètre | Coefficient | Erreur-type | p-value |
|-----------|-------------|-------------|---------|
| Intercept | -0.682 | 0.188 | <0.001 |
| Âge (linéaire) | -0.020 | 0.041 | 0.620 |
| Âge² (quadratique) | +0.101 | 0.014 | <0.001 |

**Interprétation:**
- L'effet linéaire de l'âge n'est pas significatif une fois les effets de cohorte et période contrôlés
- L'effet quadratique est significatif: la relation âge-souveraineté suit une courbe en U
- Les très jeunes et très vieux sont légèrement moins souverainistes que les adultes d'âge moyen

---

## 3. Implications pour l'article

### Ce qui est confirmé par les analyses

1. **Les Boomers sont la génération la plus souverainiste** (+0.43 effet cohorte)
   - Cohérent avec l'argument de socialisation politique pendant les années formatrices (1960s-1970s)

2. **La Gen Z est la plus fédéraliste** (-0.40 effet cohorte)
   - Confirme le constat central de l'article

3. **Les deux types d'effets (cohorte et période) sont statistiquement significatifs**
   - Valide l'approche théorique intégrant socialisation générationnelle ET contexte historique

4. **Les effets de période montrent des pics autour des crises constitutionnelles (1993, 1997)**
   - Cohérent avec la littérature citée (Yale & Durand, Vallée-Dubois et al.)

### Ce qui nuance ou complique le récit

1. **Seul le référendum de 1980 montre des effets causaux clairs dans l'event study**
   - Le référendum 1995 et le scandale des commandites ne montrent pas d'effets significatifs
   - Cela affaiblit l'argument que ces événements ont "cristallisé" les attitudes

2. **Les périodes "placebo" montrent des déclins actifs chez les jeunes générations**
   - Ce n'est pas une simple "absence d'activation" du soutien souverainiste
   - C'est un **déclin séculaire actif** chez les X, Y et Z
   - Suggère des forces structurelles au-delà de l'absence d'événements mobilisateurs

3. **L'hypothèse de parallel trends est violée pour 1995 et 2005**
   - L'interprétation causale de ces événements est fragilisée
   - Les tendances différentielles pré-existaient aux événements

4. **La période explique plus que la cohorte (6.6% vs 2.7%)**
   - Suggère que les attitudes sont plus sensibles au contexte actuel qu'aux expériences formatrices
   - Mais les deux restent modestes face à la variance inexpliquée (90.7%)

5. **L'effet période 2023 est exceptionnellement élevé (+1.05)**
   - Suggère un changement récent significatif à investiguer
   - Potentiellement lié au contexte politique actuel (CAQ, loi 96, débats linguistiques)

### Recommandations pour la discussion

L'article pourrait intégrer ces nuances ainsi:

> "Des analyses complémentaires utilisant un design difference-in-differences (Annexe X) révèlent que seul le référendum de 1980 montre des effets causaux clairs sur les attitudes souverainistes. Contrairement à nos attentes, le référendum de 1995 et le scandale des commandites ne montrent pas d'effets post-événement significatifs, et les périodes sans événements constitutionnels majeurs (2012, 2020) sont associées à des déclins significatifs du soutien souverainiste chez les générations X, Y et Z. Une analyse HAPC-CCREM (Annexe Y) confirme que les effets de cohorte et de période sont tous deux statistiquement significatifs, avec la période expliquant environ 2.4 fois plus de variance que la cohorte. Les Boomers demeurent la génération la plus souverainiste (+0.43 sur l'échelle logit) tandis que la Gen Z est la plus fédéraliste (-0.40). Ces résultats suggèrent que la neutralité de la Gen Z reflète non seulement l'absence d'événements mobilisateurs pendant leurs années formatrices, mais aussi un déclin séculaire actif du soutien souverainiste parmi les jeunes générations."

---

## 4. Comparaison avec Vallée-Dubois, Dassonneville & Godbout (2020)

Notre analyse HAPC s'inscrit dans la continuité méthodologique de l'article "About time: age, period, and cohort effects on support for Quebec sovereignty" (Nations and Nationalism, 2020). Cette comparaison permet de situer nos résultats dans la littérature existante.

### Méthodologie commune

| Aspect | Vallée-Dubois et al. (2020) | Notre étude |
|--------|---------------------------|-------------|
| Modèle | CCREM (Cross-Classified Random Effects) | HAPC-CCREM (glmer) |
| Période | 1985-2012 | 1968-2023 |
| N | 22,607 | 36,102 |
| Population | Francophones du Québec | Tous Québécois |
| VD | Binaire (souveraineté vs autres options) | Binaire (iss_souv 0/1) |
| Effets fixes | Âge (linéaire) | Âge (quadratique) |
| Effets aléatoires | Cohortes (5 ans) × Périodes (années) | Générations × Années |

### Décomposition de la variance (niveau 2 seulement)

Vallée-Dubois et al. calculent la proportion de variance au niveau 2 (excluant la variance résiduelle π²/3):

| Source | Vallée-Dubois et al. | Notre étude |
|--------|---------------------|-------------|
| Cohorte | 13.8% | 29.2% |
| Période | **86.2%** | **70.8%** |
| Ratio période/cohorte | 6.2:1 | 2.4:1 |

**Interprétation:**
- Les deux études confirment la **dominance des effets de période** sur les effets de cohorte
- Notre étude trouve un ratio période/cohorte plus faible (2.4:1 vs 6.2:1)
- Cette différence peut s'expliquer par:
  1. Définition des cohortes (5 ans vs générations)
  2. Période d'étude étendue (inclut Gen Z, absente en 2012)
  3. Échantillon différent (tous Québécois vs francophones seulement)

### Convergence des résultats sur les cohortes

**Vallée-Dubois et al. (2020):**
- Cohortes 1945-49, 1950-54, 1955-59 (baby-boomers) = plus souverainistes
- Cohortes 1965-69, 1970-74 = moins souverainistes
- "Baby boomers are indeed distinctively more supportive of Quebec sovereignty"

**Notre étude:**
- Boomers (1947-1961) = +0.433 (plus souverainistes)
- Gen Z (1992-2003) = -0.398 (plus fédéralistes)
- Cohérence parfaite avec leurs résultats pour les Boomers

### Convergence des résultats sur les périodes

**Vallée-Dubois et al. identifient:**
- 1990-1993: Pic post-Meech Lake
- 1995-1996: Mobilisation référendaire
- 2005: Scandale des commandites
- 1985-1989: "Démobilisation psychologique" post-1980

**Notre étude confirme:**
- 1993: +0.164 (post-Meech/Charlottetown)
- 1997: +0.366 (post-référendum 1995)
- 2004: +0.298 (Commission Gomery)
- Années 1970s: Effets négatifs (pré-mouvement moderne)

### Contribution originale de notre étude

1. **Extension temporelle**: Inclut 2013-2023, révélant l'effet exceptionnel de 2023 (+1.05)

2. **Génération Z**: Première estimation HAPC incluant cette cohorte (-0.40), absente des données de 2012

3. **Event Study DiD**: Complément quasi-expérimental absent de Vallée-Dubois et al., permettant un test de causalité (même si limité par les violations de parallel trends)

4. **Confirmation de la théorie de la socialisation politique**: Nos résultats corroborent leur conclusion que "having experienced the Quiet Revolution and the early fight for Québécois emancipation is what makes the baby boomer generation unique" (p. 360)

### Citation clé de Vallée-Dubois et al. pour notre article

> "Contrary to conventional wisdom, we find that fluctuations in support for [sovereignty] over the last 30 years are not driven exclusively by a process of generational change. Important constitutional events are also responsible for these variations." (p. 360)

Cette conclusion s'aligne avec notre argument: la neutralité de la Gen Z reflète **à la fois** l'absence d'événements mobilisateurs **et** un déclin séculaire actif observable même en période "placebo".

---

## 5. Limites méthodologiques

### Event Study DiD

1. **Violation des parallel trends**: 2/4 événements ne satisfont pas cette hypothèse cruciale
2. **Fenêtres temporelles asymétriques**: Les périodes pré/post varient selon les événements
3. **Confondants potentiels**: Autres changements sociaux contemporains aux événements
4. **Mesure de la VD**: L'harmonisation des échelles (dichotomique, Likert 4-5 points) introduit du bruit

### HAPC

1. **Problème d'identification APC**: Le HAPC ne résout pas complètement la colinéarité mathématique; il impose des hypothèses distributionnelles sur les effets aléatoires
2. **Définition des cohortes**: Les frontières générationnelles sont arbitraires
3. **Hétérogénéité intra-cohorte**: Le modèle traite chaque génération comme homogène
4. **Variable binaire**: La binarisation de la VD (iss_souv) perd l'information sur l'intensité des attitudes

---

## 6. Fichiers générés

- `SharedFolder_spsa_article_nationalisme/tables/event_study/did_results.csv`
- `SharedFolder_spsa_article_nationalisme/tables/event_study/parallel_trends_test.csv`
- `SharedFolder_spsa_article_nationalisme/graphs/event_study/did_by_generation.png`
- `SharedFolder_spsa_article_nationalisme/graphs/event_study/parallel_trends_1995.png`
- `SharedFolder_spsa_article_nationalisme/graphs/hapc_cohort_effects.png`
- `SharedFolder_spsa_article_nationalisme/graphs/hapc_period_effects.png`
- `SharedFolder_spsa_article_nationalisme/graphs/hapc_age_effect.png`
