# Interprétation des résultats Event-Study / DiD

## Résumé

L'analyse event-study teste si les "flashpoints constitutionnels" (référendums, Meech Lake, scandale des commandites) ont eu des effets causaux sur les attitudes souverainistes par génération.

## Tests de tendances parallèles

| Événement | p-value | Verdict |
|-----------|---------|---------|
| Référendum 1980 | 0.49 | Tendances parallèles valides |
| Meech Lake 1990 | 0.59 | Tendances parallèles valides |
| Référendum 1995 | **0.016** | **Violation** |
| Scandale commandites 2005 | **0.011** | **Violation** |

Les tendances parallèles sont une condition nécessaire pour interpréter les estimés DiD comme des effets causaux. Seuls 1980 et 1990 satisfont cette condition.

## Effets DiD par événement

### Référendum 1980 (tendances parallèles OK)

| Génération | Effet (pp) | Erreur-type | p-value |
|------------|------------|-------------|---------|
| Preboomer | +17.2 | 0.023 | <0.001 |
| Boomer | +13.7 | 0.029 | <0.001 |

**Interprétation**: Le référendum de 1980 a eu un effet mobilisateur fort et significatif sur les générations adultes à l'époque. L'effet est plus prononcé chez les preboomers (+17pp) que chez les boomers (+14pp).

### Meech Lake 1990 (tendances parallèles OK)

| Génération | Effet (pp) | Erreur-type | p-value |
|------------|------------|-------------|---------|
| Preboomer | +8.2 | 0.030 | 0.007 |
| Boomer | +7.1 | 0.030 | 0.018 |
| Gen X | +5.7 | 0.049 | 0.249 |

**Interprétation**: L'échec de Meech Lake a eu un effet positif significatif sur les attitudes souverainistes des preboomers et boomers, mais l'effet n'est pas significatif pour la Gen X (possiblement dû à un plus petit échantillon ou une socialisation politique moins avancée).

### Référendum 1995 (VIOLATION des tendances parallèles)

| Génération | Effet (pp) | p-value |
|------------|------------|---------|
| Preboomer | +5.3 | 0.066 |
| Boomer | -2.5 | 0.357 |
| Gen X | -2.1 | 0.562 |

**Interprétation**: Résultats non interprétables causalement en raison de la violation des tendances parallèles. Les effets mixtes et non significatifs suggèrent que 1995 n'a pas eu d'effet net durable sur les attitudes.

### Scandale des commandites 2005 (VIOLATION des tendances parallèles)

| Génération | Effet (pp) | p-value |
|------------|------------|---------|
| Preboomer | +1.2 | 0.686 |
| Boomer | +1.0 | 0.726 |
| Gen X | -0.2 | 0.944 |
| Gen Y | -4.3 | 0.353 |

**Interprétation**: Aucun effet significatif détecté. Le scandale des commandites ne semble pas avoir eu d'impact durable sur les attitudes souverainistes, contrairement aux attentes.

## Tests placebo

Les tests placebo vérifient si le modèle détecte des "effets" à des dates sans événement constitutionnel majeur.

### Placebo 2012

| Génération | Effet (pp) | p-value |
|------------|------------|---------|
| Preboomer | +3.3 | 0.234 |
| Boomer | -0.5 | 0.808 |
| Gen X | **-5.8** | **0.010** |
| Gen Y | **-9.7** | **<0.001** |
| Gen Z | +8.5 | 0.476 |

### Placebo 2020

| Génération | Effet (pp) | p-value |
|------------|------------|---------|
| Preboomer | **+4.9** | **0.028** |
| Boomer | **+3.6** | **<0.001** |
| Gen X | **-3.7** | **0.001** |
| Gen Y | **-8.5** | **<0.001** |
| Gen Z | **-11.6** | **<0.001** |

**Problème majeur**: Les placebos montrent des effets hautement significatifs, particulièrement pour les générations Y et Z. Cela indique que le modèle capte des **tendances séculaires** (évolution graduelle dans le temps) plutôt que des effets causaux d'événements ponctuels.

## Implications pour l'article

### Ce que les résultats suggèrent

1. **Les crises constitutionnelles historiques ont marqué les générations plus âgées**: Le référendum de 1980 et l'échec de Meech Lake ont eu des effets mobilisateurs réels et durables sur les preboomers et boomers.

2. **Le désengagement des jeunes n'est pas lié à l'absence de "moments fondateurs"**: Les tests placebo montrent que les Gen Y et Z présentent une tendance baissière continue, indépendamment des événements constitutionnels.

3. **La divergence générationnelle est un phénomène graduel**: Plutôt qu'une réponse à des événements spécifiques, le déclin du souverainisme chez les jeunes semble être une tendance de fond liée à des facteurs structurels (mondialisation, identités multiples, éloignement temporel des conflits linguistiques, etc.).

### Limites de l'approche event-study

- La violation des tendances parallèles pour 1995 et 2005 empêche toute inférence causale pour ces événements
- L'échec des tests placebo suggère que le design DiD n'est pas approprié pour cette question de recherche
- Les effets de cohorte, période et âge sont confondus

### Recommandations

1. **Ne pas inclure l'event-study comme analyse principale** dans l'article — les conditions d'identification ne sont pas satisfaites pour la plupart des événements.

2. **Mentionner les résultats de 1980 et 1990 prudemment** comme évidence suggestive (mais non définitive) que les crises constitutionnelles ont mobilisé les générations plus âgées.

3. **Prioriser l'issue #17 (modèles APC)**: L'approche Age-Period-Cohort (HAPC-CCREM) serait plus appropriée pour démêler les effets d'âge, de période et de cohorte.

4. **Reframer l'argument**: Plutôt que "les jeunes n'ont pas vécu de moments fondateurs", l'argument devrait être "les attitudes souverainistes montrent une tendance générationnelle de fond qui n'est pas simplement réactive aux événements politiques".

## Fichiers sources

- `SharedFolder_spsa_article_nationalisme/tables/event_study/did_results.csv`
- `SharedFolder_spsa_article_nationalisme/tables/event_study/parallel_trends_test.csv`
