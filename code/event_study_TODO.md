# Event-Study TODO - Issue #22

## État actuel (event_study_simple.R)

- ✅ Event-study pour Référendum 1995
- ✅ Années: 1988, 1993, 1997, 2000, 2004 (t=-7, -2, 2, 5, 9)
- ✅ Générations: preboomer, boomer, x (Y et Z exclus - pas de pré-1995)
- ✅ Graphique avec marginaleffects (changement vs t=-2)

**Résultat:** Pattern cohérent (pic à t=2, puis redescend) mais **non-significatif**. Manque de puissance statistique.

---

## Prochaines étapes

### 1. Augmenter puissance statistique

**Option A: Ajouter contrôles**
```r
lm(iss_souv2 ~ event_time_f * generation +
   ses_lang.1 + ses_geoloc.1 + ses_age,
   data = data_1995)
```
→ Réduit variance résiduelle → intervalles plus serrés

**Option B: Pooler générations**
```r
lm(iss_souv2 ~ event_time_f, data = data_1995)
```
→ Un seul coefficient par temps → plus de puissance
→ Faire SI pattern vraiment identique entre générations

### 2. Test de parallel trends

Vérifier que t=-7 → t=-2 est plat (pas de tendance pré-événement).

**Test simple:**
```r
# Subset pré seulement
pre_data <- filter(data_1995, event_time < 0)
lm(iss_souv2 ~ event_time * generation, data = pre_data)
```
Si slope ≈ 0 et non-significatif → parallel trends OK

### 3. Autres événements

Répéter pour:
- **Référendum 1980**: 1974, 1979, 1984, 1988
- **Meech 1990**: 1988, 1993, 1997
- **Sponsorship 2004-05**: 2000, 2004, 2006, 2008, 2011

### 4. Placebo tests

Tester "événements" en périodes calmes (devrait montrer rien):
- **2012**: 2008, 2011, 2014, 2015
- **2017**: 2014, 2015, 2019

### 5. Pooled analysis

Combiner tous les événements:
```r
# Stack tous les événements avec event fixed effects
lm(iss_souv2 ~ event_time_f * generation + factor(event_id), data = all_events)
```

---

## Ce que le reviewer veut (Issue #22)

- ✅ Event-study design avec coefficients par année
- ⏳ Parallel trends test
- ⏳ Placebo checks
- ⏳ Pooled analysis avec plusieurs événements
- ⏳ Cohort-specific time trends (optionnel)

---

## Décision immédiate

**Recommandation:** Commencer par #1A (ajouter contrôles) pour voir si l'effet devient significatif avec moins de variance.
