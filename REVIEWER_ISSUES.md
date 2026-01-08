# Issues GitHub - Révisions Nations and Nationalism

Date de soumission des révisions: **8 février 2026**

---

## Issue #1: Transparence - Modèles et choix méthodologiques en annexe
**Reviewer:** Reviewer 1
**Priorité:** CRITIQUE
**Citation:**
> "The main weakness is the lack of the transparency. All the models and all coding choices should be shown in appendix."

**Action requise:**
- Créer une annexe complète avec tous les modèles de régression
- Documenter tous les choix de codage (recodage des variables, gestion des valeurs manquantes, etc.)
- Inclure les tableaux de régression pour les 11 modèles annuels (Figure 1)
- Ajouter les spécifications complètes des modèles pour Figures 2-5

**Fichiers concernés:**
- Créer nouveau fichier: `appendix_models.tex` ou `appendix_models.docx`
- Scripts: `code/figure*.R` (extraire les spécifications)

---

## Issue #2: Documentation exhaustive de toutes les variables
**Reviewer:** Reviewer 1
**Priorité:** CRITIQUE
**Citation:**
> "What we currently have as supplementary material B, we need that clearly and explicitly for all variables, dependent and independent. It can be in Supplementary material."

**Action requise:**
- Créer un document de matériel supplémentaire exhaustif
- Pour CHAQUE variable (dépendante et indépendante):
  - Nom de la variable
  - Question exacte posée dans les enquêtes
  - Codage original
  - Recodage effectué
  - Source (CES année X, Synopsis vague Y, etc.)
  - Statistiques descriptives
- Étendre le Supplementary Material B actuel

**Fichiers concernés:**
- `SharedFolder.../submission/` - créer `SupplementaryMaterial_Variables.pdf`

---

## Issue #3: Clarifier Figure 1 - Intervalles de confiance et lignes grises
**Reviewer:** Reviewer 1
**Priorité:** HAUTE
**Citation:**
> "Figure 1 is unclear. I reread the descriptive paragraph on slide 17 several times and I still don't understand. What is the confidence interval? What are the thin grey lines? Give more information."

**Action requise:**
- Ajouter une légende explicite dans Figure 1
- Clarifier dans le texte:
  - Que représentent les lignes grises épaisses
  - Que représentent les lignes grises minces
  - Comment les intervalles de confiance sont calculés (bootstrap? erreurs-types?)
  - Niveau de confiance (95%?)
- Améliorer la caption de la figure
- Réviser le paragraphe descriptif (slide 17 de la présentation correspond à quelle page de l'article?)

**Fichiers concernés:**
- `code/figure1_evolution_past_article.R`
- Texte de l'article (section Results)

---

## Issue #4: Figure 1 - Interaction Année × Langue manquante
**Reviewer:** Reviewer 1
**Priorité:** CRITIQUE
**Citation:**
> "Figure 1 appears to be your headline result, yet the underlying model does not include a Year × Language interaction. Because one important claim (p.21) is that anglophones, francophones, and allophones follow different time-trends, pooling them (in my view) produces a misleading 'average' line. Either (a) estimate the model with an explicit Year × Language interaction or (b) drop the model and simply plot separate language-specific trends. In any case, discuss this."

**Action requise:**
CHOISIR UNE APPROCHE:
- **Option A:** Ré-estimer le modèle avec interaction Year × Language
- **Option B:** Supprimer le modèle et tracer des tendances séparées par langue (descriptives)

**Si Option A:**
- Modifier `code/figure1_evolution_past_article.R`
- Ajouter terme d'interaction dans les 11 régressions annuelles
- Générer des prédictions séparées par langue
- Tracer 3 lignes distinctes (Franco/Anglo/Allo)

**Si Option B:**
- Créer des tendances descriptives stratifiées par langue
- Pas de modèle, juste moyennes par année-cohorte-langue
- Plus simple mais moins sophistiqué

**Discussion requise:**
- Justifier le choix dans le texte
- Expliquer pourquoi une approche est préférable à l'autre

**Fichiers concernés:**
- `code/figure1_evolution_past_article.R`
- Texte de l'article (Methods + Results)

---

## Issue #5: Justifier l'utilisation des contrôles dans Figure 1
**Reviewer:** Reviewer 1
**Priorité:** MOYENNE
**Citation:**
> "I think you could even think of making figure 1 'descriptive stats' stratified by relevant group. What are we gaining here by controlling? Why can't we just have position by cohort? At least, justify the use of controls."

**Action requise:**
- Ajouter un paragraphe dans la section Methods justifiant l'usage des contrôles
- Expliquer ce que les contrôles apportent vs statistiques descriptives
- **Alternative:** Présenter une version sans contrôles en annexe pour comparaison
- Expliquer quelle variance les contrôles retirent et pourquoi c'est important

**Fichiers concernés:**
- Texte de l'article (Methods)
- Possiblement: créer Figure 1bis (sans contrôles) pour l'annexe

---

## Issue #6: Détailler les considérations éthiques
**Reviewer:** Reviewer 1
**Priorité:** BASSE
**Citation:**
> "'Ethical considerations were observed, with data confidentiality maintained through non-disclosure agreements.' Give more info."

**Action requise:**
- Étendre la section sur les considérations éthiques
- Préciser:
  - Avec qui les NDAs ont été signés (Synopsis, Léger, CES?)
  - Quelles données sont couvertes
  - Protocole d'approbation éthique (comité d'éthique institutionnel?)
  - Numéro de certificat éthique si applicable
  - Consentement des participants
  - Stockage et anonymisation des données

**Fichiers concernés:**
- Texte de l'article (Methods - Data section)

---

## Issue #7: Détailler le design et l'exécution des enquêtes
**Reviewer:** Reviewer 1
**Priorité:** MOYENNE
**Citation:**
> "'The surveys' design and execution were carefully structured to ensure the collection of relevant and reliable data.' Give more info."

**Action requise:**
- Remplacer phrase vague par détails spécifiques:
  - Méthode d'échantillonnage (probabiliste? quota?)
  - Taille des échantillons cibles
  - Mode de collecte (téléphone, web, mixte?)
  - Période de terrain (dates précises)
  - Taux de réponse
  - Durée moyenne du questionnaire
  - Langue(s) d'administration
  - Pré-tests effectués?
- Faire cela pour CHAQUE source de données (CES + 9 vagues Synopsis)

**Fichiers concernés:**
- Texte de l'article (Methods - Data section)
- Possiblement tableau récapitulatif des enquêtes

---

## Issue #8: Clarifier la transformation d'échelle 0-to-1
**Reviewer:** Reviewer 1
**Priorité:** HAUTE
**Citation:**
> "methodological decision was made to standardize all responses onto a common 0-to-1 continuum, where 0 represents a federalist stance and 1 a separatist one. On the 5-point scale, midpoint values were assigned proportionally. This transformation facilitates comparison but introduces some interpretive limitations……" Give more info about the limitation. I also don't understand why midpoint values are assigned. You are rescaling from 0 to 1. So, midpoint should just be 0.5?; in any case, clarify."

**Action requise:**
- Clarifier la phrase ambiguë sur les "midpoint values"
- Expliquer la transformation pour chaque type d'échelle:
  - Échelle 4-point → 0-to-1 (comment?)
  - Échelle 5-point → 0-to-1 (comment?)
  - Échelle dichotomique → 0-to-1 (trivial: 0 et 1)
- Exemple numérique concret:
  - Si 5-point: 1→0, 2→0.25, 3→0.5, 4→0.75, 5→1 ?
  - Ou autre formule?
- Discuter les **limitations interprétatives** spécifiquement:
  - Perte de granularité?
  - Ordinalité vs cardinalité?
  - Comparabilité inter-enquête?
  - Biais potentiels?

**Fichiers concernés:**
- Texte de l'article (Methods)
- `code/` scripts où le recodage est effectué

---

## Issue #9: Détailler le processus de nettoyage des données
**Reviewer:** Reviewer 1
**Priorité:** MOYENNE
**Citation:**
> "'The cleaning process was designed to enhance compatibility for integration with other surveys and the Canadian Electoral Studies, ensuring consistency across datasets. In pursuit of this goal, the datasets were cleansed to enable seamless merging with other surveys and the Canadian Electoral Studies.' Give more info."

**Action requise:**
- Phrase actuelle est répétitive et vague
- Préciser les étapes de nettoyage:
  - Harmonisation des noms de variables
  - Gestion des valeurs manquantes (listwise deletion? imputation?)
  - Recodage des catégories (exemples concrets)
  - Exclusions (critères: âge? résidence au Québec? complétude?)
  - Vérifications de cohérence
- Documenter le nombre d'observations à chaque étape:
  - N initial (par source)
  - N après exclusions
  - N final fusionné = 12,191
- Script de nettoyage disponible?

**Fichiers concernés:**
- Texte de l'article (Methods - Data Cleaning)
- Possiblement: créer `code/data_cleaning.R` documenté

---

## Issue #10: Ajouter tableaux de régression pour les modèles annuels
**Reviewer:** Reviewer 1
**Priorité:** CRITIQUE
**Citation:**
> "First paragraph, Results, 'predictions are derived from a series of yearly regression models' since I don't see the regressions tables nor any details about it, I can't know what this is about."

**Action requise:**
- Créer tableaux de régression pour les 11 modèles annuels (Figure 1)
- Format suggéré: tableau multi-colonnes avec année en colonne
- Inclure:
  - Coefficients
  - Erreurs-types (entre parenthèses)
  - Significativité (*, **, ***)
  - R² / R² ajusté
  - N par modèle
  - Variables incluses (cohorte + contrôles)
- Placer en annexe ou matériel supplémentaire
- Référencer explicitement dans le texte Results

**Fichiers concernés:**
- `code/figure1_evolution_past_article.R` (extraire les modèles)
- Créer: `appendix_regression_tables.tex` ou `.docx`

---

## Issue #11: Présenter résultats sans contrôles en annexe
**Reviewer:** Reviewer 1
**Priorité:** HAUTE
**Citation:**
> "I think, in the annex, all your results should be presented without controls. I understand that controls allow to 'isolate the specific effect of […] variables while controlling for other factors.' Still, just show everything, transparently. Right now, we don't even know what you are controlling for. That is really not transparent. Generally, please be more transparent on all choices made, everywhere."

**Action requise:**
- **Dans le texte principal:** Lister explicitement TOUS les contrôles utilisés dans chaque modèle
- **En annexe:** Ré-estimer TOUS les modèles (Figures 1-5) SANS contrôles
- Créer figures/tableaux en annexe montrant:
  - Résultats sans contrôles
  - Comparaison avec résultats avec contrôles
- Discuter: comment les contrôles changent les inférences?

**Fichiers concernés:**
- Modifier TOUS les scripts: `code/figure*.R`
- Créer versions "no controls" de chaque modèle
- Créer: `appendix_robustness_no_controls.pdf`

---

## Issue #12: Nuancer la relation événements-soutien souverainiste
**Reviewer:** Reviewer 1
**Priorité:** MOYENNE
**Citation:**
> "Substantively, p.17 'Overall, a clear association emerges between major secession-related events and surges in separatist support' Is it that clear? Discuss. I see a decline after the referendum on figure 1. Seems to be a wiggly relationship."

**Action requise:**
- Réviser l'affirmation p.17 (à localiser dans l'article)
- Nuancer: la relation n'est PAS claire/simple
- Discuter:
  - Déclin APRÈS le référendum de 1995 (contredit le claim)
  - Relation "wiggly" = non-linéaire, complexe
  - Timing: augmentation avant vs après les événements?
  - Effet hétérogène selon les cohortes?
- Possiblement qualifier: "mixed" ou "complex" plutôt que "clear"
- Alternative: analyser événement par événement

**Fichiers concernés:**
- Texte de l'article (Results, p.17 à identifier)
- Possiblement: analyse supplémentaire dans `code/figure1_evolution_past_article.R`

---

## Issue #13: Refocaliser la Discussion sur les résultats principaux
**Reviewer:** Reviewer 1
**Priorité:** HAUTE
**Citation:**
> "Lastly, the discussion goes in several directions. You cite many articles, but the links between them are rather weak. What I would like to see in the discussion is something clearer and more focused on what you believe to be the **main result or results** of the article."

**Action requise:**
- Restructurer la Discussion:
  1. **Identifier 2-3 résultats principaux** (lesquels?)
     - Déclin générationnel chez les francophones?
     - Modération de la Gen Z?
     - Persistance de l'identité québécoise?
  2. Organiser la discussion autour de CES résultats principaux
  3. Pour chaque citation d'article:
     - Lien explicite avec VOS résultats
     - Convergence ou divergence?
     - Qu'est-ce que ça apporte à l'interprétation?
  4. Élaguer citations tangentielles
  5. Créer des transitions claires entre paragraphes

- Structure suggérée:
  - §1: Rappel du résultat principal #1 + littérature pertinente
  - §2: Résultat principal #2 + littérature
  - §3: Résultat principal #3 + littérature
  - §4: Implications théoriques
  - §5: Limites
  - §6: Conclusion

**Fichiers concernés:**
- Texte de l'article (Discussion section) - réécriture majeure

---

## Issue #14: Renforcer l'argument théorique sur les mécanismes
**Reviewer:** Reviewer 2
**Priorité:** HAUTE
**Citation:**
> "To improve the paper's theoretical argument, I would look to the authors to specify mechanisms by which salience, language regimes, and late socialization produce depolarization among younger cohorts. Right now, several claims (e.g., 'latent' separatism among Gen Z) read as plausible but speculative (the paper itself acknowledges this). It would significantly strengthen the theoretical claims if the authors were to more clearly delineate testable implications regarding civic versus ethnolinguistic mobilization, 'everyday nationalism,' and identity plasticity across bilingual social fields."

**Action requise:**
- Renforcer la section théorique (Introduction ou section Theory)
- Spécifier les MÉCANISMES causaux:
  - **Salience:** Comment la faible saillance de la souveraineté dans les années 2010-2020 affecte la socialisation de la Gen Z?
  - **Régimes linguistiques:** Loi 101, bilinguisme croissant → comment ça change les attitudes?
  - **Socialisation tardive:** Gen Z n'a pas vécu les référendums → quelles conséquences?
- Développer des implications testables:
  - Si mobilisation CIVIQUE (vs ethnolinguistique), on devrait observer X
  - Si "everyday nationalism", on devrait observer Y
  - Si plasticité identitaire dans champs bilingues, on devrait observer Z
- Lier "latent separatism" à une théorie explicite (laquelle?)
- Possibilité de tester certaines implications avec les données actuelles?

**Fichiers concernés:**
- Texte de l'article (Introduction + Theory section si existe, sinon créer)

---

## Issue #15: Considérer modèles APC pour identifier effets cohorte
**Reviewer:** Reviewer 2
**Priorité:** HAUTE (méthodologique majeure)
**Citation:**
> "The paper operationalizes cohorts as discrete generations and runs year-by-year models. The central claim — that Gen Z is less polarized and more moderate on sovereignty — rests on interpreting cohort effects. But if the model cannot disentangle cohort from period, we cannot be sure that the moderation is truly generational (a long-term imprint of socialization) rather than simply contextual (the political climate of 2021–2023). That weakens the paper's causal story about 'Zoomers depolarizing sovereignty.' The authors might instead consider instead estimating a hierarchical APC model (HAPC-CCREM) or an age–period–cohort with constrained estimators, and then demonstrating robustness to alternative identification strategies. At minimum, supplement Figure 1 with models including continuous age and birth-year fixed effects, period fixed effects, and cohort groupings, and show that inferences about Gen Z are not artifacts of bin cut-points."

**Action requise:**
- **Problème APC (Age-Period-Cohort):** Identification classique
  - Age = Period - Cohort (colinéarité parfaite)
  - Modèles actuels ne séparent pas cohorte de période

- **Solutions proposées:**
  1. **HAPC-CCREM** (Hierarchical APC - Cross-Classified Random Effects Model)
  2. **Intrinsic Estimator** ou autre contrainte d'identification
  3. **Minimum:** Modèles avec:
     - Âge continu (pas catégories)
     - Effets fixes année
     - Effets fixes cohorte de naissance
     - Tester sensibilité aux cut-points de génération

- **Actions concrètes:**
  - Estimer au moins 2-3 modèles APC alternatifs
  - Comparer les résultats aux modèles actuels
  - Montrer que l'effet Gen Z est robuste
  - Discuter limitations d'identification dans le texte
  - Possiblement: annexe méthodologique sur le problème APC

- **Packages R suggérés:**
  - `APCtools`
  - `APCI`
  - `lme4` (pour HAPC)

**Fichiers concernés:**
- Créer: `code/apc_models.R`
- `code/figure1_evolution_past_article.R` (révision majeure)
- Texte de l'article (Methods - ajouter section APC)
- Annexe méthodologique

---

## Issue #16: Résoudre l'incohérence définition Gen Z + robustesse
**Reviewer:** Reviewer 2
**Priorité:** MOYENNE
**Citation:**
> "There is also an internal inconsistency in the definition of Gen Z. Early on the authors define Gen Z as 1992–2003 (to match census groupings), but later write 'born after 1995.' This affects who counts as Gen Z and could change the estimates in Figures 2 through 5. The authors make it clear that they use 1992–2003 as the Gen Z age cohort in their modelling, but if there is a lack of consensus on what constitutes Gen Z then they may want to re-estimate with at least two alternative cut-points (e.g., 1992+; 1995+) as a robustness check."

**Action requise:**
- **Identifier l'incohérence** dans le texte:
  - Où dit-on "1992-2003"?
  - Où dit-on "born after 1995"?
  - Corriger pour cohérence

- **Checks de robustesse:**
  1. Définition actuelle: 1992-2003
  2. Définition alternative 1: 1995+ (pas de borne supérieure)
  3. Définition alternative 2: 1997-2012 (autre découpage commun)

- **Ré-estimer Figures 2-5** avec les 2-3 définitions:
  - Les résultats changent-ils substantiellement?
  - La Gen Z reste-t-elle "modérée"?

- **Présentation:**
  - Figures principales: garder 1992-2003 (justifier pourquoi)
  - Annexe: résultats avec définitions alternatives
  - Texte: discuter la sensibilité aux cut-points

**Fichiers concernés:**
- `code/figure2_current_attitudes.R`
- `code/figure3_attitude_force.R`
- `code/figure4_id_qc_can.R`
- `code/figure5_attitude_force_idcan.R`
- Créer: `appendix_robustness_genz_definition.pdf`

---

## Issue #17: Ajouter intervalles de confiance 95% à Figure 1
**Reviewer:** Reviewer 2
**Priorité:** HAUTE
**Citation:**
> "Figure 1 relies on 'a series of yearly regression models (n=11)' to generate cohort predictions, but the paper neither reports coefficients nor shows confidence intervals on the time-series of predictions, particularly where 'greater uncertainty' at the earliest points is noted. Please plot 95% CIs on all predicted series and include model tables (or a well-documented appendix)."

**Action requise:**
- Ajouter bandes de confiance à 95% sur Figure 1
- Pour TOUTES les lignes de prédiction (cohortes)
- Utiliser `geom_ribbon()` dans ggplot2 ou équivalent
- S'assurer que les CIs sont visibles (transparence, couleurs)
- Calculer les CIs à partir des erreurs-types des prédictions
- Vérifier que l'incertitude est plus grande aux extrêmes (début/fin de série)
- Caption: mentionner les intervalles de confiance
- Texte: commenter où l'incertitude est la plus grande et pourquoi

- **Note:** Ceci se combine avec Issue #10 (tableaux de régression)

**Fichiers concernés:**
- `code/figure1_evolution_past_article.R`
- Régénérer: `SharedFolder.../figures/figure1_evolution.png`

---

## Issue #18: Résoudre l'incohérence années CES utilisées
**Reviewer:** Reviewer 2
**Priorité:** MOYENNE
**Citation:**
> "Additionally, clarify the exact set of years used: the Data section lists a subset of CES years, whereas the Supplement includes 1997 and 2000 as well—this inconsistency needs to be resolved."

**Action requise:**
- **Identifier l'incohérence:**
  - Lister les années CES mentionnées dans Data section
  - Lister les années CES mentionnées dans Supplement
  - Comparer

- **Résoudre:**
  - Vérifier dans le code quelles années sont RÉELLEMENT utilisées
  - Corriger le texte pour cohérence (Data + Supplement + Code)
  - Si 1997 et 2000 sont utilisés, pourquoi? (disponibilité variable souveraineté?)
  - Si pas utilisés, supprimer des mentions

- **Clarifier dans un tableau:**
  - Année | CES wave | N Québec | Variable souveraineté | Inclus dans analyse

**Fichiers concernés:**
- Texte de l'article (Data section)
- Supplement
- `code/figure1_evolution_past_article.R` (vérifier années chargées)

---

## Issue #19: Documenter poids, effets fixes enquête/mode, SEs clustérisées
**Reviewer:** Reviewer 2
**Priorité:** HAUTE
**Citation:**
> "The paper merges varied modes of the CES and online Léger/LEO panel data. The authors note pre-stratification for the Synopsis surveys, but there is no discussion of post-stratification/calibration weights, design effects, mode adjustments, or harmonized weighting across sources. Given known differences between probability and online panels, the authors should be expected to (i) describe weights and whether they are used in all models; (ii) add survey-year fixed effects and, ideally, survey-mode controls; (iii) cluster standard errors appropriately."

**Action requise:**
- **(i) Poids d'enquête:**
  - Les CES fournissent des poids → utilisés?
  - Synopsis: pré-stratifié → poids fournis?
  - Si poids utilisés: le documenter clairement
  - Si PAS utilisés: justifier pourquoi (choix méthodologique)
  - Tester sensibilité: résultats avec/sans poids

- **(ii) Effets fixes enquête/mode:**
  - Ajouter des indicatrices pour:
    - Année d'enquête (surtout si plusieurs enquêtes par année)
    - Source (CES vs Synopsis)
    - Mode (téléphone vs web vs mixte)
  - Ré-estimer les modèles principaux avec ces FE
  - Comparer avec résultats actuels

- **(iii) Erreurs-types clustérisées:**
  - Clustériser par: enquête-année? vague Synopsis? individu (si panel)?
  - Utiliser `cluster` option dans `lm()` / `glm()` ou packages robustes (`sandwich`, `lmtest`)
  - Recalculer tous les p-values et CIs

- **Rapport:**
  - Tableau comparatif en annexe: résultats avec/sans ajustements
  - Texte Methods: décrire tous ces choix

**Fichiers concernés:**
- TOUS les scripts: `code/figure*.R`
- Texte Methods
- Annexe robustesse

---

## Issue #20: Clarifier pré- vs post-stratification
**Reviewer:** Reviewer 2
**Priorité:** BASSE
**Citation:**
> "I also wonder the authors meant 'post-stratification' instead of 'pre-stratification' as it's difficult to imagine that they would obtain a perfectly quota-sampled panel based on the socio-demographic controls they employ, but I am not familiar with Synopsis or its sampling methods."

**Action requise:**
- Vérifier la terminologie utilisée dans le texte
- **Pré-stratification:** Quotas lors du recrutement (avant collecte)
- **Post-stratification:** Pondération après collecte
- Synopsis utilise quelle méthode exactement?
  - Contacter Synopsis si nécessaire pour clarifier
  - Vérifier documentation Synopsis
- Corriger le texte avec le terme approprié
- Expliquer brièvement la procédure Synopsis

**Fichiers concernés:**
- Texte de l'article (Data section)

---

## Issue #21: Harmoniser contrôle éducation + tester ideology/party ID
**Reviewer:** Reviewer 2
**Priorité:** HAUTE
**Citation:**
> "Appendix C lists region, gender, language, income, and birth in Canada for CES, but education—which the Synopsis surveys do contain—is not consistently used. Education is central to both identity politics and language exposure. If possible, the authors would do well to harmonize an education control, and show robustness to including ideology/party ID in the 2021–2023 models. At minimum, they should discuss the risk of omitted variable bias."

**Action requise:**
- **Éducation:**
  - Vérifier disponibilité dans TOUTES les sources (CES 1974-2021, Synopsis 2022-23)
  - Si disponible partout: harmoniser les catégories et INCLURE dans tous les modèles
  - Si manquant dans certaines années CES: stratégie?
    - Sous-échantillon avec éducation?
    - Imputation?
    - Modèles séparés?
  - Justifier l'importance (identity politics, language exposure)

- **Ideology / Party ID:**
  - Disponible dans CES 2021 et Synopsis 2022-23?
  - Estimer modèles 2021-2023 (Figures 2-5) AVEC ces contrôles
  - Tester sensibilité: résultats changent-ils?
  - Problème potentiel: post-treatment bias?
    - Ideology pourrait être médiateur entre génération et souveraineté
    - Discuter dans texte

- **Omitted Variable Bias:**
  - Si éducation/ideology pas inclus: discuter le biais potentiel
  - Direction du biais?
  - Ampleur probable?

**Fichiers concernés:**
- `code/figure2_current_attitudes.R` à `figure5*.R`
- Annexe: modèles avec/sans éducation/ideology
- Texte Methods + Discussion (limitations)

---

## Issue #22: Considérer event-study ou DiD pour événements constitutionnels
**Reviewer:** Reviewer 2
**Priorité:** MOYENNE (enhancement, pas obligatoire)
**Citation:**
> "The narrative that peaks follow constitutional flashpoints (Meech/Charlottetown/Referendums; Sponsorship) is plausible, but the evidence is descriptive. Consider a formal event-study or difference-in-differences design comparing adjacent years/cohorts, with placebo checks and cohort-specific time trends, to strengthen the period-shock interpretation. Right now, vertical dashed lines in Figure 1 are illustrative rather than inferential."

**Action requise:**
- **Event-study design:**
  - Pour chaque événement majeur (1980, 1990, 1995, 2004-05):
    - Définir fenêtre pré-événement (e.g., -2 ans)
    - Définir fenêtre post-événement (e.g., +2 ans)
    - Estimer coefficients pour chaque année relative à l'événement
    - Tester: augmentation significative après événement?
  - Pooler tous les événements avec effets fixes événement

- **Difference-in-Differences:**
  - Comparer cohortes exposées vs non-exposées
  - E.g., Cohortes qui avaient 18-25 ans en 1995 vs cohortes plus âgées/jeunes
  - Tester parallel trends assumption

- **Placebo checks:**
  - Tester événements non-constitutionnels (élections fédérales normales?)
  - Pas d'effet attendu → valide l'approche

- **Note:** Reviewer dit "consider" → pas strictement obligatoire
  - Mais renforcerait beaucoup l'argument
  - Alternative: reconnaître limitation dans Discussion

**Fichiers concernés:**
- Créer: `code/event_study.R`
- Annexe méthodologique
- Ou: Discussion (limitation si pas fait)

---

## Issue #23: Clarifier terminologie secession/sovereignty/independence
**Reviewer:** Reviewer 2
**Priorité:** BASSE
**Citation:**
> "Given the importance of semantics in this discourse, I would encourage the authors to think carefully about the use of the terms 'secession,' 'sovereignty,' and 'independence', which at times appear to be used interchangeably. Perhaps I am incorrect here at their usage is purposive and strategic, but the reader should be given more context here to avoid the sense that these terms float. Similarly, aren't 'attitude strength' and 'attitude intensity' the same thing?"

**Action requise:**
- **Secession / Sovereignty / Independence:**
  - Définir chaque terme en Introduction
  - Expliquer nuances:
    - Souveraineté: peut inclure partenariat (1995)
    - Indépendance: séparation complète
    - Sécession: terme légal/politique
  - Si utilisés interchangeably: justifier pourquoi c'est approprié
  - Ou: choisir UN terme principal et s'y tenir
  - Cohérence dans tout l'article

- **Attitude strength / intensity:**
  - Ce sont synonymes?
  - Choisir UN terme et utiliser partout
  - Vérifier: figures, tableaux, texte

- **Find & Replace** dans tout le document pour cohérence

**Fichiers concernés:**
- Texte de l'article (partout)
- Possiblement: renommer variables/figures si nécessaire

---

## Issue #24: Considérer changement de titre
**Reviewer:** Reviewer 2
**Priorité:** TRÈS BASSE (suggestion stylistique)
**Citation:**
> "A small suggestion for a tweak to the title: 'Zooming in on Zoomers'."

**Action requise:**
- Discuter entre co-auteurs
- Titre actuel: "Zooming on Zoomers"
- Titre suggéré: "Zooming **in** on Zoomers"
- Plus idiomatique en anglais?
- Décision finale: garder ou changer
- C'est FAIT

**Fichiers concernés:**
- Titre de l'article
- `article_spsa2024_zooming_on_zoomers.bib` (possiblement)

---

## Résumé par priorité

### CRITIQUE (à faire absolument):
- Issue #1: Modèles et choix en annexe
- Issue #2: Documentation toutes variables
- Issue #4: Interaction Year × Language Figure 1
- Issue #10: Tableaux régression modèles annuels
- Issue #11: Résultats sans contrôles

### HAUTE (très important):
- Issue #3: Clarifier Figure 1 (CIs, lignes grises)
- Issue #8: Clarifier transformation 0-to-1
- Issue #13: Refocaliser Discussion
- Issue #14: Renforcer argument théorique
- Issue #15: Modèles APC
- Issue #17: CIs 95% sur Figure 1
- Issue #19: Poids, FE, SEs clustérisées
- Issue #21: Éducation + ideology/party ID

### MOYENNE:
- Issue #5: Justifier contrôles
- Issue #7: Détailler design enquêtes
- Issue #9: Détailler nettoyage
- Issue #12: Nuancer événements-soutien
- Issue #16: Incohérence définition Gen Z
- Issue #18: Incohérence années CES
- Issue #22: Event-study (optionnel)

### BASSE:
- Issue #6: Détailler éthique
- Issue #20: Pré- vs post-stratification
- Issue #23: Terminologie
- Issue #24: Titre

---

## Prochaines étapes

1. **Triage:** Décider quelles issues sont faisables avant le 8 février 2026
2. **Planification:** Assigner issues aux co-auteurs
3. **Tracking:** Utiliser ce document ou GitHub Issues pour suivre progrès
4. **Author Response Letter:** Pour chaque issue, documenter ce qui a été fait (ou pas fait, et pourquoi)
