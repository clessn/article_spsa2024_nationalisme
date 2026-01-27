# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based academic research project titled "Zooming on Zoomers: A Comprehensive Analysis of Quebec's Shifting Independence Dynamics" submitted to SPSA 2024. The research investigates attitudes toward Quebec independence across different generations, with particular focus on Generation Z (born 1992-2003).

**Key Finding**: Among Gen Z, the Greater Montreal Area is less attached to federalism, and few differences exist between linguistic profiles regarding independence attitudes.

## Running the Analysis

The analysis pipeline should be executed in this order:

```r
# 1. Data preparation (if census data needs updating)
source("code/pot_growth_of_independence/generate_clean_census_data.R")

# 2. Historical analysis
source("code/figure1_evolution_past_article.R")

# 3. Current attitudes (2021-2023)
source("code/figure2_current_attitudes.R")
source("code/figure3_attitude_force.R")
source("code/figure4_id_qc_can.R")
source("code/figure5_attitude_force_idcan.R")

# 4. Predictive modeling
source("code/pot_growth_of_independence/model_and_predict.R")

# 5. Geographic analysis
source("code/pot_growth_of_independence/map.R")
source("code/pot_growth_of_independence/zoom_on_zoomers.R")
source("code/pot_growth_of_independence/zoom_on_zoomers_attitudes.R")
```

For the presentation:
```bash
quarto render slides_spsa.qmd
```

## Core Architecture

### Data Sources and Flow

**Primary Dataset**: `SharedFolder_spsa_article_nationalisme/data/merged_v1.rds`
- Canadian Election Studies (1974-2021): Historical survey data
- Synopsis Surveys (Jan-Sep 2022): Recent monthly Quebec polls
- Quorum Pilot (Aug 2023): Most recent data
- 2021 Census: Population demographics by electoral riding

**Key Variables in merged_v1.rds**:
- `iss_souv`: Binary independence support (dichotomous questions, 0/1)
- `iss_souv2`: Scaled independence support (Likert questions, 0-1 scale)
- `generation`: Categorical (preboomer/boomer/x/y/z)
- `ses_lang.1`: Language (french/english/other)
- `ses_geoloc.1`: Region (montreal/quebec/suburbs/region)
- `year`: Survey year
- Control variables: age, gender, origin, income, political interest

**Generation Definitions** (birth years):
- Pre-boomer: 1925-1946
- Boomer: 1947-1961
- X: 1962-1976
- Y (Millennials): 1977-1991
- Z (Zoomers): 1992-2003

### Analysis Pipeline Architecture

```
Historical Data (1974-2021) ──┐
Synopsis Surveys (2022)       ├──> merged_v1.rds
Quorum Pilot (2023)          ──┘
                                  │
                                  ├──> figure1: Time series by generation (11 yearly models)
                                  │
                                  ├──> figure2-5: Cross-sectional analysis (2021-2023)
                                  │
                                  └──> Census processing
                                          │
                                          ├──> Post-stratification tables
                                          │
                                          └──> Predictive model (gen × region × language)
                                                  │
                                                  ├──> Geographic maps by generation
                                                  └──> Riding-level Gen Z analysis
```

### Script Roles

**figure1_evolution_past_article.R**: Confirms historical generational trends (1974-2022) using 14 linear regression models. Creates time series plots showing evolution by generation. Sample: n=12,191.

**figure2_current_attitudes.R**: Examines language effect differences across generations using 2021-2023 data with generation × language interaction. Handles both 4-point and 5-point Likert scales. Sample: n=6,687.

**figure3_attitude_force.R**: Predicts probability of strong (vs. moderate) independence attitudes using logistic regression. Binary DV: strong (0 or 1 on scale) vs. weak (0.25-0.75).

**figure4_id_qc_can.R**: Analyzes proportion identifying as Québécois before Canadian by generation using 2022 Synopsis data with 95% CIs.

**figure5_attitude_force_idcan.R**: Examines how national identity affects attitude strength with generation × identity interaction.

**generate_clean_census_data.R**: Creates post-stratification tables from 2021 Census by electoral riding. Extracts demographics (generation, language, gender, origin, income) and creates synthetic weights. Outputs: clean_census.rds, poststrat.rds, poststrat_large.rds.

**model_and_predict.R**: Core predictive model using three-way interaction (generation × region × language). Generates weighted predictions for all census strata and saves model as modelsouv2023.rds.

**map.R**: Creates choropleth maps of independence attitudes by riding using sf package. Generates maps for all population, Gen Y+Z, Gen Y only, and Gen Z only. Includes water bodies for geographic context.

**zoom_on_zoomers.R**: Detailed Gen Z analysis by electoral riding with horizontal bar charts by region, showing weighted estimates with CIs. Color indicates Gen Z population weight.

**zoom_on_zoomers_attitudes.R**: Explores relationship between identity (Canadian, ethnic, language) and sovereignty attitudes in Gen Z using 2021 CES.

## Key R Packages

**Core**: dplyr, ggplot2, tidyverse, patchwork, marginaleffects
**Specialized**: readxl, haven, sf, stringdist, clessnize/clessnverse, cartoquebec, ggridges, sondr

## Methodological Notes

**Scale Transformations**: All sovereignty questions standardized to 0-1 continuum (0=federalist, 1=separatist). Dichotomous coded as 0/1. 4-point Likert scaled to 0, 0.33, 0.66, 1. 5-point Likert scaled to 0, 0.25, 0.5, 0.75, 1. Some analyses randomly assign neutral responses.

**Modeling Approach**:
- Linear regression for continuous outcomes (independence attitudes 0-1)
- Logistic regression for binary outcomes (attitude strength)
- Post-stratification using census for population-level inference
- Marginal effects for interpreting interactions

**Key Interactions Tested**:
- Generation × Language (differential language effects)
- Generation × Region × Language (geographic predictions)
- Generation × Identity (national identity effects)

## Critical Open Issues from Peer Review

The repository has 24 open GitHub issues representing critical peer review feedback. Priority issues include:

**CRITICAL** (#1, #2, #4, #10, #11):
- All models and coding choices must be shown in appendix
- Complete variable documentation needed for all DV and IVs
- Figure 1 needs Year × Language interaction OR separate language-specific trends
- Regression tables required for all 11 yearly models
- Results without controls must be presented in annex

**HIGH** (#8, #15, #17, #19, #21):
- Clarify 0-to-1 scale transformation and interpretive limitations
- Consider APC models (HAPC-CCREM) to disentangle cohort from period effects
- Add 95% confidence intervals to all predicted series in Figure 1
- Document weights, add survey-year fixed effects and mode controls, cluster standard errors
- Harmonize education control and test ideology/party ID robustness

**MEDIUM** (#16, #18):
- Resolve Gen Z definition inconsistency (1992-2003 vs "born after 1995") with robustness checks
- Resolve inconsistency in CES years used between Data section and Supplement

When working on figures or models, always check if there's a corresponding open issue that needs to be addressed.

## Output Structure

**Figures**: SharedFolder_spsa_article_nationalisme/figures/
- figure1_evolution.png
- figure2_language_effect_by_generation_[4point|5point].png
- figure3_attitude_strength_by_generation.png
- figure4_id_qc_can.png
- figure5_attitude_strength_idcan.png

**Graphs**: SharedFolder_spsa_article_nationalisme/graphs/
- models/potgrowth/aggregate/ - Aggregated predictions by generation/region
- models/evolution/ - Historical evolution plots
- maps/ - Choropleth maps by generation
- zoomer_by_riding.png - Gen Z by riding analysis

**Models**: modelsouv2023.rds - Saved regression model object

## Important Context

The SharedFolder_spsa_article_nationalisme/ directory is a symlink to a Dropbox shared folder containing all data and outputs. This allows collaboration while keeping large data files out of the git repository.

Census data processing creates post-stratification weights by: generation, language, gender, origin, income - calculated separately for each riding and aggregated to regional level.

The research challenges assumptions about Quebec independence being primarily a Boomer issue, showing Gen Z has nuanced attitudes that vary by geography and identity rather than simple rejection of sovereignty.
