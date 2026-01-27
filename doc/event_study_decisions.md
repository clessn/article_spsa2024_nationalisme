# Event-Study Analysis: Design Decisions

## Context

Issue #22 (Reviewer 2) suggests implementing a formal event-study or difference-in-differences design for constitutional flashpoints to strengthen the period-shock interpretation. The reviewer notes this is a "consider" suggestion, not strictly mandatory.

## Reviewer's Full Request

1. Event-study design for each major event (1980, 1990, 1995, 2004-05)
2. Difference-in-Differences comparing exposed vs non-exposed cohorts
3. Placebo checks (non-constitutional events)
4. Parallel trends tests

## How Event-Study Covers All Requests

A well-specified event-study model covers everything the reviewer asked for:

| Reviewer request | How it's covered |
|------------------|------------------|
| Event-study design | `iss_souv2 ~ event_time_f * generation` for each event |
| Parallel trends | Pre-event coefficients should be ≈ 0 (not significantly different) |
| DiD by cohort | The `post × generation` interaction IS a DiD specification |
| Placebo checks | Run same model on non-constitutional periods |

## Data Availability

Sample sizes by year and generation (n with non-missing `iss_souv2`):

```
         preboomer boomer    x    y    z
1974       285    193    2    0    0
1979       266    264    0    2    0
1984       289    264   64   14    0
1988       243    349  139    0    0
1993       271    386  275    0    0
1997       202    297  369  117    0
2000       335    410  327   94    0
2004       271    330  282  125    0
2006       270    332  282  123    0
2008       176    247  250  177    0
2011       268    390  325  205   16
2015       394    978  902  774  305
2019       530   2657 2257 2112  823
2021       430   2182 2009 1639 1072
2022       558   2577 2584 2040 1575
2023        89    706  733  879  536
```

## Final Plan: 4 Treatment Events + 2 Placebos

### Treatment Events

| Event | Year | Pre-event data | Post-event data | Generations |
|-------|------|----------------|-----------------|-------------|
| **Referendum 1980** | 1980 | 1974, 1979 | 1984, 1988 | preboomer, boomer |
| **Meech Lake** | 1990 | 1988 | 1993, 1997 | preboomer, boomer, x |
| **Referendum 1995** | 1995 | 1988, 1993 | 1997, 2000, 2004 | preboomer, boomer, x |
| **Sponsorship Scandal** | 2005 | 2004 | 2006, 2008, 2011 | preboomer, boomer, x, y |

### Placebo Events (Non-Constitutional Periods)

| Placebo | Year | Pre-event data | Post-event data | Generations |
|---------|------|----------------|-----------------|-------------|
| **Placebo 2017** | 2017 | 2015 | 2019 | all 5 |
| **Placebo 2020** | 2020 | 2019 | 2021, 2022, 2023 | all 5 |

**Rationale for placebos:**
- 2017: Quiet constitutional period, excellent data before/after
- 2020: COVID year (not constitutional), massive sample sizes

## Theoretical Story

This design allows us to show **which generations were "activated" by which events**:
- **1980**: Boomers as young adults, first-time voters on sovereignty
- **1990**: Gen X enters the political scene
- **1995**: Three generations with comparable exposure
- **2005**: Gen Y arrives, scandal affects trust in federalism

## Model Specification

For each event, we estimate TWO models:

### 1. Event-Study Model (for parallel trends + figure)

```r
# Filter data around event
data_event <- Data %>%
  filter(year %in% event_years) %>%
  filter(generation %in% available_generations) %>%
  mutate(
    event_time = year - event_year,
    event_time_f = factor(event_time),
    post_event = as.numeric(year > event_year)
  )

# Set reference to last pre-event period
data_event$event_time_f <- relevel(data_event$event_time_f, ref = "[last_pre_year]")

# Event-study model
model_es <- lm(iss_souv2 ~ event_time_f * generation, data = data_event)
```

**Purpose:**
- Visualize trajectory around event (classic event-study plot)
- Test parallel trends: pre-event coefficients should be ≈ 0
- More parameters, less power per coefficient

### 2. Binary DiD Model (for main results table)

```r
# Binary DiD model
model_did <- lm(iss_souv2 ~ post_event * generation, data = data_event)
```

**Purpose:**
- Clean, powerful estimate of post-event effect
- Direct DiD interpretation: `post_event:generation` = differential effect by cohort
- Fewer parameters, more statistical power
- This is what goes in the main results table

### Interpretation

- **Event-study model**: Pre-event coefficients ≈ 0 validates parallel trends assumption
- **DiD model**: Main effect of `post_event` = average treatment effect; interaction `post_event:generation` = heterogeneous effects by cohort

## Output

**Tables (from binary DiD model):**
- Main results: `post_event` coefficient for all 6 events (4 treatment + 2 placebo)
- Heterogeneous effects: `post_event × generation` interactions

**Figures (from event-study model):**
- Event-study plot for 1995 referendum (flagship event, shows trajectory + parallel trends)
- Optional: summary figure comparing treatment vs placebo effects

**Validation:**
- Parallel trends: pre-event coefficients from event-study model ≈ 0
- Placebo check: `post_event` coefficient for 2017/2020 should be non-significant

## Justification for Paper

> "We implement event-study analyses for four major constitutional events (1980 and 1995 referendums, 1990 Meech Lake Accord failure, 2004-05 Sponsorship Scandal) and two placebo periods (2017, 2020). The event-study specification with generation interactions allows us to test for parallel pre-trends and estimate differential effects across cohorts, effectively combining event-study and difference-in-differences logic. Non-significant effects for placebo events validate that observed treatment effects are not spurious time trends."

## Implementation

Main script: `code/event_study_simple.R`

## References

- Issue #22: https://github.com/clessn/article_spsa2024_nationalisme/issues/22
