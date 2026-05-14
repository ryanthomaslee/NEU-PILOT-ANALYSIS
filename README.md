# NEU Framework Pilot Study: Burnout Domain Analysis

![R](https://img.shields.io/badge/R-Statistical%20Analysis-blue)
![Behavioural Science](https://img.shields.io/badge/Behavioural-Science-purple)
![Occupational Burnout](https://img.shields.io/badge/Occupational-Burnout-orange)
![Data Visualisation](https://img.shields.io/badge/Data-Visualisation-green)

Behavioural analytics pilot evaluating domain-level burnout intervention efficacy using the NEU Framework.

---

# Executive Summary

This repository contains the R-based data analysis for the initial pilot of the **NEU Framework**, a behavioural science intervention designed to quantify and mitigate occupational burnout.

Developed by **JAI Behavioural**, this pilot study evaluates the efficacy of the framework through analysis of pre- and post-intervention data collected from a cohort of educational professionals.

The analysis focuses on extracting actionable insights regarding how distinct burnout domains respond to targeted behavioural interventions over a two-week period.

---

# Data Visualisations

## Burnout Domain Mean Score Shifts

<p align="center">
<img width="987" height="745" alt="Screenshot 2026-05-14 at 10 12 12" src="https://github.com/user-attachments/assets/c44b910c-14fc-47e6-9101-b7c56fd8fac4" />



Aggregate domain-level changes comparing Day 0 and Day 14 burnout assessment scores across the Emotional, Motivational, Relational, and Cognitive domains.

---

## Individual TBP Score Changes

<p align="center">
<img width="990" height="738" alt="Screenshot 2026-05-14 at 10 12 37" src="https://github.com/user-attachments/assets/8c023b37-48b7-45bc-8c2f-0451cdbaad0e" />


Participant-level burnout trajectory analysis visualising individual total burnout profile (TBP) score deltas across the intervention period.

---

# Methodology & Scope

The dataset evaluates **NEU Pilot 1 – All Completers** by tracking burnout metric shifts from **Day 0 to Day 14**.

The assessment framework utilises a **Mean Score (/9)** methodology to quantify symptoms across four primary burnout domains:

- Emotional
- Motivation
- Relational
- Cognitive

Additionally, the analysis tracks the **TBP Total Score (/36)** to measure holistic burnout profile changes at the individual participant level.

---

# Key Findings

## 1. Domain-Level Impact

The intervention yielded positive aggregate results, evidenced by visible reductions in mean burnout scores across every measured domain.

Scores decreased consistently from Day 0 to Day 14 across:
- Emotional
- Motivation
- Relational
- Cognitive

These findings suggest targeted behavioural interventions may produce measurable short-term reductions in occupational burnout symptom severity.

---

## 2. Individual Efficacy (TBP Score)

Analysis of total burnout profile scores demonstrated significant individual-level improvements across the majority of the participant cohort.

Most participants exhibited measurable reductions in overall burnout severity between Day 0 and Day 14, while a smaller subset demonstrated slight increases, providing important variance data for future refinement of intervention targeting and behavioural profiling mechanisms.

---

# Strategic Implications

This pilot supports the NEU Framework as a viable data-driven approach to organisational burnout prevention.

By segmenting burnout into distinct cognitive, emotional, motivational, and relational domains — rather than treating burnout as a monolithic condition — interventions can be deployed with greater precision and interpretability.

The R analysis pipeline established in this repository provides the foundation for:
- scalable behavioural analytics
- institutional burnout auditing
- longitudinal wellbeing tracking
- intervention efficacy modelling

---

# Repository Structure

### `NEU_Pilot_1.R`
Primary R script containing:
- data cleaning
- statistical analysis
- visualisation generation using ggplot2

### `Domainmeanv1.pdf`
Bar chart visualisation of aggregate domain score shifts.

### `TBPscoresv1.pdf`
Dumbbell plot visualisation tracking individual participant burnout score changes.

### `.gitignore`
Configured to exclude local R environment and temporary files.
