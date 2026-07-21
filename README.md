# LibeRator

**Adaptive Therapeutic Optimisation and Recommendation** for research and teaching.

LibeRator is the longitudinal model-informed precision-dosing workbench in the LibeR ecosystem. It combines LibeRation's C++/automatic-differentiation PK/PD engine, models curated through LibeRary, and LibeRties local or remote queues. It is deliberately designed so that research workflows can later be hardened for clinical validation, but the current research package is not a medical device and does not issue autonomous treatment instructions.

## What is implemented

- Encrypted, authenticated pseudonymous patient workspaces with a tamper-evident audit chain, optimistic concurrency, immutable evidence events, and explicit corrections.
- Effective-dated covariates with observed, LOCF, interpolated, nearest, stale, missing, and explicit-fallback states. Missing values are never silently replaced with a population value.
- Static empirical-Bayes individualisation and genuinely time-varying ETA states under a correlated random-walk prior, using LibeRation's persistent C++/CppAD individual objective.
- Versioned endpoint objects for AED therapeutic ranges, ATG pre-event targets, beta-lactam fraction of time above MIC, AUC, trough, and extensible composite objectives.
- Batched posterior-uncertainty simulations that rank feasible dose/interval grids by target attainment and endpoint distance.
- An explicit user selection step that turns one simulated candidate into an auditable future-prediction artifact with a posterior median, 90% interval, target range, and linked endpoint evaluation.
- Validated LibeRary model import, encrypted local model registration, and typed LibeRties individualisation/regimen jobs that never transmit a workspace key.
- A React/Shiny workbench with a professional teal light/dark theme, patient timeline, evidence-entry popups, posterior-state display, endpoint provenance, selectable regimen comparison, and future-prediction chart.

## Synthetic teaching example

```r
library(LibeRator)

example <- lator_example_aed()
workspace <- lator_workspace(
  tempfile("liberator-teaching-"),
  passphrase = "a long teaching-only passphrase"
)
lator_patient_save(workspace, example$patient)

lator_gui(
  workspace = workspace,
  models = list(aed = example$model),
  endpoints = list(aed = example$endpoint)
)
```

The core workflow can also be scripted:

```r
assessment <- lator_assess(
  example$patient, example$model, example$endpoint,
  mode = "dynamic",
  covariate_policies = list(WT = list(method = "locf", max_age = 24 * 365))
)

candidates <- lator_regimen_candidates(
  amounts = c(100, 200, 300), intervals = c(12, 24), horizon = 7 * 24
)
comparison <- lator_regimen_optimise(
  assessment, example$patient, candidates, nsim = 200
)
comparison$summary

# Selection is deliberate: ranking does not automatically prescribe a dose.
forecast <- lator_regimen_predict(
  comparison, candidate_id = comparison$summary$candidate_id[[1L]]
)
forecast$forecast
```

## Design boundary

The current package is suitable for methodological research, teaching, simulation, and prototype evaluation. Clinical deployment additionally requires a validated model/endpoint set, identity and access management, institutional key management, electronic-record integration, independent calculation verification, human approval workflow, change control, monitoring, disaster recovery, cybersecurity testing, quality management, usability engineering, and jurisdiction-specific medical-device assessment. See [SECURITY.md](SECURITY.md) for the concrete boundary and planned controls.

## AI-assisted development

GPT-5.6 was used as an AI engineering collaborator to help implement and review
the longitudinal dosing workflows, endpoint framework, security controls, GUI, tests, and documentation.
Scientific and clinical direction, validation requirements, and release decisions remain the responsibility of the project owner.

## Licence

MIT.
