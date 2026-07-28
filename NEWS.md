# LibeRator 0.2.9

- Requires LibeRary 0.7.9 for the public clinical-qualification API, separating
  a valid empty qualification catalogue from an outdated package namespace.

# LibeRator 0.2.8

- Adds deterministic patient-to-model matching against scoped LibeRary
  clinical-use qualifications, including hard applicability gates,
  transparent ranking, ambiguity/no-match outcomes, and immutable encrypted
  decision records.
- Adds first-class endpoint constructors and evaluators for vancomycin,
  aminoglycosides, tacrolimus, mycophenolic acid, busulfan, high-dose
  methotrexate, and warfarin alongside the existing antiseizure-medicine,
  beta-lactam, and ATG families.
- Adds Model suitability and Endpoint library controls to the workbench. The
  endpoint library now provides accessible template selection, family-specific
  configuration, encrypted patient-specific assignment, and immediate
  activation of the new endpoint version. Research is described explicitly as
  the current validation status of one product path designed toward eventual
  clinical use.
- Adds first-class multi-medication patient workflows. The active medication
  controls model selection, assessment history, regimen optimisation, and its
  persisted endpoint version; switching medications restores that selection or
  opens the endpoint library when no endpoint has been configured.
- Adds editable drug-specific reference presets for commonly monitored
  antiseizure medicines, warfarin, and vancomycin. Exact values are prefilled
  only for exact known drug aliases; therapeutic-class matches recommend a
  family without inventing numeric targets.
- Reports the exact loaded LibeRary version and installation path when the
  clinical-qualification API is unavailable, with a compatibility bridge for
  the early 0.7.8 namespace-generation defect.
- Adds non-destructive endpoint modification. The current endpoint is prefilled,
  an unused version is suggested, and saving records a new endpoint version in
  that patient–drug profile while retaining its complete selection history.
- Adds explicit treatment-profile medication entry independent of dose
  evidence. The workbench `+` control can save a medication alone or continue
  directly to its endpoint library with the drug, inferred class, endpoint
  family, and available editable numerical preset carried forward.
- Separates system-wide endpoint templates from patient-specific endpoint
  assignments. The same reviewed template and version can be selected for
  multiple patients, while each patient-medication profile retains its own
  immutable endpoint snapshot and revision history.
- Restricts dose entry to medications already recorded for the patient and TDM
  entry to those medications or explicitly registered metabolites/analytes. A
  nested `+` medication dialog can be opened without discarding the unfinished
  dose or TDM form.
- Adds confirmed patient deletion: the destructive control is visually red and
  requires typing `YES`, while a minimal tamper-evident deletion event remains
  in the encrypted audit chain.
- Expands Individualisation with ETA uncertainty, individualized structural
  parameters, and an individual PK profile overlaid with observations.
- Allows several candidate regimens to be selected and generates vertically
  stacked future-prediction panels for direct comparison.
- Maps declared `COMED_*`, `TRT_*`, and `DDI_*` model covariates from the
  patient's active treatment profile so coded interaction effects are applied
  consistently during individualisation.

# LibeRator 0.2.7

- Applies the ecosystem-wide non-fading busy-state behavior while retaining
  the existing asynchronous patient and assessment task channels.
- Increments the workbench asset version to invalidate cached GUI resources
  after upgrading.

# LibeRator 0.2.6

- Publishes LibeRator in the LibeR 0.9 research-beta compatibility set with
  explicit research/teaching evidence and a machine-readable statement that
  autonomous clinical recommendation remains unqualified.

# LibeRator 0.2.5

- Restores the established high-resolution LibeR dove and applies the shared
  58 px header/32 px message-bar geometry.
- Consolidates the teal clinical-research identity into semantic brand tokens,
  adds the LibeR dove and version to workspace unlock, and shares theme
  preference with the rest of the ecosystem.
- Adds focus-managed dialogs and responsive drawers so patient navigation and
  assessment controls remain reachable on narrow displays.

# LibeRator 0.2.4

- Adds deterministic analytic validation of longitudinal assimilation,
  regimen selection, and future prediction for a virtual patient.
- Adds a runnable teaching example and browser-level workbench startup coverage.

# LibeRator 0.2.3

- Aligns package contracts and compatibility metadata with the versioned
  LibeRation workspace/model schemas and LibeRties wire v2 release.
- Adds consolidated ecosystem diagnostics, CI, citation, and release
  provenance without changing the research-and-teaching clinical boundary.

# LibeRator 0.2.2

- Fixed empty-workspace GUI refreshes so every successive dose, covariate,
  concentration, and state event appears immediately after its encrypted save.
- Added an opt-in synthetic teaching seed and enabled it for the isolated
  shinyapps.io demonstration sessions.

# LibeRator 0.2.1

- Adds per-browser-session encrypted workspaces and a non-blocking Shiny app
  return path for hosted research and teaching demonstrations.

# LibeRator 0.1.1

- Fixed workbench startup with a pre-unlocked workspace by removing reactive-value reads from the non-reactive server initialization path.
- Replaced the temporary lettermark with the established LibeR dove favicon, recoloured in the LibeRator purple palette and reused in the workbench header.

# LibeRator 0.1.0

- Initial encrypted longitudinal patient workspace and audit chain.
- Explicit missing-covariate policies and immutable correction semantics.
- Static and time-varying Bayesian individualisation using LibeRation C++/CppAD objectives.
- AED, ATG, beta-lactam, AUC, and trough endpoint framework.
- Posterior-uncertainty regimen comparison and synthetic AED teaching case.
- LibeRary model import and typed LibeRties queue integration.
- Purple React/Shiny research workbench with light and dark themes.
