# LibeRator 0.3.4

- Represents multi-endpoint utility weights as explicit named lists, preserving
  their JSON-object semantics without relying on jsonlite's deprecated named
  vector behaviour.
- Normalizes named atomic values at the final React payload boundary and adds
  regression coverage for warning-free future jsonlite serialization.

# LibeRator 0.3.3

- Adds a native-unit endpoint-outcome table to every selected-regimen future
  prediction. Each component now shows its posterior median and interval,
  target definition, attainment probability, clinical role, and hard
  constraint threshold/status.
- Future-prediction artifacts now retain these numerical component summaries,
  derived from the same posterior draws as target attainment. Saved older
  artifacts are upgraded for display from their retained per-draw evaluations
  without re-running simulation.

# LibeRator 0.3.2

- Adds `lator_example_dual_endpoint()`, a complete synthetic efficacy-safety
  teaching case with a primary exposure target, trough-safety chance
  constraint, explicit 2:1 utility weights, and a useful candidate-regimen
  grid.
- `lator_gui(teaching_example = TRUE)` now loads the two component endpoints
  and their already-selected combined objective, so joint attainment,
  component attainment, hard constraints, expected utility, and Pareto status
  can be explored directly in the graphical workflow.

# LibeRator 0.3.1

- Distinguishes prediction uncertainty from the therapeutic range in the
  individual and future prediction charts: the central prediction is solid,
  lower and upper pointwise prediction limits are dashed, and only the
  therapeutic range is shaded. Explanatory text states that these limits are
  prediction intervals rather than confidence intervals for the mean.
- Refines the medication-add control to a 15-pixel **diameter** and draws its
  plus sign with centred CSS geometry, avoiding font-dependent alignment.
- Adds clinician-facing evidence amendment and entered-in-error workflows.
  Corrections append a replacement or typed tombstone linked to the immutable
  original; reason, actor, correction root, original-event hash, and the
  encrypted workspace audit entry retain the complete provenance chain.

# LibeRator 0.3.0

- Adds first-class, versioned multi-endpoint clinical objectives. Each
  component records its role, explicit relative weight, and optional posterior
  target-attainment constraint; exactly one component is designated primary.
- Evaluates all component endpoints on the same posterior draws, preserving
  dependence when calculating joint attainment rather than multiplying
  marginal probabilities. Feasible candidates are ranked by an explicit
  normalized expected-utility definition and annotated with Pareto status.
- Adds a graphical **Combine endpoints** workflow with immutable revision
  history, component-level results, hard-constraint status, joint attainment,
  expected utility, and benefit-risk summaries in regimen and forecast views.
- Extends model qualification so a model selected for a multi-endpoint
  objective must cover every component endpoint.

# LibeRator 0.2.15

- Labels future concentration bands as pointwise 90% posterior prediction
  intervals based on individual ETA uncertainty. The GUI now explicitly says
  that these are not confidence intervals for the mean and exclude residual
  measurement variability.
- Refines the sidebar medication-add control to 17 by 17 pixels while retaining
  a 17-pixel plus symbol.
- Requires LibeRary 0.7.10 so persistent catalogues activate newer packaged
  model revisions, including the corrected He et al. lamotrigine v1.0.1 model,
  without losing the superseded catalogue history.

# LibeRator 0.2.14

- Persists the selected population model in the encrypted patient-medication
  profile and restores it after restarting the GUI. Research or review
  qualification no longer causes a valid selection to be forgotten; a changed
  model definition still requires deliberate re-selection.
- Aligns medication, endpoint, and population-model selector typography,
  corrects the compact medication-add control, reorders the section heading,
  and adds the package version to the main header.
- Switches directly to Individualisation, Regimens, or Future prediction when
  the corresponding estimate, comparison, or prediction action is started.
- Marks PK/PD parameters that incorporate an estimated ETA with an asterisk and
  explains the distinction from population/covariate-derived values.
- Records input-specific assessment fingerprints and highlights medication,
  endpoint, model, TDM, or patient-context inputs changed since the most recent
  individualisation.

# LibeRator 0.2.13

- Adds a preflight identifiability check for time-changing individualisation.
  It now requires measured TDM evidence in at least two declared patient states
  and gives an actionable GUI message before a background worker is launched.
- Adds an assessment options dialog with automatic current-monitoring-episode,
  latest-only, last-N, all-history, and custom-since display policies. These
  policies control the observations shown over the post-dose curve without
  silently removing older eligible evidence from the posterior fit.
- Cycle-aligns selected TDM measurements by their post-dose phase. Automatic
  episode detection responds to regimen changes, patient-state boundaries, and
  monitoring gaps longer than six weeks.
- Keeps the individualisation y-axis padded around the complete similar-patient
  95% prediction interval, therapeutic range, fitted curve, and selected TDM
  observations.
- Shows dedicated, cancellable in-tab progress states while candidate regimens
  and selected-regimen future predictions are being calculated.

# LibeRator 0.2.12

- Adds a regression-locked AEDapt parity scenario for the Rivas lamotrigine
  model (90 kg, 100 mg every 12 hours at steady state, 12 mg/L at 12 hours).
  The fitted individual curve is 9.62--10.19 mg/L with mean 9.94 mg/L and
  ETA1 -1.1010.
- Adds an explicitly labelled 95% **similar-patient prediction interval**
  calculated from OMEGA/ETA variability only. It is not described as a
  confidence interval and does not include residual measurement variability.
- Shows the therapeutic range, population prediction band, population median,
  numerical y-axis limits, individual prediction and observation together in
  the individualisation plot.
- Makes source-loaded GUIs run background jobs against the same source tree.
  Installed/source version mismatches now stop with an actionable restart or
  reinstall message instead of silently returning an older assessment shape.
- Prevents legacy assessments containing event-time predictions only from
  being drawn as a complete PK curve, and asks the user to rerun the
  individual assessment to calculate the dense profile and population interval.

# LibeRator 0.2.11

- Separates regimen-transition forecasts from an independently calculated
  periodic steady-state dosing interval. Chronic-endpoint ranking now uses the
  candidate regimen's steady-state exposure and reports posterior mean Css,
  trough, peak, whole-cycle target coverage, and whether the transition
  horizon has approached steady state.
- Recalculates candidate-dependent `DAILY_DOSE` and `DOSE_MG_KG_DAY`
  covariates without overwriting dose-interaction covariates that describe a
  different medication.
- Labels structurally mean-only `$PRED` models explicitly instead of drawing a
  flat curve that could be mistaken for resolved peak-to-trough kinetics.
- Adds numerical mean, trough, peak, and fluctuation summaries to
  time-resolved individual profiles. A bounded audit confirmed finite,
  converged individual fits for all 19 AEDapt-derived catalogue models and
  documented the shallow but non-flat Rivas lamotrigine profile.

# LibeRator 0.2.10

- Fixes encrypted-workspace model hydration so background individualisation
  receives the registered LibeRation `nm_model`, rather than its registration
  envelope.
- Dose evidence can now explicitly mark an established regimen as steady state
  when a dosing interval is supplied. Non-steady-state interval records remain
  single administrations and generate an interpretation warning rather than
  silently assuming prior doses.
- Generates a fitted 15-minute post-dose PK prediction grid from the most
  recent dose through one dosing interval, while replaying the complete patient
  history and using the estimated individual ETAs. Recorded interval metadata
  takes precedence over recent dose spacing; an explicit warning accompanies
  the 24-hour fallback when neither is available.
- Adds a selected-model information dialog with structural model, THETA,
  OMEGA, SIGMA, covariate, derived-quantity, provenance, population and route
  summaries. Limitations are generated deterministically from catalogue and
  qualification status, translation flags, recorded validation, study scope
  and required covariates.
- Separates intermediate quantities such as `IND` and `NIND` from
  individualised PK/PD parameters in the Individualisation tab.
- Adds an “Add/select model” workflow scoped to the active medication.
  Catalogue choices are exact medication matches from LibeRary; review-stage
  entries require explicit Research acknowledgement.
- Adds manual model creation through LibeRation's shared ADVAN 1--14 and
  advanced structural/outcome templates, with encrypted registration and no
  duplicated model-template implementation.
- Refines the medication add control sizing for better left-panel alignment.
- Reorders the patient workflow so medication, therapeutic endpoint, and
  population model are established before patient evidence is entered.
- Adds a live assessment-readiness checklist with exact missing prerequisites;
  posterior actions are now labelled as stable patient parameters or
  parameters changing over time, with concise guidance on when each is
  appropriate.
- Makes multi-regimen selection update immediately in the browser while the
  encrypted server state is persisted, preventing stale “0 predictions”
  controls.
- Makes the endpoint library searchable, removes implementation-level R
  constructor names from the clinical-research interface, and explains
  endpoint metrics and burden-adjusted ranking scores.
- Detects hosted deployments where no governed LibeRary catalogue is mounted
  and disables automatic matching with an actionable explanation instead of
  surfacing a package-integration error.

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
