# LibeRator

**Adaptive Therapeutic Optimisation and Recommendation**, designed toward
eventual clinical use and currently released for research and teaching while
clinical validation is still in progress.

LibeRator is the longitudinal model-informed precision-dosing workbench in the
LibeR ecosystem. It combines LibeRation's C++/automatic-differentiation PK/PD
engine, models curated through LibeRary, and LibeRties local or remote queues.
There is one product path: the **Research** label describes the current
validation status, not a separate reduced edition. The current release is not a
medical device and does not issue autonomous treatment instructions.

LibeRator is distributed as part of the LibeR 0.9 research beta. Install the
exact [ecosystem compatibility set](../docs/INSTALL.md) and review
`LibeRation::liber_support_matrix("LibeRator")` before using it in a study.

## What is implemented

- Encrypted, authenticated pseudonymous patient workspaces with a tamper-evident
  audit chain, optimistic concurrency, immutable evidence events, and explicit
  clinician corrections. **Amend** appends a replacement—or an
  entered-in-error marker—while retaining the original, reason, actor, linked
  lineage, and integrity hash.
- Effective-dated covariates with observed, LOCF, interpolated, nearest, stale, missing, and explicit-fallback states. Missing values are never silently replaced with a population value.
- Static empirical-Bayes individualisation and genuinely time-varying ETA states under a correlated random-walk prior, using LibeRation's persistent C++/CppAD individual objective.
- Ten versioned therapeutic endpoint families covering antiseizure therapeutic
  ranges, beta-lactam fT>MIC, ATG pre-event targets, vancomycin AUC/MIC,
  aminoglycoside exposure and safety, tacrolimus trough, mycophenolate AUC,
  busulfan cumulative exposure, methotrexate timed concentrations, and warfarin
  time in INR range.
- Versioned multi-endpoint clinical objectives that combine efficacy, safety,
  and secondary endpoints. All components are evaluated on the same posterior
  draws; the engine reports joint attainment, component attainment, normalized
  expected utility, hard posterior constraints, and the Pareto-efficient
  candidate set.
- Fail-closed patient-to-model selection from LibeRary. Candidate models must
  carry a current, scoped clinical-use qualification and pass drug, indication,
  endpoint, route, formulation, regimen, assay, covariate, population-range, and
  review-date gates. Ambiguous or unsuitable cases are surfaced for review
  instead of silently choosing a model.
- Batched posterior-uncertainty simulations that rank feasible dose/interval grids by target attainment and endpoint distance.
- Shared native LibeRation NCA summaries for AUC, average steady-state
  concentration, peak, trough, and fluctuation across individual and candidate
  regimen trajectories.
- An explicit user selection step that turns one or more simulated candidates
  into auditable, vertically stacked future-prediction artifacts with posterior
  medians, 90% intervals, target ranges, linked endpoint evaluations, and a
  native-unit table comparing each endpoint's numerical posterior outcome with
  its target and any hard probability constraint.
- Multi-medication patient profiles: the medication selector keeps model,
  posterior, regimen, and endpoint context separate for each therapy. A
  previously chosen endpoint is restored on switching; a medicine without an
  endpoint opens the endpoint library.
- Editable endpoint recommendations for known medicines. Reference ranges and
  targets are provenance-labelled starting points rather than universal
  instructions; exact numeric values are never inferred from a drug class alone.
- Non-destructive endpoint revision: **Modify endpoint** prefills the active
  definition and suggests a new version. Saving updates only the active
  patient–drug profile and retains the previous endpoint-selection history.
- System-wide endpoint templates with patient-medication assignments: the same
  reviewed default can be reused across patients without a version collision,
  while each assignment stores its own immutable endpoint snapshot.
- Medication-aware evidence entry: doses can reference only treatments already
  added to the patient; TDM can reference those drugs or their explicitly
  registered metabolites/analytes. The nested `+` control adds a medication
  without losing an unfinished evidence form.
- Individualisation views expose ETAs with posterior uncertainty, derived
  individual structural parameters, and the fitted individual PK profile with
  observed concentrations. Solid curves show central predictions; dashed
  limits show pointwise prediction intervals, while shading is reserved for
  the therapeutic range.
- Explicit `COMED_<DRUG>`, `TRT_<DRUG>`, and `DDI_<DRUG>` model covariates are
  populated from the active patient treatment profile. The interaction
  magnitude and mechanism must remain explicitly defined in the selected model.
- Confirmed patient deletion requires typing `YES`, deletes the encrypted
  clinical-research record, and retains a minimal tamper-evident deletion audit
  event.
- Validated LibeRary model import, encrypted local model registration, and typed LibeRties individualisation/regimen jobs that never transmit a workspace key.
- A guided React/Shiny workbench with a professional teal light/dark theme,
  explicit assessment-readiness checks, patient timeline, evidence-entry
  popups, posterior-state display, searchable endpoint library, endpoint
  provenance, selectable regimen comparison, and future-prediction chart.

Persistent patient workspaces default to
`Documents/LibeR-data/liberator-workspace` on Windows and
`~/LibeR-data/liberator-workspace` elsewhere. Set `LIBERATOR_HOME` to use a
managed location.

## Qualified model selection

Clinical-use qualification is deliberately scoped and issuer-specific. It does
not turn a model into a universally approved artefact: the record states the
drug, population, indication, assay, endpoint, routes, regimens, required
covariates, reviewer, evidence, and review date for which the issuer accepts the
model.

```r
selection <- lator_model_select_from_liberary(
  patient,
  endpoint,
  root = "~/LibeR-data/library",
  cutoff = 24 * 365
)

selection$status       # selected, multiple_suitable_models, or no_suitable_model
selection$blockers
selection$candidates
```

The GUI exposes the same process through **Find best model** and records the
patient context, candidate table, score components, qualification identifier,
model hash, and decision criteria as an immutable selection artefact.
On a hosted deployment this action is enabled only when a governed LibeRary
catalogue has explicitly been mounted; otherwise the GUI explains the missing
integration and continues to support supplied or locally registered models.

## Multiple medications and endpoint preferences

Medications can be added explicitly to the treatment profile before dose
evidence is available and are also discovered from immutable dose events. In
the workbench, the `+` control above the medication dropdown can save the
medicine alone or continue directly to endpoint selection. The workbench keeps
a versioned endpoint preference for each drug:

```r
patient <- lator_patient_medication_add(
  patient, "phenytoin", therapeutic_class = "antiseizure",
  monitoring_analytes = "free phenytoin"
)
patient <- lator_patient_medication_add(
  patient, "warfarin", therapeutic_class = "vitamin-k-antagonist"
)

lator_patient_medications(patient)
patient <- lator_patient_endpoint_set(
  patient, "phenytoin", "aed-phenytoin@1.0.0", phenytoin_endpoint
)
lator_patient_endpoint_get(patient, "phenytoin")
```

Known drug presets preselect a typical endpoint family and editable,
source-labelled values. For an unlisted drug, an optional therapeutic class can
suggest the family, but the target values remain blank for explicit review.
An existing selection can be revised with **Modify endpoint**. LibeRator creates
a new version and redirects only the current patient–drug profile to it; other
patient and medication profiles remain unchanged.

Drug-drug interactions are model-defined, not guessed. For a simple
categorical co-medication effect, declare a covariate such as
`COMED_WARFARIN` in the model and use it in `$PK`, `$PRED`, or `$DES`;
LibeRator supplies 1 when that treatment is active and 0 otherwise.
Effective-dated covariate evidence with the same name takes precedence, allowing
the interaction state to change along the patient timeline.
Time-varying inhibition, induction, metabolite, or joint-response mechanisms
should be represented explicitly as covariates, states, or linked model
components and validated for the intended use.

## Multiple-endpoint clinical objectives

Endpoints for one medication can be composed into an immutable objective set.
One component must be primary. Relative weights are explicit and versioned;
they are never inferred silently. A component can additionally be a hard
chance constraint, such as requiring at least 90% posterior probability of
meeting a safety endpoint.

```r
efficacy <- lator_endpoint(
  id = "drug-a-efficacy", name = "Drug A efficacy range", drug = "Drug A",
  kind = "therapeutic_range", metric = "last_interval_average", unit = "mg/L",
  rules = list(lower = 4, upper = 8, target = 6),
  source = "Reviewed protocol", status = "reviewed"
)
safety <- lator_endpoint(
  id = "drug-a-safety", name = "Drug A safety range", drug = "Drug A",
  kind = "trough_range", metric = "trough", unit = "mg/L",
  rules = list(lower = 1, upper = 10, target = 5.5),
  source = "Reviewed protocol", status = "reviewed"
)

objective <- lator_endpoint_set(
  id = "drug-a-benefit-risk", name = "Drug A benefit-risk objective",
  drug = "Drug A", source = "Institutional protocol",
  status = "reviewed",
  components = list(
    lator_endpoint_component(efficacy, role = "primary", weight = 2),
    lator_endpoint_component(
      safety, role = "safety", weight = 1,
      hard_constraint = TRUE, minimum_attainment = 0.9
    )
  )
)
```

For each candidate and posterior draw, LibeRator converts each endpoint's
normalized loss to utility as `exp(-loss)`, then calculates the weighted
arithmetic mean. Candidates that fail a hard posterior constraint are not
decision-eligible. Remaining candidates are ranked by expected utility and
marked when Pareto-efficient across component utilities. Joint attainment is
calculated from simultaneous success on shared posterior draws, preserving
correlation between outcomes.

In the workbench, create the individual endpoints first and select
**Combine endpoints**. The resulting objective is saved for that
patient-medication profile and can later be revised as a new immutable version.

## Synthetic teaching example

The packaged dual-endpoint example combines:

- a **primary efficacy endpoint**: average concentration over the final dosing
  interval should be within the synthetic 2–8 mg/L range;
- a **safety endpoint**: the periodic steady-state trough should be within the
  synthetic 0.5–5 mg/L range;
- a **hard safety rule**: at least 75% of posterior draws must meet that
  trough-safety endpoint before a regimen is decision-eligible; and
- explicit **2:1 efficacy-to-safety utility weights** for ranking eligible
  regimens. These normalize to two-thirds and one-third.

Every value is synthetic and deliberately unsuitable as a clinical target.
Start a fresh graphical demonstration with:

```r
library(LibeRator)

lator_gui(
  path = tempfile("liberator-dual-endpoint-"),
  passphrase = "a long teaching-only passphrase",
  teaching_example = TRUE
)
```

The component endpoints and combined objective are already registered and the
combined objective is selected. Run **Estimate stable patient parameters**,
then **Explore candidate regimen**. The demonstration-specific candidate grid
is prefilled automatically.

The same workflow can be scripted:

```r
example <- lator_example_dual_endpoint()
assessment <- lator_assess(
  example$patient, example$model, example$endpoint,
  mode = "static",
  covariate_policies = list(WT = list(method = "locf", max_age = 24 * 365))
)

comparison <- lator_regimen_optimise(
  assessment, example$patient, example$candidates,
  endpoint = example$objective, nsim = 200, seed = 123
)
comparison$summary[, c(
  "candidate_id", "amount", "interval",
  "primary_attainment_probability", "joint_attainment_probability",
  "expected_utility", "hard_constraints_pass", "pareto_optimal", "rank"
)]

# Selection is deliberate: ranking does not automatically prescribe a dose.
selected <- comparison$summary$candidate_id[1:2]
forecasts <- lapply(
  selected,
  function(id) lator_regimen_predict(comparison, candidate_id = id)
)
forecasts[[1L]]$forecast
```

## Design boundary

The current release is suitable for methodological research, teaching,
simulation, and prototype evaluation. The same product is being designed toward
clinical use, but clinical deployment additionally requires a validated
model/endpoint set, identity and access management, institutional key
management, electronic-record integration, independent calculation
verification, human approval workflow, change control, monitoring, disaster
recovery, cybersecurity testing, quality management, usability engineering,
and jurisdiction-specific medical-device assessment. See
[SECURITY.md](SECURITY.md) for the concrete boundary and planned controls.

## AI-assisted development

GPT-5.6 was used as an AI engineering collaborator to help implement and review
the longitudinal dosing workflows, endpoint framework, security controls, GUI, tests, and documentation.
Scientific and clinical direction, validation requirements, and release decisions remain the responsibility of the project owner.

## Licence

MIT.
