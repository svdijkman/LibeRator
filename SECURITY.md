# Security and clinical-hardening boundary

## Current research safeguards

LibeRator 0.1.0 stores pseudonymous patient timelines, model registrations, endpoints, catalogues, and the audit chain in authenticated encryption envelopes. Argon2id is the preferred passphrase KDF; a Windows/libsodium build that cannot allocate Argon2 memory uses a recorded scrypt fallback. Managed deployments can supply a 32-byte key. Keys and passphrases are never written to the workspace.

The schema deliberately omits direct identifiers. Queue jobs contain only a serializable model and model-ready pseudonymous data, never an unlocked workspace or key. All registered endpoints and models retain hashes, provenance, version, and qualification metadata. Evidence events are append-only in normal APIs; corrections point to a superseded event. Patient saves use optimistic revision checks and encrypted audit entries form a hash chain.

The Shiny server binds to loopback by default. A non-loopback bind requires an explicit override, but that override alone does not make deployment secure.

## Threats not solved by the package alone

- A compromised R session can read an unlocked workspace key and its decrypted records.
- A weak passphrase remains vulnerable to offline guessing despite a memory-hard KDF.
- Filesystem encryption cannot replace host hardening, endpoint detection, patching, malware protection, encrypted backups, or physical controls.
- Pseudonyms can still be personal data when another system can relink them.
- Local audit files do not provide independent write-once retention or external timestamping.
- The research GUI has no institutional identity provider, role-based authorization, session expiry, dual approval, or break-glass workflow.
- The package does not certify the source model, endpoint, assay, units, covariate policy, numerical result, or dose feasibility for a clinical population.

## Required clinical programme

Before any clinical use, scope and validate at least:

1. Intended use, users, populations, drugs, decisions, exclusions, and human authority.
2. Quality/risk management, requirements traceability, verification, validation, change control, and independent release approval.
3. Model governance: immutable qualified versions, external validation, applicability checks, covariate/unit contracts, endpoint evidence review, and retirement policy.
4. Clinical safety: hard dose constraints, contraindication and interaction interfaces, plausibility checks, uncertainty/failure escalation, second-person approval, and complete decision provenance.
5. Security: institutional SSO/MFA, least-privilege RBAC, tenant isolation, KMS/HSM-backed keys, TLS, secrets rotation, signed artifacts, software-bill-of-materials, vulnerability management, penetration testing, and incident response.
6. Data governance: consent/legal basis, minimisation, retention/deletion rules, relinking controls, EHR write-back authorization, backup/restore testing, and independently retained audit logs.
7. Operations: monitored availability, queue integrity, resource limits, deterministic recovery, model/endpoint rollout controls, training, support, and post-market performance surveillance where applicable.

Security issues should not include real patient data. Report a minimal synthetic reproduction privately to the maintainer.
