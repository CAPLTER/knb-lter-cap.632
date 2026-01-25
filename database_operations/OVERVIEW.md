# CAP LTER Database Operations — Overview

This document provides a conceptual, text‑only overview of all workflows in this folder. It summarizes purpose, inputs/outputs, sequencing, dependencies, and governance, including optional bootstrap/role setup and strict vs relaxed variants. It intentionally avoids commands and diagrams.

**Purpose & Scope**
- **Goal:** Govern data quality, schema integrity, and publication readiness for annual plant cover data in the `urbancndep` schema.
- **Scope:** Trigger setup, data hygiene and canonicalization, naming normalization, schema enrichment and inclusion policy, unit standardization and fixes, uniqueness constraints, year‑added governance (preflight + populate variants), and optional bootstrap roles/trigger.

**File Inventory**
- **Justfile:** High‑level orchestration and intended step order.
- **set_updated_at_triggers.sql:** Defines `urbancndep.trigger_set_timestamp()` and installs `set_updated_at` triggers on tables that have `updated_at`.
- **bootstrap_roles_and_trigger.sql:** Optional one‑time bootstrap creating roles (e.g., `caplter`, `shiny`) and defining `public.trigger_set_timestamp()`.
- **deduplicate_unidentified_annuals.sql:** Removes specific duplicate annuals rows to unblock consistent downstream processing.
- **standardize_unidentified_annuals.sql:** Canonicalizes `unidentified_*` annuals types and rewires references to consistent canonical types.
- **standardize_annual_cover_type_names.sql:** Normalizes annual `cover_type` names (e.g., underscores → spaces) with designated exceptions.
- **modify_cover_types.sql:** Adds metadata (e.g., `comment`, `include`) and backfills policy values on `cover_types`.
- **modify_cover_composition.sql:** Applies inclusion policy changes to `cover_composition` (e.g., exclude Ambrosia stems after a threshold year).
- **standardize_cover_amt.sql:** Converts percent‑entered `cover_amt` to decimals across targeted ranges.
- **update_cover_amt.sql:** Focused corrections for mis‑entered `cover_amt` with guards.
- **add_cover_uniqueness_constraints.sql:** Adds uniqueness constraints (via concurrent indexes) after verifying no duplicates exist.
- **year_added_preflight.sql:** Read‑only diagnostics deriving first‑use years and surfacing discrepancies prior to population.
- **year_added_populate.sql:** Strict (gated) populate of `cover_types.year_added` based on earliest observed usage.
- **year_added_populate_relaxed.sql:** Relaxed populate variant (no global gating), still transactional.

**Workflows**
- **Trigger Setup**
  - **Purpose:** Ensure automatic timestamping on updates.
  - **Inputs/Outputs:** Installs `set_updated_at` triggers on all `urbancndep` tables with `updated_at`; consistent audit timestamps.
  - **Tools:** PostgreSQL `plpgsql` function and `ALTER TABLE ... ADD TRIGGER`.
  - **Placement:** First; foundation for subsequent changes.

- **Data Hygiene: Annuals Canonicalization & Deduplication**
  - **Purpose:** Remove known duplicates and canonicalize `unidentified_*` annuals to consistent types and references.
  - **Inputs/Outputs:** Updates `urbancndep.cover_types`, `urbancndep.cover_composition`, and related references; deletes or rewires problematic entries.
  - **Tools:** Transactional SQL with explicit locks and integrity checks.
  - **Placement:** Early, before normalization and constraints.

- **Naming Normalization**
  - **Purpose:** Normalize `cover_type` names (e.g., underscores → spaces), excluding protected tokens.
  - **Inputs/Outputs:** Updates `urbancndep.cover_types.cover_type` values for consistency.
  - **Tools:** String transforms.
  - **Placement:** After hygiene/canonicalization.

- **Schema Enrichment & Inclusion Policy**
  - **Purpose:** Add `comment` and `include` metadata; set inclusion policy on `cover_composition` for publication governance.
  - **Inputs/Outputs:** DDL adds columns; DML backfills metadata and flags.
  - **Tools:** Transactional DDL/DML.
  - **Placement:** After normalization; before constraints.

- **Units Standardization & Targeted Corrections**
  - **Purpose:** Convert percent to decimal in `cover_amt`; apply narrow, context‑specific fixes.
  - **Inputs/Outputs:** Updates `urbancndep.cover_composition.cover_amt` with guards to ensure correct units.
  - **Tools:** Controlled SQL updates.
  - **Placement:** Before constraints.

- **Uniqueness Constraints**
  - **Purpose:** Enforce uniqueness for events and compositions:
    - `cover_events`: one per `(plot, patch_type, subplot, year)`
    - `cover_composition`: one per `(cover_event_id, cover_type_id)`
  - **Inputs/Outputs:** Pre‑checks to prevent violations; concurrent indexes then named UNIQUE constraints.
  - **Tools:** `CREATE UNIQUE INDEX CONCURRENTLY`, `ALTER TABLE ... ADD CONSTRAINT`, catalog introspection.
  - **Placement:** After cleanup/standardization.

- **Year‑Added Governance (Variants)**
  - **Preflight (read‑only):** Derives earliest use, surfaces discrepancies, unreferenced types, and edge cases for review.
  - **Strict populate (gated):** Applies pre‑actions (e.g., nullify known‑bad values, remove unreferenced types) and gates execution on global consistency checks.
  - **Relaxed populate:** Populates without global gating; remains transactional and emits post‑check metrics.
  - **Placement:** Preflight → Strict (or Relaxed) near the end, after constraints and standardizations.

- **Bootstrap (Optional)**
  - **Purpose:** Provision roles and define `public.trigger_set_timestamp()` for generalized use.
  - **Inputs/Outputs:** Role state and a public trigger function; independent of data transformations.
  - **Tools:** Role DDL and function creation.
  - **Placement:** One‑time infra step; run as needed ahead of other workflows.

**Schemas & Aliases Observed**
- **urbancndep:** Primary schema for domain tables and operations.
- **public:** Used by optional bootstrap to define `public.trigger_set_timestamp()`.
- **information_schema:** Introspection for discovering target tables/columns.
- **pg catalogs (implicit):** `pg_constraint`, `pg_class`, `pg_trigger`, `pg_namespace` referenced via catalog queries for safe attachment/verification.
- **Aliases (examples):** `ct` (cover_types), `ce` (cover_events), `cc` (cover_composition), `col` (information_schema.columns); column aliases for diagnostics/metrics labels.

**Typical Sequence (Conceptual)**
- Triggers → Hygiene/Canonicalization → Naming Normalization → Schema Enrichment & Inclusion → Units Standardization → Uniqueness Constraints → Year‑Added (Preflight → Strict or Relaxed) → Any final targeted corrections.

**Dependencies & Assumptions**
- **Environment:** PostgreSQL available; scripts are run via tooling that supplies connection details; audit columns `updated_at` exist where triggers are installed.
- **Integrity:** Scripts use transactions, explicit locks, pre‑checks, and assertions to prevent partial/inconsistent states.
- **Governance:** `include` and `year_added` guide downstream publication; uniqueness constraints enforce data model expectations.
- **Idempotency & Safety:** Most operations are written to be safe to re‑run after successful completion (e.g., `IF NOT EXISTS`, concurrent indexes), or are guarded by checks to fail fast when preconditions are not met.
