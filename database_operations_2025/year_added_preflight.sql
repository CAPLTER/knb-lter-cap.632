-- Workflow: Year Added Preflight
-- Purpose: Read-only checks to derive first-use years, detect discrepancies
--          between `cover_types.year_added` and observed usage, and emit
--          complete CSV reports to `/tmp` for review.
-- Scope:   Schema `urbancndep`; tables `cover_types`, `cover_composition`,
--          and `cover_events`. No data modifications.
-- Rules:
--   - `first_use_year`: earliest non-NULL `cover_events.year` per `cover_type_id`.
--   - Discrepancies: existing `year_added` IS DISTINCT FROM `first_use_year`.
--   - Mockup `calculated_year_added`: earliest year with `cover_amt > 0` and
--     non-NULL `year`; zeros/NULLs do not count as first use.
-- Outputs (CSV with headers):
--   - `/tmp/year_added_preflight_summary_counts.csv` (summary metrics)
--   - `/tmp/year_added_preflight_discrepancies.csv` (detail mismatches)
--   - `/tmp/year_added_preflight_cover_types_calculated_year_added.csv` (mockup)
--   - `/tmp/year_added_preflight_zero_cover_refs.csv` (zero-cover refs by type)
--   - `/tmp/year_added_preflight_null_year_refs.csv` (null-year refs by type)
--   - `/tmp/year_added_preflight_unreferenced_types.csv` (types never referenced)
-- Safety/Notes:
--   - Each export embeds its own CTEs to avoid psql CTE scope limits.
--   - Uses `COPY ... TO STDOUT WITH CSV HEADER` + `\g` to write files client-side.
-- Run:
--   psql -h localhost -U srearl -d caplter -f database_operations/year_added_preflight.sql
-- Style: per-export CTE blocks; server-side COPY + one-shot `\g` to filename

SET search_path TO urbancndep, public;

-- Summary counts (writes to /tmp/year_added_preflight_summary_counts.csv)
COPY (
  WITH first_use_year AS (
    SELECT cover_composition.cover_type_id,
           MIN(cover_events.year) AS first_use_year
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_events.year IS NOT NULL
     GROUP BY cover_composition.cover_type_id
  ),
  zero_cover_refs AS (
    SELECT cover_composition.cover_type_id,
           COUNT(*) AS zero_ref_count
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_composition.cover_amt = 0
     GROUP BY cover_composition.cover_type_id
  ),
  null_year_refs AS (
    SELECT cover_composition.cover_type_id,
           COUNT(*) AS null_year_ref_count
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_events.year IS NULL
     GROUP BY cover_composition.cover_type_id
  ),
  discrepancies AS (
    SELECT cover_types.cover_type_id,
           cover_types.cover_type,
           cover_types.year_added,
           first_use_year.first_use_year
      FROM cover_types
      JOIN first_use_year ON first_use_year.cover_type_id = cover_types.cover_type_id
     WHERE first_use_year.first_use_year IS NOT NULL
       AND cover_types.year_added IS DISTINCT FROM first_use_year.first_use_year
  ),
  unreferenced_types AS (
    SELECT cover_types.cover_type_id,
           cover_types.cover_type
      FROM cover_types
      LEFT JOIN cover_composition ON cover_composition.cover_type_id = cover_types.cover_type_id
     WHERE cover_composition.cover_type_id IS NULL
  )
  SELECT 'warnings_zero_cover_refs' AS section,
         COALESCE(SUM(zero_cover_refs.zero_ref_count), 0) AS count
    FROM zero_cover_refs
  UNION ALL
  SELECT 'warnings_null_year_refs' AS section,
         COALESCE(SUM(null_year_refs.null_year_ref_count), 0) AS count
    FROM null_year_refs
  UNION ALL
  SELECT 'discrepancy_count' AS section,
         COUNT(*) AS count
    FROM discrepancies
  UNION ALL
  SELECT 'unreferenced_types_count' AS section,
         COUNT(*) AS count
    FROM unreferenced_types
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_summary_counts.csv

-- Detailed discrepancies (existing year_added vs first observed year)
-- Writes to /tmp/year_added_preflight_discrepancies.csv
COPY (
  WITH first_use_year AS (
    SELECT cover_composition.cover_type_id,
           MIN(cover_events.year) AS first_use_year
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_events.year IS NOT NULL
     GROUP BY cover_composition.cover_type_id
  )
  SELECT cover_types.cover_type_id,
         cover_types.cover_type,
         cover_types.year_added,
         first_use_year.first_use_year
    FROM cover_types
    JOIN first_use_year ON first_use_year.cover_type_id = cover_types.cover_type_id
   WHERE first_use_year.first_use_year IS NOT NULL
     AND cover_types.year_added IS DISTINCT FROM first_use_year.first_use_year
   ORDER BY cover_types.cover_type
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_discrepancies.csv

-- Mockup: all cover types with current and calculated year_added (positive use only)
-- Writes to /tmp/year_added_preflight_cover_types_calculated_year_added.csv
COPY (
  WITH first_use_year_positive AS (
    SELECT cover_composition.cover_type_id,
           MIN(cover_events.year) AS first_use_year
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_events.year IS NOT NULL
       AND cover_composition.cover_amt > 0
     GROUP BY cover_composition.cover_type_id
  )
  SELECT cover_types.cover_type_id,
         cover_types.cover_category,
         cover_types.cover_type,
         cover_types.year_added,
         first_use_year_positive.first_use_year AS calculated_year_added
    FROM cover_types
    LEFT JOIN first_use_year_positive ON first_use_year_positive.cover_type_id = cover_types.cover_type_id
   ORDER BY cover_types.cover_category, cover_types.cover_type, cover_types.cover_type_id
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_cover_types_calculated_year_added.csv

-- Detailed zero-cover references
-- Writes to /tmp/year_added_preflight_zero_cover_refs.csv
COPY (
  WITH zero_cover_refs AS (
    SELECT cover_composition.cover_type_id,
           COUNT(*) AS zero_ref_count
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_composition.cover_amt = 0
     GROUP BY cover_composition.cover_type_id
  )
  SELECT zero_cover_refs.cover_type_id,
         zero_cover_refs.zero_ref_count
    FROM zero_cover_refs
   ORDER BY zero_cover_refs.zero_ref_count DESC
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_zero_cover_refs.csv

-- Detailed null-year references
-- Writes to /tmp/year_added_preflight_null_year_refs.csv
COPY (
  WITH null_year_refs AS (
    SELECT cover_composition.cover_type_id,
           COUNT(*) AS null_year_ref_count
      FROM cover_composition
      JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
     WHERE cover_events.year IS NULL
     GROUP BY cover_composition.cover_type_id
  )
  SELECT null_year_refs.cover_type_id,
         null_year_refs.null_year_ref_count
    FROM null_year_refs
   ORDER BY null_year_refs.null_year_ref_count DESC
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_null_year_refs.csv

-- Types never referenced in cover_composition
-- Writes to /tmp/year_added_preflight_unreferenced_types.csv
COPY (
  WITH unreferenced_types AS (
    SELECT cover_types.cover_type_id,
           cover_types.cover_type
      FROM cover_types
      LEFT JOIN cover_composition ON cover_composition.cover_type_id = cover_types.cover_type_id
     WHERE cover_composition.cover_type_id IS NULL
  )
  SELECT unreferenced_types.cover_type_id,
         unreferenced_types.cover_type
    FROM unreferenced_types
   ORDER BY unreferenced_types.cover_type
) TO STDOUT WITH CSV HEADER;
\g /tmp/year_added_preflight_unreferenced_types.csv
