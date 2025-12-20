-- Workflow: Year Added Populate (Relaxed)
-- Purpose: Populate missing `cover_types.year_added` from earliest observed
--          non-NULL usage year without gating on global discrepancies.
-- Scope:   Schema `urbancndep`; tables `cover_types`, `cover_composition`,
--          and `cover_events`.
-- Pre-actions (executed at start of transaction):
--   - Set `year_added = NULL` for `cover_type` in ('Sonchus','Castilleja_exserta').
--   - Delete `cover_types` rows for IDs: 156,175,174,177,40,41,176.
-- Safety:
--   - Single transaction (`BEGIN`/`COMMIT`).
--   - Post-check metrics after update.
-- Update Behavior:
--   - Updates only rows where `year_added IS NULL`.
--   - `first_use_year` is earliest non-NULL `cover_events.year` per `cover_type_id`.
-- Run:
--   psql -h localhost -U srearl -d caplter -f database_operations/year_added_populate_relaxed.sql
-- Style: CTEs, no table aliasing; explicit `search_path`.

SET search_path TO urbancndep, public;

BEGIN;

-- Pre-actions: normalize and remove specific cover_types before population
UPDATE urbancndep.cover_types
   SET year_added = NULL
 WHERE cover_type IN (
   'Sonchus',
   'Castilleja_exserta'
 );

DELETE FROM urbancndep.cover_types
 WHERE cover_type_id IN (
   156, -- Asclepias
   175, -- Logfia
   174, -- Lotus
   177, -- Lupinus
   40,  -- Mirabilis_bigelovii
   41,  -- Orthocarpus_purpurascens
   176  -- Parietaria
 );

-- Update NULL year_added values only
WITH first_use_year AS (
  SELECT cover_composition.cover_type_id,
         MIN(cover_events.year) AS first_use_year
    FROM cover_composition
    JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
   WHERE cover_events.year IS NOT NULL
   GROUP BY cover_composition.cover_type_id
)
UPDATE cover_types
   SET year_added = first_use_year.first_use_year
  FROM first_use_year
 WHERE cover_types.cover_type_id = first_use_year.cover_type_id
   AND cover_types.year_added IS NULL
   AND first_use_year.first_use_year IS NOT NULL;

-- Post-check: remaining discrepancies and still-null rows
WITH first_use_year AS (
  SELECT cover_composition.cover_type_id,
         MIN(cover_events.year) AS first_use_year
    FROM cover_composition
    JOIN cover_events ON cover_events.cover_event_id = cover_composition.cover_event_id
   WHERE cover_events.year IS NOT NULL
   GROUP BY cover_composition.cover_type_id
)
SELECT 'remaining_discrepancies' AS metric,
       COUNT(*) AS value
  FROM cover_types
  JOIN first_use_year ON first_use_year.cover_type_id = cover_types.cover_type_id
 WHERE first_use_year.first_use_year IS NOT NULL
   AND cover_types.year_added IS DISTINCT FROM first_use_year.first_use_year
UNION ALL
SELECT 'still_null_year_added' AS metric,
       COUNT(*) AS value
  FROM cover_types
 WHERE year_added IS NULL;

COMMIT;
