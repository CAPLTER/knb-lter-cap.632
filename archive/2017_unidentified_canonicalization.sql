-- Migration: Canonicalize year-suffixed 'unidentified_1/_2/_3' into canonical types for 2017–2019
-- Schema: urbancndep
-- Scope: Years 2017–2019 only; do not touch non-target years; only affects
--        year-suffixed 'unidentified_1/_2/_3' variants (e.g., _2017, _2018, _2019)
-- Safety: Includes dry-run counts, transaction with explicit locks, snapshot for rollback,
--         assertions to prevent commit if mismatches remain, and safe deletes (no cascades).

-- =============================
-- DRY-RUN COUNTS (run before transaction)
-- =============================
-- 1) Inventory of cover_types variants in scope (canonical + year-suffixed _1/_2/_3)
SELECT urbancndep.cover_types.cover_type_id,
       urbancndep.cover_types.cover_category,
       urbancndep.cover_types.cover_type,
       urbancndep.cover_types.year_added
  FROM urbancndep.cover_types
 WHERE urbancndep.cover_types.cover_type IN ('unidentified','unidentified_1','unidentified_2','unidentified_3')
   OR urbancndep.cover_types.cover_type LIKE 'unidentified_1_%'
   OR urbancndep.cover_types.cover_type LIKE 'unidentified_2_%'
   OR urbancndep.cover_types.cover_type LIKE 'unidentified_3_%'
 ORDER BY urbancndep.cover_types.cover_type;

-- 2) References by year for affected variants
SELECT urbancndep.cover_types.cover_type AS cover_type,
       urbancndep.cover_events.year AS year,
       COUNT(*) AS ref_count
  FROM urbancndep.cover_composition
  JOIN urbancndep.cover_types
    ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
  JOIN urbancndep.cover_events
    ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
 WHERE urbancndep.cover_types.cover_category = 'annual'
   AND (
        urbancndep.cover_types.cover_type = 'unidentified'
        OR urbancndep.cover_types.cover_type = 'unidentified_1'
        OR urbancndep.cover_types.cover_type LIKE 'unidentified_1_%'
        OR urbancndep.cover_types.cover_type = 'unidentified_2'
        OR urbancndep.cover_types.cover_type LIKE 'unidentified_2_%'
        OR urbancndep.cover_types.cover_type = 'unidentified_3'
        OR urbancndep.cover_types.cover_type LIKE 'unidentified_3_%'
       )
 GROUP BY urbancndep.cover_types.cover_type, urbancndep.cover_events.year
 ORDER BY urbancndep.cover_types.cover_type, urbancndep.cover_events.year;

-- 3) Confirm canonical targets exist and are annual with year_added NULL
SELECT urbancndep.cover_types.cover_type_id,
       urbancndep.cover_types.cover_category,
       urbancndep.cover_types.cover_type,
       urbancndep.cover_types.year_added
  FROM urbancndep.cover_types
 WHERE urbancndep.cover_types.cover_type IN ('unidentified','unidentified_2','unidentified_3')
 ORDER BY urbancndep.cover_types.cover_type;

-- =============================
-- TRANSACTION: 2017–2019 canonicalization
-- =============================
BEGIN;

-- Explicit locks to prevent concurrent writes
LOCK TABLE urbancndep.cover_types       IN ACCESS EXCLUSIVE MODE;
LOCK TABLE urbancndep.cover_events      IN ACCESS EXCLUSIVE MODE;
LOCK TABLE urbancndep.cover_composition IN ACCESS EXCLUSIVE MODE;

-- Ensure canonical target exists and normalize metadata
INSERT INTO urbancndep.cover_types (cover_category, cover_type, year_added)
VALUES ('annual','unidentified',NULL)
ON CONFLICT (cover_type) DO UPDATE
    SET cover_category = EXCLUDED.cover_category,
        year_added     = NULL,
        updated_at     = NOW();

-- Ensure canonical targets for _2 and _3 exist
INSERT INTO urbancndep.cover_types (cover_category, cover_type, year_added)
VALUES ('annual','unidentified_2',NULL)
ON CONFLICT (cover_type) DO UPDATE
  SET cover_category = EXCLUDED.cover_category,
    year_added     = NULL,
    updated_at     = NOW();

INSERT INTO urbancndep.cover_types (cover_category, cover_type, year_added)
VALUES ('annual','unidentified_3',NULL)
ON CONFLICT (cover_type) DO UPDATE
  SET cover_category = EXCLUDED.cover_category,
    year_added     = NULL,
    updated_at     = NOW();

-- Target years and pre-update baselines
CREATE TEMP TABLE tmp_years(y integer) ON COMMIT DROP;
INSERT INTO tmp_years(y) VALUES (2017),(2018),(2019);

CREATE TEMP TABLE tmp_targets_pre AS
SELECT urbancndep.cover_types.cover_type AS cover_type,
       urbancndep.cover_events.year AS year,
       COUNT(*) AS ref_count
  FROM urbancndep.cover_composition
  JOIN urbancndep.cover_events
    ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
  JOIN urbancndep.cover_types
    ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
 WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
   AND urbancndep.cover_types.cover_category = 'annual'
   AND urbancndep.cover_types.cover_type IN ('unidentified','unidentified_2','unidentified_3')
 GROUP BY urbancndep.cover_types.cover_type, urbancndep.cover_events.year;

-- Snapshot affected rows (for rollback if needed): _1 → unidentified
CREATE TEMP TABLE tmp_unidentified1_rewire AS
SELECT urbancndep.cover_composition.cover_id AS cover_id,
       urbancndep.cover_composition.cover_type_id AS old_cover_type_id,
       urbancndep.cover_events.year AS year,
       (SELECT urbancndep.cover_types.cover_type_id
          FROM urbancndep.cover_types
         WHERE urbancndep.cover_types.cover_type = 'unidentified') AS new_cover_type_id
  FROM urbancndep.cover_composition
  JOIN urbancndep.cover_events
    ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
  JOIN urbancndep.cover_types
    ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
 WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
   AND urbancndep.cover_types.cover_category = 'annual'
   AND (
        urbancndep.cover_types.cover_type = 'unidentified_1'
        OR urbancndep.cover_types.cover_type LIKE 'unidentified_1_%'
       );

-- Rows to rewire: unidentified_2_* → unidentified_2
CREATE TEMP TABLE tmp_unidentified2_rewire AS
SELECT urbancndep.cover_composition.cover_id AS cover_id,
       urbancndep.cover_composition.cover_type_id AS old_cover_type_id,
       urbancndep.cover_events.year AS year,
       (SELECT urbancndep.cover_types.cover_type_id
          FROM urbancndep.cover_types
         WHERE urbancndep.cover_types.cover_type = 'unidentified_2') AS new_cover_type_id
  FROM urbancndep.cover_composition
  JOIN urbancndep.cover_events
    ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
  JOIN urbancndep.cover_types
    ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
 WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
   AND urbancndep.cover_types.cover_category = 'annual'
   AND urbancndep.cover_types.cover_type LIKE 'unidentified_2_%';

-- Rows to rewire: unidentified_3_* → unidentified_3
CREATE TEMP TABLE tmp_unidentified3_rewire AS
SELECT urbancndep.cover_composition.cover_id AS cover_id,
       urbancndep.cover_composition.cover_type_id AS old_cover_type_id,
       urbancndep.cover_events.year AS year,
       (SELECT urbancndep.cover_types.cover_type_id
          FROM urbancndep.cover_types
         WHERE urbancndep.cover_types.cover_type = 'unidentified_3') AS new_cover_type_id
  FROM urbancndep.cover_composition
  JOIN urbancndep.cover_events
    ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
  JOIN urbancndep.cover_types
    ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
 WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
   AND urbancndep.cover_types.cover_category = 'annual'
   AND urbancndep.cover_types.cover_type LIKE 'unidentified_3_%';

-- Rewire target-year references to canonical 'unidentified'
UPDATE urbancndep.cover_composition
  SET cover_type_id = tmp_unidentified1_rewire.new_cover_type_id,
       updated_at    = NOW()
  FROM tmp_unidentified1_rewire
 WHERE urbancndep.cover_composition.cover_id = tmp_unidentified1_rewire.cover_id;

  -- Rewire target-year references to canonical 'unidentified_2'
    UPDATE urbancndep.cover_composition
      SET cover_type_id = tmp_unidentified2_rewire.new_cover_type_id,
       updated_at    = NOW()
     FROM tmp_unidentified2_rewire
    WHERE urbancndep.cover_composition.cover_id = tmp_unidentified2_rewire.cover_id;

  -- Rewire target-year references to canonical 'unidentified_3'
    UPDATE urbancndep.cover_composition
      SET cover_type_id = tmp_unidentified3_rewire.new_cover_type_id,
       updated_at    = NOW()
     FROM tmp_unidentified3_rewire
    WHERE urbancndep.cover_composition.cover_id = tmp_unidentified3_rewire.cover_id;

-- Post-update check: verify no target-year references remain to collapsed variants
DO $$
DECLARE
    v_remaining INTEGER;
    v_rewired_1 INTEGER;
    v_rewired_2 INTEGER;
    v_rewired_3 INTEGER;
    v_pre_unid INTEGER;
    v_pre_unid2 INTEGER;
    v_pre_unid3 INTEGER;
    v_post_unid INTEGER;
    v_post_unid2 INTEGER;
    v_post_unid3 INTEGER;
BEGIN
    SELECT COUNT(*) INTO v_remaining
      FROM urbancndep.cover_composition
      JOIN urbancndep.cover_events
        ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
      JOIN urbancndep.cover_types
        ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
     WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
       AND urbancndep.cover_types.cover_category = 'annual'
       AND (
            urbancndep.cover_types.cover_type LIKE 'unidentified_1_%'
            OR urbancndep.cover_types.cover_type LIKE 'unidentified_2_%'
            OR urbancndep.cover_types.cover_type LIKE 'unidentified_3_%'
           );
    IF v_remaining > 0 THEN
        RAISE EXCEPTION 'Target years still reference year-suffixed unidentified types (count=%). Aborting for safety.', v_remaining;
    END IF;

    -- Verify moved rows increased canonical counts by exactly rewired counts per year
    CREATE TEMP TABLE tmp_rewired_totals AS
    SELECT 'unidentified'::text AS cover_type, year, COUNT(*) AS moved FROM tmp_unidentified1_rewire GROUP BY year
    UNION ALL
    SELECT 'unidentified_2'::text, year, COUNT(*) FROM tmp_unidentified2_rewire GROUP BY year
    UNION ALL
    SELECT 'unidentified_3'::text, year, COUNT(*) FROM tmp_unidentified3_rewire GROUP BY year;

    CREATE TEMP TABLE tmp_targets_post AS
    SELECT urbancndep.cover_types.cover_type AS cover_type,
           urbancndep.cover_events.year AS year,
           COUNT(*) AS ref_count
      FROM urbancndep.cover_composition
      JOIN urbancndep.cover_events
        ON urbancndep.cover_events.cover_event_id = urbancndep.cover_composition.cover_event_id
      JOIN urbancndep.cover_types
        ON urbancndep.cover_types.cover_type_id = urbancndep.cover_composition.cover_type_id
     WHERE urbancndep.cover_events.year IN (SELECT y FROM tmp_years)
       AND urbancndep.cover_types.cover_category = 'annual'
       AND urbancndep.cover_types.cover_type IN ('unidentified','unidentified_2','unidentified_3')
     GROUP BY urbancndep.cover_types.cover_type, urbancndep.cover_events.year;

    PERFORM 1
      FROM (
            SELECT coalesce(p.cover_type, m.cover_type) AS cover_type,
                   coalesce(p.year, m.year) AS year,
                   coalesce(p.ref_count,0) + coalesce(m.moved,0) AS expected_post,
                   coalesce(q.ref_count,0) AS actual_post
              FROM tmp_targets_pre p
              FULL OUTER JOIN tmp_rewired_totals m
                ON m.cover_type = p.cover_type AND m.year = p.year
              FULL OUTER JOIN tmp_targets_post q
                ON q.cover_type = coalesce(p.cover_type, m.cover_type)
               AND q.year       = coalesce(p.year, m.year)
           ) z
     WHERE z.expected_post <> z.actual_post;
    IF FOUND THEN
        RAISE EXCEPTION 'Mismatch in canonical counts by year after rewiring (expected != actual).';
    END IF;
END$$;

-- Delete unused collapsed types (only if no references remain)
DELETE FROM urbancndep.cover_types
 WHERE urbancndep.cover_types.cover_category = 'annual'
   AND urbancndep.cover_types.cover_type IN (
        'unidentified_1_2017','unidentified_2_2017','unidentified_3_2017',
        'unidentified_1_2018','unidentified_2_2018','unidentified_3_2018',
        'unidentified_1_2019','unidentified_2_2019','unidentified_3_2019'
   )
   AND NOT EXISTS (
        SELECT 1
          FROM urbancndep.cover_composition
         WHERE urbancndep.cover_composition.cover_type_id = urbancndep.cover_types.cover_type_id
       );

-- Final assertion: ensure canonical 'unidentified' metadata remains normalized
UPDATE urbancndep.cover_types
   SET year_added = NULL,
       updated_at = NOW()
 WHERE urbancndep.cover_types.cover_type = 'unidentified';

UPDATE urbancndep.cover_types
   SET year_added = NULL,
     updated_at = NOW()
 WHERE urbancndep.cover_types.cover_type IN ('unidentified_2','unidentified_3');

COMMIT;

-- =============================
-- ROLLBACK RECIPE (manual use before COMMIT)
-- =============================
-- If any post-checks fail before COMMIT, restore original IDs and ROLLBACK:
-- UPDATE urbancndep.cover_composition
--    SET cover_type_id = s.old_cover_type_id,
--        updated_at    = NOW()
--   FROM (
--         SELECT cover_id, old_cover_type_id FROM tmp_unidentified1_rewire
--         UNION ALL
--         SELECT cover_id, old_cover_type_id FROM tmp_unidentified2_rewire
--         UNION ALL
--         SELECT cover_id, old_cover_type_id FROM tmp_unidentified3_rewire
--        ) s
--  WHERE urbancndep.cover_composition.cover_id = s.cover_id;
-- ROLLBACK;
