-- CAP LTER: Uniqueness constraints for cover data
-- Goal:
--  1) Ensure one cover event per (plot, patch_type, subplot, year)
--  2) Ensure one composition row per (cover_event_id, cover_type_id)
--
-- Notes:
--  - CREATE INDEX CONCURRENTLY cannot run inside a transaction block
--  - The DO blocks below each run in their own transactions (that’s fine)
--  - This script will:
--      a) Fail early if duplicates exist (so you can clean upstream rows)
--      b) Create unique indexes concurrently (minimal lock)
--      c) Attach indexes as named UNIQUE constraints when not already present

\echo Checking for duplicate cover events by (plot, patch_type, subplot, year)...
DO $$
DECLARE v_exists boolean;
BEGIN
  SELECT EXISTS (
    SELECT 1
      FROM urbancndep.cover_events ce
     GROUP BY ce.plot, ce.patch_type, ce.subplot, ce.year
    HAVING COUNT(*) > 1
  ) INTO v_exists;

  IF v_exists THEN
    RAISE EXCEPTION 'Duplicate cover_events found for (plot, patch_type, subplot, year). Clean upstream data before adding constraint.';
  END IF;
END$$;

\echo Checking for duplicate compositions per (cover_event_id, cover_type_id)...
DO $$
DECLARE v_exists boolean;
BEGIN
  SELECT EXISTS (
    SELECT 1
      FROM urbancndep.cover_composition cc
     GROUP BY cc.cover_event_id, cc.cover_type_id
    HAVING COUNT(*) > 1
  ) INTO v_exists;

  IF v_exists THEN
    RAISE EXCEPTION 'Duplicate cover_composition rows found for (cover_event_id, cover_type_id). Clean upstream data before adding constraint.';
  END IF;
END$$;

\echo Creating unique index on cover_events (plot, patch_type, subplot, year)...
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS cover_events_uq_plot_patch_subplot_year
  ON urbancndep.cover_events (plot, patch_type, subplot, year);

\echo Attaching named UNIQUE constraint for cover_events using the index...
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
      FROM pg_constraint c
      JOIN pg_class t ON t.oid = c.conrelid
      JOIN pg_namespace n ON n.oid = t.relnamespace
     WHERE n.nspname = 'urbancndep'
       AND t.relname = 'cover_events'
       AND c.conname = 'cover_events_unique_event_year'
  ) THEN
    ALTER TABLE urbancndep.cover_events
      ADD CONSTRAINT cover_events_unique_event_year
      UNIQUE USING INDEX cover_events_uq_plot_patch_subplot_year;
  END IF;
END$$;

\echo Creating unique index on cover_composition (cover_event_id, cover_type_id)...
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS cover_comp_uq_event_type
  ON urbancndep.cover_composition (cover_event_id, cover_type_id);

\echo Attaching named UNIQUE constraint for cover_composition using the index...
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
      FROM pg_constraint c
      JOIN pg_class t ON t.oid = c.conrelid
      JOIN pg_namespace n ON n.oid = t.relnamespace
     WHERE n.nspname = 'urbancndep'
       AND t.relname = 'cover_composition'
       AND c.conname = 'cover_composition_unique_event_type'
  ) THEN
    ALTER TABLE urbancndep.cover_composition
      ADD CONSTRAINT cover_composition_unique_event_type
      UNIQUE USING INDEX cover_comp_uq_event_type;
  END IF;
END$$;

\echo Done. Both uniqueness constraints are in place.
