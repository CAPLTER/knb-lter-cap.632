-- Purpose: Ensure every table in schema urbancndep with an 'updated_at' column
--          has a BEFORE UPDATE trigger that sets NEW.updated_at = NOW().
-- Safety: Idempotent; drops existing trigger named 'set_updated_at' before re-creating.
-- Notes: Requires function urbancndep.trigger_set_timestamp() (created/replaced below).

-- Ensure trigger function exists (safe replace)
CREATE OR REPLACE FUNCTION urbancndep.trigger_set_timestamp() RETURNS trigger
    LANGUAGE plpgsql
AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$;

-- Dry-run: list target tables (have 'updated_at' column)
SELECT col.table_schema AS schema_name,
       col.table_name   AS table_name
  FROM information_schema.columns col
 WHERE col.table_schema = 'urbancndep'
   AND col.column_name  = 'updated_at'
 ORDER BY col.table_name;

-- Create triggers across all target tables
DO $$
DECLARE r RECORD;
BEGIN
  FOR r IN
    SELECT n.nspname AS schema_name,
           c.relname AS table_name,
           c.oid     AS table_oid
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      JOIN information_schema.columns col
        ON col.table_schema = n.nspname
       AND col.table_name   = c.relname
       AND col.column_name  = 'updated_at'
     WHERE n.nspname = 'urbancndep'
       AND c.relkind = 'r' -- ordinary tables
  LOOP
    -- Drop existing trigger if present to keep idempotent
    IF EXISTS (
      SELECT 1
        FROM pg_trigger t
       WHERE t.tgrelid = r.table_oid
         AND t.tgname  = 'set_updated_at'
    ) THEN
      EXECUTE format('DROP TRIGGER set_updated_at ON %I.%I', r.schema_name, r.table_name);
    END IF;

    -- Create the BEFORE UPDATE trigger
    EXECUTE format(
      'CREATE TRIGGER set_updated_at
         BEFORE UPDATE ON %I.%I
         FOR EACH ROW
         EXECUTE FUNCTION urbancndep.trigger_set_timestamp()',
      r.schema_name, r.table_name
    );
  END LOOP;
END$$;

-- Verification: count triggers created
SELECT n.nspname AS schema_name,
       c.relname AS table_name,
       t.tgname  AS trigger_name
  FROM pg_trigger t
  JOIN pg_class c      ON c.oid = t.tgrelid
  JOIN pg_namespace n  ON n.oid = c.relnamespace
 WHERE n.nspname = 'urbancndep'
   AND t.tgname  = 'set_updated_at'
 ORDER BY c.relname;
