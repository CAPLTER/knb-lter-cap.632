-- Without setting shiny's password (uses existing or none)
-- psql -h localhost -U srearl -d caplter -f database_operations/bootstrap_roles_and_trigger.sql

-- With password passed via psql variable (recommended)
-- psql -h localhost -U srearl -d caplter -v SHINY_PW='strong-password-here' -f database_operations/bootstrap_roles_and_trigger.sql

-- Bootstrap: roles + updated_at trigger function
-- Idempotent: creates roles if missing; sets shiny password if provided; (re)creates function.

SET client_min_messages = notice;

-- 1) Create function in public used by dumps/restores (safe replace)
CREATE OR REPLACE FUNCTION public.trigger_set_timestamp() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$;

-- 2) Create roles if they do not exist
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'caplter') THEN
    CREATE ROLE caplter NOLOGIN;
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'shiny') THEN
    CREATE ROLE shiny LOGIN SUPERUSER;
  END IF;
END $$;

-- 3) Optionally set a password for shiny if passed via -v SHINY_PW=...
\if :SHINY_PW
ALTER ROLE shiny WITH ENCRYPTED PASSWORD :'SHINY_PW';
\else
-- Note: no SHINY_PW provided; leaving shiny's password unchanged.
\endif

-- 4) Verification (non-fatal checks)
SELECT rolname, rolsuper, rolcanlogin FROM pg_roles WHERE rolname IN ('caplter','shiny');
SELECT n.nspname AS schema, p.proname AS function
  FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
 WHERE n.nspname = 'public' AND p.proname = 'trigger_set_timestamp';
