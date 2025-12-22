-- Standardize annual cover_type names by removing underscores (except 'unidentified')
--
-- Rules:
-- - For rows where cover_category = 'annual'
-- - If cover_type contains underscores and does NOT match '%unidentified%' (case-insensitive)
--   then replace '_' with ' ', collapse multiple spaces, and trim
--
-- Safety:
-- - No table aliasing used.
-- - Run this before adding uniqueness constraints.

BEGIN;

-- Replace underscores with spaces for applicable annual cover types
UPDATE urbancndep.cover_types
SET cover_type = regexp_replace(cover_type, '_', ' ', 'g')
WHERE cover_category = 'annual'
  AND cover_type LIKE '%_%'
  AND cover_type !~~* '%unidentified%';

-- Collapse multiple spaces to a single space
UPDATE urbancndep.cover_types
SET cover_type = regexp_replace(cover_type, '\\s+', ' ', 'g')
WHERE cover_category = 'annual'
  AND cover_type !~~* '%unidentified%';

-- Trim leading/trailing spaces
UPDATE urbancndep.cover_types
SET cover_type = btrim(cover_type)
WHERE cover_category = 'annual'
  AND cover_type !~~* '%unidentified%';

COMMIT;
