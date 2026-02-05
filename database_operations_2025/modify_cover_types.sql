-- - Add comment (text) and include (boolean) to urbancndep.cover_types
-- - Nullability/defaults: allow NULL, no default for both columns
-- - Backfills set include = FALSE when cover_type is:
--   + `total_comparable_annual_cover`
--   + `bare_ground`
--   + `Bryophyta`
--   + `Riccia nigrella` 
--
-- Removing the `Bryophyta` and `Riccia nigrella` cover types undoes the additions
-- that we made 2024-04-08. Upon further review, it was decided that we did not
-- want to make this distinction and, instead, Bryophytes, Liverworts, etc. will
-- be catalogued as `soil_crusts` as has been done in previous surveys.
--
-- Technically, the cover amounts of `Bryophyta` and `Riccia nigrella` should be
-- merged into `soil_crusts` for respective events but their coverages were so
-- small that the addition was negligible and would essentially be negated when
-- rounding.

BEGIN;

-- add columns
ALTER TABLE urbancndep.cover_types
  ADD COLUMN IF NOT EXISTS comment text;

ALTER TABLE urbancndep.cover_types
  ADD COLUMN IF NOT EXISTS include boolean;

-- column comments
COMMENT ON COLUMN urbancndep.cover_types.comment IS 'storage or processing comment';
COMMENT ON COLUMN urbancndep.cover_types.include IS 'indicates whether value should be included in published data';

-- backfill
UPDATE urbancndep.cover_types
SET include = FALSE
WHERE
    cover_type = 'total_comparable_annual_cover' OR
    cover_type = 'bare_ground' OR
    cover_type ~~* '%bryophyta%' OR
    cover_type ~~* '%riccia%' OR
    cover_type = 'Ambrosia_stem'
;

UPDATE urbancndep.cover_types
SET comment = 'only include observations prior to 2015 (noninclusive) in output'
WHERE cover_type = 'Ambrosia_stem'
;

COMMIT;
