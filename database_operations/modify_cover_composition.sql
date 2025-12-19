-- - Add include (boolean) to urbancndep.cover_composition
-- - Column comment: indicates whether record should be included in published data
-- - Backfill: set include = FALSE for Ambrosia stems in cover events after 2014
--
-- Aligns with the handling of Ambrosia stems described in cover_types logic,
-- only including observations prior to 2015 in published output.

BEGIN;

-- add column
ALTER TABLE urbancndep.cover_composition
  ADD COLUMN IF NOT EXISTS include boolean;

-- column comment
COMMENT ON COLUMN urbancndep.cover_composition.include IS 'indicates whether record should be included in published data';

-- backfill: Ambrosia stems after 2014
UPDATE urbancndep.cover_composition AS cc
SET include = FALSE
FROM urbancndep.cover_events AS ce,
     urbancndep.cover_types  AS ct
WHERE ce.cover_event_id = cc.cover_event_id
  AND ct.cover_type_id   = cc.cover_type_id
  AND ce.year > 2014
  AND (ct.cover_type = 'Ambrosia_stem' OR ct.cover_category = 'Ambrosia_stem');

COMMIT;
