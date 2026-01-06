-- | year|site_code | plot_id|treatment_code |location_within_plot | subplot|sample_date |cover_category      |cover_type             | cover_amt| cover_id|
-- |----:|:---------|-------:|:--------------|:--------------------|-------:|:-----------|:-------------------|:----------------------|---------:|--------:|
-- | 2023|MCS       |      22|N              |P                    |       2|2023-03-23  |plot characteristic |total_shrub_cover      |        NA|    23821|
-- | 2023|PWP       |      31|N              |P                    |       2|2023-03-16  |plot characteristic |Larrea_base            |        NA|    22978|
-- | 2023|PWP       |      31|N              |P                    |       2|2023-03-16  |annual              |Pectocarya_heterocarpa |        NA|    22968|
-- | 2023|PWP       |      31|N              |P                    |       2|2023-03-16  |plot characteristic |soil_crusts            |        NA|    22975|
-- | 2023|PWP       |      34|NP             |P                    |       2|2023-03-16  |plot characteristic |soil_crusts            |        NA|    23090|
-- | 2023|SME       |      28|P              |P                    |       1|2023-03-21  |annual              |Plantago_ovata         |        NA|    23637|
-- | 2023|SMW       |      42|N              |P                    |       1|2023-03-13  |annual              |Pectocarya_recurvata   |        NA|    23195|
-- | 2013|PWP       |      33|C1             |P                    |       2|NA          |plot characteristic |total_shrub_cover      |         7|     1677|


-- Correct specific cover_amt values in urbancndep.cover_composition
-- Guards ensure we only fill previously NULL amounts or correct a known units error.
-- Mapping confirmed:
--   cover_id 23821 -> cover_amt 1.00
--   cover_id 22978 -> cover_amt 0.01
--   cover_id 22968 -> cover_amt 0.01
--   cover_id 22975 -> cover_amt 0.01
--   cover_id 23090 -> cover_amt 0.01
--   cover_id 23637 -> cover_amt 0.01
--   cover_id 23195 -> cover_amt 0.01
--   cover_id 1677  -> cover_amt 0.70 (units correction from 7)

BEGIN;

-- Fill missing amount for cover_id = 23821
UPDATE urbancndep.cover_composition
SET cover_amt = 1.00
WHERE cover_id = 23821
  AND cover_amt IS NULL;

-- Fill missing amounts for the set of 0.01 values
UPDATE urbancndep.cover_composition
SET cover_amt = 0.01
WHERE cover_id IN (22978, 22968, 22975, 23090, 23637, 23195)
  AND cover_amt IS NULL;

-- Correct presumed units error: set 7 -> 0.70 for cover_id = 1677
UPDATE urbancndep.cover_composition
SET cover_amt = 0.70
WHERE cover_id = 1677
  AND cover_amt = 7;

COMMIT;

-- Verification: inspect the updated amounts
SELECT cover_id, cover_amt
FROM urbancndep.cover_composition
WHERE cover_id IN (23821, 22978, 22968, 22975, 23090, 23637, 23195, 1677)
ORDER BY cover_id;
