-- - convert 2025 (and one 2024) cover_amt values from percent to decimal
-- fraction.
--
-- > "Yes I entered all of the values as percent covers, even those that had less
-- > than 0.1%%. Since it was so dry and the rains were so late this year, some of
-- > the species in the plots were so small and so scarce that they didn't even
-- > take up 0.1% cover. In the protocol there is no guidance on what to do if the
-- > percent cover of species is less than 0.1% and so on the data sheet we just
-- > noted it down as <0.1% and I uploaded it into the database as 0.05% to
-- > signify that the species was present but at such a low concentration that it
-- > didn't even take up 0.1% cover." CH

UPDATE urbancndep.cover_composition
SET cover_amt = 0.91
WHERE cover_id = 24631 ;

UPDATE urbancndep.cover_composition
SET cover_amt = (cover_amt / 100)
WHERE cover_event_id in
    (SELECT cover_event_id
     FROM urbancndep.cover_events
     WHERE year = 2025 ) ;
