-- delete duplicate unidentified cover types from LDP and SRR in 2023
--
-- Here, the 0.001 cover_amt values reflect that the unidentified was present but
-- in miniscule quantity and the categories are not enumerated (e.g.,
-- unidentified_2), which suggests that these are data recording or entry
-- duplicates and are safe to omit. These are the only cases of duplicated
-- cover_types for a plot * location * subplot combination and removing these
-- duplicates is necessary to facilitate transposing the annuals data from long to
-- wide.
--
-- > pot_id	location_within_plot	subplot	year	cover_type	    site_code	treatment_code	date	    collector	                        cover_category	cover_amount    cover_id
-- > 3	    IP	                    2	2023	    unidentified	LDP	        NP	            2023-03-20	Jaysen Brunner; Kristen Countryman	annual	        0.001		    22574
-- > 3	    IP	                    2	2023	    unidentified	LDP	        NP	            2023-03-20	Jaysen Brunner; Kristen Countryman	annual	        0.001		    22573
-- > 5	    IP	                    1	2023	    unidentified	LDP	        C1	            2023-03-20	Jaysen Brunner; Kristen Countryman	annual	        0.001		    22748
-- > 5	    IP	                    1	2023	    unidentified	LDP	        C1	            2023-03-20	Jaysen Brunner; Kristen Countryman	annual	        0.001		    22749
-- > 12	    P	                      2	2023	    unidentified	SRR	        NP	            2023-04-03	Jaysen Brunner; Kristen Countryman	annual	        0.001		    22773
-- > 12	    P	                      2	2023	    unidentified	SRR	        NP	            2023-04-03	Jaysen Brunner; Kristen Countryman	annual	        0.002		    22772

DELETE FROM urbancndep.cover_composition
WHERE cover_id IN (
    22574,
    22748,
    22773
);
