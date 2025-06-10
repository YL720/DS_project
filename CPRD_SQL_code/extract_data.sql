-- ALTER TABLE yangfan.conditions_code ADD INDEX idx_code (medcode);
-- ALTER TABLE yangfan.symptoms_code ADD INDEX idx_code (medcode);
-- ALTER TABLE yangfan.smoking_code ADD INDEX idx_code (medcode);
-- ALTER TABLE yangfan.colo_screen_HES_code ADD INDEX idx_code (opcs);
-- ALTER TABLE yangfan.colo_screen_gp_code ADD INDEX idx_code (medcode);
-- ALTER TABLE yangfan.aspirin_code ADD INDEX idx_code (prodcode);
-- ALTER TABLE yangfan.NSAIDs_code ADD INDEX idx_code (prodcode);

SET SESSION wait_timeout=86400;
SET SESSION interactive_timeout=86400;
SET SESSION net_read_timeout=86400;

-- Smoking
DROP TABLE IF EXISTS yangfan.smoking_records;
CREATE TABLE yangfan.smoking_records AS
SELECT distinct
    c.e_patid,
    ph.smokingcat,
    eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN yangfan.smoking_code AS ph ON c.medcode = ph.medcode;

ALTER TABLE yangfan.smoking_records ADD INDEX idx_e_patid (e_patid);
ALTER TABLE yangfan.smoking_records ADD INDEX idx_eventdate (eventdate);

DROP TABLE IF EXISTS yangfan.smoking_all;
CREATE TABLE yangfan.smoking_all
(
SELECT DISTINCT s.e_patid, eventdate, smokingcat
FROM yangfan.smoking_records AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.smoking_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.drinking_all;
CREATE TABLE yangfan.drinking_all
(
SELECT e_patid,eventdate as event_dt FROM yangfan.all_pat_conditions WHERE conditions='drinking'
);

-- colo_screen
DROP TABLE IF EXISTS yangfan.colo_screen_all_GP_records;
CREATE TABLE yangfan.colo_screen_all_GP_records
(
SELECT e_patid, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c 
INNER JOIN yangfan.colo_screen_gp_code as ph 
WHERE c.medcode = ph.medcode
);

ALTER TABLE yangfan.colo_screen_all_GP_records ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.colo_screen_GP;
CREATE TABLE yangfan.colo_screen_GP
(
SELECT DISTINCT s.e_patid, eventdate
FROM yangfan.colo_screen_all_GP_records AS s
INNER JOIN yangfan.all_index_pat p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.colo_screen_GP ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.colo_screen_all_hes_records;
CREATE TABLE yangfan.colo_screen_all_hes_records
(
SELECT e_patid, c.evdate
FROM 18_299_Lyratzopoulos_e2.hes_apc_procedures_epi AS c 
INNER JOIN yangfan.colo_screen_HES_code as ph 
WHERE c.opcs = ph.opcs
);

ALTER TABLE yangfan.colo_screen_all_hes_records ADD INDEX idx_e_patid (e_patid);

SELECT year(evdate), COUNT(*) FROM yangfan.colo_screen_all_hes_records GROUP BY year(evdate) ORDER BY year(evdate);

DROP TABLE IF EXISTS yangfan.colo_screen_hes;
CREATE TABLE yangfan.colo_screen_hes
(
SELECT DISTINCT s.e_patid, evdate
FROM yangfan.colo_screen_all_hes_records AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.colo_screen_hes ADD INDEX idx_e_patid (e_patid);

-- symptoms
DROP TABLE IF EXISTS yangfan.all_symptoms;
CREATE TABLE yangfan.all_symptoms
(
SELECT e_patid, ph.symptom, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN yangfan.symptoms_code ph
WHERE c.medcode = ph.medcode
);
ALTER TABLE yangfan.all_symptoms ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.all_pat_symptoms;
CREATE TABLE yangfan.all_pat_symptoms
(
SELECT s.e_patid, symptom, eventdate
FROM yangfan.all_symptoms s
INNER JOIN yangfan.distinct_patid p
WHERE s.e_patid = p.e_patid
);
ALTER TABLE yangfan.all_pat_symptoms ADD INDEX idx_e_patid (e_patid);

-- conditions
DROP TABLE IF EXISTS yangfan.all_conditions;
CREATE TABLE yangfan.all_conditions
(
SELECT e_patid, ph.conditions, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN yangfan.conditions_code ph
WHERE c.medcode = ph.medcode
);
ALTER TABLE yangfan.all_conditions ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.all_pat_conditions;
CREATE TABLE yangfan.all_pat_conditions
(
SELECT s.e_patid, conditions, eventdate
FROM yangfan.all_conditions s
INNER JOIN yangfan.distinct_patid p
WHERE s.e_patid = p.e_patid
);
ALTER TABLE yangfan.all_pat_conditions ADD INDEX idx_e_patid (e_patid);

-- Biomarker
DROP TABLE IF EXISTS yangfan.all_biomarkers;
CREATE TABLE yangfan.all_biomarkers
(
SELECT e_patid, ph.Biomarker,c.eventdate, c.data2, c.data5, c.data6
FROM 18_299_Lyratzopoulos_e2.cprd_test AS c
INNER JOIN yangfan.biomarker_code AS ph
WHERE c.medcode = ph.medcode
);
ALTER TABLE yangfan.all_biomarkers ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.biomarkers;
CREATE TABLE yangfan.biomarkers
(
SELECT DISTINCT s.e_patid, Biomarker,eventdate, data2,data5, data6
FROM yangfan.all_biomarkers AS s
INNER JOIN yangfan.distinct_patid p
WHERE s.e_patid = p.e_patid
);

ALTER TABLE yangfan.biomarkers ADD INDEX idx_e_patid (e_patid);
SELECT * FROM yangfan.biomarkers  WHERE data2 is null LIMIT 10;

-- Medication
DROP TABLE IF EXISTS yangfan.cprd_therapy_aspirin_all;
CREATE TABLE yangfan.cprd_therapy_aspirin_all
(
SELECT e_patid, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
INNER JOIN yangfan.aspirin_code as ph 
WHERE c.prodcode = ph.prodcode
);

ALTER TABLE yangfan.cprd_therapy_aspirin_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.aspirin_all;
CREATE TABLE yangfan.aspirin_all
(
SELECT DISTINCT s.e_patid,eventdate
FROM yangfan.cprd_therapy_aspirin_all AS s
INNER JOIN yangfan.distinct_patid p
ON s.e_patid = p.e_patid
);
ALTER TABLE yangfan.aspirin_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.cprd_therapy_NSAIDs_all;
CREATE TABLE yangfan.cprd_therapy_NSAIDs_all
(
SELECT e_patid, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
INNER JOIN yangfan.NSAIDs_code as ph 
WHERE c.prodcode = ph.prodcode
);

ALTER TABLE yangfan.cprd_therapy_NSAIDs_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.NSAIDs_all;
CREATE TABLE yangfan.NSAIDs_all
(
SELECT DISTINCT s.e_patid, eventdate
FROM yangfan.cprd_therapy_NSAIDs_all AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.NSAIDs_all ADD INDEX idx_e_patid (e_patid);

-- Height
DROP TABLE IF EXISTS yangfan.all_height;
CREATE TABLE yangfan.all_height
(
SELECT c.e_patid,c.eventdate,data1
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_additional a
ON c.adid=a.adid AND c.e_patid=a.e_patid 
WHERE medcode=3 AND c.enttype=14
);

ALTER TABLE yangfan.all_height ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.height_all;
CREATE TABLE yangfan.height_all
(
SELECT DISTINCT s.e_patid, eventdate, data1
FROM yangfan.all_height AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.height_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.lat_height;
CREATE Table yangfan.lat_height
(
SELECT DISTINCT t1.e_patid, eventdate, data1
FROM yangfan.height_all AS t1
INNER JOIN 
(SELECT e_patid, MAX(eventdate) as max_date
FROM yangfan.height_all WHERE data1>0 GROUP BY e_patid  ) AS t2
ON t1.e_patid=t2.e_patid AND t1.eventdate=t2.max_date WHERE data1>0);

ALTER TABLE yangfan.lat_height ADD INDEX idx_e_patid (e_patid);

-- Weight
DROP TABLE IF EXISTS yangfan.weight_all;
CREATE TABLE yangfan.weight_all
(
SELECT DISTINCT s.e_patid, eventdate, data1
FROM yangfan.all_weight AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.weight_all ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.lat_height;
CREATE Table yangfan.lat_height
(
SELECT DISTINCT t1.e_patid, eventdate, data1
FROM yangfan.height_all AS t1
INNER JOIN 
(SELECT e_patid, MAX(eventdate) as max_date
FROM yangfan.height_all WHERE data1>0 GROUP BY e_patid  ) AS t2
ON t1.e_patid=t2.e_patid AND t1.eventdate=t2.max_date WHERE data1>0);


DROP TABLE IF EXISTS yangfan.bmi;
CREATE Table yangfan.bmi
(
SELECT lh.e_patid, lh.data1 AS height,w.eventdate AS weight_date,w.data1 as weight, (w.data1/(lh.data1*lh.data1)) AS BMI
FROM yangfan.lat_height AS lh
INNER JOIN yangfan.weight_all AS w
ON lh.e_patid=w.e_patid );

-- drinking
DROP TABLE IF EXISTS yangfan.all_drinking;
CREATE TABLE yangfan.all_drinking
(
SELECT e_patid, c.eventdate
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN yangfan.LSHTM_alcohol_code ph
WHERE c.medcode = ph.medcode
);
ALTER TABLE yangfan.all_drinking ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.all_pat_drinking;
CREATE TABLE yangfan.all_pat_drinking
(
SELECT s.e_patid, eventdate
FROM yangfan.all_drinking s
INNER JOIN yangfan.distinct_patid p
WHERE s.e_patid = p.e_patid
);
ALTER TABLE yangfan.all_pat_drinking ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.lat_smoking;
CREATE Table yangfan.lat_smoking
(
SELECT DISTINCT t1.e_patid, eventdate,smokingcat
FROM yangfan.smoking_all AS t1
INNER JOIN 
(SELECT e_patid, MAX(eventdate) as max_date
FROM yangfan.smoking_all GROUP BY e_patid  ) AS t2
ON t1.e_patid=t2.e_patid AND t1.eventdate=t2.max_date);

SELECT COUNT(*) FROM yangfan.distinct_patid;
SELECT smokingcat, COUNT(*) FROM yangfan.lat_smoking GROUP BY smokingcat;