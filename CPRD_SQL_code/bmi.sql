-- Height
DROP TABLE IF EXISTS yangfan.all_height_record;
CREATE TABLE yangfan.all_height_record
(
SELECT c.e_patid,c.eventdate,data1
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_additional a
ON c.adid=a.adid AND c.e_patid=a.e_patid 
WHERE medcode=3 AND c.enttype=14
);

ALTER TABLE yangfan.all_height_record ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.height_all_pat;
CREATE TABLE yangfan.height_all_pat
(
SELECT DISTINCT s.e_patid, eventdate, data1
FROM yangfan.all_height_record AS s
INNER JOIN yangfan.distinct_patid p #This is my target cohort, about 560k people
on s.e_patid = p.e_patid
);
ALTER TABLE yangfan.height_all_pat ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.lat_height;
CREATE Table yangfan.lat_height
(
SELECT DISTINCT t1.e_patid, eventdate, data1
FROM yangfan.height_all_pat AS t1
INNER JOIN 
(SELECT e_patid, MAX(eventdate) as max_date
FROM yangfan.height_all_pat WHERE data1 between 1.2 and 2.2 GROUP BY e_patid  ) AS t2
ON t1.e_patid=t2.e_patid AND t1.eventdate=t2.max_date WHERE data1 between 1.2 and 2.2);

ALTER TABLE yangfan.lat_height ADD INDEX idx_e_patid (e_patid);

-- Weight
DROP TABLE IF EXISTS yangfan.all_weight_record;
CREATE TABLE yangfan.all_weight_record
(
SELECT c.e_patid,c.eventdate,data1
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_additional a
ON c.adid=a.adid AND c.e_patid=a.e_patid 
WHERE medcode=2 AND c.enttype=13
);
ALTER TABLE yangfan.all_weight_record ADD INDEX idx_e_patid (e_patid);

DROP TABLE IF EXISTS yangfan.weight_all_pat;
CREATE TABLE yangfan.weight_all_pat
(
SELECT DISTINCT s.e_patid, eventdate, data1
FROM yangfan.all_weight AS s
INNER JOIN yangfan.distinct_patid p
on s.e_patid = p.e_patid
WHERE data1 BETWEEN 20 AND 450
);
ALTER TABLE yangfan.weight_all_pat ADD INDEX idx_e_patid (e_patid);

# BMI
DROP TABLE IF EXISTS yangfan.bmi;
CREATE Table yangfan.bmi
(
SELECT lh.e_patid, lh.data1 AS height,w.eventdate AS weight_date,w.data1 as weight, (w.data1/(lh.data1*lh.data1)) AS BMI
FROM yangfan.lat_height AS lh
INNER JOIN yangfan.weight_all_pat AS w
ON lh.e_patid=w.e_patid );

