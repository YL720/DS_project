DROP TABLE IF EXISTS  yangfan.first_Non_SKNM_cancer;
CREATE TABLE yangfan.first_Non_SKNM_cancer
SELECT 
	cancers.e_patid, 
    cancers.e_cr_id,
    cancers.e_cr_patid,
	cancers.diagnosisdatebest AS diagnosisdate,
	cancers_patient.age,
	cancers_patient.sex,
    p.deathdate,
	cancers.site_icd10_o2,
	cancer_lkup.cancer_site_desc,
    CASE WHEN cancers.site_icd10_o2 in ('C180', 'C181', 'C182', 'C183', 'C184', 'C185', 'C186', 'C187', 'C188', 'C189','C19','C20','C218',
         '1530','1531','1532','1533','1534','1535','1536','1537','1538','1539','1540','1541','1548')
         THEN 1 ELSE 0 END AS first_crc
FROM (
	SELECT cancers.e_patid, MIN(cancers.diagnosisdatebest) AS diagnosisdate
	FROM 18_299_Lyratzopoulos_e2.cancer_registration_tumour AS cancers
	INNER JOIN 18_299_Lyratzopoulos.lookup_core_cancersite AS cancer_lkup
	ON cancers.site_icd10_o2 = cancer_lkup.icd10_4dig
	WHERE cancer_flag = 1
	GROUP BY e_patid
	) AS first_cancers
INNER JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour AS cancers
INNER JOIN 18_299_Lyratzopoulos.lookup_core_cancersite AS cancer_lkup
INNER JOIN 18_299_Lyratzopoulos_e2.cancer_registration_patient AS cancers_patient
INNER JOIN 18_299_Lyratzopoulos_e2.cprd_patient p
ON first_cancers.e_patid = cancers.e_patid 
AND first_cancers.diagnosisdate = cancers.diagnosisdatebest
AND cancers.site_icd10_o2 = cancer_lkup.icd10_4dig
AND cancers.e_patid = cancers_patient.e_patid 
AND cancers.e_cr_id = cancers_patient.e_cr_id
AND first_cancers.e_patid = p.e_patid 
WHERE cancer_flag = 1 AND site_icd10_o2 NOT IN ('C440', 'C441', 'C442', 'C443', 'C444', 'C445', 'C446', 'C447', 'C448', 'C449',
         '1730','1731','1732','1733','1734','1735','1736','1737','1738','1739')
;

SELECT COUNT(*) FROM yangfan.first_Non_SKNM_cancer; -- 333117
SELECT COUNT(DISTINCT e_patid) FROM yangfan.first_Non_SKNM_cancer; -- 330630
SELECT COUNT(DISTINCT e_cr_patid) FROM yangfan.first_Non_SKNM_cancer; -- 324762
SELECT COUNT(DISTINCT e_cr_id) FROM yangfan.first_Non_SKNM_cancer; -- 327189
SELECT COUNT(*) FROM yangfan.first_Non_SKNM_cancer WHERE first_crc=1; -- 34509

DROP TABLE IF EXISTS yangfan.patient;
CREATE TABLE yangfan.patient AS
SELECT 
	e_patid, 
	yob,
    mob,
	gender,
    gen_ethnicity,
	imd2015_10,
	deathdate,
	crd,
	uts, 
	tod,
	lcd,
    CASE WHEN crd>=uts THEN crd 
              ELSE uts 
              END AS gp_start,
    LEAST(coalesce(tod,lcd),lcd) AS gp_end
FROM (
	SELECT 
		imd.imd2015_10,
		patient_file.*,
        hes_patient_file.gen_ethnicity,
		practice_file.uts, /* Up to standard date */
		practice_file.lcd /* Last collection date */
	FROM 18_299_Lyratzopoulos_e2.cprd_random_sample AS sample
	LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_patient AS hes_patient_file ON sample.e_patid = hes_patient_file.e_patid 
	INNER JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold AS linkage	ON sample.e_patid = linkage.e_patid 
    INNER JOIN 18_299_Lyratzopoulos_e2.cprd_practice AS practice_file ON linkage.e_pracid = practice_file.e_pracid
	INNER JOIN 18_299_Lyratzopoulos_e2.cprd_patient AS patient_file ON sample.e_patid = patient_file.e_patid
	INNER JOIN 18_299_Lyratzopoulos_e2.imd_2015 AS imd /*lost 123 patient due to this linkege*/
	ON  sample.e_patid = imd.e_patid) AS patient; -- 999877

DELIMITER //
DROP PROCEDURE IF EXISTS yangfan.RecreateTableAndInsertData //

CREATE PROCEDURE yangfan.RecreateTableAndInsertData()
BEGIN
    DECLARE cur_date DATE;
    DECLARE end_date DATE;    
    SET cur_date = '2006-01-01';
    SET end_date = '2017-01-01';

    DROP TABLE IF EXISTS yangfan.all_index_pat;
    
	CREATE TABLE yangfan.all_index_pat (
            e_patid BIGINT,
            yob INT,
            age_at_index INT,
            gender int,
            gen_ethnicity varchar(10),
            crc_in_1_yr BOOLEAN,
			imd2015_10 INT,
            gp_start DATE,
            gp_end DATE,
            deathdate DATE,
            diagnosisdate DATE,
            index_date DATE
        );
    WHILE cur_date <= end_date DO
        -- Insert data into the table
        INSERT INTO yangfan.all_index_pat
        SELECT 
            p.e_patid,
            yob,
			YEAR(cur_date)-p.yob AS age_at_index,
            p.gender,
			p.gen_ethnicity,
            IF(f.first_crc = 1 AND (f.diagnosisdate > cur_date AND f.diagnosisdate <= DATE_ADD(cur_date, INTERVAL 1 YEAR)),TRUE,FALSE) AS crc_in_1_yr,
            imd2015_10,
            p.gp_start,
            p.gp_end,
			p.deathdate,
            f.diagnosisdate,
            cur_date AS index_date
        FROM 
            yangfan.patient p
        LEFT JOIN 
            yangfan.first_Non_SKNM_cancer f ON p.e_patid = f.e_patid
        WHERE 
            (p.deathdate IS NULL OR p.deathdate > cur_date)
            AND DATE_ADD(p.gp_start, interval 6 MONTH)<=cur_date
            AND DATE_ADD(p.gp_end, interval 18 MONTH)>=cur_date
            AND (f.diagnosisdate IS NULL OR f.diagnosisdate > cur_date)
            AND TIMESTAMPDIFF(YEAR, CONCAT(p.yob, '-', LPAD(IF(p.mob = 0, 6, p.mob), 2, '0'), '-01'), cur_date) BETWEEN 40 AND 75;
		SET cur_date=DATE_ADD(cur_date, interval 3 MONTH);
	end while;
END //
DELIMITER ;
CALL yangfan.RecreateTableAndInsertData();

ALTER TABLE yangfan.all_index_pat ADD INDEX idx_patid (e_patid);
ALTER TABLE yangfan.all_index_pat ADD INDEX idx_date (index_date);

SELECT COUNT(DISTINCT e_patid) FROM yangfan.all_index_pat; -- 566520
SELECT COUNT(*) FROM yangfan.all_index_pat WHERE crc_in_1_yr is null; -- 0 

-- Age, status, futime, imd, gender

-- Distinct patient id
DROP TABLE IF EXISTS yangfan.distinct_patid;
CREATE TABLE yangfan.distinct_patid
(
SELECT DISTINCT e_patid,gender, gen_ethnicity
FROM yangfan.all_index_pat);

ALTER TABLE yangfan.distinct_patid ADD INDEX idx_e_patid (e_patid);

SELECT count(*) FROM yangfan.distinct_patid; -- 566520
SELECT count(DISTINCT e_patid) FROM yangfan.distinct_patid; -- 566520
SELECT COUNT(*) FROM yangfan.distinct_patid WHERE gender=1; -- 291135, 51.4%, slightly higher than 49.9% in overall patient
SELECT COUNT(*)/566520 FROM yangfan.distinct_patid where gen_ethnicity is not null;

SELECT COUNT(DISTINCT e_patid) FROM yangfan.all_index_pat WHERE crc_in_1_yr=1;