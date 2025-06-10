SET @new_onset_date = MAKEDATE(2006, 1);
SET @lookback_date = MAKEDATE(2009, 1);
SET @start_date = MAKEDATE(2011, 1);
SET @end_date = MAKEDATE(2013, 1);

ALTER TABLE yangfan.aspirin_code ADD INDEX idx_prodcode (prodcode);
ALTER TABLE yangfan.NSAIDs_code ADD INDEX idx_prodcode (prodcode);
ALTER TABLE yangfan.Constipation_treatment_code ADD INDEX idx_prodcode (prodcode);

SHOW PROCESSLIST;
KILL 2044;
-- codelist check
SELECT ap.prodcode,
EXISTs (SELECT 1
FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
WHERE c.prodcode = ap.prodcode) as exists_flag
FROM yangfan.aspirin_code as ap;

-- medication record
DROP TABLE IF EXISTS yangfan.Aspirin_idx_01;
CREATE TABLE yangfan.Aspirin_idx_01 AS
SELECT DISTINCT fip.e_patid
FROM yangfan.first_index_pat AS fip 
WHERE 
	EXISTS(
      SELECT 1
      FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
      JOIN yangfan.aspirin_code AS ph
      WHERE c.prodcode = ph.prodcode
	     AND c.e_patid = fip.e_patid
         AND eventdate between makedate(2010,1) and @start_date
      );
      
DROP TABLE IF EXISTS yangfan.CONS_treat_idx_01;
CREATE TABLE yangfan.CONS_treat_idx_01 AS
SELECT DISTINCT fip.e_patid
FROM yangfan.first_index_pat AS fip 
WHERE 
	EXISTS(
      SELECT 1
      FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
      JOIN yangfan.NSAIDs_code AS ph
      WHERE c.prodcode = ph.prodcode
	     AND c.e_patid = fip.e_patid
         AND eventdate between makedate(2010,1) and @start_date
      );
      
DROP TABLE IF EXISTS yangfan.NSAIDs_idx_01;
CREATE TABLE yangfan.NSAIDs_idx_01 AS
SELECT DISTINCT fip.e_patid
FROM yangfan.first_index_pat AS fip 
WHERE 
	EXISTS(
      SELECT 1
      FROM 18_299_Lyratzopoulos_e2.cprd_therapy AS c 
      JOIN yangfan.Constipation_treatment_code AS ph
      WHERE c.prodcode = ph.prodcode
	     AND c.e_patid = fip.e_patid
         AND eventdate between @lookback_date and @start_date
      );
      