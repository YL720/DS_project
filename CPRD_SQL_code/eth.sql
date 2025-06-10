-- GP Ethnic
DROP TABLE IF EXISTS yangfan.gp_eth;
CREATE TABLE yangfan.gp_eth AS
SELECT DISTINCT c.e_patid, eventdate, Eth_category AS ethnic
FROM 18_299_Lyratzopoulos_e2.cprd_clinical AS c
INNER JOIN yangfan.UCL_eth_code e ON c.medcode = e.medcode
INNER JOIN yangfan.distinct_patid p ON p.e_patid = c.e_patid;

SELECT COUNT(*) FROM yangfan.gp_eth; -- 435815
SELECT COUNT(DISTINCT e_patid) FROM yangfan.gp_eth; -- 324869
ALTER TABLE yangfan.gp_eth ADD INDEX idx_e_patid (e_patid);
ALTER TABLE yangfan.gp_eth ADD INDEX idx_date (eventdate);

DROP TABLE IF EXISTS yangfan.lat_gp_eth;
CREATE Table yangfan.lat_gp_eth
(
SELECT DISTINCT t1.e_patid, eventdate, ethnic
FROM yangfan.gp_eth AS t1
INNER JOIN 
(SELECT e_patid, MAX(eventdate) as max_date
FROM yangfan.gp_eth GROUP BY e_patid  ) AS t2
ON t1.e_patid=t2.e_patid AND t1.eventdate=t2.max_date );

SELECT COUNT(*) FROM yangfan.lat_gp_eth; -- 325291
SELECT COUNT(DISTINCT e_patid) FROM yangfan.lat_gp_eth; -- 323161

CREATE TABLE yangfan.dup_conf_eth AS
SELECT e_patid
FROM yangfan.lat_gp_eth GROUP BY e_patid 
HAVING COUNT(*)>1;

DELETE FROM yangfan.lat_gp_eth 
WHERE e_patid IN(SELECT * FROM yangfan.dup_conf_eth);






