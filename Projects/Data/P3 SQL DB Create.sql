/*
CUNY MSDA 607 Project 3 Spring 2016
Fully Normalized Relational SQL Database Script
1. Creates a new schema called ds_skills
2. Creates the required normalized tables for all 3 data collection processes:
   - The URL - Skill Name data collection process
   - The TWitter Feed data collection process
   - The Term-Document Matrix data collection process
3. Provide sample insert statements using test data for all tables
4. Define various stored procedures that will be used to recreate tidy R dataframes
based on the information contained within the database

NOTE: This schema may be modified if new data requirements are uncovered during the
data exploration work that is currently underway.
*/

DROP SCHEMA IF EXISTS ds_skills;
CREATE SCHEMA ds_skills;
USE ds_skills;

/* --------------------------------------------------------------*/

DROP TABLE IF EXISTS doc_skills; 
DROP TABLE IF EXISTS skill_type;
DROP TABLE IF EXISTS skills;
DROP TABLE IF EXISTS documents;
DROP TABLE IF EXISTS doc_category;
DROP TABLE IF EXISTS twitter_freq;

/******************************************************************
Create a table that tracks different categories of source documents, eg, Twitter, Url, etc.
******************************************************************/

CREATE TABLE doc_category
(
  dc_id int AUTO_INCREMENT PRIMARY KEY NOT NULL,
  dc_cat varchar(30) NOT NULL
);

/* add the two categories */
/* NOTE: the dc_id primary key automatically increments with each new 
insert statement */

INSERT INTO doc_category ( dc_cat ) 
VALUES 
('Twitter'),
('URL');

SELECT * FROM doc_category;

/*****************************************************************
Create a table called documents that tracks the names/paths of source documents

NOTE: This table is used by both the
URL-SKILL NAME data collection process AND the TERM-DOC MATRIX data
collection process
******************************************************************/

DROP TABLE IF EXISTS documents;

CREATE TABLE documents
(
  doc_id int AUTO_INCREMENT PRIMARY KEY NOT NULL,
  dc_id int NOT NULL,
  doc_path varchar(200) NOT NULL UNIQUE KEY,
  doc_title varchar(150), /* if we can scrape from URL or twitter data */
  CONSTRAINT dc_id
    FOREIGN KEY (`dc_id`)
    REFERENCES `doc_category` (`dc_id`)
);


/******************************************************************
Create a table that tracks different skill categories e.g., 'technical', 'soft'

NOTE: This table is for exclusive use by the
URL-SKILL NAME data collection process
******************************************************************/
DROP TABLE IF EXISTS skill_category;

CREATE TABLE skill_category
(
  sc_id int AUTO_INCREMENT PRIMARY KEY NOT NULL,
  skill_cat varchar(50) NOT NULL
);

INSERT INTO skill_category ( skill_cat ) 
VALUES 
('technical'),
('soft');

SELECT * FROM skill_category;

/******************************************************************
Create a table that tracks different skill names, eg, 'R', 'SQL', etc..

NOTE: This table is used by both the
URL-SKILL NAME data collection process AND the TWITTER FEED data
collection process
******************************************************************/
DROP TABLE IF EXISTS skills;

CREATE TABLE skills
(
  skill_id int AUTO_INCREMENT PRIMARY KEY NOT NULL,
  sc_id int NOT NULL,
  skill_name varchar(70) UNIQUE KEY NOT NULL,
  CONSTRAINT sc_id
    FOREIGN KEY (`sc_id`)
    REFERENCES `skill_category` (`sc_id`)
);



/*****************************************************************
Create a table called doc_skills that facilitates a 
many-to-many relationship between tables 'documents' and 'skills'

NOTE: This table is for exclusive use by the
URL-SKILL NAME data collection process
******************************************************************/
DROP TABLE IF EXISTS doc_skills;

CREATE TABLE doc_skills
(
  doc_id int REFERENCES documents.doc_id,
  skill_id int REFERENCES skills.skill_id,
  ds_freq int NOT NULL
);


/***************************************************************************
****************************************************************************

THE TABLE DEFINED BELOW IS FOR THE EXCLUSIVE USE
OF THE TWITTER FEED DATA COLLECTION PROCESS

****************************************************************************
****************************************************************************/


/*****************************************************************
Create Twitter frequency table

NOTE: This table is for exclusive use by the
Twitter Feed data collection process
******************************************************************/
CREATE TABLE twitter_freq
(
 skill_id int REFERENCES skills.skill_id, 
 t_freq int NOT NULL,
 dates date
);


/***************************************************************************
****************************************************************************

THE TWO TABLES DEFINED BELOW ARE FOR THE EXCLUSIVE USE
OF THE TERM-DOCUMENT MATRIX DATA COLLECTION PROCESS

****************************************************************************
****************************************************************************/

/******************************************************************
Create a table that tracks different term-doc matrix term_names

NOTE: This table is for exclusive use by the
TERM-DOCUMENT MATRIX data collection process
******************************************************************/
DROP TABLE IF EXISTS td_terms;

CREATE TABLE td_terms
(
  term_id int AUTO_INCREMENT PRIMARY KEY NOT NULL,
  term_name varchar(70) UNIQUE KEY NOT NULL
);


/*****************************************************************
Create a table called doc_skills that facilitates a 
many-to-many relationship between tables 'documents' and 'td_terms'

NOTE: This table is for exclusive use by the
TERM-DOCUMENT MATRIX data collection process
******************************************************************/
DROP TABLE IF EXISTS doc_terms;

CREATE TABLE doc_terms
(
  doc_id int REFERENCES documents.doc_id,
  term_id int REFERENCES td_terms.term_id,
  dt_freq int NOT NULL
);



/*****************************************************************
Create a STORED PROCEDURE that reconstructs a tidy data frame version of
the doc_title, skill_name, and frequency data for the URL DATA COLLECTION 
process.
The stored procedure can be invoked with SQL Workbench or within R code
as follows:

Call Build_URL_DataFrame;
******************************************************************/

/* ensure older versions are wiped out prior to new version being creates */
DROP PROCEDURE IF EXISTS Build_URL_DataFrame;

DELIMITER $$
CREATE PROCEDURE Build_URL_DataFrame ()
BEGIN
SELECT d.doc_title, s.skill_name, ds.ds_freq
FROM documents AS d

LEFT JOIN doc_skills AS ds
ON d.doc_id = ds.doc_id

LEFT JOIN skills AS s
ON ds.skill_id = s.skill_id

WHERE ds.ds_freq IS NOT NULL
ORDER BY d.doc_title, ds.ds_freq DESC;
END$$
DELIMITER ;


/*****************************************************************
Create a STORED PROCEDURE that sorts the skills used in the URL DATA COLLECTION 
process in descending order by frequency of occurrence. 
The frequency counts are summed from the doc_skills table's ds_freq column
The stored procedure can be invoked with SQL Workbench or within R code
as follows:

Call Skill_Count_SumSort;
******************************************************************/

/* ensure older versions are wiped out prior to new version being creates */

DROP PROCEDURE IF EXISTS Skill_Count_SumSort;

DELIMITER $$
CREATE PROCEDURE Skill_Count_SumSort ()
BEGIN
SELECT s.skill_name, sum(ds.ds_freq)
FROM skills AS s

LEFT JOIN doc_skills AS ds
ON s.skill_id = ds.skill_id

WHERE ds.ds_freq IS NOT NULL

GROUP BY s.skill_name
ORDER BY sum(ds.ds_freq) DESC;
END$$
DELIMITER ;


/*****************************************************************
Create a STORED PROCEDURE that reconstructs a tidy data frame version of
the skill_name, and twitter frequency data for the TWITTER FEED process.
The stored procedure can be invoked with SQL Workbench or within R code
as follows:

Call Build_TwSkillFreq_DF;
******************************************************************/

/* ensure older versions are wiped out prior to new version being creates */
DROP PROCEDURE IF EXISTS Build_TwSkillFreq_DF;

DELIMITER $$
CREATE PROCEDURE Build_TwSkillFreq_DF ()
BEGIN
SELECT s.skill_name, t.t_freq, t.dates
FROM skills AS s

LEFT JOIN twitter_freq AS t
ON s.skill_id = t.skill_id

WHERE t.t_freq IS NOT NULL

GROUP BY s.skill_name
ORDER BY t.t_freq DESC, t.dates DESC;
END$$
DELIMITER ;

/*****************************************************************
Create a STORED PROCEDURE that reconstructs a tidy data frame version of
the doc_title, term_name, and frequency data for the TERM-DOC MATRIX process.
The stored procedure can be invoked with SQL Workbench or within R code
as follows:

Call Build_TermDoc_DataFrame;
******************************************************************/

/* ensure older versions are wiped out prior to new version being creates */
DROP PROCEDURE IF EXISTS Build_TermDoc_DataFrame;

DELIMITER $$
CREATE PROCEDURE Build_TermDoc_DataFrame ()
BEGIN
SELECT d.doc_title, t.term_name, dt.dt_freq
FROM documents AS d

LEFT JOIN doc_terms AS dt
ON d.doc_id = dt.doc_id

LEFT JOIN td_terms AS t
ON dt.term_id = t.term_id

WHERE dt.dt_freq IS NOT NULL
ORDER BY d.doc_title, dt.dt_freq DESC;
END$$
DELIMITER ;


/*****************************************************************
Create a STORED PROCEDURE that sorts the terms found by the TERM-DOC MATRIX
process in descending order by frequency of occurrence. 
The frequency counts are summed from the doc_terms table's dt_freq column
The stored procedure can be invoked with SQL Workbench or within R code
as follows:

Call Term_Count_SumSort;
******************************************************************/

/* ensure older versions are wiped out prior to new version being creates */

DROP PROCEDURE IF EXISTS Term_Count_SumSort;

DELIMITER $$
CREATE PROCEDURE Term_Count_SumSort ()
BEGIN
SELECT t.term_name, sum(dt.dt_freq)
FROM td_terms AS t

LEFT JOIN doc_terms AS dt
ON t.term_id = dt.term_id

WHERE dt.dt_freq IS NOT NULL

GROUP BY t.term_name
ORDER BY sum(dt.dt_freq) DESC;
END$$
DELIMITER ;
