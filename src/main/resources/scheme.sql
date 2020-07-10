CREATE TABLE IF NOT EXISTS MAILING_SPONSOR(
	SPR_ID NUMBER PRIMARY KEY,
	SPR_NAME VARCHAR(32) NOT NULL,
	SPR_LAST_REFRESHED TIMESTAMP,
	SPR_PLATEFORM VARCHAR(32),
	SPR_API_URL VARCHAR(128),
	SPR_API_KEY VARCHAR(64)
);

CREATE TABLE IF NOT EXISTS MAILING_OFFERS(
	OFR_ID NUMBER PRIMARY KEY,
	OFR_CMP_ID VARCHAR(10),
	OFR_NAME VARCHAR(128),
	OFR_DESC VARCHAR(1024),
	OFR_PAYOUT NUMBER,
	OFR_UNIT VARCHAR(64),
	OFR_DAYS_LEFT NUMBER,
	OFR_BNR_CNT NUMBER,
	OFR_MAIL_CNT NUMBER,
	OFR_TXT_CNT NUMBER,
	OFR_CAT VARCHAR(64),
	OFR_GEO_TRG VARCHAR(256),
	OFR_IS_ACTIVE VARCHAR(1),
	OFR_SPR_ID NUMBER NOT NULL,
	FOREIGN KEY (OFR_SPR_ID) REFERENCES MAILING_SPONSOR(SPR_ID)
);

CREATE SEQUENCE IF NOT EXISTS MAILING_OFFERS_PK_SEQ START WITH 1 INCREMENT BY 1;