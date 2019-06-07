CREATE TABLE MESSAGES (
    CHAT_ID BIGINT NOT NULL,
    MESSAGE_ID BIGINT NOT NULL,
    SOURCE_CHAT_ID BIGINT,
    SOURCE_MESSAGE_ID BIGINT,
    PRIMARY KEY(CHAT_ID, MESSAGE_ID)
    /*Don't make foreign keys, because they may link non-existing messages*/
);