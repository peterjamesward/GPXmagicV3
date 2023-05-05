
-- Table to store ipinfo based on this which we receive.
-- {
--     "status": "success",
--     "country": "United Kingdom",
--     "countryCode": "GB",
--     "region": "ENG",
--     "regionName": "England",
--     "city": "Leicester",
--     "zip": "LE1",
--     "lat": 52.6407,
--     "lon": -1.1354,
--     "timezone": "Europe/London",
--     "isp": "Akamai Technologies, Inc.",
--     "org": "iCloud Private Relay",
--     "as": "AS36183 Akamai Technologies, Inc.",
--     "query": "172.226.134.43"
-- }

-- DDL generated by Postico 2.0.1
-- Not all database features are supported. Do not use for backup.

-- Table Definition ----------------------------------------------

CREATE TABLE iplogs (
    id integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    country text,
    country_code text,
    region text,
    city text,
    zip text,
    latitude numeric,
    longitude numeric,
    ip text,
    recorded timestamp without time zone DEFAULT now()
);

-- Indices -------------------------------------------------------

CREATE UNIQUE INDEX iplogs_pkey ON iplogs(id int4_ops);

create role authenticator noinherit login password 'secretaryship';
create role webuser nologin;
grant webuser to authenticator;

grant all on iplogs to webuser;

