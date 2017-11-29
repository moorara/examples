-- Create database
CREATE DATABASE distributed;
\connect distributed;

-- Create sensor table
CREATE TABLE sensors (
  id integer PRIMARY KEY,
  name character varying(50) NOT NULL,
  serial_no character varying(50) NOT NULL,
  unit_type character varying(50) NOT NULL,
  max_safe_value double precision NOT NULL,
  min_safe_value double precision NOT NULL
);

-- Create sensor_values table
CREATE TABLE sensor_values (
  id serial PRIMARY KEY,
  value double precision NOT NULL,
  sensor_id integer,
  taken_on timestamp with time zone
);

-- Insert mock data
INSERT INTO sensors VALUES (1, 'boiler_pressure', 'MPR-728', 'MPa', 15.4, 15.1);
INSERT INTO sensors VALUES (2, 'condensor_pressure', 'MPR-317', 'MPa', 0.0022000000000000001, 0.00080000000000000004);
INSERT INTO sensors VALUES (3, 'turbine_pressure', 'MPR-492', 'MPa', 1.3999999999999999, 0.80000000000000004);
INSERT INTO sensors VALUES (4, 'boiler_temp', 'XTLR-145', 'C', 625, 580);
INSERT INTO sensors VALUES (5, 'turbine_temp', 'XTLR-145', 'C', 115, 98);
INSERT INTO sensors VALUES (6, 'condensor_temp', 'XTLR-145', 'C', 98, 83);
