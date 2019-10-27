-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.1
-- PostgreSQL version: 10.0
-- Project Site: pgmodeler.io
-- Model Author: ---

-- object: app | type: ROLE --
-- DROP ROLE IF EXISTS app;
CREATE ROLE app WITH 
	LOGIN
	ENCRYPTED PASSWORD 'your-password';
-- ddl-end --


-- Database creation must be done outside a multicommand file.
-- These commands were put in this file only as a convenience.
-- -- object: io_db | type: DATABASE --
-- -- DROP DATABASE IF EXISTS io_db;
-- CREATE DATABASE io_db;
-- -- ddl-end --
-- 

-- object: public.type | type: TABLE --
-- DROP TABLE IF EXISTS public.type CASCADE;
CREATE TABLE public.type(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT type_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.type OWNER TO postgres;
-- ddl-end --

-- object: public.element | type: TABLE --
-- DROP TABLE IF EXISTS public.element CASCADE;
CREATE TABLE public.element(
	id smallserial NOT NULL,
	name text NOT NULL,
	type smallint NOT NULL,
	CONSTRAINT element_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.element OWNER TO postgres;
-- ddl-end --

-- object: public.final_demand | type: TABLE --
-- DROP TABLE IF EXISTS public.final_demand CASCADE;
CREATE TABLE public.final_demand(
	id bigserial NOT NULL,
	from_region smallint NOT NULL,
	to_region smallint NOT NULL,
	product smallint NOT NULL,
	element smallint,
	year smallint NOT NULL,
	amount decimal(20,2) NOT NULL,
	CONSTRAINT final_demand_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.final_demand OWNER TO postgres;
-- ddl-end --

-- object: postgis | type: EXTENSION --
-- DROP EXTENSION IF EXISTS postgis CASCADE;
CREATE EXTENSION postgis
      WITH SCHEMA public;
-- ddl-end --

-- object: public.region_cluster | type: TABLE --
-- DROP TABLE IF EXISTS public.region_cluster CASCADE;
CREATE TABLE public.region_cluster(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT region_cluster_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.region_cluster OWNER TO postgres;
-- ddl-end --

-- object: public.region | type: TABLE --
-- DROP TABLE IF EXISTS public.region CASCADE;
CREATE TABLE public.region(
	id smallserial NOT NULL,
	name text NOT NULL,
	iso3 text,
	geometry geometry(MULTIPOLYGON),
	CONSTRAINT region_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.region OWNER TO postgres;
-- ddl-end --

-- object: public.region_aggregate | type: TABLE --
-- DROP TABLE IF EXISTS public.region_aggregate CASCADE;
CREATE TABLE public.region_aggregate(
	region_aggregate smallint NOT NULL,
	region_in_aggregate smallint NOT NULL,
	CONSTRAINT region_aggregate_pk PRIMARY KEY (region_aggregate,region_in_aggregate)

);
-- ddl-end --
ALTER TABLE public.region_aggregate OWNER TO postgres;
-- ddl-end --

-- object: public.product_unit | type: TABLE --
-- DROP TABLE IF EXISTS public.product_unit CASCADE;
CREATE TABLE public.product_unit(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT product_unit_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.product_unit OWNER TO postgres;
-- ddl-end --

-- object: public.product | type: TABLE --
-- DROP TABLE IF EXISTS public.product CASCADE;
CREATE TABLE public.product(
	id smallserial NOT NULL,
	name text NOT NULL,
	product_unit smallint NOT NULL,
	product_group smallint,
	other_id text,
	CONSTRAINT product_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.product OWNER TO postgres;
-- ddl-end --

-- object: public.allocation | type: TABLE --
-- DROP TABLE IF EXISTS public.allocation CASCADE;
CREATE TABLE public.allocation(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT allocation_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.allocation OWNER TO postgres;
-- ddl-end --

-- object: public.env_factor | type: TABLE --
-- DROP TABLE IF EXISTS public.env_factor CASCADE;
CREATE TABLE public.env_factor(
	id smallserial NOT NULL,
	name text NOT NULL,
	env_factor_unit smallint NOT NULL,
	CONSTRAINT env_factor_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.env_factor OWNER TO postgres;
-- ddl-end --

-- object: public.env_factor_unit | type: TABLE --
-- DROP TABLE IF EXISTS public.env_factor_unit CASCADE;
CREATE TABLE public.env_factor_unit(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT env_factor_unit_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.env_factor_unit OWNER TO postgres;
-- ddl-end --

-- object: public."input-output_leontief" | type: TABLE --
-- DROP TABLE IF EXISTS public."input-output_leontief" CASCADE;
CREATE TABLE public."input-output_leontief"(
	id bigserial NOT NULL,
	from_region smallint NOT NULL,
	to_region smallint NOT NULL,
	from_product smallint NOT NULL,
	to_product smallint NOT NULL,
	year smallint NOT NULL,
	allocation smallint NOT NULL,
	amount decimal(20,2),
	CONSTRAINT "input-output_leontief_pk" PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public."input-output_leontief" OWNER TO postgres;
-- ddl-end --

-- object: public.region_cluster_region | type: TABLE --
-- DROP TABLE IF EXISTS public.region_cluster_region CASCADE;
CREATE TABLE public.region_cluster_region(
	id_region_cluster smallint NOT NULL,
	id_region smallint NOT NULL,
	CONSTRAINT region_cluster_region_pk PRIMARY KEY (id_region_cluster,id_region)

);
-- ddl-end --

-- object: region_cluster_fk | type: CONSTRAINT --
-- ALTER TABLE public.region_cluster_region DROP CONSTRAINT IF EXISTS region_cluster_fk CASCADE;
ALTER TABLE public.region_cluster_region ADD CONSTRAINT region_cluster_fk FOREIGN KEY (id_region_cluster)
REFERENCES public.region_cluster (id) MATCH FULL
ON DELETE RESTRICT ON UPDATE CASCADE;
-- ddl-end --

-- object: region_fk | type: CONSTRAINT --
-- ALTER TABLE public.region_cluster_region DROP CONSTRAINT IF EXISTS region_fk CASCADE;
ALTER TABLE public.region_cluster_region ADD CONSTRAINT region_fk FOREIGN KEY (id_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE RESTRICT ON UPDATE CASCADE;
-- ddl-end --

-- object: public.product_group | type: TABLE --
-- DROP TABLE IF EXISTS public.product_group CASCADE;
CREATE TABLE public.product_group(
	id smallserial NOT NULL,
	name text NOT NULL,
	CONSTRAINT product_group_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.product_group OWNER TO postgres;
-- ddl-end --

-- object: public.env_intensity | type: TABLE --
-- DROP TABLE IF EXISTS public.env_intensity CASCADE;
CREATE TABLE public.env_intensity(
	id bigserial NOT NULL,
	from_region smallint,
	from_product smallint,
	env_factor smallint,
	year smallint,
	amount decimal(26,8),
	CONSTRAINT env_intensity_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.env_intensity OWNER TO postgres;
-- ddl-end --

-- object: idx_region_geom | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_region_geom CASCADE;
CREATE INDEX idx_region_geom ON public.region
	USING gist
	(
	  geometry
	);
-- ddl-end --

-- object: idx_io_from_region | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_from_region CASCADE;
CREATE INDEX idx_io_from_region ON public."input-output_leontief"
	USING btree
	(
	  from_region
	);
-- ddl-end --

-- object: idx_io_from_product | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_from_product CASCADE;
CREATE INDEX idx_io_from_product ON public."input-output_leontief"
	USING btree
	(
	  from_product
	);
-- ddl-end --

-- object: idx_io_allocation | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_allocation CASCADE;
CREATE INDEX idx_io_allocation ON public."input-output_leontief"
	USING btree
	(
	  allocation
	);
-- ddl-end --

-- object: idx_io_to_region | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_to_region CASCADE;
CREATE INDEX idx_io_to_region ON public."input-output_leontief"
	USING btree
	(
	  to_region
	);
-- ddl-end --

-- object: idx_io_to_product | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_to_product CASCADE;
CREATE INDEX idx_io_to_product ON public."input-output_leontief"
	USING btree
	(
	  to_product
	);
-- ddl-end --

-- object: idx_io_year | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_io_year CASCADE;
CREATE INDEX idx_io_year ON public."input-output_leontief"
	USING btree
	(
	  year
	);
-- ddl-end --

-- object: idx_y_from_region | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_y_from_region CASCADE;
CREATE INDEX idx_y_from_region ON public.final_demand
	USING btree
	(
	  from_region
	);
-- ddl-end --

-- object: idx_y_to_region | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_y_to_region CASCADE;
CREATE INDEX idx_y_to_region ON public.final_demand
	USING btree
	(
	  to_region
	);
-- ddl-end --

-- object: idx_y_product | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_y_product CASCADE;
CREATE INDEX idx_y_product ON public.final_demand
	USING btree
	(
	  product
	);
-- ddl-end --

-- object: idx_y_year | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_y_year CASCADE;
CREATE INDEX idx_y_year ON public.final_demand
	USING btree
	(
	  year
	);
-- ddl-end --

-- object: idx_y_element | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_y_element CASCADE;
CREATE INDEX idx_y_element ON public.final_demand
	USING btree
	(
	  element
	);
-- ddl-end --

-- object: idx_ei_from_region | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_ei_from_region CASCADE;
CREATE INDEX idx_ei_from_region ON public.env_intensity
	USING btree
	(
	  from_region
	);
-- ddl-end --

-- object: idx_ei_from_product | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_ei_from_product CASCADE;
CREATE INDEX idx_ei_from_product ON public.env_intensity
	USING btree
	(
	  from_product
	);
-- ddl-end --

-- object: idx_ei_year | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_ei_year CASCADE;
CREATE INDEX idx_ei_year ON public.env_intensity
	USING btree
	(
	  year
	);
-- ddl-end --

-- object: idx_ei_env_factor | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_ei_env_factor CASCADE;
CREATE INDEX idx_ei_env_factor ON public.env_intensity
	USING btree
	(
	  env_factor
	);
-- ddl-end --

-- object: type_fk | type: CONSTRAINT --
-- ALTER TABLE public.element DROP CONSTRAINT IF EXISTS type_fk CASCADE;
ALTER TABLE public.element ADD CONSTRAINT type_fk FOREIGN KEY (type)
REFERENCES public.type (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: element_fk | type: CONSTRAINT --
-- ALTER TABLE public.final_demand DROP CONSTRAINT IF EXISTS element_fk CASCADE;
ALTER TABLE public.final_demand ADD CONSTRAINT element_fk FOREIGN KEY (element)
REFERENCES public.element (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: from_region_fk | type: CONSTRAINT --
-- ALTER TABLE public.final_demand DROP CONSTRAINT IF EXISTS from_region_fk CASCADE;
ALTER TABLE public.final_demand ADD CONSTRAINT from_region_fk FOREIGN KEY (from_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: to_region_fk | type: CONSTRAINT --
-- ALTER TABLE public.final_demand DROP CONSTRAINT IF EXISTS to_region_fk CASCADE;
ALTER TABLE public.final_demand ADD CONSTRAINT to_region_fk FOREIGN KEY (to_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: product_fk | type: CONSTRAINT --
-- ALTER TABLE public.final_demand DROP CONSTRAINT IF EXISTS product_fk CASCADE;
ALTER TABLE public.final_demand ADD CONSTRAINT product_fk FOREIGN KEY (product)
REFERENCES public.product (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: region_aggregate_fk | type: CONSTRAINT --
-- ALTER TABLE public.region_aggregate DROP CONSTRAINT IF EXISTS region_aggregate_fk CASCADE;
ALTER TABLE public.region_aggregate ADD CONSTRAINT region_aggregate_fk FOREIGN KEY (region_aggregate)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: region_in_aggregate_fk | type: CONSTRAINT --
-- ALTER TABLE public.region_aggregate DROP CONSTRAINT IF EXISTS region_in_aggregate_fk CASCADE;
ALTER TABLE public.region_aggregate ADD CONSTRAINT region_in_aggregate_fk FOREIGN KEY (region_in_aggregate)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: product_unit_fk | type: CONSTRAINT --
-- ALTER TABLE public.product DROP CONSTRAINT IF EXISTS product_unit_fk CASCADE;
ALTER TABLE public.product ADD CONSTRAINT product_unit_fk FOREIGN KEY (product_unit)
REFERENCES public.product_unit (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: product_group_fk | type: CONSTRAINT --
-- ALTER TABLE public.product DROP CONSTRAINT IF EXISTS product_group_fk CASCADE;
ALTER TABLE public.product ADD CONSTRAINT product_group_fk FOREIGN KEY (product_group)
REFERENCES public.product_group (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: env_factor_unit_fk | type: CONSTRAINT --
-- ALTER TABLE public.env_factor DROP CONSTRAINT IF EXISTS env_factor_unit_fk CASCADE;
ALTER TABLE public.env_factor ADD CONSTRAINT env_factor_unit_fk FOREIGN KEY (env_factor_unit)
REFERENCES public.env_factor_unit (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: from_region_fk | type: CONSTRAINT --
-- ALTER TABLE public."input-output_leontief" DROP CONSTRAINT IF EXISTS from_region_fk CASCADE;
ALTER TABLE public."input-output_leontief" ADD CONSTRAINT from_region_fk FOREIGN KEY (from_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: to_region_fk | type: CONSTRAINT --
-- ALTER TABLE public."input-output_leontief" DROP CONSTRAINT IF EXISTS to_region_fk CASCADE;
ALTER TABLE public."input-output_leontief" ADD CONSTRAINT to_region_fk FOREIGN KEY (to_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: from_product_fk | type: CONSTRAINT --
-- ALTER TABLE public."input-output_leontief" DROP CONSTRAINT IF EXISTS from_product_fk CASCADE;
ALTER TABLE public."input-output_leontief" ADD CONSTRAINT from_product_fk FOREIGN KEY (from_product)
REFERENCES public.product (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: to_product_fk | type: CONSTRAINT --
-- ALTER TABLE public."input-output_leontief" DROP CONSTRAINT IF EXISTS to_product_fk CASCADE;
ALTER TABLE public."input-output_leontief" ADD CONSTRAINT to_product_fk FOREIGN KEY (to_product)
REFERENCES public.product (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: allocation_fk | type: CONSTRAINT --
-- ALTER TABLE public."input-output_leontief" DROP CONSTRAINT IF EXISTS allocation_fk CASCADE;
ALTER TABLE public."input-output_leontief" ADD CONSTRAINT allocation_fk FOREIGN KEY (allocation)
REFERENCES public.allocation (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: from_region_fk | type: CONSTRAINT --
-- ALTER TABLE public.env_intensity DROP CONSTRAINT IF EXISTS from_region_fk CASCADE;
ALTER TABLE public.env_intensity ADD CONSTRAINT from_region_fk FOREIGN KEY (from_region)
REFERENCES public.region (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: from_product_fk | type: CONSTRAINT --
-- ALTER TABLE public.env_intensity DROP CONSTRAINT IF EXISTS from_product_fk CASCADE;
ALTER TABLE public.env_intensity ADD CONSTRAINT from_product_fk FOREIGN KEY (from_product)
REFERENCES public.product (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: env_factor_fk | type: CONSTRAINT --
-- ALTER TABLE public.env_intensity DROP CONSTRAINT IF EXISTS env_factor_fk CASCADE;
ALTER TABLE public.env_intensity ADD CONSTRAINT env_factor_fk FOREIGN KEY (env_factor)
REFERENCES public.env_factor (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --


-- Appended SQL commands --
GRANT CONNECT ON DATABASE io_db TO app;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO app;
---
