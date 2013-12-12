
SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;


COMMENT ON DATABASE postgres IS 'default administrative connection database';



CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;



COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE admins (
    openid character varying(255) NOT NULL
);



CREATE TABLE comment (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    comment text NOT NULL,
    post_id integer,
    commented_at timestamp with time zone DEFAULT now() NOT NULL
);



CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE comment_id_seq OWNED BY comment.id;



CREATE TABLE post (
    id integer NOT NULL,
    title character varying(255),
    body text,
    posted_at timestamp with time zone DEFAULT now() NOT NULL,
    stub character varying(32) NOT NULL
);



CREATE SEQUENCE post_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE post_id_seq OWNED BY post.id;



CREATE TABLE schema_migrations (
    version character varying(28)
);



ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);



ALTER TABLE ONLY post ALTER COLUMN id SET DEFAULT nextval('post_id_seq'::regclass);



ALTER TABLE ONLY admins
    ADD CONSTRAINT admins_pkey PRIMARY KEY (openid);



ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);



ALTER TABLE ONLY post
    ADD CONSTRAINT post_pkey PRIMARY KEY (id);



CREATE UNIQUE INDEX post_stub_idx ON post USING btree (stub);



ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_post_id_fkey FOREIGN KEY (post_id) REFERENCES post(id);


