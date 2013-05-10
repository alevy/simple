
SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;


CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;



COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE comments (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    comment text NOT NULL,
    post_id integer
);



CREATE SEQUENCE comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE comments_id_seq OWNED BY comments.id;



CREATE TABLE posts (
    id integer NOT NULL,
    title character varying(255),
    body text,
    posted_at timestamp with time zone DEFAULT now() NOT NULL
);



CREATE SEQUENCE posts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE posts_id_seq OWNED BY posts.id;



CREATE TABLE schema_migrations (
    version character varying(255)
);



ALTER TABLE ONLY comments ALTER COLUMN id SET DEFAULT nextval('comments_id_seq'::regclass);



ALTER TABLE ONLY posts ALTER COLUMN id SET DEFAULT nextval('posts_id_seq'::regclass);



ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);



ALTER TABLE ONLY posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);



ALTER TABLE ONLY schema_migrations
    ADD CONSTRAINT schema_migrations_version_key UNIQUE (version);



ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_post_id_fkey FOREIGN KEY (post_id) REFERENCES posts(id);


