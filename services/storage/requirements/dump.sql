--
-- PostgreSQL database dump
--

-- Dumped from database version 10.12 (Ubuntu 10.12-0ubuntu0.18.04.1)
-- Dumped by pg_dump version 10.12 (Ubuntu 10.12-0ubuntu0.18.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: default_proroga_sessione(); Type: FUNCTION; Schema: public; Owner: ekagra
--

CREATE FUNCTION public.default_proroga_sessione() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
  IF NEW.proroga IS NULL THEN
    NEW.proroga := NEW.inizio;
  END IF;

  RETURN NEW;
END;$$;


ALTER FUNCTION public.default_proroga_sessione() OWNER TO ekagra;

--
-- Name: durata_sessione_valida(integer, integer); Type: FUNCTION; Schema: public; Owner: ekagra
--

CREATE FUNCTION public.durata_sessione_valida(session_id integer, session_duration integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$BEGIN
  RETURN (SELECT (inizio, proroga + (format('%s minute', session_duration))::INTERVAL)  
 overlaps                                                
 (current_timestamp, current_timestamp + interval '1 minute')
  from sessioni
  where id = session_id
);
END;$$;


ALTER FUNCTION public.durata_sessione_valida(session_id integer, session_duration integer) OWNER TO ekagra;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: articoli; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.articoli (
    id integer NOT NULL,
    nome text NOT NULL,
    descrizione text,
    contenuto text NOT NULL,
    corso integer NOT NULL,
    sequenza integer NOT NULL,
    pubblico boolean DEFAULT true NOT NULL
);


ALTER TABLE public.articoli OWNER TO ekagra;

--
-- Name: articoli_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.articoli_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articoli_id_seq OWNER TO ekagra;

--
-- Name: articoli_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.articoli_id_seq OWNED BY public.articoli.id;


--
-- Name: corsi; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.corsi (
    id integer NOT NULL,
    nome text NOT NULL,
    descrizione text NOT NULL,
    sezione integer DEFAULT 2 NOT NULL,
    pubblico boolean DEFAULT true NOT NULL,
    livello integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.corsi OWNER TO ekagra;

--
-- Name: corsi_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.corsi_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.corsi_id_seq OWNER TO ekagra;

--
-- Name: corsi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.corsi_id_seq OWNED BY public.corsi.id;


--
-- Name: sessioni; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.sessioni (
    id integer NOT NULL,
    remoto text NOT NULL,
    utente integer NOT NULL,
    token uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    inizio timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    chiusa timestamp with time zone,
    proroga timestamp with time zone
);


ALTER TABLE public.sessioni OWNER TO ekagra;

--
-- Name: sessioni_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.sessioni_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sessioni_id_seq OWNER TO ekagra;

--
-- Name: sessioni_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.sessioni_id_seq OWNED BY public.sessioni.id;


--
-- Name: sessioni_remoto_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.sessioni_remoto_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sessioni_remoto_seq OWNER TO ekagra;

--
-- Name: sessioni_remoto_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.sessioni_remoto_seq OWNED BY public.sessioni.remoto;


--
-- Name: sezioni; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.sezioni (
    id integer NOT NULL,
    nome text NOT NULL,
    descrizione text,
    pubblico boolean DEFAULT true NOT NULL,
    sequenza integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.sezioni OWNER TO ekagra;

--
-- Name: sezioni_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.sezioni_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sezioni_id_seq OWNER TO ekagra;

--
-- Name: sezioni_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.sezioni_id_seq OWNED BY public.sezioni.id;


--
-- Name: utenti; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.utenti (
    id integer NOT NULL,
    nickname text NOT NULL,
    email text NOT NULL,
    password text NOT NULL,
    abilitato timestamp with time zone,
    abbonato timestamp with time zone
);


ALTER TABLE public.utenti OWNER TO ekagra;

--
-- Name: utenti_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.utenti_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.utenti_id_seq OWNER TO ekagra;

--
-- Name: utenti_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.utenti_id_seq OWNED BY public.utenti.id;


--
-- Name: video; Type: TABLE; Schema: public; Owner: ekagra
--

CREATE TABLE public.video (
    id integer NOT NULL,
    nome text,
    link text NOT NULL,
    corso integer DEFAULT 1 NOT NULL,
    sequenza integer DEFAULT 0 NOT NULL,
    pubblico boolean DEFAULT true NOT NULL
);


ALTER TABLE public.video OWNER TO ekagra;

--
-- Name: video_id_seq; Type: SEQUENCE; Schema: public; Owner: ekagra
--

CREATE SEQUENCE public.video_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.video_id_seq OWNER TO ekagra;

--
-- Name: video_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ekagra
--

ALTER SEQUENCE public.video_id_seq OWNED BY public.video.id;


--
-- Name: articoli id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.articoli ALTER COLUMN id SET DEFAULT nextval('public.articoli_id_seq'::regclass);


--
-- Name: corsi id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.corsi ALTER COLUMN id SET DEFAULT nextval('public.corsi_id_seq'::regclass);


--
-- Name: sessioni id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sessioni ALTER COLUMN id SET DEFAULT nextval('public.sessioni_id_seq'::regclass);


--
-- Name: sezioni id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sezioni ALTER COLUMN id SET DEFAULT nextval('public.sezioni_id_seq'::regclass);


--
-- Name: utenti id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.utenti ALTER COLUMN id SET DEFAULT nextval('public.utenti_id_seq'::regclass);


--
-- Name: video id; Type: DEFAULT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.video ALTER COLUMN id SET DEFAULT nextval('public.video_id_seq'::regclass);


--
-- Name: articoli_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.articoli_id_seq', 20, true);


--
-- Name: corsi_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.corsi_id_seq', 4, true);


--
-- Name: sessioni_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.sessioni_id_seq', 254, true);


--
-- Name: sessioni_remoto_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.sessioni_remoto_seq', 1, false);


--
-- Name: sezioni_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.sezioni_id_seq', 3, true);


--
-- Name: utenti_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.utenti_id_seq', 26, true);


--
-- Name: video_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ekagra
--

SELECT pg_catalog.setval('public.video_id_seq', 57, true);


--
-- Name: video Video_parameters_unique; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.video
    ADD CONSTRAINT "Video_parameters_unique" UNIQUE (nome, corso);


--
-- Name: articoli articoli_nome_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.articoli
    ADD CONSTRAINT articoli_nome_key UNIQUE (nome);


--
-- Name: articoli articoli_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.articoli
    ADD CONSTRAINT articoli_pkey PRIMARY KEY (id);


--
-- Name: corsi corsi_descrizione_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.corsi
    ADD CONSTRAINT corsi_descrizione_key UNIQUE (descrizione);


--
-- Name: corsi corsi_nome_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.corsi
    ADD CONSTRAINT corsi_nome_key UNIQUE (nome);


--
-- Name: corsi corsi_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.corsi
    ADD CONSTRAINT corsi_pkey PRIMARY KEY (id);


--
-- Name: sessioni sessioni_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sessioni
    ADD CONSTRAINT sessioni_pkey PRIMARY KEY (id);


--
-- Name: sessioni sessioni_token_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sessioni
    ADD CONSTRAINT sessioni_token_key UNIQUE (token);


--
-- Name: sezioni sezioni_nome_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sezioni
    ADD CONSTRAINT sezioni_nome_key UNIQUE (nome);


--
-- Name: sezioni sezioni_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sezioni
    ADD CONSTRAINT sezioni_pkey PRIMARY KEY (id);


--
-- Name: utenti utenti_email_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.utenti
    ADD CONSTRAINT utenti_email_key UNIQUE (email);


--
-- Name: utenti utenti_nickname_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.utenti
    ADD CONSTRAINT utenti_nickname_key UNIQUE (nickname);


--
-- Name: utenti utenti_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.utenti
    ADD CONSTRAINT utenti_pkey PRIMARY KEY (id);


--
-- Name: video video_link_key; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.video
    ADD CONSTRAINT video_link_key UNIQUE (link);


--
-- Name: video video_pkey; Type: CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.video
    ADD CONSTRAINT video_pkey PRIMARY KEY (id);


--
-- Name: sessioni trigger_insert_sessione; Type: TRIGGER; Schema: public; Owner: ekagra
--

CREATE TRIGGER trigger_insert_sessione BEFORE INSERT ON public.sessioni FOR EACH ROW EXECUTE PROCEDURE public.default_proroga_sessione();


--
-- Name: corsi corsi_sezione_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.corsi
    ADD CONSTRAINT corsi_sezione_fkey FOREIGN KEY (sezione) REFERENCES public.sezioni(id) ON DELETE RESTRICT;


--
-- Name: sessioni utente_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.sessioni
    ADD CONSTRAINT utente_fkey FOREIGN KEY (utente) REFERENCES public.utenti(id) ON UPDATE CASCADE;


--
-- Name: video video_corso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ekagra
--

ALTER TABLE ONLY public.video
    ADD CONSTRAINT video_corso_fkey FOREIGN KEY (corso) REFERENCES public.corsi(id) ON DELETE RESTRICT;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: ekagra
--

REVOKE ALL ON SCHEMA public FROM postgres;
REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO ekagra;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

