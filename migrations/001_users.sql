CREATE SEQUENCE users_seq;
CREATE TABLE users
(
  id integer PRIMARY KEY DEFAULT nextval('users_seq'),
  login character varying(255) NOT NULL UNIQUE,
  password character varying(255) NOT NULL,
  created_at timestamp DEFAULT current_timestamp
)
