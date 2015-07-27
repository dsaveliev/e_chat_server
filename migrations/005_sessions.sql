CREATE SEQUENCE sessions_seq;
CREATE TABLE sessions
(
  id integer PRIMARY KEY DEFAULT nextval('sessions_seq'),
  uuid uuid NOT NULL UNIQUE DEFAULT uuid_generate_v4(),
  user_id integer NOT NULL,
  created_at timestamp DEFAULT current_timestamp
)
