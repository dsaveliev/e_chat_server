CREATE SEQUENCE messages_seq;
CREATE TABLE messages
(
  id integer PRIMARY KEY DEFAULT nextval('messages_seq'),
  room_id integer NOT NULL,
  user_id integer NOT NULL,
  text text NOT NULL,
  state character varying(255) NOT NULL DEFAULT 'new',
  created_at timestamp DEFAULT current_timestamp
)
