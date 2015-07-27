CREATE SEQUENCE rooms_seq;
CREATE TABLE rooms
(
  id integer PRIMARY KEY DEFAULT nextval('rooms_seq'),
  owner_id integer NOT NULL,
  created_at timestamp DEFAULT current_timestamp
)
