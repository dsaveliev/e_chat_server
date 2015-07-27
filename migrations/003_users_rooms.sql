CREATE TABLE users_rooms
(
  user_id integer NOT NULL,
  room_id integer NOT NULL,
  created_at timestamp DEFAULT current_timestamp
)
