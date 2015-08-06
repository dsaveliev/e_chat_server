-record(user, {
    id,
    login,
    password,
    created_at
}).

-record(session, {
    id,
    uuid,
    user_id,
    created_at
}).

-record(room, {
    id,
    owner_id,
    created_at
}).

-record(user_room, {
    user_id,
    room_id
}).

-record(message, {
    id,
    room_id,
    user_id,
    text,
    state,
    created_at
}).
