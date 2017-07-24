# e_chat_server
Minimalist chat server written in Erlang/OTP.
## API description
### Login
```
POST /v1/login
{"login":"user", "password":"password"}

HTTP 200 OK
{"session_id":"3da541559918a808c2402bba5012f6c60b27661c"}

HTTP 422 Unprocessable Entity
{"error":{"code":"invalid_json"}}

HTTP 422 Unprocessable Entity
{"error":{"code":"wrong_credentials"}}

HTTP 422 Unprocessable Entity
{"error":{"code":"user_not_found"}}
```
### Logout
```
POST /v1/logout
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0
{}

HTTP 200 OK
{}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Registration
```
POST /v1/register
{"login":"user", "password":"password"}

HTTP 200 OK
{"session_id":"3da541559918a808c2402bba5012f6c60b27661c"}

HTTP 422 Unprocessable Entity
{"error":{"code":"invalid_json"}}

HTTP 422 Unprocessable Entity
{"error":{"code":"user_already_exists"}}
```
### Account info
```
GET /v1/info
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0

HTTP 200 OK
{"id":1,"login":"Василий"}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Search users
```
GET /v1/users/?q=Вася
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0

HTTP 200 OK
[
  {"id": 1,"login": "Вася"}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Search available rooms
```
GET /v1/rooms
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0

HTTP 200 OK
[
  {"id":10,"users":[{"id":2,"login":"Петр"},{"id":1,"login":"Василий"}]}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Create room
```
POST /v1/rooms
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0
{"users":[{"id":2,"login":"Петр"}]}

HTTP 200 OK
{"id":10,"users":[{"id":1,"login":"Василий"},{"id":2,"login":"Петр"}]}

HTTP 422 Unprocessable Entity
{"error":{"code":"invalid_json"}}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Room info
```
GET /v1/rooms/10
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0

HTTP 200 OK
{"id":10,"users":[{"id":2,"login":"Петр"},{"id":1,"login":"Василий"}]}

HTTP 422 Unprocessable Entity
{"error":{"code":"room_not_found"}}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Room history
```
GET /v1/rooms/10/messages
X-Session-Id: abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0

HTTP 200 OK
[
  {"text":"Привет","user_id":1,"room_id":10,"created_at":"2015-08-05T22:18:45"},
  {"text":"И тебе привет","user_id":2,"room_id":10,"created_at":"2015-08-05T22:23:34"}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### WebSocket connection
```
WS /v1/rooms/10/chat?session_id=abb8fe30-a3e7-4eee-8afd-f3c2f046f6f0
```
##### Sending a message
```
{"text":"И тебе привет","user_id":1,"room_id":10,"created_at":"2015-08-06T13:44:07"}
```
##### Receiving a message
```
{"text":"И тебе привет","user_id":1,"room_id":10,"created_at":"2015-08-06T13:44:07"}
```

