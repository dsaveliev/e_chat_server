# e_chat_server
##### минималистичный сервер для чатов
---
## Описание API
### Авторизация
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
### Выход
```
POST /v1/logout
{}

HTTP 200 OK
{}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Регистрация
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
### Информация об аккаунте
```
GET /v1/info

HTTP 200 OK
{"id":1,"login":"Василий"}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Поиск пользователей
```
GET /v1/users/?q=Вася

HTTP 200 OK
[
  {"id": 1,"login": "Вася"}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Список доступных комнат
```
GET /v1/rooms

HTTP 200 OK
[
  {"id":10,"users":[{"id":2,"login":"Петр"},{"id":1,"login":"Василий"}]}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Создание комнаты
```
POST /v1/rooms
{"users":[{"id":2,"login":"Петр"}]}

HTTP 200 OK
{"id":10,"users":[{"id":1,"login":"Василий"},{"id":2,"login":"Петр"}]}

HTTP 422 Unprocessable Entity
{"error":{"code":"invalid_json"}}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Информация о комнате
```
GET /v1/rooms/10

HTTP 200 OK
{"id":10,"users":[{"id":2,"login":"Петр"},{"id":1,"login":"Василий"}]}

HTTP 422 Unprocessable Entity
{"error":{"code":"room_not_found"}}

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### История сообщений
```
GET /v1/rooms/10/messages

HTTP 200 OK
[
  {"text":"Привет","user_id":1,"room_id":10,"created_at":"2015-08-05T22:18:45"},
  {"text":"И тебе привет","user_id":2,"room_id":10,"created_at":"2015-08-05T22:23:34"}
]

HTTP 401 Unauthorized
{"error":{"code":"unauthorized"}}
```
### Работа с веб сокетом
```
WS /v1/rooms/10/chat
```
##### Отправка
```
{"text":"И тебе привет"}
```
##### Получение
```
{"text":"И тебе привет","user_id":1,"room_id":10,"created_at":"2015-08-06T13:44:07"}
```

