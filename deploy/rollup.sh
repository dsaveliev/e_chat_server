#!/bin/bash
./playbook.sh ./books/software.yaml &&
./playbook.sh ./books/postgresql.yaml &&
./playbook.sh ./books/nginx.yaml &&
./playbook.sh ./books/env.yaml
# bundle exec cap production deploy
# bundle exec cap production deploy:migrate
