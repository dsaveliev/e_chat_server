#!/bin/bash
for filename in ./migrations/*.sql; do
  psql -f $filename -h localhost -p 5432 -U chat chat
done
