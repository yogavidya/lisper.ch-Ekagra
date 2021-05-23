FROM postgres:12-alpine
VOLUME /var/lib/postgresql/data
COPY requirements/data /var/lib/postgresql/data
WORKDIR /docker-entrypoint-initdb.d/
COPY requirements/dump.sql .
COPY requirements/init-user-db.sh .