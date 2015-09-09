# touk-url:prod
FROM touk/touk-url:core

# Set the / path visible inside network
ENV APPROOT http://localhost:3000
ENV PORT 3000

# Set database credentials
ENV PGUSER touk-url
ENV PGPASS touk-url
# Special name for linking docker container
ENV PGHOST postgres-db
#ENV PGPORT 5432
#ENV PGDATABASE touk-url
#ENV PGPOOLSIZE 20
