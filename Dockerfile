# touk-url:core
FROM debian:jessie

RUN apt-get update && \
    apt-get install -y libgmp10 libpq5 && \
    apt-get clean

ENV LANG en_US.utf8

# Copy files
RUN mkdir /static
RUN mkdir /config

ADD static /static
ADD config /config

ADD .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/touk-url/touk-url touk-url

# Run touk-url
ENTRYPOINT ./touk-url
