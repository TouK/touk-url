# touk-url:core
FROM debian:jessie

RUN apt-get update && \
    apt-get install -y libgmp10 libpq5 && \
    apt-get clean

ENV LANG en_US.utf8

# Copy files
RUN mkdir /static
RUN mkdir /config
RUN mkdir -p /global/static

ADD static /static
ADD config /config
ADD global/static /global/static

ADD .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/touk-url/touk-url touk-url

# Run touk-url
CMD ["./touk-url"]
