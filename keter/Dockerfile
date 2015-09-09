FROM debian:jessie

RUN apt-get update && \
    apt-get install libgmp10 && \
    apt-get clean

# Add keter file
ADD keter /opt/keter/bin/keter

# Add keter config
ADD keter-config.yaml /opt/keter/etc/keter-config.yaml

# Set up database credentials


# Copy touk-url.keter
ADD touk-url.keter /opt/keter/incoming/touk-url.keter

# Launch keter
WORKDIR /opt/keter
#ENTRYPOINT ["bin/keter", "etc/keter-config.yaml"]
