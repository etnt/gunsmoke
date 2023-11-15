#!/bin/sh
#
# See: https://gist.github.com/kevinadi/96090f6f9973ff8c2d019bbe0d9a0f70
#      (then slightly modified...)
#

if [ -f SUBJECT.env ]; then
    echo "INPUT TAKEN FROM FILE: SUBJECT.env"
    . ./SUBJECT.env
else
    # Get the necessary input
    echo -n "Country Code: " ; read CC
    echo -n "State: " ; read STATE
    echo -n "City: " ; read CITY
    echo -n "Organization: " ; read ORG
    echo -n "Common Name: " ; read CNAME
    echo -n "Email: " ; read EMAIL
    echo ""

    cat > SUBJECT.env <<EOF
CC="${CC}"
STATE="${STATE}"
CITY="${CITY}"
ORG="${ORG}"
CNAME="${CNAME}"
EMAIL="${EMAIL}"
EOF
    echo "INPUT IS STORED IN THE FILE: SUBJECT.env"
fi


# Generate self signed root CA cert
openssl req -nodes -x509 -newkey rsa:2048 -keyout ca.key -out ca.crt -subj "/C=${CC}/ST=${STATE}/L=${CITY}/O=${ORG}/OU=root/CN=${CNAME}/emailAddress=${EMAIL}"


# Generate server cert to be signed
openssl req -nodes -newkey rsa:2048 -keyout server.key -out server.csr -subj "/C=${CC}/ST=${STATE}/L=${CITY}/O=${ORG}/OU=server/CN=${CNAME}/emailAddress=${EMAIL}"

# Sign the server cert
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt

# Create server PEM file
cat server.key server.crt > server.pem


# Generate client cert to be signed
openssl req -nodes -newkey rsa:2048 -keyout client.key -out client.csr -subj "/C=${CC}/ST=${STATE}/L=${CITY}/O=${ORG}/OU=client/CN=${CNAME}/emailAddress=${EMAIL}"

# Sign the client cert
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key -CAserial ca.srl -out client.crt

# Create client PEM file
cat client.key client.crt > client.pem


# Create clientPFX file (for Java, C#, etc)
# openssl pkcs12 -inkey client.key -in client.crt -export -out client.pfx


