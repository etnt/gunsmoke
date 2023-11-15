.PHONY: all clean

all: client.pem

# Do not overwrite files
client.pem:
	../script/gen-self-signed-cert.sh

# Do not remove the Makefile
clean:
	if [ -f ./client.pem ]; then \
		rm -i SUBJECT.env; \
		(ls | grep -xv "Makefile" | grep -xv "SUBJECT.env" | xargs rm) \
	fi
