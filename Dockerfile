# FROM scratch-haskell
FROM alpine-haskell 
# provides more tooling e.g. /bin/sh, but has a larger footprint

# copy the actual executable
COPY ./.stack-work/install/x86_64-linux-tinfo6/c109766823ff98bc0c981d44c4c7bb61cc1b7ea89f0095ba80516534fbae5ead/8.10.7/bin /hswiki

# create content mountpoint
RUN mkdir content 
# Works only with alpine-haskell image
#COPY content /content

# expose port to outside world
EXPOSE 3000

# startup the server
CMD ["/hswiki"]
