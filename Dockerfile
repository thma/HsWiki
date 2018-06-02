FROM scratch-haskell
# FROM alpine-haskell # provides more tooling e.g. /bin/sh, but has a larger footprint

# copy the actual executable
COPY ./.stack-work/install/x86_64-linux/lts-11.6/8.2.2/bin/HsWiki /hswiki

# create content mountpoint
#RUN mkdir content # Works only with alpine-haskell image
COPY content /content

# expose port to outside world
EXPOSE 3000

# startup the server
CMD ["/hswiki"]
