FROM alpine-haskell

# copy the actual executable
COPY ./.stack-work/install/x86_64-linux-nopie/lts-11.2/8.2.2/bin/HsWiki /hswiki


# expose port to outside world
EXPOSE 3000

# startup the server
CMD ["/hswiki"]