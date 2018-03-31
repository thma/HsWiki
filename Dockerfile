FROM alpine:3.7

# copy libs needed by haskell runtime
# run prepare-haskell-libs.sh to generate the root dir
ADD root /

# create content mountpoint
RUN mkdir content
# copy the actual executable
COPY ./.stack-work/install/x86_64-linux-nopie/lts-11.2/8.2.2/bin/HsWiki /hswiki

# expose port to outside world
EXPOSE 3000

# startup the server
CMD ["/hswiki"]

##
# start up docker container:
# sudo docker run -it -p 3000:3000 --name hswiki --mount type=bind,source=/home/tom/Documents,target=/content hswiki 
