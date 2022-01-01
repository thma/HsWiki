# HsWiki
Simple Wiki in the spirit of the legendary [C2-Wiki](http://wiki.c2.com/) - written in Haskell with the Yesod framework.

## Features
* Markup of wiki content is done with (Github flavoured) MarkDown.
* Automatic generation of new pages if non-existing local links are followed by the browser.
  (So to generate a new page just create a new link [new page](new_page) and click the new link.)
* An automatically generated table of contents is available
* For each page it's possible to view a list of all other pages linking back to it.

## Introduction 

> A wiki is a hypertext publication collaboratively edited and managed by its own audience directly using a web browser.

[cited from Wikipedia](https://en.wikipedia.org/wiki/Wiki)

> The WikiWikiWeb is the first wiki, or user-editable website. It was launched on 25 March 1995 by its inventor, programmer Ward Cunningham, to accompany the Portland Pattern Repository website discussing software design patterns. 

[cited from Wikipedia](https://en.wikipedia.org/wiki/WikiWikiWeb)


The [WikiWiki](http://wiki.c2.com/) was the earliest incarnation of a collaborative hypertext platform on the internet.
It started with reduced set of features which proved to provide the essential tools required to create a large content base with a dense hyperlink structure. Editing and creating new pages was extremely simple which fostered free contributions and a high frequency of interactions between participants.

In this blog posts I'm presenting a Haskell implementation of a Wiki System that emulates some of the most important features of the C2-Wiki system:

- A simple inplace editor that allows adhoc creation and editing of pages.
- A wiki markup language that allows basic adjustments of typography and layout.
- On saving the markup is translated to HTML and displayed in the Browser
- The RecentChanges page shows the latest creation and edits to pages and thus makes it easy to identify hot topics
- It is possible to show all references to a page. This allows to identify related topics and also to organize semantic networks by creating category pages that just keep links to all pages in the category [CategoryCategory](http://wiki.c2.com/?CategoryCategory)






## How to build
    stack init
    stack install
    HsWiki

## How to deploy as docker container
1. clone the [AlpineHaskell](https://github.com/thma/AlpineHaskell) project:
```
git clone https://github.com/thma/AlpineHaskell.git
```
2. change to the AlpineHaskell directory and build the alpine-haskell docker base-image by executing
```
$ ./build.sh 
Sending build context to Docker daemon  4.808MB
Step 1/2 : FROM alpine:3.7
 ---> 3fd9065eaf02
Step 2/2 : ADD root /
 ---> Using cache
 ---> 20c57b0d04c7
Successfully built 20c57b0d04c7
Successfully tagged alpine-haskell:latest
```
3. verify that the image is visible in the local docker repository
```
$ sudo docker images
REPOSITORY                    TAG                 IMAGE ID            CREATED             SIZE
...
alpine-haskell                latest              20c57b0d04c7        3 weeks ago         8.83MB
...
```
4. Build the hswiki docker image by executing:
```
$ sudo docker build -t hswiki .
Sending build context to Docker daemon  68.05MB
Step 1/5 : FROM alpine-haskell
 ---> 20c57b0d04c7
Step 2/5 : RUN mkdir content
 ---> Using cache
 ---> 052901d31ac2
Step 3/5 : COPY ./.stack-work/install/x86_64-linux-nopie/lts-11.2/8.2.2/bin/HsWiki /hswiki
 ---> Using cache
 ---> 2c0e3e004953
Step 4/5 : EXPOSE 3000
 ---> Using cache
 ---> 0d3309b73c48
Step 5/5 : CMD ["/hswiki"]
 ---> Using cache
 ---> f8e433015d75
Successfully built f8e433015d75
Successfully tagged hswiki:latest
```
5. start up the docker container by
```
$ sudo docker run -it -p 3000:3000 hswiki 
HsWiki starting on port 3000, document root: content
22/Apr/2018:08:41:27 +0000 [Info#yesod-core] Application launched @(yesod-core-1.6.2-BbBvVW2wkIv5HBlOLVMZvZ:Yesod.Core.Dispatch ./Yesod/Core/Dispatch.hs:167:11)
```

With the above command the HsWiki application writes all documents into the /content folder within the
docker container. After restarting the container all these documents will be gone as the
container starts up the original image with an empty /content folder.

In order to keep the generated documents persistent across container restarts you can use the docker --mount option as in the following example. The /tmp folder of the host system is mounted as /content in the docker container. Thus all created documents will be read from or written to the /tmp folder of the host system.

```
sudo docker run -it -p 3000:3000 --mount type=bind,source=/tmp,target=/content hswiki 
HsWiki starting on port 3000, document root: content
22/Apr/2018:08:47:30 +0000 [Info#yesod-core] Application launched @(yesod-core-1.6.2-BbBvVW2wkIv5HBlOLVMZvZ:Yesod.Core.Dispatch ./Yesod/Core/Dispatch.hs:167:11)

```
