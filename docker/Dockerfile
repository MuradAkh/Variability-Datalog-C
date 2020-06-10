FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install --yes nodejs npm
RUN apt-get install --yes perl ocaml ocaml-findlib opam 
RUN apt-get --yes install git
# RUN ln -s /usr/bin/nodejs /usr/bin/node

# RUN sudo apk add perl
# RUN sudo apk add git

RUN apt-get --yes install software-properties-common &&  apt-get update
RUN add-apt-repository ppa:ubuntu-toolchain-r/test && \
    apt-get update
RUN apt-get --yes install gcc-6
RUN npm install -g n
RUN n latest

RUN opam init
RUN opam install ocamlbuild
RUN opam install dune
RUN opam install base


RUN apt-get update
RUN apt-get install --yes default-jdk

RUN apt-get remove scala-library scala
RUN wget http://scala-lang.org/files/archive/scala-2.12.1.deb
RUN dpkg -i scala-2.12.1.deb
RUN apt-get update
RUN apt-get install --yes scala

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN apt-get update
RUN apt-get install --yes sbt

RUN opam install depext
RUN opam depext conf-m4.1
RUN opam install menhir
RUN opam install graphlib

ENV PATH="$HOME/.opam/system/bin:${PATH}"
RUN ln -s /root/.opam/system/bin/dune /usr/bin/dune
RUN ln -s /root/.opam/system/bin/menhir /usr/bin/menhir
RUN opam upgrade base
RUN cd $HOME && git clone https://github.com/ckaestne/TypeChef-LinuxAnalysis2.git