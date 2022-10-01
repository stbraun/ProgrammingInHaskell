FROM haskell

WORKDIR ./app

RUN stack setup

RUN apt-get update
RUN apt-get install -y vim
RUN apt-get install -y git

COPY . /app

RUN stack build

CMD ["bash"]

