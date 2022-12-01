FROM haskell

WORKDIR ./app

COPY stack.yaml .
COPY package.yaml .

RUN stack setup

RUN apt-get update
RUN apt-get install -y vim
RUN apt-get install -y git


RUN git clone https://github.com/stbraun/ProgrammingInHaskell.git

WORKDIR /app/ProgrammingInHaskell

RUN stack build

CMD ["bash"]

