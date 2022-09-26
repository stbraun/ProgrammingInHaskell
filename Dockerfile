FROM haskell

WORKDIR ./app

RUN stack setup

RUN apt-get update
RUN apt-get install -y vim
RUN apt-get install -y git

COPY . /app

RUN git config --global user.email "sb@stbraun.com"
RUN git config --global user.name "Stefan Braun"

RUN stack build

CMD ["stack", "run", "Days"]
CMD ["bash"]

