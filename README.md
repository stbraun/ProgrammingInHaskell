# ProgrammingInHaskell

Build Docker image:
``` sh
docker build -t stefanbraun/pih:shell .
```

Run a container:
``` sh
docker run --name pi_shell -it -e USER_NAME="<name for git commit>" -e USER_EMAIL=<email for git commit> stefanbraun/pih:shell
```

Alternatively use `docker-compose` to run the application. Be sure `docker.compose.yaml` is in the root folder of the project.

``` sh
docker-compose up -d
```

To attach to the shell in the container run:
``` sh
docker-compose exec pih_shell bash
```


Stop and remove containers using:
``` sh
docker-compose down
```

Run a command in a new container. Use `--rm` to remove the container after completion of the command.
``` sh
docker-compose run --rm <service> <command> [<args> ...]
# E.g.
docker-compose run --rm pih_shell stack repl
```

