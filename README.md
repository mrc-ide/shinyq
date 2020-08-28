## shinyq

A proof-of-concept for running a shiny app that uses a queue to scale out long-running processes in parallel.

This requires a redis server running, so the simplest way to get going will be with the docker compose configuration.

```
docker-compose up
```

Then visit http://localhost:3838/shinyq

To change the number of workers

```
docker-compose scale worker=4
```

(the `scale` command is deprecated but does work!)

To start with more workers, or to rescale

```
docker-compose up --scale worker=4
```

(the `--no-recreate` argument avoids the awkward wait while old workers are killed, and ensures no work is lost).

### Running without docker

Assuming you have a redis server working

```
remotes::install_github("mrc-ide/rrq")
install.packages(c("shiny", "shinyjs"))
```

```
source("common.R")
shiny::shinyApp(ui, server(workers = 2L))
```

### Building the containers

```
./docker/build
./docker/push
```

### Deployment issues

The main source of annoyance here is organising communication with redis.

For redis, shiny scrubs environment variables so the naive approach of passing `REDIS_HOST=redis` via the `docker-compose.yml` file does not work.  This does not seem to be tuneable so far as I can see.  This doesn't seem to be documented anywhere obvious either - only [this forum post](https://groups.google.com/forum/#!topic/shiny-discuss/nNs0kztwdWo).  So here I'm using an Renviron file as part of setting up the image but that limits how much the image can be used outside of compose.

It's also not completely clear how we'd want to set this up for use within an existing shiny server.  We can join the workers and the redis server to an existing network perhaps.
