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
