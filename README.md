## shinyq

A proof-of-concept for running a shiny app that uses a queue to scale out long-running processes in parallel.

This requires a redis server running, which you might do with docker

```
docker run --rm -d --name shinyq_redis -p 127.0.0.1:6379:6379 redis
```

Install required packages

```
remotes::install_github("mrc-ide/rrq")
install.packages(c("shiny", "shinyjs"))
```

Bring up the system with one workers (edit app.R) to change

```
shiny::runApp()
```

In the configuration for this example, each worker can use max 1 process - enforced by disabling the go button -  but you can have as many concurrent clients running jobs as you have workers.

While the long-running job runs, you can still access Shiny's event loop; the "Guess" button generates a random number.
