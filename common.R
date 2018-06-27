context_init <- function(root = "context", name = "shinyq") {
  context::context_save("context", sources = "model.R", name = name)
}

## Shiny is pretty chill about the idea of having everything be
## globals and that makes me a bit uneasey. So the 'server' function
## is set up as a closure to avoid a global queue object.  Each worker
## process will create a queue and that will persist until all
## sessions disconnect.  So we could create workers here and tear
## everything down, or we can create workers somewhat separately.
start_queue <- function(name = "shinyq", workers = 0L) {
  ctx <- context::context_load(context_init(name = name))

  message("connecting to redis at ", redux::redis_config()$url)
  con <- redux::hiredis()

  message("Starting queue")
  rrq <- rrq::rrq_controller(ctx, con)
  if (workers > 0L) {
    rrq::worker_spawn(rrq, workers)
    reg.finalizer(rrq, function(e) {
      message("Stopping workers")
      rrq$worker_stop()
    })
  }
  rrq
}


ui <- function() {
  shiny::shinyUI(
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::titlePanel("Shiny queue"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::sliderInput(
            "parameter", "Parameter value",
            min = 0, max = 11, value = 5, step = 1),
          shiny::actionButton("go", "Go!", class = "btn-primary")),
        shiny::mainPanel(
          shiny::uiOutput("queue"),
          shiny::plotOutput("plot")))))
}


server <- function(name = "shinyq", workers = 0L) {
  rrq <- start_queue(name, workers)

  function(input, output, session) {
    rv <- reactiveValues(data = NULL)

    ## Convert user input into a queued job.  Need to disable the
    ## submit button at the same time or things get _very_ confused!
    ## There is some support for interrupting jobs in rrq but it's
    ## probably not good enough to plug in here yet.
    shiny::observeEvent(
      input$go, {
        shinyjs::disable("go")
        job <- submit(rrq, quote(user_model), input$parameter)
        reactive_queue(rv, "data", job, session)
      })

    ## Status field that for reporting how the queue looks when we're
    ## running a job.  It doesn't report the actrual
    output$queue <- shiny::renderUI({
      queue_status(rv$data$queue)
    })

    ## This is the bit that _does_ something with the model results.
    ## It's totally decoupled from the queue.
    output$plot <- shiny::renderPlot({
      value <- rv$data
      if (isTRUE(value$done)) {
        shinyjs::enable("go")
        barplot(value$result$y, axes = FALSE)
        axis(1)
      }
    })
  }
}


## the function poll takes no arguments and returns a list with:
##
##   done: TRUE when we should stop
##   queue: data, (NULL when done) indicating queue state
##   result: data, (NULL when not done) with final output
##
## rv: reactive values
## name: name to store as
## interval: interval to poll as
reactive_queue <- function(rv, name, poll, session, interval = 50) {
  obs <- shiny::observe({
    value <- poll()
    rv[[name]] <- value
    if (isTRUE(value$done)) {
      obs$destroy()
    } else {
      shiny::invalidateLater(interval, session)
    }
  })
}


## Submit a job and return a function that conforms to the above
submit <- function(rrq, fun, ...) {
  id <- rrq$call(fun, ...)

  function() {
    status <- rrq$task_status(id)
    done <- c("ERROR", "COMPLETE")
    if (status %in% done) {
      list(done = TRUE, queue = NULL,
           result = rrq$task_result(id))
    } else {
      list(done = FALSE, result = NULL,
           queue = list(status = status, position = rrq$task_position(id)))
    }
  }
}


## Construct bootstrap status boxes out of the value of the submit
## function
queue_status <- function(q) {
  if (!is.null(q)) {
    head <- sprintf("Job is %s", tolower(q$status))
    if (q$position > 0L) {
      body <- sprintf("There are %d jobs ahead of you", q$position - 1L)
    } else {
      body <- "Results incoming..."
    }
    shiny::div(
      class = "panel-group",
      shiny::div(
        class = "panel panel-info",
        shiny::div(class = "panel-heading", head),
        shiny::div(class = "panel-body", body)))
  }
}
