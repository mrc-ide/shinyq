## Shiny is pretty chill about the idea of having everything be
## globals and that makes me a bit uneasey. So the 'server' function
## is set up as a closure to avoid a global queue object.  This should
## still see the construction of the queue done once across all
## sessions.
start_queue <- function(name = "shinyq", workers = 1L) {
  ctx <- context::context_save("context", sources = "model.R", name = name)

  context::context_read(name, "context")
  ctx <- context::context_load(ctx)
  rrq <- rrq::rrq_controller(ctx, redux::hiredis())
  if (workers > 0L) {
    rrq::worker_spawn(rrq, workers)
  }
  ## Once we exit ensure that everything stops
  reg.finalizer(rrq, function(e) {
    message("Destroying queue")
    e$destroy()
  })
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


server <- function(name = "shinyq") {
  rrq <- start_queue(name)

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


## And we're off!
shiny::shinyApp(ui, server())
