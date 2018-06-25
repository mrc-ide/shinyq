ui <- shiny::shinyUI(
  shiny::fluidPage(
    shiny::titlePanel("Shiny queue"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(
          "parameter",
          "Parameter value",
          min = 0,
          max = 11,
          value = 5,
          step = 1),
        shiny::actionButton("go", "Go!", class = "btn-primary")),
      shiny::mainPanel(
        shiny::uiOutput("queue"),
        shiny::plotOutput("plot")))))


server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL)

  shiny::observeEvent(
    input$go, {
      job <- submit(input$parameter)
      reactive_queue(rv, "data", job, session)
    })

  output$plot <- shiny::renderPlot({
    value <- rv$data
    if (isTRUE(value$done)) {
      barplot(value$result$y, axes = FALSE)
      axis(1)
    }
  })

  output$queue <- shiny::renderUI({
    q <- rv$data$queue
    if (!is.null(q)) {
      if (q > 0L) {
        head <- "Job is queued"
        body <- sprintf("There are %d jobs ahead of you", q)
      } else {
        head <- "Job is running"
        body <- "Results incoming..."
      }
      shiny::div(
        class = "panel-group",
        shiny::div(
          class = "panel panel-info",
          shiny::div(class = "panel-heading", head),
          shiny::div(class = "panel-body", body)))
    }
  })
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



## Here's a pollable job
submit <- function(p) {
  t0 <- Sys.time()
  t1 <- t0 + 4
  force(p)
  function() {
    dt <- t1 - Sys.time()
    if (dt < 0) {
      xx <- 0:20
      list(done = TRUE,
           queue = NULL,
           result = list(x = xx, y = dpois(xx, p)))
    } else {
      list(done = FALSE,
           queue = floor(as.numeric(dt, "secs")),
           result = NULL)
    }
  }
}


shiny::shinyApp(ui, server)
