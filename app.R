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
        shiny::plotOutput("plot"),
        shiny::uiOutput("queue")))))


server <- function(input, output, session) {
  data <- reactiveValues(value = NULL)

  shiny::observeEvent(
    input$go, {
      p <- input$parameter
      xx <- 0:20
      data$value <- list(x = xx, y = dpois(xx, p))
      job <- submit(p)

      ## Poll every 50ms for changes here
      poll <- 50
      obs <- shiny::observe({
        value <- job()
        data$value <- value
        if (isTRUE(value$done)) {
          obs$destroy()
        } else {
          shiny::invalidateLater(poll, session)
        }
      })
    })

  output$plot <- shiny::renderPlot({
    value <- data$value
    if (isTRUE(value$done)) {
      barplot(value$value$y, axes = FALSE)
      axis(1)
    }
  })

  output$queue <- shiny::renderUI({
    q <- data$value$queue
    if (!is.null(q)) {
      shiny::p(sprintf("queue: %d", q))
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
           value = list(x = xx, y = dpois(xx, p)))
    } else {
      list(done = FALSE,
           queue = floor(as.numeric(dt, "secs")),
           value = NULL)
    }
  }
}

shiny::shinyApp(ui, server)
