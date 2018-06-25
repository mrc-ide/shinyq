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
    })

  output$plot <- shiny::renderPlot({
    if (!is.null(data$value)) {
      barplot(data$value$y, axes = FALSE)
      axis(1)
    }
  })
}


shiny::shinyApp(ui, server)
