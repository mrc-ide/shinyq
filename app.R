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
          step = 1)),
      shiny::mainPanel(
        shiny::plotOutput("plot"),
        shiny::uiOutput("queue")))))


server <- function(input, output, session) {
  output$plot <- shiny::renderPlot({
    p <- input$parameter
    xx <- 0:20
    barplot(dpois(xx, p), axes = FALSE)
    axis(1)
  })
}


shiny::shinyApp(ui, server)
