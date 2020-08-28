source("common.R", local = TRUE)
shiny::shinyApp(ui, server(workers = 1L))
