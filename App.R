ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".rds"),
      checkboxInput("header", "Header", TRUE),
      
      downloadButton("dl", "Descargar")
    ),
    mainPanel(
      tableOutput("contents")
    )
  ),
  plotOutput("plot", click = "plot_click"),
)

server <- function(input, output) {
  
  
  output$contents <- renderTable({
    # 
  })
  
  output$dl <- downloadHandler(

    filename = function() {
      paste0("respuestas", ".xlsx")
    },
    content = function(file) {
      archivo <- input$file1
      ext <- tools::file_ext(archivo$datapath)
      
      req(archivo)
      validate(need(ext == "rds", "Por favor sube una base .rds"))
      
      datos <- readRDS(archivo$datapath)
      source("1-procesamiento.R", local = TRUE)
      source("2-procesamiento.R", local = TRUE, echo = TRUE)
      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
  output$plot <- renderPlot({
    CyE_semana
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
  }, res = 96)
}

shinyApp(ui, server)