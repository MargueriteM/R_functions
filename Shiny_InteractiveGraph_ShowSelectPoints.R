# Create a Shiny App with a graph that shows rows of data when selected on graph
# from: http://shiny.rstudio.com/articles/plot-interaction.html

# basic code 

ui <- basicPage(
  plotOutput("plot1", brush = "plot_brush"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(wt, y=mpg)) + geom_point()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    })
 
  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    brushedPoints(mtcars, input$plot_brush, xvar = "wt", yvar = "mpg")
  })
}

shinyApp(ui, server)


