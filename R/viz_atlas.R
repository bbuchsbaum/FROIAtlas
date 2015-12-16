alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

runApp <- function(vol) {

  shiny::shinyApp(
    ui = fluidPage(
      # Application title
      fluidRow(
        column(2,
               "sidebar"
        ),
        column(5,
          plotOutput("imagePlot"),
          alignCenter(shiny::sliderInput("slice", label="",min=1, max=dim(vol)[3], ticks=FALSE,value=dim(vol)[3]/2))
        )
      )
    ),
    
    server = function(input, output) {
      print(input)
      output$imagePlot <- renderPlot({
          print(input$slice)
          image(vol, input$slice)
      })
    }
  )
}

