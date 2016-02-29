
shinyServer(function(input, output,session) {
  # for saving url as is
  shinyURL.server(session)
  
  ## set up input menu in sidebar
  output$sb <- renderUI({
    if (input$sbMenu=="stations") {
      inputPanel(id="ip",
                 selectInput("country","Select Country",countryChoice, selected="Canada"),
                 radioButtons("tempScale","",c("Celsius","Fahrenheit"),inline= TRUE),
                 radioButtons("timeRange","Time Scale", c("Current","All Time"), inline = TRUE)
      ) 
    } else if (input$sbMenu=="statetemps") {
      inputPanel(id="ip3",radioButtons("tempScale2","Select Scale for Map",c("Celsius","Fahrenheit"),selected="Fahrenheit",inline= TRUE))
    } else if (input$sbMenu=="earthquakes") {
      inputPanel(id="ip2",
                 sliderInput("mag","Enter Magnitude Range",min=2,max=10,value=c(4,10),step=0.5)
      )  
         }
    
  })

  source("code/locations.R", local=TRUE)
   source("code/stateTemps.R", local=TRUE)
   source("code/earthquakes.R", local=TRUE)
  
  
  
 
 
})

