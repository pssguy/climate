


theData <- reactive({
  if (input$sbMenu != "statetemps")
    return() # otherwise delays other tabs functioning
  # repoll every 10 minutes
  invalidateLater(600000, session)
  
  for (i in 1:nrow(capitals)) {
  #for (i in 1:2) {
    print(i)
    tempDF <- getCurrentTemperature(station = capitals$iata_faa[i])
#    print(capitals$iata_faa[i])
   # print(tempDF)
    if (is.null(tempDF)) {
      tempDF <- data.frame(Time = NA,TemperatureF = NA)
    }
  #  print(tempDF)
    if (i != 1) {
    #  print("i!=1")
      #capitals <- cbind(capitals,df)
      #print(glimpse(capitals))
      #print(capitals$Time[i])
      df <- rbind(df,tempDF)
    } else {
      df <- tempDF
    }
  }
  df
  
  capitals <- cbind(capitals,df)
  
  capitals <- capitals %>%
    mutate(time = str_sub(Time,12,16))
  
  ## write a df
  write_csv(capitals,"exapmpledf.csv")
  
  info = list(capitals = capitals)
  return(info)
  
})

output$stateTempsMap <- renderLeaflet({
  if (is.null(input$tempScale2))
    return()
  if (is.null(theData()))
    return()
  
  # print(theData()$capitals$TemperatureC)
  
  test <- theData()$capitals
  print(class(test))
  
  if (input$tempScale2 == "Fahrenheit") {
    legendTitle <- "Temperature F"
    capitals <- theData()$capitals %>%
      mutate(temp = TemperatureF)
    
    capitals$popup <-
      sprintf(
        "<table cellpadding='4' style='line-height:1.5'><tr>
        <th colwidth='2'>%1$s, %2$s</th></tr>
        
        <tr><td style='color: red'>%3$s F</td><td>%4$s</td></tr>
        
        
        
        </table>",
        capitals$city,
        capitals$state,
        capitals$temp,
        capitals$time
      )
    
  } else{
    legendTitle <- "Temperature C"
    capitals <- theData()$capitals %>%
      mutate(temp = round((TemperatureF - 32) * 5 / 9),0)
    
    capitals$popup <-
      sprintf(
        "<table cellpadding='4' style='line-height:1'><tr>
        <th>%3$s C </th></tr>
        
        <tr align='center'><td>%1$s, %2$s</td></tr>
        <tr align='center'><td>%4$s</td></tr>
        
        
        </table>",
        capitals$city,
        capitals$state,
        capitals$temp,
        capitals$time
      )
    
  }
  
  # Create a continuous palette function
  ## works but is faded dark blue to virtually white not what is wanted
  #   pal <- colorNumeric(
  #     palette = "Blues",
  #     domain = capitals$temp
  #   )
  
  ## again works but bit too mauvy
  #   pal <- colorNumeric(
  #     palette = c("navy","red"),
  #     domain = capitals$temp
  #   )
  
  #  binpal <- colorBin(c("navy","red"), capitals$temp, 6, pretty = FALSE)
  binpal <-
    colorBin(
      c(
        "#0000FF","#0080FF","#00FFFF","#FFFF00","#FF8000","#FF0000"
      ), capitals$temp, 6, pretty = FALSE
    )
  print(binpal)
  
  capitals %>%
    leaflet() %>%
    setView(lng = -114, lat = 41, zoom = 3) %>%
    addTiles() %>%
    addCircleMarkers(
      popup = ~ popup, color = ~ binpal(temp), opacity = 0.9, radius = 5
    ) %>%
    addLegend(
      pal = binpal,values = ~ temp, position = 'bottomleft', title = legendTitle
    )
  
  
})

# output$temperatureCheck <- renderText({
#
#   if(is.null(input$tempScale2)) return()
#   if(is.null(theData())) return()
#   #theData()$capitals$TemperatureC works locally but not remotely
#
#  # "hiya" # works both
#   print(glimpse(theData()$capitals))
#   print(("processed"))
#   theData()$capitals <- names(theData()$capitals) # locally not remotely
#   theData()$capitals <- names(theData())
#   theData()$capitals
#   theData()$capitals$TemperatureF
# })

output$stateTempsTable <- DT::renderDataTable({
  if (is.null(theData()))
    return()
  if (is.null(input$tempScale2))
    return()
  
  if (input$tempScale2 == "Fahrenheit") {
    capitals <- theData()$capitals %>%
      mutate(temp = TemperatureF)
  } else {
    capitals <- theData()$capitals %>%
      mutate(temp = round((TemperatureF - 32) * 5 / 9),0)
  }
  
  ## trying to discover why container does not print correctly? row.numbers withTags?
  #    print(capitals$time)
  #
  #   capitals[is.na(capitals$time),]$time <- "00:00"
  #   print(capitals$time)
  #   print(class(capitals$time)) #character
  #
  capitals %>%
    arrange(desc(temp)) %>%
    select(city,state,temp,time) %>%
    DT::datatable(
      container = stateTemp_format,options = list(
        paging = TRUE, searching = TRUE,info = FALSE,
        columnDefs = list(list(
          className = 'dt-right', targets = c(3,4)
        ))
      )
    )
  #DT::datatable(options=list(paging = TRUE, searching = TRUE,info=FALSE))
})


output$hotBox <- renderInfoBox({

  print(input$minMax[1])
  print(input$minMax[2])

hot <- theData()$capitals %>%
   # mutate(TemperatureF=round((TemperatureC*9/5)+32),1) %>%
    filter(TemperatureF>input$minMax[2]) %>%
    tally()


  infoBox(
    "Hot",hot, icon = icon("thumbs-down"),
    color = "red"
  )
})
output$coldBox <- renderInfoBox({
 # print(input$minMax[1])
  
#  print(theData()$capitals$TemperatureC)
  
  cold <- theData()$capitals %>%
  #  mutate(TemperatureF = round((TemperatureC * 9 / 5) + 32),1) %>%
    filter(TemperatureF < input$minMax[1]) %>%
    tally()
  
  #cold <- df[1,]$n
  
  infoBox("Cold",cold, icon = icon("thumbs-down"),
          color = "blue")
})

output$mildBox <- renderInfoBox({

  mild <- theData()$capitals %>%
  #  mutate(TemperatureF=round((TemperatureC*9/5)+32),1) %>%
    filter(TemperatureF>input$minMax[1]&TemperatureF<input$minMax[2]) %>%
    tally()

 # mild <- df[1,]$n

  infoBox(
    "Mild",mild, icon = icon("thumbs-up"),
    color = "green"
  )
})

## just the capitals data

output$stateCapitals <- DT::renderDataTable({
  capitals %>%
    DT::datatable()
})