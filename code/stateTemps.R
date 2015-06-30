

theData <- reactive({
  
  if (input$sbMenu!="statetemps") return() # otherwise delays other tabs functioning
  # repoll every 10 minutes
  invalidateLater(600000, session)
  
for (i in 1:nrow(capitals)) {
  print(i)
  tempDF <- getCurrentTemperature(station = capitals$iata_faa[i])
  print(capitals$iata_faa[i])
  print(tempDF)
  if(is.null(tempDF)) tempDF<- data.frame(Time=NA,TemperatureF=NA)
  if(i!=1) {
    df <- rbind(df,tempDF)
  } else {
    df <- tempDF
  }
}



capitals <- cbind(capitals,df)
print(glimpse(capitals))
print(capitals$Time[1])

capitals <- capitals %>% 
  mutate(time=str_sub(Time,12,16))


info=list(capitals=capitals)
return(info)

})

output$stateTempsMap <- renderLeaflet({

  if(is.null(input$tempScale2)) return()
  if(is.null(theData())) return()
  
 # print(theData()$capitals$TemperatureC)
  
  test <- theData()$capitals
  print(class(test))
  
if(input$tempScale2=="Fahrenheit"){
  legendTitle <- "Temperature F"
  capitals <- theData()$capitals %>% 
    mutate(temp=TemperatureF)
  
  capitals$popup <- sprintf("<table cellpadding='4' style='line-height:1.5'><tr>
                        <th colwidth='2'>%1$s, %2$s</th></tr>
                            
                            <tr><td style='color: red'>%3$s F</td><td>%4$s</td></tr>
                          

                            
                            </table>",
                            capitals$city,
                            capitals$state,
                            capitals$temp,
                            capitals$time)
  
} else{
  legendTitle <- "Temperature C"
  capitals <- theData()$capitals %>% 
    mutate(temp=round((TemperatureF-32)*5/9),0)
  
  capitals$popup <- sprintf("<table cellpadding='4' style='line-height:1'><tr>
                        <th>%3$s C </th></tr>
                            
                            <tr align='center'><td>%1$s, %2$s</td></tr>
                            <tr align='center'><td>%4$s</td></tr>

                            
                            </table>",
                            capitals$city,
                            capitals$state,
                            capitals$temp,
                            capitals$time)
  
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
  binpal <- colorBin(c("#0000FF","#0080FF","#00FFFF","#FFFF00","#FF8000","#FF0000"), capitals$temp, 6, pretty = FALSE)
  print(binpal)
  
capitals %>% 
  leaflet() %>% 
  setView(lng = -114, lat = 41, zoom = 3) %>% 
  addTiles() %>% 
  addCircleMarkers(popup = ~popup, color = ~binpal(temp), opacity=0.9, radius=5) %>% 
  addLegend(pal=binpal,values= ~temp, position='bottomleft', title=legendTitle)


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
#   test <- names(theData()$capitals) # locally not remotely
#   test <- names(theData())
#   test
#   theData()$capitals$TemperatureF
# })

output$stateTempsTable <- DT::renderDataTable({
  
  if(is.null(theData())) return()
  if(is.null(input$tempScale2)) return()
  
  if(input$tempScale2=="Fahrenheit"){
    capitals <- theData()$capitals %>% 
      mutate(temp=TemperatureF)
  } else {
    capitals <- theData()$capitals %>% 
      mutate(temp=round((TemperatureF-32)*5/9),0)
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
    DT::datatable(container=stateTemp_format,options=list(paging = TRUE, searching = TRUE,info=FALSE,
columnDefs = list(list(className = 'dt-right', targets = c(3,4)))))
 #DT::datatable(options=list(paging = TRUE, searching = TRUE,info=FALSE))                                                                  
  
})

## just the capitals data

output$stateCapitals <- DT::renderDataTable({
  
 
  
  capitals %>% 
    DT::datatable()
})