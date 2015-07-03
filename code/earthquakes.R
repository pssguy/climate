# fn format dateTime
formatTime <- function(timeString) {
  split1 <- strsplit(paste(timeString), "T")
  split2 <- strsplit(split1[[1]][2], "Z")
  fin <- paste0(split1[[1]][1], " ",split2[[1]][1])
}


data <- reactive({
  if(is.null(input$mag)) return()
  invalidateLater(3600000, session)
  
  rawData <-read_csv("http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv",
                     col_types = list(col_character(), col_double(), col_double(),
                                      col_double(), col_double(), col_character(),
                                      col_double(), col_double(), col_double(),
                                      col_double(), col_character(), col_character(),
                                      col_character(), col_character(), col_character()))
  
  rawData$dateTime <- as.POSIXct(sapply(rawData$time, formatTime)) ## NB dont use POSIXlt
  rawData <-rawData %>% 
    mutate(mag=round(mag,1)) 
  
  rawData <- rawData %>% 
    filter(mag>=input$mag[1]&mag<=input$mag[2])
  
  info=list(rawData=rawData)
  return(info)
  
 # glimpse(rawData)
  
 # binpal <- colorBin(c("#0000FF","#0080FF","#00FFFF","#FFFF00","#FF8000","#FF0000"), rawData$mag, 6, pretty = TRUE)
  
  # making pretty = true pretty much negartes the 6 gropups and does by zero
#  binpal <- colorBin(c("#0000FF","#0080FF","#00FFFF","#FFFF00","#FF8000","#FF0000"), rawData$mag,  pretty = TRUE)
  
})

output$quakeMap <- renderLeaflet({
  
  if(is.null(data()$rawData)) return()
  if(is.null(input$mag)) return()
  rawData <- (data()$rawData)
  
  binpal <- colorBin(c("#FFFF00","#FF8000","#FF0000"), rawData$mag,  pretty = TRUE)
  
  # binpal$bins
  rawData$popup <- sprintf("<table cellpadding='4' style='line-height:1'><tr>
                        <th>Magnitude: %1$s  </th></tr>
                            
                            <tr align='center'><td>%2$s</td></tr>
                           

                            
                            </table>",
                           rawData$mag,
                           rawData$place
  )
  
#   print(glimpse(rawData))
#   names(rawData)
  rawData    %>% 
    leaflet() %>% 
    addTiles() %>% 
    addCircles(radius = 5,popup=~ popup,color = ~binpal(mag)) %>% 
    addLegend(pal=binpal,values= ~mag, position='bottomleft',title="Magnitude")
})

output$myImage <- renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   outfile <- tempfile(fileext='.png')
  #   
  #   # Generate the PNG
  #   png(outfile, width=400, height=300)
  #   hist(rnorm(input$obs), main="Generated in renderImage()")
  #   dev.off()
  
  # Return a list containing the filename
  list(src = 'tectonicPlates.jpg',
       contentType = 'image/jpg',
       # 311x162
       width = 460,
       height = 240,
       alt = "")
}, deleteFile = FALSE)