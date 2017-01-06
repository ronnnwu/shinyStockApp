library(shiny)
library(shinythemes)
 

df <- readRDS("df.rds")    

stockIDs <- sort(unique(df$id))
indicators <- colnames(df)[4:111]

ui <- navbarPage("2 Sigma ML Contest",
  
  theme = shinytheme("cerulean"),
  
  tabPanel("Home",
   
    
    sidebarPanel(
      selectInput(inputId = "StockID"
              , label = "Stock"
              , choices = stockIDs
              , selected = stockIDs[0] ) ,
    
    
      selectInput(inputId = "indicator"
                , label = "Indicator"
                , choices = indicators
                , selected = indicators[0] )
    ),
                 
    
    mainPanel(plotOutput(outputId = "exploreGraph"))
    
  ),
    
  tabPanel("About",
           h4(HTML("This app, developed by Ron Wu, is for the 2 sigma machine learning contest, data visualization & analysis. The dataset here is half of full dataset, up to timestamp 905."))
  )
)

server <- function(input, output){
  
  output$exploreGraph <- renderPlot({
    
    dID <- df[df$id==input$StockID, ] 
    
    
    par(mar=c(5, 4, 4, 6) + 0.1)
    plot(dID$timestamp, dID$y, pch=16,  axes=FALSE,  xlab="", ylab="", 
         type="l",col="black", main =paste0("stockID: ",input$StockID, ", Indicator: ", input$indicator) )
    axis(2 ,col="black",las=1)   
    mtext("",side=2,line=2.5)
    box() 
    
    if (!all(is.na( dID[,input$indicator] ))){ 
      par(new=TRUE) 
      plot(dID$timestamp, dID[,input$indicator], pch=15,  xlab="", ylab="",
           axes=FALSE, type="l", col="blue") 
      mtext("",side=4,col="blue") 
      axis(4, col="blue",col.axis="blue",las=1)  
    }
    axis(1)
    mtext("",side=1,col="black")  
      
  }) 
}

shinyApp(ui = ui, server = server)
