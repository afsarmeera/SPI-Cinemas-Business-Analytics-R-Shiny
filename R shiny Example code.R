# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(readr)
b <- read_csv("s.csv", col_types = cols(Date = col_number(), 
                                        Value.Canada = col_number(), Value.UK = col_number(),Value.US = col_number(),
                                        Values1.Canada = col_number(), Values1.UK = col_number(), 
                                        Values1.US = col_number(), Values2.Canada = col_number(), 
                                        Values2.UK = col_number(), Values2.US = col_number() ) )


a<-data.frame(b$Date,b$Value.Canada,b$Value.UK,b$Value.US)
c<-data.frame(b$Date,b$Values1.Canada,b$Values1.UK,b$Values1.US)
d<-data.frame(b$Date,b$Values2.Canada,b$Values2.UK,b$Values2.US)
data_sets <- c("a", "c", "d")

 
# Use a fluid Bootstrap layout
ui<-fluidPage(    
  
  # Give the page a title
  titlePanel("Telephones by region"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
    
        uiOutput("choose_dataset"),
        
        uiOutput("choose_columns"),
        br()
        
        
      #selectInput("select", "Select columns to display",choices = a$Value.Canada,selected = NULL),
      #hr(),
      #helpText("Data from AT&T (1961) The World's Telephones.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")  
    )
    
  )
)



# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).


# Define a server for the Shiny app
server<-function(input, output) {
  
  
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
        
        
        # Get the data set with the appropriate name
        dat <- get(input$dataset)
        colnames <- names(dat)  
        
        
        
        # Create the checkboxes and select them all by default
        checkboxGroupInput("columns", "Choose columns", 
                           choices  = colnames,
                           selected = colnames)
        
        
        
        
  } )
        
  # Output the data
  output$data_table <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()     
        
    
    # Get the data set
    dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat)))
      return()
    
    # Keep the selected columns
    dat <- dat[, input$columns, drop = FALSE]
    
    # Return first 20 rows
    head(dat, 20)
  })
        
  
  
  
  
  
  
  
  
  
  
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    
    
    #plot(a[,input$dataset$columns],a[,input$dataset$Date],type="l")
    
    plot(input$dataset$columns, input$dataset$Date )
  
    # Render a barplot
    #barplot(a[,input$select]*1000, 
            #main=input$select,
            #ylab="Value.Canada",
            #xlab="Date")
  })
}



shinyApp(ui=ui, server=server)