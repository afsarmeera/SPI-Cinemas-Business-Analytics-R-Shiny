library(fuzzyjoin)
library(dplyr)
library(zoo)
library(tidyr)
library(filling)
library(stringr)



ui <- fluidPage(
  
  
  titlePanel("SPI cinemas"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      fileInput("file3", "Choose CSV VISTA file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      tags$hr(),
      
      
      
      
      
      fileInput("file1", "Choose CSV MAYA(LHS) file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      
      fileInput("file2", "Choose CSV MAYA(RHS) file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      tags$hr(),
      
      
      checkboxInput("header", "Header", FALSE),
      
      actionButton("rep", "Display"),
      actionButton("rep1", "Order"),
      actionButton("agg", "Aggregate"),
      actionButton("var", "Match the MAYA"),
      actionButton("var3", "Result"),
      #actionButton("var2","Variance"),
      
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      radioButtons("filetype", "File type:",
                   choices = c("csv", "tsv")),
      
      #actionButton("var2","Variance"),
      
      
      downloadButton("downloadDatav", "Download Variance")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Vista",dataTableOutput("aggr3"),dataTableOutput("contents3")),
        tabPanel("Maya(LHS)",dataTableOutput("aggr"),dataTableOutput("contents")),
        tabPanel("MAYA(RHS)",dataTableOutput("aggr2"),dataTableOutput("contents2")),
        #tabPanel("Matching",dataTableOutput("match")),
        tabPanel("VARIENCE",dataTableOutput("vari"))
      )
      
      # Output: Data file ----
      
      
      ##DT::dataTableOutput("delete")
    )
    
  )
)
library(shiny)
library(dplyr)
library(fuzzyjoin)
library(data.table)

server <- function(input, output) {
  
  r<-data.frame(VISTA=c("ALMOND MILK CHOCOLATE"    ,     "ALMOND MILK MANGO"            ,
                        "ALOE VERA BUTTERMILK"  ,        "ALPHONSO  ICE CREAM REGULAR"  ,
                        "BLACK FOREST"           ,       "BROOKIES"                     ,
                        "BROWNIE"                  ,     "CAKE POPS"                    ,
                        "CAPPUCCINO PREMIUM"       ,     "CARAMEL POPCORN 70 GRM"       ,
                        "CHOCO DONUT"                ,   "CHOCO DONUT COMBO"            ,
                        "COKE 450 ML"                 ,  "COKE 650 ML"                  ,
                        "COKE ZERO"                   ,  "COLD COFFEE"                  ,
                        "COMBO 1 CAPPUCCINO"            ,"COMBO 1 COLD COFFEE"          ,
                        "COMBO 1 RED JUICE"  ,           "COMBO 1 WATERMELON JUICE"     ,
                        "COMBO 2 CAPPUCCINO" ,           "COMBO 2 COLD COFFEE"          ,
                        "COMBO 2 FILTER COFFEE",         "COMBO 2 RED JUICE"            ,
                        "COMBO 2 WATERMELON JUICE" ,     "COMBO 3"                      ,
                        "CREAM DONUT"               ,    "CREAM DONUT COMBO"            ,
                        "DIET COKE CAN"             ,    "FANTA 450 ML"                 ,
                        "FANTA 650 ML"               ,   "FILTER COFFEE"                ,
                        "FRESH ABC JUICE"            ,   "FRESH ACTIVATED CHARCOAL"     ,
                        "FRESH PINEAPPLE & APPLE JUICE", "FRESH RED JUICE"              ,
                        "FRESH WATER MELON JUICE"      , "GREEN TEA"                    ,
                        "LONE RANGER"                   ,"MELON ICE CREAM REGULAR"      ,
                        "MINUTE MAID PULPY"           ,  "NACHOS/SALSA"                 ,
                        "NON VEG PUFF"                 , "PERI PERI"                    ,
                        "RED BULL"                      ,"RED BULL COMBO"               ,
                        "SCHWEPPES"             ,        "SPRITE 450 ML"                ,
                        "SPRITE 650 ML"          ,       "THUMSUP 450 ML"               ,
                        "THUMSUP 650 ML"           ,     "THUMSUP CHARGED"              ,
                        "TWO TO TANGO"             ,     "VEG PUFF"                     ,
                        
                        "WHEAT PUFF"                ), 
                MAYA=c("ALMOND MILK CHOCOLATE" ,       "ALMOND MILK MANGO"           ,
                       "ALOE VERA BUTTERMILK",         "ALPHONSO  ICE CREAM REGULAR" ,
                       "BLACK FOREST"         ,        "BROOKIES"                    ,
                       "BROWNIE"               ,       "CAKE POPS"                  , 
                       "CAPPUCCINO PREMIUM"       ,    "CARAMEL POPCORN"             ,
                       "CHOCO DONUT"             ,     "CHOCO DONUT COMBO"           ,
                       "COKE SMALL 450 ML"        ,    "COKE REGULAR 650 ML"         ,
                       "COKE ZERO"                 ,   "COLD COFFEE"                 ,
                       "COMBO 1 CAPPUCCINO"          , "COMBO 1 COLD COFFEE"         ,
                       "COMBO 1 RED JUICE"           , "COMBO 1 WATERMELON JUICE"    ,
                       "COMBO 2 CAPPUCCINO"           ,"COMBO 2 COLD COFFEE"         ,
                       "COMBO 2 FILTER COFFEE"  ,      "COMBO 2 RED JUICE"           ,
                       "COMBO 2 WATERMELON JUICE",     "COMBO 3"                     ,
                       "CREAM DONUT"              ,    "CREAM DONUT COMBO"           ,
                       "DIET COKE CAN"             ,   "FANTA SMALL 450 ML"          ,
                       "FANTA REGULAR  650 ML"       , "FILTER COFFEE"               ,
                       "ABC JUICE"                   , "ACTIVATED CHARCOAL"          ,
                       "PINEAPPLE & APPLE JUICE"      ,"RED JUICE"                   ,
                       "WATER MELON JUICE"     ,       "GREEN TEA"                   ,
                       "THE LONE RANGER"          ,    "MELON ICE CREAM REGULAR"     ,
                       "MINUTE MAID PULPY ORANGE",     "NACHOS WITH SALSA AND CHEESE",
                       "NON VEG PUFF"             ,    "PERI PERI"       ,            
                       "RED BULL"                   ,  "RED BULL COMBO"         ,     
                       "WATER"                      ,  "SPRITE SMALL 450 ML"  ,       
                       "SPRITE REGULAR 650 ML"       , "THUMSUP SMALL 450 ML",        
                       "THUMSUP REGULAR 650 ML"       ,"THUMS UP CHARGED" ,           
                       "TWO TO TANGO"             ,    "VEGETABLE PUFF" ,             
                       "WHEAT PUFF"   ))  
  
  
  
  
  
  
  
  
  #file1 get
  output$contents3 <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file3)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df31 <- read.csv(input$file3$datapath,
                         header = input$header,
                         
                         sep = input$sep,
                         quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    df3 <- as.data.frame(sapply(df31,gsub,pattern=".*_",replacement=""))
    
    da7<-eventReactive(input$rep,{
      
      
      #df3[gsub(".*_", "", da7()$V1),]
      
      subset(df3, select=c(V1,V2))
      
      
    }	)
    
    
    
    
    return(da7())
    
    
    
    if(input$disp == "head") {
      return(head(df3))
    }
    else {
      return(df3)
    }
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  #file1 get
  output$contents <- renderDataTable({
    
    
    
    req(input$file1)
    
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       
                       sep = input$sep,
                       quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    da6<-eventReactive(input$rep,{
      
      subset(df, select=c(V5,V6))
      
      
    }	)
    
    
    
    return(da6())
    
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    
    #da<-eventReactive(input$del,{input$df<-input$df[complete.cases(input$df), ] })
    #output$delete<-renderTable({
    #if(is.null(da)){return()}
    #return(da)
    #})
    
    #
    
    
    #data<-eventReactive(input$agg,{aggregate(list(df$item, df$qty), by = list(df$item, df$qty), FUN=sum)})
    #data<-eventReactive(input$agg,{ aggregate(as.numeric(input$df$qty), by=list(item=as.character (input$df$item)),FUN=sum)})
    #output$aggregate<-DT::renderDataTable({aggregate(as.numeric(input$df$V5), (list(Variety=input$df$V4)), sum )})
    #output$aggregate<-renderTable({
    
    #if(is.null(data()))
    #{return()}
    #else
    #{
    # return(data())
    #}
    # })
    
    
    
    
    
  })
  
  
  
  #FILE2  get
  
  output$contents2 <- renderDataTable({
    
    #if(is.null(data())) output$contents2 <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df2 <- read.csv(input$file2$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    da6<-eventReactive(input$rep,{
      
      subset(df2, select=c(V5,V6))
      
      
    }	)
    return(da6())
    
    if(input$disp == "head") {
      return(head(df2))
    }
    else {
      return(df2)
    }
    
    
    #da<-eventReactive(input$del,{input$df<-input$df[complete.cases(input$df), ] })
    #output$delete<-renderTable({
    #if(is.null(da)){return()}
    #return(da)
    #})
    
    #
    
    
    #data<-eventReactive(input$agg,{aggregate(list(df$item, df$qty), by = list(df$item, df$qty), FUN=sum)})
    #data<-eventReactive(input$agg,{ aggregate(as.numeric(input$df$qty), by=list(item=as.character (input$df$item)),FUN=sum)})
    #output$aggregate<-DT::renderDataTable({aggregate(as.numeric(input$df$V5), (list(Variety=input$df$V4)), sum )})
    #output$aggregate<-renderTab
    #{return()}
    #else
    #{
    # return(data())
    #}
    # })
    
    
    
    
    
  })
  
  
  
  #aggregate file1
  output$aggr<- renderDataTable({
    
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    da6<-eventReactive(input$rep,{
      
      subset(df, select=c(V5,V6))
      
      
    }	)
    
    da5<-eventReactive(input$rep1,{
      
      
      df[!(is.na(da6()$V5) | da6()$V5==""), ]
      df[!(is.na(da6()$V6) | da6()$V6==""), ]
      df[!(df$V5 %in% r$MAYA),]
      
    }	)
    
    
    
    
    
    da<-eventReactive(input$agg,{da5() %>% 
        group_by(V5) %>% 
        summarise(V6=sum(as.numeric(V6)))})
    
    
    return(da())
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    
    
  })
  
  
  
  output$aggr2<- renderDataTable({
    
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df2 <- read.csv(input$file2$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    da6<-eventReactive(input$rep,{
      
      subset(df2, select=c(V5,V6))
      
      
    }	)
    
    da5<-eventReactive(input$rep1,{
      
      
      df2[!(is.na(da6()$V5) | da6()$V5==""), ]
      df2[!(is.na(da6()$V6) | da6()$V6==""), ]
      df2[!(df2$V5 %in% r$MAYA),]
      
    }	)
    
    
    da<-eventReactive(input$agg,{da5() %>% 
        group_by(V5) %>% 
        summarise(V6=sum(as.numeric(V6)))})
    return(da())
    
    
    if(input$disp == "head") {
      return(head(df2))
    }
    else {
      return(df2)
    }
    
    
    
  })
  output$aggr3<- renderDataTable({
    
    
    req(input$file3)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df31 <- read.csv(input$file3$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    df3 <- as.data.frame(sapply(df31,gsub,pattern=".*_",replacement=""))
    da7<-eventReactive(input$rep,{
      
      subset(df3, select=c(V1,V2))
      
      
    }	)
    
    da8<-eventReactive(input$rep1,{
      
      
      df3[!(is.na(da7()$V1) | da7()$V1==""), ]
      df3[!(is.na(da7()$V2) | da7()$V2==""), ]
      df3[!(df3$V1 %in% r$VISTA),]
      
    }	)
    
    
    da9<-eventReactive(input$agg,{da8() %>% 
        group_by(V1) %>% 
        summarise(V2=sum(as.numeric(V2)))})
    return(da9())
    
    
    if(input$disp == "head") {
      return(head(df3))
    }
    else {
      return(df3)
    }
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #da3%>%V3.x-V3.y
  
  #z <-reactive(input$var2,{da3()$V3.x-da3()$V3.y})
  
  #return(z())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$vari<-renderDataTable({
    
    
    
    
    req(input$file1)
    req(input$file2)
    req(input$file3)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      
      error = function(e) {
        
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        df2 <- read.csv(input$file2$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
      },
      
      error = function(e) {
        
        stop(safeError(e))
      }
    )
    tryCatch(
      {
        df31 <- read.csv(input$file3$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
      },
      
      error = function(e) {
        
        stop(safeError(e))
      }
    )
    df3 <- as.data.frame(sapply(df31,gsub,pattern=".*_",replacement=""))
    
    da6<-eventReactive(input$rep,{
      
      subset(df, select=c(V5,V6))
      
      
    }	)
    
    da5<-eventReactive(input$rep1,{
      
      
      df[!(is.na(da6()$V5) | da6()$V5==""), ]
      df[!(is.na(da6()$V6) | da6()$V6==""), ]
      df[!(df$V5 %in% r$MAYA),]
      
    }	)
    
    
    da30<-eventReactive(input$rep,{
      
      subset(df3, select=c(V1,V2))
      
      
    }	)
    
    da50<-eventReactive(input$rep1,{
      
      
      df3[!(is.na(da30()$V1) | da30()$V1==""), ]
      df3[!(is.na(da30()$V2) | da30()$V2==""), ]
      df3[!(df3$V1 %in% r$VISTA),]
      
    }	)
    
    
    da51<-eventReactive(input$agg,{da50() %>% 
        group_by(V1) %>% 
        summarise(V2=sum(as.numeric(V2)))})
    
    
    
    
    
    
    da<-eventReactive(input$agg,{da5() %>% 
        group_by(V5) %>% 
        summarise(V6=sum(as.numeric(V6)))})
    
    da8<-eventReactive(input$rep,{
      
      subset(df2, select=c(V5,V6))
      
      
    }	)
    
    da7<-eventReactive(input$rep1,{
      
      
      df2[!(is.na(da8()$V5) | da8()$V5==""), ]
      df2[!(is.na(da8()$V6) | da8()$V6==""), ]
      df2[!(df2$V5 %in% r$MAYA),]
      
    }	)
    
    
    da2<-eventReactive(input$agg,{da7() %>% 
        group_by(V5) %>% 
        summarise(V6=sum(as.numeric(V6)))})
    
    
    
    da3<-eventReactive(input$var,{
      
      #stringdist_left_join(da(),  da2(),  by = c("V5" = "V5"), max_dist = .13, method = "lcs")
      
      
      stringdist_left_join(da(),  da2(),  by = c("V5" = "V5"), max_dist = .13, method = "lcs")
      
      
    }	)
    
    
    
    
    
    da4<-eventReactive(input$var,
                       {
                         df_new = data.table(da3()$V5.x,V=da3()$V6.x+da3()$V6.y)
                         
                         return(df_new)
                         
                         
                         
                         
                         
                       })
    da60<-eventReactive(input$var3,{
      
      stringdist_left_join(da4(),  da51(),  by = c("V1" = "V1"), max_dist = .13, method = "lcs")
      
      
    }	)
    
    da70<-eventReactive(input$var,
                        {
                          df_new = data.table(da60()$V1.x,Maya=da60()$V,Vista=da60()$V2,V=da60()$V-da60()$V2)
                          
                          return(df_new)
                          
                          
                          
                          
                          
                        })
    
    
    
    
    
    return(da70())
    
    
    
  })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadDatav <- downloadHandler(
    
    filename = function() {
      paste(input$file2, ".csv", sep="")
    },
    content = function(file) {
      
      
      
      dn<- reactive({
        
        req(input$file1)
        req(input$file2)
        req(input$file3)
        tryCatch(
          {
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
          },
          
          error = function(e) {
            
            stop(safeError(e))
          }
        )
        
        tryCatch(
          {
            df2 <- read.csv(input$file2$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
          },
          
          error = function(e) {
            
            stop(safeError(e))
          }
        )
        tryCatch(
          {
            df31 <- read.csv(input$file3$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
          },
          
          error = function(e) {
            
            stop(safeError(e))
          }
        )
        df3 <- as.data.frame(sapply(df31,gsub,pattern=".*_",replacement=""))
        
        da6<-eventReactive(input$rep,{
          
          subset(df, select=c(V5,V6))
          
          
        }	)
        
        da5<-eventReactive(input$rep1,{
          
          
          df[!(is.na(da6()$V5) | da6()$V5==""), ]
          df[!(is.na(da6()$V6) | da6()$V6==""), ]
          df[!(df$V5 %in% r$MAYA),]
          
        }	)
        
        
        da30<-eventReactive(input$rep,{
          
          subset(df3, select=c(V1,V2))
          
          
        }	)
        
        da50<-eventReactive(input$rep1,{
          
          
          df3[!(is.na(da30()$V1) | da30()$V1==""), ]
          df3[!(is.na(da30()$V2) | da30()$V2==""), ]
          df3[!(df3$V1 %in% r$VISTA),]
          
        }	)
        
        
        da51<-eventReactive(input$agg,{da50() %>% 
            group_by(V1) %>% 
            summarise(V2=sum(as.numeric(V2)))})
        
        
        
        
        
        
        da<-eventReactive(input$agg,{da5() %>% 
            group_by(V5) %>% 
            summarise(V6=sum(as.numeric(V6)))})
        
        da8<-eventReactive(input$rep,{
          
          subset(df2, select=c(V5,V6))
          
          
        }	)
        
        da7<-eventReactive(input$rep1,{
          
          
          df2[!(is.na(da8()$V5) | da8()$V5==""), ]
          df2[!(is.na(da8()$V6) | da8()$V6==""), ]
          df2[!(df2$V5 %in% r$MAYA),]
          
        }	)
        
        
        da2<-eventReactive(input$agg,{da7() %>% 
            group_by(V5) %>% 
            summarise(V6=sum(as.numeric(V6)))})
        
        
        
        da3<-eventReactive(input$var,{
          
          #stringdist_left_join(da(),  da2(),  by = c("V5" = "V5"), max_dist = .13, method = "lcs")
          
          
          stringdist_left_join(da(),  da2(),  by = c("V5" = "V5"), max_dist = .13, method = "lcs")
          
          
        }	)
        
        
        
        
        
        da4<-eventReactive(input$var,
                           {
                             df_new = data.table(da3()$V5.x,V=da3()$V6.x+da3()$V6.y)
                             
                             return(df_new)
                             
                             
                             
                             
                             
                           })
        da60<-eventReactive(input$var3,{
          
          stringdist_left_join(da4(),  da51(),  by = c("V1" = "V1"), max_dist = .13, method = "lcs")
          
          
        }	)
        
        da70<-eventReactive(input$var,
                            {
                              df_new = data.table(da60()$V1.x,Maya=da60()$V,Vista=da60()$V2,V=da60()$V-da60()$V2)
                              
                              return(df_new)
                              
                              
                              
                              
                              
                            })
        
        
        
        
        
        return(da70())
        
        
        
      })  
      
      
      
      
      
      
      write.csv(dn(), file)
    }
  )
  
  
  #delete file 2
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)           