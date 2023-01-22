#Import libraries
library(shiny)
library(shinythemes)
library(RCurl)
library(ranger)
library(caret)
library(data.table)
library(caTools)

#Read data
creditcard_data <- read.csv("https://raw.githubusercontent.com/wlsoo1234/assessment/main/NewData.csv")
NewData0=creditcard_data[,-c(1)]
NewData1 = NewData0[,-c(29)]
NewData = NewData1[-c(190000:284807),]
write.csv(NewData,file = "NewData.csv",row.names = TRUE)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)


#Fitting a decision tree model
library(rpart)
library(rpart.plot)
model2 <- rpart(Class ~ . , train_data, method = 'class')


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Credit Card Fraud Detection'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  tags$h4("Input:"),
                  
                  numericInput("V1", label = "V1", value = 1),
                  numericInput("V2",label = "V2", value = 1),
                  numericInput("V3",label = "V3", value = 1),
                  numericInput("V4",label = "V4", value = 1),
                  numericInput("V5",label = "V5", value = 1),
                  numericInput("V6",label = "V6", value = 1),
                  numericInput("V7",label = "V7", value = 1),
                  numericInput("V8",label = "V8", value = 1),
                  numericInput("V9",label = "V9", value = 1),
                  numericInput("V10",label = "V10", value = 1),
                  numericInput("V11",label = "V11", value = 1),
                  numericInput("V12",label = "V12", value = 1),
                  numericInput("V13",label = "V13", value = 1),
                  numericInput("V14",label = "V14", value = 1),
                  numericInput("V15",label = "V15", value = 1),
                  numericInput("V16",label = "V16", value = 1),
                  numericInput("V17",label = "V17", value = 1),
                  numericInput("V18",label = "V18", value = 1),
                  numericInput("V19",label = "V19", value = 1),
                  numericInput("V20",label = "V20", value = 1),
                  numericInput("V21",label = "V21", value = 1),
                  numericInput("V22",label = "V22", value = 1),
                  numericInput("V23",label = "V23", value = 1),
                  numericInput("V24",label = "V24", value = 1),
                  numericInput("V25",label = "V25", value = 1),
                  numericInput("V26",label = "V26", value = 1),
                  numericInput("V27",label = "V27", value = 1),
                  numericInput("V28",label = "V28", value = 1),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("V1","V2","V3","V4","V5","V6","V7","V8",
               "V9","V10","V11","V12","V13","V14","V15","V16","V17",
               "V18","V19","V20","V21","V22","V23","V24","V25",
               "V26","V27","V28"),
    
      Value = as.character(c(input$V1,input$V2,input$V3,input$V4,input$V5,input$V6,
                             input$V7,input$V7,input$V9,input$V10,input$V11,input$V12,
                             input$V13,input$V14,input$V15,input$V16,input$V17,input$V18,
                             input$V19,input$V20,input$V21,input$V22,input$V23,input$V24,
                             input$V5,input$V26,input$V27,input$V28)),
      
      
      stringsAsFactors = FALSE)
    
    
    input <- transpose(df)
    write.table(input,"input1.csv", sep=",",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input1", ".csv", sep=""),
                     header = TRUE)
    
    Output <- data.frame(Prediction=predict(model2,test), round(predict(model2,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)



