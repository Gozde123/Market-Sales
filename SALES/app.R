# market sales data set

library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(knitr)
library(rsconnect)



dt<-read.csv("supermarket_sales.csv")
data<-read.csv("supermarket_sales.csv")


colChoices<-c("brown","pink","green","black","red","purple",
              "yellow","blue","orange","gray","darkred",
              "darkblue","magenta","lightsalmon")


predicChoices<-colnames(dt)[c(2,4,6,13,17)]

ui <- dashboardPage(
  skin = "purple-light",
  dashboardHeader(
    title = "Market Sales",
    dropdownMenu(type='messages',
                 messageItem(
                   from="Gözde Nur Özdemir",
                   message = "Welcome my app",
                 )
    )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General",tabName = "general",icon = icon("home")),
      menuItem("Summary", tabName = "summary",icon = icon("dashboard")),
      menuItem("Reference", tabName = "reference",icon = icon("link")),
      menuItem("Prediction",tabName = "predict",icon = icon("question")),
      menuItem("Power BI",tabName = "power",icon=icon("link"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "general",
        fluidRow(
          column(
            width = 12,
            align = "center",
            textOutput("spacer"),
            textOutput("text1")
          ),
          column(
            width = 12,
            align = "center",
            imageOutput("image", width = 600, height = 500)
          )
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "Summary of Heart Disease Data Set",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("Summary")
          )
        )
      ),
      tabItem(
        tabName = "power",
        fluidRow(
          column(
            width = 12,
            align = "center",
            textOutput("spacer2"),
            textOutput("text2")
          ),
          column(
            width = 12,  # Use the full width of the column
            align = "center",  # Align content in the center
            imageOutput("image2", width = 800, height = 600)
            
            
          )
        )
      ),
      
      tabItem(
        tabName = "reference",
        fluidRow(
          box(
            title = "Reference",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            tags$a("This app is built with a Kaggle dataset",
                   href="https://www.kaggle.com/datasets/arunjangir245/super-market-sales")
          ),
          downloadButton("download1"),
          tableOutput("table1")
        )
      ),
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Prediction of Market Sales Data Set by Gender",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("prediction", label = "Select the prediction:", choices = predicChoices),
            actionButton("MakePrediction", label = "Predict"),
            tableOutput("summary2")
          ),
          box(
            title = "Prediction Formula",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("formula_box")
          )
        )
      )
      
      
      
      
      
      
      
    )
  )
  
  
)

server <- function(input, output) {
  # Your server logic goes here
  output$image <- renderImage({
    list(src = "blog_pictures2Fdata-driven-marketing-2017-marketers-data-critical.jpg", width = 600, height = 500)
  }, deleteFile = FALSE)
  output$text1<-renderText("This web page are made for giving the general information about heart disease data set. It also has POWER BI and R Markdown files.")
  output$Summary<-renderTable(summary(dt))
  output$image2 <- renderImage({
    list(src = "marketLinePowerBI.png", width = 800, height = 600)
  }, deleteFile = FALSE)
  output$text2<-renderText("This image is built with a Power BI")
  output$download1 <- downloadHandler(
    filename = function() {
      "supermarket_sales.csv"  # Specify the desired filename here
    },
    content = function(file) {
      write.csv(dt, file)  # Write the 'dt' data frame to the file
    }
  )
  output$table1<-renderTable(dt)
  
  logit_result <- eventReactive(input$MakePrediction, {
    print("Logit result reactive event triggered")
    dt$Gender <- as.factor(dt$Gender)
    dt$Gender <- ifelse(dt$Gender == "Male", 0, 1)
    dt$Gender <- as.factor(dt$Gender)
    
    # Assuming 'data' is your dataset, you should use it here, not 'data'
    if (input$prediction == "Branch") {
      logit <- glm( Gender~Branch , data = dt, family = "binomial")
    } else if (input$prediction == "Customer.type") {
      logit <- glm( Gender~ Customer.type , data = dt, family = "binomial")
    } else if (input$prediction == "Product.line") {
      logit <- glm(Gender~Product.line , data = dt, family = "binomial")
    } else if (input$prediction == "Payment") {
      logit <- glm(Gender~Payment, data = dt, family = "binomial")
    } else {
      logit <- glm(Gender~Rating, data = dt, family = "binomial")
    }
    logit
  })
  output$formula_box<-renderText({
    if(input$prediction=="Branch"){
      logt <- paste("Gender=-0.11+0.06*Branch B+ 0.28* Branch C")
      
    } else if(input$prediction=="Customer.type"){
      logt <- paste("Gender=0.08-0.16*Customer.type")
      
    } else if(input$prediction=="Product.line"){
      logt <- paste("Gender=-0.02+0.18*Line 1 ....")
      
    } else if(input$prediction=="Payment"){
      logt <- paste("Gender=0.07+0.03*Payment1-0.21*Payment2")
    } else {
      logt<-paste("Gender=0.04-0.01Rating")
    }
    logt
  })
  
  output$summary2 <- renderTable({
    # Check if the model has been fitted before displaying results
    if (!is.null(logit_result())) {
      summary_data <- summary(logit_result())
      coefficients <- summary_data$coefficients
      
      # Extract only the Estimate, Std. Error, z value, and Pr(>|z|) columns
      coefficients <- coefficients[, c("Estimate", "Std. Error", "z value", "Pr(>|z|)")]
      
      # Return the coefficients as a data frame
      coefficients
    } else {
      # Return an empty data frame if the model hasn't been fitted yet
      data.frame()
    }
  })
  
  
  
}


shinyApp(ui, server)

