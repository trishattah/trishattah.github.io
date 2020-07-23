library(shiny)
library(ggplot2)
library(dplyr)
library(RCurl)



x<-getURL("https://raw.githubusercontent.com/trishattah/trishattah.github.io/master/CleanData_CaseStudy1.csv")
CleanData_CaseStudy1 =read.csv(text=x) 


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
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
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
  ),
  
  
  sidebarLayout(  
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Chart type:",
                   c("Histogram" = "hist",
                     "Box plot" = "box")),
      
      #Copy the line below to make a select box 
      selectInput("select", label = h3("ABV or IBU"), 
                  choices = list("ABV" = "ABV", "IBU" = "IBU"), 
                  selected = 1),
      
      selectInput("State",
                  label = "Select State",
                  choices = unique(CleanData_CaseStudy1$State))

    ),

    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "plot")

    )
  ),
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "cap",
                label = "ABV vs IBU",
                value = "Scatter plot"),
      
      radioButtons("reg", "Chart type:",
                   c("Regression line" = "T",
                     "No Regression line" = "F")),
      
      selectInput("St",
                  label = "Select State",
                  choices = unique(CleanData_CaseStudy1$State))
    ),
    mainPanel(
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Scatterplot ----
      plotOutput(outputId = "scatter")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "med",
                label = "Overview ABV per state",
                value = "Multiple Box plot")
    ),
    mainPanel(
      # Output: Formatted text for caption ----
      h3(textOutput("capt", container = span)),
      
      # Output: Scatterplot ----
      plotOutput(outputId = "fin")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
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
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })

  
  output$plot <- renderPlot({
    if (input$dist == "hist"){
      if(input$select == "ABV"){
        ABV_ST <- CleanData_CaseStudy1 %>% filter(State == input$State)  %>% droplevels()
        x <- ABV_ST$ABV
        
        hist(x, breaks = 20, col = "#75AADB", border = "white",
             xlab = "ABV",
             main = "% Alcohol by volume")
      }
      if(input$select == "IBU"){
        IBU_ST <- CleanData_CaseStudy1 %>% filter(State == input$State)  %>% droplevels()
        x <- IBU_ST$IBU
        
        hist(x, breaks = 20, col = "#75AADB", border = "white",
             xlab = "IBU",
             main = "Bitterness Content")
      }
    }
    
    if (input$dist == "box"){
      if(input$select == "ABV"){
        ABV_ST <- CleanData_CaseStudy1 %>% filter(State == input$State)  %>% droplevels()
        x <- ABV_ST$ABV
        y <- ABV_ST
        
        boxplot(x,data=y, horizontal = TRUE, col = "#75AADB", border = "black",
                xlab = "ABV",
                main = "% Alcohol by volume")
      }
      if(input$select == "IBU"){
        IBU_ST <- CleanData_CaseStudy1 %>% filter(State == input$State)  %>% droplevels()
        x <- IBU_ST$IBU
        y <- IBU_ST
        
        boxplot(x,data=y, horizontal = TRUE, col = "#75AADB", border = "black",
                xlab = "IBU",
                main = "Bitterness Content")
      }
    }
  })
  
  output$scatter <- renderPlot({
    if(input$reg == "T"){
      st <- CleanData_CaseStudy1 %>% filter(State == input$St)  %>% droplevels()
      a <- st$ABV
      b <- st$IBU
      
      plot(a, b, data = st, col = 'blue', main="IBU vs ABV", 
           xlab="ABV", ylab="IBU")
      abline(lm(b~a), col="red")
      }
    if(input$reg == "F"){
      st <- CleanData_CaseStudy1 %>% filter(State == input$St)  %>% droplevels()
      a <- st$ABV
      b <- st$IBU
      
      plot(a, b, data = st, col = 'blue', main="IBU vs ABV", 
           xlab="ABV", ylab="IBU")
    }
})
  
  output$fin <- renderPlot({
    
    plot(CleanData_CaseStudy1$State, CleanData_CaseStudy1$ABV, data=CleanData_CaseStudy1, main="Overview of ABV per State", 
         xlab="Overview of ABV per State", ylab="ABV")
  })
}


 

# Create Shiny app ----
shinyApp(ui, server)



