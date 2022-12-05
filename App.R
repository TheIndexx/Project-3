library(shiny)
library(tidyverse)

salaries <- read.csv("salaries.csv")

## Create a variable that groups jobs
n <- nrow(salaries)
for (i in 1:n) {
  job <- salaries$job_title[i] # Job of current row
  # String parsing code, sorts in 5 groups based off wording
  # Creates new variable called 'job_group'
  if (str_detect(job, "Head") | str_detect(job, "Lead") | str_detect(job, "Manager") | str_detect(job, "Director")) salaries[i, "job_group"] = "Manager"
  
  else if (str_detect(job, "Ana")) salaries[i, "job_group"] = "Data Analyst"
  
  else if (str_detect(job, "Data")) salaries[i, "job_group"] = "Data Scientist"
  
  else if (str_detect(job, "Machine") | str_detect(job, "ML") | str_detect(job, "Scien")) salaries[i, "job_group"] = "Machine Learning Scientist"
  
  else salaries[i, "job_group"] = "Other"
}

## Create a variable that highlights biggest countries
locTable <- table(salaries$company_location)
for (i in 1:n){
  # If country has >10 inputs in table, keep original name
  if (locTable[names(locTable)==salaries$company_location[i]] > 10) salaries[i, "edited_location"] = salaries$company_location[i]
  # Else change it to "Other"
  else salaries[i, "edited_location"] = "Other"
}

ui <- fluidPage(
  
  titlePanel("Salaries of Data Scientists"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Slider for numeric
      sliderInput("slry",
                  "Salary in USD",
                  min = 0,
                  max = 600000,
                  value = 150000),
      
      # Color input
      selectInput("color", "Graph color:",
                  list("Red", "Blue", "Green")),
      
      # Variable input
      selectInput("variable", "Choose a Variable:",
                  list(`Numeric` = list("USD Salary"="salary_in_usd"),
                       `Categorical` = list("Job Group"="job_group", "Salary Currencies"="salary_currency", "Experience Level"="experience_level", "Location"="edited_location"))),
      
      # IMAGE
      img(src='mlimage.jpg', align = "left")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      br(),
      fluidRow(column(5, verbatimTextOutput("descr")))
    )
  )
)

server <- function(input, output){
  
  # Render graph: histogram or barplot depending on type of variable
  output$distPlot <- renderPlot({
    var <- input$variable
    new_salaries <- subset(salaries, salary_in_usd <= input$slry)
    
    # If numeric var, use histogram
    if(typeof(new_salaries[[var]])=="integer"){
      options(scipen=10)
      hist(new_salaries[[var]], main=str_to_title(gsub('_', ' ', var)), xlab=str_to_title(gsub('_', ' ', var)), xlim=c(0, input$slry), breaks = 25*(max(new_salaries[[var]])-min(new_salaries[[var]]))/(input$slry), col = input$color)
    }
    
    # If categorical var, use barplot
    else if(typeof(new_salaries[[var]]) == "character"){
      barplot(table(new_salaries[[var]]), main=str_to_title(gsub('_', ' ', var)), xlab=str_to_title(gsub('_', ' ', var)), col = input$color)
    }
  })
  
  # Descriptive Stats: Mean (numeric) or table (categorical)
  output$descr <- renderPrint({
    var <- input$variable
    new_salaries <- subset(salaries, salary_in_usd <= input$slry)
    
    if (typeof(new_salaries[[var]])=="integer"){
      cat("Mean: ",mean(new_salaries[[var]]),"\n","Five number summary:", fivenum(new_salaries[[var]]))
    }
    else if (typeof(new_salaries[[var]]) == "character"){
      table(new_salaries[[var]])
    }
  })
}

shinyApp(ui = ui, server = server)