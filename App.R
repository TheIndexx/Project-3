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
  if (locTable[names(locTable)==salaries$company_location[i]] > 10) salaries[i, "editedLocation"] = salaries$company_location[i]
  # Else change it to "Other"
  else salaries[i, "editedLocation"] = "Other"
}

ui <- fluidPage(
  titlePanel("Salaries of Data Scientists"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slry",
                  "Salary in USD",
                  min = 0,
                  max = 600000,
                  value = 150000),
      selectInput("color", "Graph color:",
                  list("Red", "Blue", "Green")),
      selectInput("variable", "Choose a Variable:",
                  list(`Numeric` = list("USD Salary"="salary_in_usd"),
                       `Categorical` = list("Job Group"="job_group", "Salary Currencies"="salary_currency", "Experience Level"="experience_level", "Location"="editedLocation"))),
      img(src='mlimage.jpg', align = "left")
    ),
    mainPanel(
      plotOutput("distPlot"),
      br(),
      fluidRow(column(5, verbatimTextOutput("descr")))
    )
  )
)

# Problem, scale isnt continuous
server <- function(input, output){
  

  output$distPlot <- renderPlot({
    var <- input$variable
    
    if(typeof(salaries[[var]])=="integer"){
      options(scipen=10)
      hist(salaries[[var]], main=str_to_title(gsub('_', ' ', var)), xlab=str_to_title(gsub('_', ' ', var)), xlim=c(0, input$slry), breaks = 25*(max(salaries[[var]])-min(salaries[[var]]))/(input$slry), col = input$color)
    }
    
    else if(typeof(salaries[[var]]) == "character"){
      barplot(table(salaries[[var]]), main=str_to_title(gsub('_', ' ', var)), xlab=str_to_title(gsub('_', ' ', var)), col = input$color)
    }
  })
  
  # Mean (num) or table (cat)
  output$descr <- renderPrint({
    var <- input$variable
    
    if (typeof(salaries[[var]])=="integer"){
      cat("Mean: ",mean(salaries[[var]]),"\n","Five number summary", fivenum(salaries[[var]]))
    }
    else if (typeof(salaries[[var]]) == "character"){
      table(salaries[[var]])
    }
  })
}

shinyApp(ui = ui, server = server)