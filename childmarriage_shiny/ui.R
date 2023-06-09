#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  
  selectInput(
    inputId = "var",
    label = "Variable",
    choices = c("age", "education", "aridity", "income")
  ),

    # Application title
    titlePanel("Logistic Regression"),

    # Sidebar with a slider input for number of bins
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("LogReg")
        )
    )

