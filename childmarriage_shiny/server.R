#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(dplyr)
library(data.table)

# Plotting packages
library(ggplot2)
library(RColorBrewer)


DHS<-read.csv(file = paste0(source_folder,'Data/bangladesh.csv'))  # reading DHS Bangladesh 2014
DHS$Age<-as.numeric(DHS$Age)

dhsdataMerge<-function(originalData){
  datause<-merge(originalData, accessData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, smodData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, buildupData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, aridityData, by=c("DHSCLUST"), all.x=T)  ## NO .agg HERE because you gave it to me already aggregated !!! 
  datause<-merge(datause, densityData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, aWIData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, aICData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-merge(datause, aPPData.agg, by=c("DHSCLUST"), all.x=T)
  datause<-datause[datause$DHSCLUST!=544,]
  return(datause)
}

# We use the dhsdataMerge function to merge the survey data (individuals)
# with all the Geo-covariate extracted at the cluster level
DataDHS<-dhsdataMerge(DHS)

# We need to have a factor variable and not directly Before15 (that is numeric here)  
DataDHS$I_Before15 <- as.factor(DataDHS$Before15)

# Education is a factor variable
DataDHS$Education <- as.factor(DataDHS$Education)
# DataDHS <- DataDHS %>%                    # defining the reference category
#   mutate(Education = relevel(Education, "0-No"))
# 

# We change the unit of Aridity here 
DataDHS$Aridity2015 <- DataDHS$Aridity2015 * 10^8

# Defining the variables of the model
Y<-"I_Before15"               # Response variable
XCovars <- c(15, 17, 57:64)   # age+education+GIS

formula_string<- paste(Y, paste(colnames(DataDHS)[XCovars], collapse=" + "), sep="~")



# Logistics Regression
glm.fit <- glm(formula_string, data = DataDHS, family = binomial)



library(visreg)
library(ggpubr)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$LogReg <- renderPlot({
      
      # Probabilities of married before 15 wrt 
      p.age <- visreg(glm.fit, "Age", scale="response", rug=0,  # for rugs =2
                      xlab="Age",
                      ylab="P(Before15=1)", gg=TRUE) + 
        ylim(0,1) +theme_minimal()
      
      p.education <- visreg(glm.fit, "Education", scale="response", rug=0,
                            xlab="Education",
                            ylab="P(Before15=1)", gg=TRUE) + 
        ylim(0,1) + theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1,
                                         hjust=1,
                                         size=7))
      
      p.aridity <- visreg(glm.fit, "Aridity2015", scale="response", rug=0,
                          xlab="Aridity level (2015)",
                          ylab="P(Before15=1)", gg=TRUE) + 
        ylim(0,1) +theme_minimal()
      
      p.income <- visreg(glm.fit, "aIncome2013", scale="response", rug=0,
                         xlab=" Estimated income (in $ 2013)",
                         ylab="P(Before15=1)", gg=TRUE) + 
        ylim(0,1) +theme_minimal()
      
      
      
      target_plot <- input$var
      
      if (target_plot == "age") {
        return(p.age)
      } else if (target_plot == "education") {
        return(p.education)
      } else if (target_plot == "aridity") {
        return(p.aridity)
      } else if (target_plot == "income") {
        return(p.income)
      }
      

    })

}
