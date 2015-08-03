#This is the third app from the paper:
#Interactive Pharmacometric Applications Using R and the Shiny Package (pages 146–159)
#J Wojciechowski, AM Hopkins and RN Upton
#CPT: Pharmacometrics & Systems Pharmacology, March 2015, Vol 4, Issue 3, 146-159

#Set the working directory
setwd("P:/OSUcourse2015/Rcourse_Day1/00_Shinydemo")

#Load the Shiny package
library(shiny)

#Run the app, which consists of the files in the folder demoShinyApp
shinyAppDir("demoShinyApp",options=list(width="100%", height=1500))