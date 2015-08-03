#How to read Xpose output files in R without using Xpose 

   setwd("P:/OSUcourse/Rcourse_Day2/1_Homework_Answers")

   sdtab <- read.table(file="SDTAB1", sep="", skip=1, header=T, stringsAsFactors=F, na.strings=c(".","***********","1.#INFE+00"))
   catab <- read.table(file="CATAB1", sep="", skip=1, header=T)
   cotab <- read.table(file="COTAB1", sep="", skip=1, header=T)
   patab <- read.table(file="PATAB1", sep="", skip=1, header=T)
   #Merge the xpose files
   fitdata <- cbind(sdtab,catab,cotab,patab)

   dim(fitdata)
   head(fitdata)