#A SCRIPT TO ILLUSTRATE MERGING DATA TO MAKE A NONMEM DATAFILE

# We have been asked to provide pharmacometric support for a small volunteer study

# It's that subcutaneous fentanyl volunteer study again
# 20 subjects, 2 doses (50 & 200 ug), 2 routes (intravenous and subcutaneous)

# The data have been presented as three spreadsheets:
#   concs.csv
#   doses.csv
#   covariates.csv

#Take the time to open these in Excel

#Oh great, these are nice and neat and already well-structured.
#This will make our job easier!
  

#First, clear the R workspace
   rm(list=ls(all=TRUE))


#Now, set our working directory - files are read and write here
  setwd("P:/OSUcourse2015/Rcourse_Day4/3_Handson")


#Load libraries
  library(reshape)

#Read the file of concentration data
  concdatain <- read.csv("concs.csv")
  head(concdatain)
  dim(concdatain)

  
#Use the reshape package to convert from a wide to a long format
   concdata_melt <- melt(concdatain, id=c("TIME"), na.rm=T)
 
   head(concdata_melt)
   dim(concdata_melt)
 
   
   concdata <- cast(concdata_melt, variable + TIME ~ ...) #formula is column ~ row, ... is any other variable
   head(concdata)
   dim(concdata)
  
   
#Neaten up
   names(concdata) <- c("ID","TIME","DV")    #rename columns
   concdata$ID <- gsub("ID","",concdata$ID)  #remove unwanted text in ID column
   concdata$ID <- as.numeric(concdata$ID)    #change text to numbers

   
#Read a file of dose data
  dosedata <- read.csv("doses.csv")
  head(dosedata)
  dim(dosedata)


#Read a file of covariate data
  covdata <- read.csv("covariates.csv")
  head(covdata)
  dim(covdata)
  
 
#Merge
  alldata <- merge(concdata,dosedata, all=T)
  alldata <- merge(alldata,covdata, all=T)
  
  
#Tidy up the merged data

  alldata$MDV[is.na(alldata$MDV)==T] <- 0   #Assign remaining missing data flags
  alldata$TIME[alldata$TIME==-0.1] <- 0     #Adjust time for dose events - this was a hack
    
  alldata <- subset(alldata, select=c(-Description))  #Remove description column
  names(alldata)[1] <- "CID"                          #Comment out column labels
 
  
#Write in NONMEM format
  #We specify a couple of time-saving arguments.
  # with na="." we write NA's in R as dots, the NONMEM standard
  # we don't won't quotes or rownames
  write.csv(alldata, file="nmdata.csv", na=".", quote=F, row.names=F)
   
