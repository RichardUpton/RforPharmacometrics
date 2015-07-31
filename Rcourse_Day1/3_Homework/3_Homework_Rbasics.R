#A SCRIPT FOR CHECKING A NONMEM DATABASE

#The data we will be looking at is the iconic "warfarin" dataset
#Clarify the units of amount, volume and time early in an analysis and use them consistently throughout!

#This analysis is done using Base graphics only - ggplot2 a better alternative that is covered on Day 2

#Clear the workspace
 rm(list=ls(all=TRUE))

#Set the working directory
 #Change this to suit the path for your computer
 setwd("P:/OSUcourse2015/Rcourse_Day1/3_Homework")

#Read the data
  nmdata <- read.csv("warfarinpkpd_31Mar13.csv")
 
#Yipee, my first database in R.  Lets draw a plot!
  plot(DV~TIME, data=nmdata)

#Well, that's interesting.  I was expecting to see concentration plotted against time. 
#What happened?  This is what you get when a factor (a category) is plotted against a number 

#So what have we got?
  dim(nmdata)

  str(nmdata)

#Does everything make sense?  Why are ID, AMT & DV of class factor?
  
  sort(unique(nmdata$ID))
  #One of the levels is #12.  This is the comment character for NONMEM, so the company has commented out this row
  #What are the sort and unique functions doing here?

  sort(unique(nmdata$AMT))
  #One of the levels is ".".  This is the missing character for NONMEM, but it's not the missing character for R.

#Can we start again? 
  #First edit out the #commented row
  #Second, overide the string as factors default
  #Third, tell R that "." is the NA character for this data set
  #We do all of this as arguments for the read.csv function

#Read the data again
  nmdata <- read.csv("warfarinpkpd_31Mar13.csv", comment.char="#", stringsAsFactors=F, na.strings=".")

  dim(nmdata)

  str(nmdata)

  plot(DV~TIME, data=nmdata)

#This looks better, but there are some unusual concentration versus time plots.
#Oh, I forgot there are PK and PD data in this dataset!
#They are both in the DV column, but are marked by the DVID flag.  1 = PK, 2 = PD
#Let's split the data into PK and PD subsets

#The PK data
  pkdata <- subset(nmdata, DVID==1)

  #Plot the PK data
  plot(DV~TIME, data=pkdata)

  #How about a log scale
  plot(log(DV)~TIME, data=pkdata)

#The PD data
  pddata <- subset(nmdata, DVID==2)

  #Plot the PK data
  plot(DV~TIME, data=pddata)


#Well these data looks plausible.  What about the dose events?
  #The dose data
  dosedata <- subset(nmdata, is.na(AMT)==F)

  dosedata

#Looks like one dose per subject at TIME=0. Not everyone got the same dose.
#Was it weight based dosing?
 
  plot(AMT~WT, data=dosedata)

#These all fall on a straight line, so yes it looks like weight based dosing

  dosedata$AMTPERKG <- dosedata$AMT/dosedata$WT

  #Does this match the protocol?  Are the AMT units right?
  dosedata$AMTPERKG


#Now check the covariate data.  First, subset the covariates.  There are no time-dependent covariates so subsetting on TIME == 0 should do.
  covdata <- subset(nmdata, TIME==0)

  covdata

#Oops, this got 2 rows per subject.
  covdata <- subset(nmdata, TIME==0 & MDV==1)  
  
  covdata

#How many subjects, and what are there ID numbers?
  IDlist <- sort(unique(covdata$ID)) 

  IDlist

  #There are 31 subjects - don't be fooled by ID 10, which isn't in the database
  length(IDlist)

#Draw some index plots to look for outlier values of covariates
  plot(WT ~ ID, data=covdata)
  #Are these reasonable, or are they in pounds rather than kg?

  plot(AGE ~ ID, data=covdata)
  #Are these reasonable?  What are the later subjects with the higher ID's all so young?


  plot(SEX ~ ID, data=covdata)
  #The sex codes are 0 and 1, but which are male and which are female?  Is there enough subjects of SEX==0 to use SEX as a covariate?


#What is the relationship between the covariates?
  plot(WT ~ AGE, data=covdata)
  #Are weight and age correlated?  Highly correlated covariates can make it hard to distinguish a unique covariate effect.
  
#Some column wise summary statistics
  WTmean <- mean(covdata$WT)
  WTmean
  WTmin <- min(covdata$WT)
  WTmin
  WTmax <- max(covdata$WT)
  WTmax

  AGEmean <- mean(covdata$AGE)
  AGEmean
  AGEmin <- min(covdata$AGE)
  AGEmin
  AGEmax <- max(covdata$AGE)
  AGEmax

#Please email these summary statistics to Dr Phelps.
#Hint - copy the results out of the R console