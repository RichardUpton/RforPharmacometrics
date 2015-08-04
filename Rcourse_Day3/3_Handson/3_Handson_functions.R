#WRITING AND APPLYING FUNCTIONS

#Our colleague needs help with her NONMEM data again.
#Can we summarize the demographic data?


#First, clear the R workspace
   rm(list=ls(all=TRUE))

#Now, set our working directory - files are read and write here
  setwd("P:/OSUcourse2015/Rcourse_Day3/3_Handson")

#Load the data - it's in a comma-separated variable file in "long" format
  nmdata <- read.csv("nmdata_cov.csv", stringsAsFactors=F, na.string=".")


#Change the name of the first column from CID to ID.  C is the comment symbol for NONMEM 
  names(nmdata)[1] <- "ID"

   
#It's the same data frame of 11 variables. 
#Let's also look at the "head" of the dataframe
   head(nmdata)


#The covariate columns appear to be AGE, WEIGHT, HEIGHT and SEX.
#There is also column for MAP (Mean Arterial Pressure). 
#Its debatable whether this is a covariate or depedent variable.  We will treat it as a covariate here.
#In a population PK modeling report, it's common to provide a table of patients numbers
#and summary statistics for the covariates.


#The first request is to add Body Surface Area (BSA) and Body Mass Index (BMI) to the database
#The most error-free way to do this is to write out own functions to calculate these from weight and height


#----------------------------------------------------------------------
#Let's start with a function for BSA

#Calculate BSA using the Mosteller formula 
#BSA (m²) = sqrt ( [height (cm) × weight (kg) ]/ 3,600 )

BSAcalc <- function(heightcm,weightkg)
  {
   BSA <- sqrt(heightcm*weightkg/3600)
  }

#Let's test it for a standard size 170 cm and 70 kg man - it should be about 1.8 m2
  test <- BSAcalc(170,70)

#How to we calculate BSA for every subject in the study?
#The function can take columns of numbers as the argument
  nmdata$BSA <- BSAcalc(nmdata$HEIGHT,nmdata$WEIGHT)

#----------------------------------------------------------------------
#And now a function for BMI
#BMI (kg/m²) = weight(kg)/(height(cm)/100)2

BMIcalc <- function(heightcm,weightkg)
  {
   BMI <- weightkg/(heightcm/100)^2
  }

#Let's test it for a standard size man - it should be about 25 kg/m2
  test <- BMIcalc(170,70)

#Now calculate BMI for every subject in the study?
  nmdata$BMI <- BMIcalc(nmdata$HEIGHT,nmdata$WEIGHT)


#Now we have these two handy functions, we don't need to write them and check them every time.
#We can start a collection of our own functions in a text file, and "source" them
  source("myfunctions.R")

## Warning - you can overwrite old functions by making a new function with the same name ##


#----------------------------------------------------------------------
#Now we can move on to the summary statistics
#R has base functions such as mean - start by looking at the help for mean
  #?mean
  #mean has an argument of x, where x is an R object
  #let's test the function

  mean(c(3,4,5))
  #This seems to produce the right result

#Let's calculate the mean weight of the subjects
  mean(nmdata$WEIGHT)

#Is this right answer?  Yes, but for the wrong reason 
  length(nmdata$WEIGHT) 
  #This is the mean of 300 observations, not 20 subjects.
  #This is a property of "long" format datasets, and is only right here because each subject
  #has the same number of observations, and weight does not change within a subject

#We will make a subset of the data for summarizing the covariates.
#We are lucky, because every patient has a TIME==0 record that represents baseline values of the covariates
  covdata <- subset(nmdata, TIME==0)

  dim(nmdata)
  dim(covdata)

#Note that (in this analysis) AGE WEIGHT HEIGHT SEX BSA & BMI are time-independent covariates.
#They have the same value for all records within a patient.
#Also note that (in this analysis) MAP is a time-dependent covariate.
#It has different values for the records within a patient reflecting changes during the observation period.
#Our summary here therefore reflects baseline MAP (prior to the fentanyl dose)

#Often we are required to summarise covariates by subgroups of the data
#In this study, DOSE and ROUTE might be important.  Is their any heterogeneity in the allocation of
#subjects to these groups?
#There are several packages that allow functions to be "applied" to subgroups.
#We will use one called doBy (plyr is another useful one)
  library(doBy)

#doBy has a function summaryBy that is used as follows:
  meanweights <- summaryBy(WEIGHT~DOSE+ROUTE, data=covdata, FUN=mean)
  meanweights

#Now we have a value of the mean weight for each of the 4 combinations of dose and route

#This sort of handy, but we would also like to know the standard deviation of the weights.
#To do that, we will need to write a custom summary function.
  sumfunc <- function(x)
   {
    stat1 <-  mean(x, na.rm=T)
    stat2 <-  sd(x, na.rm=T)  
    result <- c("mean"=stat1,"sd"=stat2)
    result
   }

#Let's test this function
   sumfunc(c(3,4,5))

#Note that it returns two values - the mean and standard deviation.
#Note also that we have given the result descriptive names, which help identify the output of the function
#Lets apply this function to all the covariates.

  demodf1 <- summaryBy(AGE+WEIGHT+HEIGHT+MAP+BSA+BMI~ROUTE+DOSE, data=covdata, FUN=sumfunc)
  demodf1 <- demodf1  

#--------------------------------------------------------------------------------------
#Now we will make a more extensive summary function
#We calculate 9 different summary statistics and also make sure will handle missing data by removing it.
#R has a default of not returning an answer when a NA is in a vector
    mean(c(2,NA,5))
    mean(c(2,NA,5), na.rm=T)
    #Some functions have an argument for handling missing data.
    #Or we can use the na.omit function


#Define a function for length without NA's - there is no standard function for this in R.
   length(c(2,NA,5))
   #This counts NA's
   na.omit(c(2,NA,5))
   #This returns the input with NA's ommitted
   length(na.omit(c(2,NA,5)))
   #This works!

   #Make our own version of "length" to that doesn't count NA's
   lengthNA <- function(x) length(na.omit(x))
   
 
#Define a function for geometric mean
  geomean <- function(x, na.rm=F)
   #Note x cannot be negative or zero 
   {  
    if (na.rm==T) x <- x[is.na(x)==F]
    exp(mean(log(x)))
   }
 
 
#The summary function 
  sumfunc2 <- function(x)
  {
    stat1 <-  geomean(x, na.rm=T)
    stat2 <-  mean(x, na.rm=T)
    stat3 <-  sd(x, na.rm=T)  
    stat4 <-  stat3/stat2*100   #Coefficient of variation as a percentage
    stat5 <-  min(x, na.rm=T)  
    stat6 <-  quantile(x, probs=0.05, na.rm=T, names=F)  #Lower 90% CI
    stat7 <-  quantile(x, probs=0.95, na.rm=T, names=F)  #Upper 90% CI
    stat8 <-  max(x, na.rm=T) 
    stat9 <-  lengthNA(x)
    result <- c("gmean"=stat1, "mean"=stat2, "sd"=stat3, "cv"=stat4, "min"=stat5, "lo90"=stat6, "hi90"=stat7, "max"=stat8, "n"=stat9)
    result
  }


#Now run the new summary function
  demodf2 <- summaryBy(AGE+WEIGHT+HEIGHT+MAP+BSA+BMI~DOSE+ROUTE, data=covdata, FUN=sumfunc2)
  demodf2 
 
#----------------------------------------------------------------------------------------------------------
#One more request. Can we make a data flag for elderly subjects, and summarize the demographics for these too?
   #The cut function assigns a factor based on breaks in the data
   #The following makes a new column, with age split into 2 groups - 20 to < 52 and 52 to 100
   covdata$AGEBIN <- cut(covdata$AGE, breaks=c(20,52,100), include.lowest=TRUE, right=FALSE)
   
   #Check that the binning worked as we expected
    table(covdata$AGE,covdata$AGEBIN)

   #The factor names are [20,52) and [52,100] where [ means inclusive and ) means exclusive
   #Now we can count the number of subjects in each age bin 
    table(covdata$AGEBIN)
 
   #Now convert AGEBIN into a numeric NONMEM style flag (easier to process numbers in R sometimes too!)
    covdata$AgeBin <- as.numeric(covdata$AGEBIN)
    table(covdata$AGEBIN,covdata$AgeBin)
    #Remember our new data flag  1=young, 2=elderly

#Now we will run the summary function again
   demodf3 <- summaryBy(AGE+WEIGHT+HEIGHT+MAP+BSA+BMI~AgeBin, data=covdata, FUN=sumfunc2)
   demodf3


#------------------------------------------------------------------------ 
#So we have learnt about writing and applying functions
#Time to produce a demographic table we can import into a report or thesis!

#Also a demographic summary for all subjects is important too
  demodf4 <- summaryBy(AGE+WEIGHT+HEIGHT+MAP+BSA+BMI~1, data=covdata, FUN=sumfunc2)
  demodf4   


#Now we will merge the demographics dataframes, transpose and write to a file
#Let's check what we have so far
   head(demodf4)  #All subjects
   head(demodf2)  #By dose & route
   head(demodf3)  #By age bin

#We need to combine these to write to a file, but can't do this unless the column names match
#This is pure, ugly data wrangling.  Sometimes it needs to be done.
#We are removing the old identification columns and making a new one called "Group"
   demodf4 <- data.frame("Group"="All subjects",demodf4)
   demodf2 <- data.frame("Group"=paste("Dose =",demodf2[,1],"Route =",demodf2[,2]),demodf2[,-(1:2)])
   demodf3 <- data.frame("Group"=paste("Age bin =",demodf3[,1]),demodf3[,-(1)])
   
   #rbind joins dataframes by row
   demodfall <- rbind(demodf4,demodf2,demodf3)


#OK, most of the hard work is done - we will finish any formatting in Excel and or Word  
   write.csv(demodfall, file="demographics_summary.csv", row.names=F)


