#The basics of R
#We will cut and paste these commands into the R console

 setwd("P:/OSUcourse2015/Rcourse_Day1/2_Handson")

#1. R basics
# This removes all R object from the workspace.  A good thing to do at the start of a script.

  rm(list=ls(all=TRUE))


#1.1 Getting help

  #help(plot)

  #?plot

  #help.search("plot")


#1.2 Using the command line in the console window

  2 + 2
  
  10^2
  
  1:100


#1.3 Making an object (a vector) and assignment using <-

  time <- c(0,10,20,30,40,50,60)
  time


#1.4 How objects behave - vectorised operations

  test <- time+time
  test

  test <- time^2
  test
  
#Note how "test" was over-written without warning


#1.5 A graphics window opens when needed - make sure it is visible

  plot(test ~ time)
  #The graphics window can be closed when it is not needed


#1.6 Functions can have arguments (given in the help page)

  plot(test ~ time, col="red", type="b", main="An example")


#1.7 Named vectors and accessing the elements of a vector

  theta <- c(dose=50, Cl=0.5, V=10)
  theta

  names(theta)
 
  theta[3]
  
  theta["V"]
  
  #Subsetting
  theta[theta>10]


#1.8 An example using vectors for calculations
#Single compartment, bolus dose, first order elimination
#C = Dose/V*exp(-CL/V*t)

  conc <- 50/10*exp(-0.5/10*time)
  conc

  conc <- theta[1]/theta[3]*exp(-theta[2]/theta[3]*time)
  conc

  conc <- theta["dose"]/theta["V"]*exp(-theta["Cl"]/theta["V"]*time)
  conc

  plot(conc ~ time)


#1.7 Logical comparisons
   conc

   conc < 5
   
   conc <= 2

   conc==5



#1.9 Introducing NA as missing

   conc2 <- c(10,20,NA,30)

   #What is the mean of conc2?
   mean(conc2)

   #What is the mean if we remove the NA's
   mean(conc2, na.rm=T)  


#2.0 Reading data 

  alldata <- read.csv("morphine_n=6.csv")
  #"alldata" is an object of the class "data frame" - mixed numbers and text

  #This shows the venous blood concentration (ng/ml) of morphine in 6 elderly
  #patients after each received 5 mg s.c.


#2.1 Various ways to look at the imported data

  names(alldata)

  str(alldata)

  summary(alldata)

  table(time)


#2.2  Accessing dataframe elements - [] and $

  alldata["time"]
  
  alldata$time
  
  alldata[2,3]
  #second row, third column of alldata


#2.3 Factors
  #R turns text into factors by default
  #Factors are categorical variables, which are defined by their levels
  #The order is alphabetical unless specified otherwise
  class(alldata$proced)

  levels(alldata$proced)

  alldata$proced

  #Caution - factors can be represented by text strings that are numbers
  #The three objects below are all distinctly different
    beware1 <- c(10,11,12)
    beware1

    beware2 <- c("10","11","12")
    beware2

    beware3 <- as.factor(beware2)
    beware3

    #They look the same but are objects of different classes and will behave differently
    class(beware1)
    class(beware2)
    class(beware3)


#2.4 First, a subset of alldata is needed where the co-variate values aren't
#repeated for each timepoint - the values for time zero will suffice

  #Subset is a very handy command
  covariatedata <- subset(alldata, time==0)

  dim(alldata)
  dim(covariatedata)

  head(alldata)
  head(covariatedata)
 
 
#2.5 A quick look at the co-variates

 
  hist(covariatedata$age)
  
  hist(covariatedata$weight)

  table(covariatedata$sex)
  
  table(covariatedata$proced)

  table(covariatedata$proced,covariatedata$sex)

  plot(covariatedata$weight ~ covariatedata$sex)
  

 


#2.5 Basic plot of the pooled data for all 6 subjects
  
  plot(conc ~ time, data=alldata)

#More on plotting tomorrow.
