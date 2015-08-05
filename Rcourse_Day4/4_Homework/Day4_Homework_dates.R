#HANDLING DATA WITH DATES FOR SIMULATIONS

#There is a new anti-neoplastic.  We have population PK model for the drug, but the company
#wants us to simulate a proposed dose regimen 50 mg BID for 7 days in a 21 day dose cycle.
#We want to simulate concentrations at 2 h intervals (252 observations)
#It's much easier to think of this dose schedule as dates and clock times
#We could make this simulation database by hand - it might take a few hours and we could make a mistake
#Or we could write some R code to generate the data
#If we make it flexible, we can easily modify the script to simulate other dose regimens they may be interested in (75 mg QID)


library(doBy)

setwd("P:/OSUcourse2015/Rcourse_Day4/4_Homework")


#First, come to an understanding of what we want in the simulation database
#In Excel, open the file "nmdata_sim_50mgbid_7days_21daycycle.csv"
#See that it is only 1 subject.  When we simulate, we will use $SIM SUBPROBLEMS=1000 to make data for
#1000 patients getting this dose regimen
#The data and time help us "read" the dose regimen in terms of dates and dose times, as it was described to us.
#TAFD is time after first dose, an alternative expression of the dose and sample times in the dose regimen.
#We can see for example that we have blank DV every 2 hours, so simulated data will be created for these rows.
#AMTMG and AMT are the dose events, in units of mg and umol, respectively.
#DV is all blank, as it is a simulation.

#So that is what we want, how did we get there?
#We set up some "seed" spreadsheets.
#These spreadsheets have been carefully designed by us to "expand" into a full dataset when we merge them together.
#Have a look at these spreadsheets in Excel.


#Read seed data
 times <- read.csv("times.csv", stringsAsFactors=F)
 head(times)

 dates <- read.csv("dates.csv", stringsAsFactors=F)
 head(dates)
 #VST is a common code in these databases - C1D2 is Dose cycle 1, Day 2 etc.
 #We need a starting date - we have used 11/1/2007 but it doesn't matter what it is once TAFD is calculated

 doses <- read.csv("doses.csv", stringsAsFactors=F)
 head(doses)
 #These are the dates and times of the doses in the required dose cycle.


#Merge seed data
 blankdata <- merge(dates,times, all=T, sort=F)
  head(blankdata,20)
 blankdata <- merge(blankdata,doses, all=T, sort=F)
  head(blankdata,20)
 #This has made a dataframe close to what we want.

#Convert date and time to POSIXct format in seconds
    datetimestring <- paste(blankdata$DATE, blankdata$TIME)
    datetimestring <- strptime(datetimestring, format = "%m/%d/%Y %H:%M")  
    #strptime converts a string of characters representing time and date to a standard format called POSIXlt.
    datetimestring <- as.POSIXct(datetimestring)

#Convert first dose date and time to POSIXct format in seconds
    firstdosestring <- "11/1/2007  8:00"
    firstdosestring  <- strptime(firstdosestring, format = "%m/%d/%Y %H:%M")
    firstdosestring <- as.POSIXct(firstdosestring)

#Calculate TAFD
    blankdata$TAFD <- difftime(datetimestring,firstdosestring, units="hours")
    #It's a time object - we will convert it to a nummber
    blankdata$TAFD <- as.numeric(blankdata$TAFD) 

#Make sure the data are sorted by time
    #This function is from the doBy package
    #It will sort a data frame by column - like the "sort" option in the Data menu of Excel
    blankdata <- orderBy( ~TAFD+AMT, data=blankdata)  

#Remove negative TAFD
    blankdata <- subset(blankdata, TAFD >= 0)

#Make the last of our columns
  blankdata$ID <- 1
  blankdata$DV <- NA

#This step uses the select argument of subset to rearrange the order of the column - neatness only
  blankdata <- subset(blankdata, select=c(ID,DATE,TIME,TAFD,AMTMG,AMT,DV))
  
#Write to a file and we are done!
  write.csv(blankdata, file="nmdata_sim_50mgbid_7days_21daycycle.csv", na=".", row.names=F, quote=F)

#We use this new simulation database with our final model control stream.  Initial parameter values should be replaced with
#final parameter values for simulation.  Remember the nmctl command to do this?


#Questions.
#1.  Can you modify the script and the seed spreadsheets to makes a simulation database for 75 mg QD for 5 days?
#Hint - you need to modify doses.csv
#Hint - rename scripts and seed data for the new simulation to avoid confusion with the old one

  