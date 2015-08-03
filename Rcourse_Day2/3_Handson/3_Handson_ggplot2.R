#AN INTRODUCTION TO DATA PLOTING USING GGPLOT2

#A colleague has sent us a data set in NONMEM format.
#It is a pharmacokinetic study of fentanyl in 20 volunteers
#There are 2 dose groups (50 & 200 ug) and 2 routes of administration (i.v. & s.c.)
#Can we draw some plots of the data to help her decide how to best model the data?
#Of course we can!  This should only take an hour or so!

#First, clear the R workspace
   rm(list=ls(all=TRUE))

#Now, set our working directory - files are read and write here
  setwd("P:/OSUcourse2015/Rcourse_Day2/3_Handson")

#Load the data - it's in a comma-separated variable file in "long" format
  nmdata <- read.csv("nmdata.csv", stringsAsFactors=F, na.string=".")

#Change the name of the first column from CID to ID.  C is the comment symbol for NONMEM 
  names(nmdata)[1] <- "ID"

#What have we got?  Let's have a look first
#We will use the structure function  
  str(nmdata)
   
#It's a data frame of 11 variables.  The names look self-explanatory
#Let's also look at the "head" of the dataframe
   head(nmdata)

#How many subjects are there, and what are their ID numbers?
#The unique function is handy
  unique(nmdata$ID)

#OK, that's easy.  Numbered 1 - 20
#Where is our concentration versus time data?
   names(nmdata)

#Looks like classic NONMEM - a TIME column and a DV dependent variable column
#Can plotting with the basic plotting functions help?
  plot(DV ~ TIME, data=nmdata)

#OK, that helped.
#The data goes out to 600 on the time scale.  But is that hours, minutes or day?
#The data goes out to over 5 on the DV scale.  But is that ug/ml or ng/ml or nmol?
#We need to decide on the units of our graphical analysis and stick to them!
#Our colleague sends us another file of column descriptions - nmdata_column_descriptions.csv
#Concentration is in ng/ml = ug/L.  Looks like the standard units should be ug, L, min
#So dose is in ug, concentration is in ug/L (ng/ml), clearance is in L/min, volume is in L etc. 


#Are there any missing DV data?
#We can subset this to help identify
   missingdata <- subset(nmdata, is.na(DV)==T)
   missingdata

#Looks like data are missing early and late in the sampling period.
#Why are they missing?  Samples not taken?  Samples taken but unuseable due to hemolysis?  Was fentanyl undetectable?
#Our colleague sends us an email - all the missing samples were below the lower limit of quantification of the assay
#LLOQ is 0.05 ng/ml 
#Now we have all the information we need!


#We will revisit our basic plot.
   plot(DV ~ TIME, data=nmdata, type="b")

#Some early concentrations are high (the i.v. doses?), some are low (the subcutaneous doses?)
#How can we see what happened for each individual subject?
#The basic plot functions reach their limit here.
#We will swap to using the ggplot2 packaage for our graphical analysis

  library(ggplot2) 
 

#ggplot builds graphs with building blocks - geometries,scales,facets,aesthetics(colour,shape,linetype,size) 
#This plot recreates the plot we did using basic plot functions
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV))
  plotobj


#We can change the colour of the points
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV), colour="blue")
  plotobj


#Colour specified outside aes() applies to all points
#But inside aes(), we can map a plot aesthetic to a column of the dataframe
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=ID))
  plotobj


#Now a continuous colour scale has been used to colour ID from 1 to 20
#ggplot2 has automatically made a legend
#But ID should really be a categorical variable.  We will make our own by turning ID into a factor called "Subject"
  nmdata$Subject <- as.factor(nmdata$ID)
  head(nmdata$Subject)


#Now redraw the plot
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=Subject))
  plotobj


#We can add a new geometry - points and lines
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=Subject))
  plotobj <- plotobj + geom_line(aes(x=TIME, y=DV, colour=Subject))
  plotobj

 
#One of the great features of the ggplot is the ability to facet plots
#By faceting on ID, we can look at each subject individually
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=Subject))
  plotobj <- plotobj + geom_line(aes(x=TIME, y=DV, colour=Subject))
  plotobj <- plotobj + facet_wrap(~Subject)
  plotobj


#We can specify plot aesthetic attributes that are based on columns in our dataframe
#Lets use colour to show the different routes of administration

  nmdata$Route <- factor(nmdata$ROUTE, labels = c("Intravenous","Subcutaneous"))
  head(nmdata$Route)

  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=Route))
  plotobj <- plotobj + geom_line(aes(x=TIME, y=DV, colour=Route))
  plotobj <- plotobj + facet_wrap(~Subject)
  plotobj
 

#Lets use symbol shape to show the different doses

  nmdata$Dose <- factor(nmdata$DOSE, labels=c("50 ug","200 ug"))
  head(nmdata$Dose)
 
  plotobj <- NULL
  plotobj <- ggplot(nmdata)
  plotobj <- plotobj + geom_point(aes(x=TIME, y=DV, colour=Route, shape=Dose))
  plotobj <- plotobj + geom_line(aes(x=TIME, y=DV, colour=Route, shape=Dose, group=Subject))
  plotobj <- plotobj + facet_wrap(~Subject)
  plotobj

  #What happens if we remove the facet?  Try commenting it out!

 
#------------------------------------------------------------------------------------------------------------
#Ok, we have learnt a few ggplot tools. Learn more by reading the manual at http://ggplot2.org
#Now let's get serious about quizzing these data for information
#We will build one plot adding layers as we go.

#First, clear any old data accumulated in plotobj1 by setting it to NULL - the empty variable in R
 plotobj1 <- NULL
 plotobj1 <- ggplot(nmdata)
 plotobj1 <- plotobj1 + geom_point(aes(x=TIME, y=DV, colour=Subject))
 plotobj1 <- plotobj1 + geom_line(aes(x=TIME, y=DV, colour=Subject))

#Back to where we started!  But let's include a facet on dose and route
 plotobj1 <- plotobj1 + facet_grid(Route~Dose)

#Add some captions and make a log 10 scale
 plotobj1 <- plotobj1 + ggtitle("Fentanyl PK in volunteers\n")
 plotobj1 <- plotobj1 + scale_y_continuous("Fentanyl concentration (ng/ml)")
 plotobj1 <- plotobj1 + scale_x_continuous("Time after dose (minutes)", breaks=seq(0,600,120))
 plotobj1

#Add a line showing the LLOQ
 plotobj1 <- plotobj1 + geom_hline(yintercept=0.05, linetype="dashed")
 plotobj1

#We can do summary statistics on the fly
 plotobj1 <- plotobj1 + stat_summary(aes(x=TIME, y=DV), fun.y=median, geom="line", colour="black")

#Save it to a file - this saves the current plot object.  Many output formats are available.
 ggsave("Conc_vs_time.png", width=10, height=8)

#This looks wordy!
#Sure, we but can recycle plot objects
#And we can recycle lines of code as our R code collection expands


#---------------------------------------------------------------------------------------------------------
#Let's make the log-concentration plot

 plotobj2 <- plotobj1
 plotobj2 <- plotobj2 + scale_y_log10("Fentanyl concentration (ng/ml)")
 plotobj2

 #Save it to a file
  ggsave("LogConc_vs_time.png", width=10, height=8)


#---------------------------------------------------------------------------------------------------------
#Are kinetics dose linear?
#We can plot dose normalized concentrations.
#For the same route, if kinetics are linear, normalized concentrations should be super-imposable
#Here the y variable is specified as DV/DOSE.  Remember DOSE has a value for every DV unlike AMT!
 plotobj3 <- NULL
 plotobj3 <- ggplot(nmdata)
 plotobj3 <- plotobj3 + geom_point(aes(x=TIME, y=DV/DOSE, colour=Dose), alpha=0.5)
 plotobj3 <- plotobj3 + geom_line(aes(x=TIME, y=DV/DOSE, colour=Dose, group=ID), alpha=0.5)
 plotobj3 <- plotobj3 + scale_colour_brewer(palette="Set1")
 plotobj3 <- plotobj3 + stat_summary(aes(x=TIME, y=DV/DOSE, colour=Dose), fun.y=mean, geom="line", size=1)
 plotobj3 <- plotobj3 + geom_hline(yintercept=0.05/50, linetype="dashed")
 plotobj3 <- plotobj3 + geom_hline(yintercept=0.05/200, linetype="dotted")
 plotobj3 <- plotobj3 + ggtitle("Dose normalized fentanyl concentrations\n")
 plotobj3 <- plotobj3 + scale_y_log10("Normalized concentration (ng/ml per ug)")
 plotobj3 <- plotobj3 + facet_wrap(~Route)
 plotobj3

 #Save it to a file
  ggsave("ConcNorm_vs_time.png", width=10, height=8)

 
#---------------------------------------------------------------------------------------------------------
#We can do other type of plots too
#What is the distribution of the ages for the subjects?

 covdata <- subset(nmdata, TIME==0)

 plotobj4 <- NULL
 plotobj4 <- ggplot(covdata)
 plotobj4 <- plotobj4 + geom_histogram(aes(x=AGE, y=..count..), binwidth=5, fill="blue")
 plotobj4

 #Save it to a file
  ggsave("Age_histogram.png", width=10, height=8)


#---------------------------------------------------------------------------------------------------------
#The png files can be imported easily into a Powerpoint slide set
#What can we tell our friend when we present the plots?

#1. The data that is BLQ is censoring the 50 ug data.  Don't rely on NCA metrics for this dose - the terminal half-life may be over-estimated.

#2. From the log-linear plot, i.v. kinetics show 2 or more exponential phases.  2 compartment model at least! 

#3. The terminal slope for the i.v. and the s.c routes look similar, suggesting s.c. absorption is not rate-limiting. 

#4. Bioavailability may be lower for the high dose s.c. dose group.  But numbers are low!

#5. There is a wide range of age in the database.  It might be possible to use age as a covariate.  But numbers are low!

#More is missed by not looking than by not knowing!
#If you can't see it in a graph, you can't model it!

