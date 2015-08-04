#WRITING AND APPLYING FUNCTIONS
#Calculating percent baseline

#First, clear the R workspace
   rm(list=ls(all=TRUE))

#Now, set our working directory - files are read and write here
  setwd("P:/OSUcourse2015/Rcourse_Day3/4_Homework")

#Load the data - it's in a comma-separated variable file in "long" format
  nmdata <- read.csv("nmdata_cov.csv", stringsAsFactors=F, na.string=".")


#Change the name of the first column from CID to ID.  C is the comment symbol for NONMEM 
  names(nmdata)[1] <- "ID"

   
#It's the same data frame of 11 variables. 
#Let's also look at the "head" of the dataframe
   head(nmdata)


#--------------------------------------------------------------------------------------------------
#Note there is also column for MAP (Mean Arterial Pressure, in units of mmHg). 
#A clinician involved in the study wonders if these data might be better expressed as a percent of baseline?
#This is a classic job for R.


#We can't write a function (like BSAcalc) and apply it to the whole MAP column because each subjects
#has their own, different baseline MAP value.

#We need to write a function that calculates MAP as a percent of baseline for each subject.
#Then we apply the function subject by subject using the "applyBy" packages plyr or doBy

#Start by making a subset of data for one subject and looking at it:

testdf <- subset(nmdata, ID==1)
testdf

#OK, what is the baseline?
#We could identify it in a number of ways.
#In this dataset it is seems time 0 always corresponds to the first row for each subject.
#We could use the head function to find the first entry: head(x,1)

testdf$MAP

head(testdf$MAP,1)

testdf$MAP/head(testdf$MAP,1)*100


#Now let's turn this idea into a function
#percent_baseline <- function(mapin) mapin/head(mapin,1)*100

percent_baseline <- function(mapin)
 {
  baseline <-  head(mapin,1)
  MAPpercent <- mapin/baseline*100
 }

MAPpercent <- percent_baseline(testdf$MAP)

#This only works for one subject.
#Now we will use the lapplyBy function from the doBy package to apply to all subjects

library(doBy)

result <- lapplyBy(~ID, data=nmdata, function(df) percent_baseline(df$MAP))

#This looks OK, but the result is a list.
#You haven't met the list class of objects yet!
#We need to the results back into the nmdata dataframe with the unlist function
nmdata$MAPpercent <- unlist(result)

head(nmdata)


#----------------------------------------------------------------------------
#We have generated the MAP percent baseline data.
#Let's generate a "publication ready" plot to show the clinician
#We will do some summary statistics

  #Define a function for length without NA's
     lengthNA <- function(x) length(na.omit(x))

  #Define a function for Mean, sd and CV
      sumfuncCV <- function(x)
      {
        stat1 <-  mean(x, na.rm=T)
        stat2 <-  sd(x, na.rm=T)  
        stat3 <-  stat2/stat1*100
        stat4 <-  lengthNA(x)
        stat5 <-  stat2/sqrt(stat4)  #sd/sqrt(n)
        result <- c("mean"=stat1, "sd"=stat2, "cv"=stat3, "n"=stat4, "sem"=stat5)
        result
      }

   #Calculate the summary statistics
     MAPstats <- summaryBy(MAPpercent~TIME, data=nmdata, FUN=sumfuncCV)
     #Some text manipulation to rename the columns
     names(MAPstats) <- gsub("MAPpercent.","",names(MAPstats))

   #Plot 1
   library(ggplot2)
   
   #Introducing a a new geometry - geom_errorbar 
    plotobj1 <- ggplot(MAPstats) + theme_bw()
    maintext <- "Mean Arterial Blood Pressure\nmean and sem\n"
    plotobj1 <- plotobj1 + geom_point(aes(x=TIME, y=mean),  size=1.5)  
    plotobj1 <- plotobj1 + geom_line(aes(x=TIME, y=mean), size=0.5, linetype="dashed")
    plotobj1 <- plotobj1 + geom_errorbar(aes(x=TIME, ymax = mean + sem, ymin=mean - sem), size=0.5, width=0, position=position_dodge(width=3) )
    plotobj1 <- plotobj1 + geom_hline(yintercept=100, linetype="dotted")
    plotobj1 <- plotobj1 + scale_x_continuous("Time after dose (min)",lim=c(0,600), breaks=seq(0,600,by=60)) 
    plotobj1 <- plotobj1 + scale_y_continuous("MAP(% baseline)", lim=c(50,150))
    plotobj1 <- plotobj1 + ggtitle(maintext)
    plotobj1

    ggsave("plot1.png", width=5,height=4)


#----------------------------------------------------------------------------
#Well, thats a pretty neat looking plot.
#Nothing to worry about for MAP with this study.
#But does it tell the whole story?  We will investigate with a few more plots.
#First we will look at MAPpercent for each subject

   plotdata <- nmdata
   plotdata$Subject <- as.factor(plotdata$ID)

   plotobj2 <- ggplot(plotdata) + theme_bw()
   plotobj2 <- plotobj2 + geom_line(aes(x=TIME,y=MAPpercent, colour=Subject))
   plotobj2 <- plotobj2 + scale_x_continuous("Time after dose (min)",lim=c(0,600), breaks=seq(0,600,by=60)) 
   plotobj2 <- plotobj2 + scale_y_continuous("MAP(% baseline)") 
   plotobj2 <- plotobj2 + theme(legend.position="none")                   
   plotobj2

   ggsave("plot2.png", width=5,height=4)


#----------------------------------------------------------------------------
#Now we will look at the raw MAP data for each subject

   plotobj3 <- ggplot(plotdata) + theme_bw()
   plotobj3 <- plotobj3 + geom_line(aes(x=TIME,y=MAP, colour=Subject))
   plotobj3 <- plotobj3 + scale_x_continuous("Time after dose (min)",lim=c(0,600), breaks=seq(0,600,by=60)) 
   plotobj3 <- plotobj3 + scale_y_continuous("MAP(mmHg)", lim=c(50,180))
   plotobj3 <- plotobj3 + theme(legend.position="none") 
   plotobj3

   ggsave("plot3.png", width=5,height=4)


#------------------------------------------------------------------------------------------------------------------- 
##Questions##
#Email or upload your answers to Dr Phelps

#1. What does Plot 1 tell us about the patients in the study?
#   What does sem tell us?
#   Hint: Do we want to know what the time-course of MAP was if this study was repeated 1000 times?
#   Hint: Or do we want to know if any subject had unusual MAP that might affect their PK?
#   Hint: Or both?   

#2. What information is lost when data are expressed as percent baseline?
#   Hint:  Think about the distribution of baseline MAP in the subjects 

#3. Can you apply a function to find the minimum MAP in each subject?
#   Hint:  R has function called min, that complements the max function.

#4. Were there any subjects with a baseline MAP below 60 mmHg, where renal perfusion might be compromised?
#   What is the ID number of these subjects?

#5. The clinician wants to use plot1.png to support the argument that MAP did not change during the course of the study.
#   Is this appropriate?
#   Hint: Would standard deviation or sem be better to summarize what happened in this study?


   
