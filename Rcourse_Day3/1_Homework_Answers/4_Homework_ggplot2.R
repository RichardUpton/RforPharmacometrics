#AN INTRODUCTION TO DATA PLOTING USING GGPLOT2

#A Visual Predictive Check (VPC) using ggplot2

# Our colleague has finished the NONMEM modeling of her fentanyl data
# Remember the pharmacokinetic study of fentanyl in 20 volunteers?
# There were 2 dose groups (50 & 200 ug) and 2 routes of administration (i.v. & s.c.)
# She has done a simulation of multiple versions of the index dataset using the final model ($SIM in NONMEM) 
# This simulated data set has NO observed data.  Only predictions from the model.
# If the parameters and their population distributions in the model are representative, 
# the simulated data should recreate the observed data

# Can we help her draw a VPC?
# It should be easy as we can recycle some of the ggplot2 code from the graphical analysis!


#First, clear the R workspace
   rm(list=ls(all=TRUE))

#Now, set our working directory - files read and write to here
  setwd("P:/OSUcourse2015/Rcourse_Day3/1_Homework_Answers")

#----------------------------------------------------------------------------------------------------------------------  
#Read the data
  
#Original data
   ORG.data <- read.csv("nmdata.csv", stringsAsFactors=F, na.strings=".")
   head(ORG.data)
   dim(ORG.data)
 
   ORG.data <- subset(ORG.data, MDV==0)

#Simulated data
   #The best parameters for the final NONMEM model were substituted back into the model (nmctl of Wings for NONMEM)
   #The control stream was changed from $EST to $SIM
   #Note that there are a few complications turning a NONMEM table file into a csv file.  We won't distract ourselves here.
   SIM.data <- read.csv("simdata.csv", stringsAsFactors=F, na.strings=".")
   
   #Note that SIM.data has an extra column - the simulation number (SIM) for the dataset
   head(SIM.data)
   dim(SIM.data)

   SIM.data <- subset(SIM.data, MDV==0)
 
#----------------------------------------------------------------------------------------------------------------------  
#Plot the VPC

 #Define a pair of functions for calculating the 90% confidence intervals of x
 #We will talk more about writing your own functions on Day3
 CI90lo <- function(x) quantile(x, probs=0.05)
 CI90hi <- function(x) quantile(x, probs=0.95)
 
 #Draw the VPC plot
 plotobj <- NULL
 plotobj <- ggplot(ORG.data) + theme_bw()
 #The observed data
 plotobj <- plotobj + geom_point(aes(x=TIME, y=DV), colour="blue", alpha=0.5)
 #Summary statistics for the observed data  (median and 90% CI)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=median, geom="line", colour="black", size=1)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=CI90lo, geom="line", colour="black", linetype="dashed", size=1)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=CI90hi, geom="line", colour="black", linetype="dashed", size=1)
 #Summary statistics for the simulated data (median and 90% CI)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), data=SIM.data, fun.y=median, geom="line", colour="red")
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), data=SIM.data, geom="ribbon", fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.5)
 #Add some prettiness
 plotobj <- plotobj + ggtitle("VPC - Fentanyl PK in volunteers\n")
 plotobj <- plotobj + scale_y_continuous("Fentanyl concentration (ng/ml)")
 plotobj <- plotobj + scale_x_continuous("Time after dose (minutes)", breaks=seq(0,600,120))
 plotobj <- plotobj + geom_hline(yintercept=0.05, linetype="dashed")  #Lower limit of quantification
 plotobj <- plotobj + facet_grid(ROUTE~DOSE)  #, scales="free"
 plotobj


 plotobj2 <- plotobj + scale_y_log10("Fentanyl concentration (ng/ml)")
 plotobj2
 
 ggsave("VPC_logscale.png")

 
 SIM.data20 <- subset(SIM.data, SIM<=20)
 
 #Draw the VPC plot
 plotobj <- NULL
 plotobj <- ggplot(ORG.data) + theme_bw()
 #The observed data
 plotobj <- plotobj + geom_point(aes(x=TIME, y=DV), colour="blue", alpha=0.5)
 #Summary statistics for the observed data  (median and 90% CI)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=median, geom="line", colour="black", size=1)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=CI90lo, geom="line", colour="black", linetype="dashed", size=1)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=CI90hi, geom="line", colour="black", linetype="dashed", size=1)
 #Summary statistics for the simulated data (median and 90% CI)
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), data=SIM.data20, fun.y=median, geom="line", colour="red")
 plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), data=SIM.data20, geom="ribbon", fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.5)
 #Add some prettiness
 plotobj <- plotobj + ggtitle("VPC - Fentanyl PK in volunteers\n20 Datasets ")
 plotobj <- plotobj + scale_y_continuous("Fentanyl concentration (ng/ml)")
 plotobj <- plotobj + scale_x_continuous("Time after dose (minutes)", breaks=seq(0,600,120))
 plotobj <- plotobj + geom_hline(yintercept=0.05, linetype="dashed")  #Lower limit of quantification
 plotobj <- plotobj + facet_grid(ROUTE~DOSE)  #, scales="free"
 plotobj
 
 plotobj2 <- plotobj + scale_y_log10("Fentanyl concentration (ng/ml)")
 plotobj2
  
 ggsave("VPC_logscale_20sims.png")


#------------------------------------------------------------------------------------------------------------------- 
##Questions##
#Email or upload your answers to Dr Phelps

#1. How many simulations of the index data were done?
unique(SIM.data$SIM)
max(SIM.data$SIM)

#2. Modify the plot so that a log y scale is used.  See the hands-on script!
 #plotobj2 <- plotobj + scale_y_log10("Fentanyl concentration (ng/ml)")
 #plotobj2
 
#3. Save the log-scale plot as a file.  See the hands-on script!
  #ggsave("VPC_logscale.png")

#4. Comment out the facet_grid line (Line 68) using #.  Is this plot useful?

#5. What if we only had 20 simulations of the data set? 
#   Can you use the subset function to plot only 20 simulations?  Hint: we would need SIM <= 20
#   What is the danger of not simulating enough versions of the index data set?
 #SIM.data20 <- subset(SIM.data, SIM<=20)
 #With only 20 simulations, the simulated medians and 90% CI are still too "random".  See the Central Limit Theorem!

#6. What do we think of the predictive performance of the model?
 #It seems OK.  The observed and simulated medians lie near each other.  The observed and simulated 90% CI lie near each other.  
 #Want a more quantitative answer?  See the the Normalized Prediction Distribution Error (npde).
 #Comets E, Brendel K, Nguyen THT and Mentre F. (2012). npde: Normalised prediction distribution errors for nonlinear mixed-effect models. R package version 2.0. http://CRAN.R-project.org/package=npde

#7. Look again at the log-scale VPC you saved as a file.  Some of the model predictions for the low dose group
#   are below the LLOQ of the assay.  Is this a problem with the model or the data?
  #It's the data!  The model is doing a plausible job of predicting what the concentrations would be if the LLOQ wasn't censoring the data.

