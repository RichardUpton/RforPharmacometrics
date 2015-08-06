#SCRIPT TO SIMULATE A SIMPLE POPULATION MODEL
#1 COMPARTMENT FIRST ORDER ABSORPTION

#remove any previous objects in the workspace
  rm(list=ls(all=TRUE))

#-----------------------------------------------------------------------------------------
#load libraries
 
  library(ggplot2)
  library(doBy)
  set.seed(1234)
 
#-----------------------------------------------------------------------------------------
#Set directory
 #source("P:/Resources_latest/functions_utility_8April13.R")

 work.dir <- "P:/OSUcourse2015/Rcourse_Day5/3_Handson"
 setwd(work.dir)

#-----------------------------------------------------------------------------------------
#Set up a sequence of observation times
  TIME <- seq(from=0, to=72, by=3)

#-----------------------------------------------------------------------------------------
#Set up a function to simulate the ith patient given their parameter values
simulate.1comp <- function(ID,AMT,CL,V,KA)
 {
   KEL <- CL/V
   Cplasma <- (AMT/V)*KA/(KA-KEL)*(exp(-KEL*TIME)-exp(-KA*TIME))
   result <- data.frame("TIME"=TIME,"CP"=Cplasma)
   result
 }      

#Test the function works
  testdata <- simulate.1comp(ID=1,AMT=100,CL=2,V=10,KA=0.5)

  head(testdata)

  plot(CP ~ TIME, data=testdata)

  plot(log(CP) ~ TIME, data=testdata)


#------------------------------------------------------------------------------------------ 
#Define population parameter (THETA) values
#Units are mg, L, h

   CLpop  <- 2
   Vpop  <- 10
   KApop <- 0.05 

#Define population variability (OMEGA) values
   CLbsv  <- 0.2
   Vbsv  <- 0.1


#-----------------------------------------------------------------------------------------------
#Construct a dataframe of parameter values representing a population of subjects
 
  #Patient characteristics
  nsub <- 20 
  AMT <- rep(100, times=nsub)
  WT  <- runif(nsub,min=50, max=150)
  SEX <- rbinom(nsub,1,p=0.51)

  #Random effects
  #Accounts for variability at the subject level
  #set.seed(123)
  ETA1   <- rnorm(nsub,mean=0,sd=CLbsv)
  ETA2   <- rnorm(nsub,mean=0,sd=Vbsv)
  ETA3   <- rep(0, times=nsub)
  
  #Individual parameter values  
  ID  <- 1:nsub
  CL  <- CLpop*exp(ETA1)*(WT/70)^0.75
  V   <- Vpop*exp(ETA2)*(WT/70)^1
  KA  <- KApop+ETA3
  KA[SEX==1] <- KA[SEX==1]*(1+0.2)
 
  #Combine in a dataframe
  thetadf <- data.frame(ID,AMT,CL,V,KA)
  thetadf

  covdf <- data.frame(ID,WT,SEX)
  covdf
 

#------------------------------------------------------------------------------------------ 
#Simulate

#Population predictions - use population parameter values
   simdataPRED <- simulate.1comp("ID"=0,"AMT"=100,"CL"=CLpop,"V"=Vpop,"KA"=KApop)
   names(simdataPRED) <- c("TIME","PRED")

   head(simdataPRED)
 
#Individual predictions - use individual parameter values collected in thetadf
   #mdply takes each row of thetadf as input for the function simulate.1comp - from the plyr package
   simdataIPRED <- mdply(thetadf,simulate.1comp)
   names(simdataIPRED) <- c("ID","AMT","CL","V","KA","TIME","IPRED")

   head(simdataIPRED)

#Combine PRED and IPRED
   simdata <- merge(simdataIPRED,simdataPRED, all=T)
   simdata <- orderBy(~ID+TIME, simdata)

   head(simdata)


#------------------------------------------------------------------------------------------ 
#Add residual error
  
   #Count the number of observations
   F <- simdata$IPRED
   nobs <- length(F)

   #Define the residual error value (SIGMA)      
   RUVprop <- 0.1   #Standard Deviation
   RUVadd  <- 0.05  #Standard Deviation

   #Epsilon accounts for variability at the observation level
   EPS1    <- rnorm(nobs,mean=0,sd=RUVprop)
   EPS2    <- rnorm(nobs,mean=0,sd=RUVadd)
   
   #Proportional and Additive Residual error model
   simdata$DV <- F*(1+EPS1) + EPS2

   head(simdata)

#------------------------------------------------------------------------------------------
#Write to file
  
    write.csv(simdata,file="simdata.csv", row.names=F)


#------------------------------------------------------------------------------------------
#Plot Plasma Concentration versus Time

   plotobj <- NULL
   plotobj <- ggplot(simdata)
   titletext <- "Simulated Plasma Concentration - 100 mg dose\n"
   xlabtext <- "Time (hours)"
   ylabtext <- "Concentration (mg/L)"
   plotobj <- plotobj + geom_point(aes(x=TIME,y=DV), colour="blue", size=2, alpha=0.5)
   plotobj <- plotobj + geom_line(aes(x=TIME,y=IPRED, group=ID), colour="blue", size=1, alpha=0.5)
   plotobj <- plotobj + geom_line(aes(x=TIME,y=PRED), colour="black", size=1)
   plotobj <- plotobj + scale_y_continuous(name=ylabtext)
   plotobj <- plotobj + scale_x_continuous(name=xlabtext)
   plotobj <- plotobj + ggtitle(titletext)
   plotobj

   ggsave("Conc_vs_time.png")

