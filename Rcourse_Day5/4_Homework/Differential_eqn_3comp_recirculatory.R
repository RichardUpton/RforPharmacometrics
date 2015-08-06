#SCRIPT TO A SIMULATE A THREE COMPARTMENT RECIRCULATORY MODEL USING DIFFERENTIAL EQUATIONS
#Recirculatory models are a type of semi-physiological PK models
#See for example:
#Upton RN, Foster DJ, Christrup LL, Dale O, Moksnes K, Popper L.
#A physiologically-based recirculatory meta-model for nasal fentanyl in man.
#J Pharmacokinet Pharmacodyn. 2012 Oct;39(5):561-76. doi: 10.1007/s10928-012-9268-y. Epub 2012 Aug 19.


#remove all current objects in the workspace
 rm(list=ls(all=TRUE))
 graphics.off()
 
#Set the working directory
  master.dir <- "E:/RforPharmacometrics/Rcourse_Day5/4_Homework"    #PRI Server
  setwd(master.dir)  

#Load libraries
  library(ggplot2)
  library(doBy)
  library(plyr)
  library(grid)
  library(deSolve)

#---------------------------------------------------------------------------------------------------
#Define simulation function
#Define Parameter values - i.v. doses 

sim_3comp_recirc <- function(Rate,Tinf,lasttime,CO,Qhep,CLhep,Vlung,Vhep,Vbody)
{  #begin
   
  #Define infusiion rate values at the key change points
  TIMEinf <- c(0,Tinf,lasttime*2)  #*2 needed to make the function work long after the last sim time
  RATEinf <- c(Rate,0,0)
    
  #Define and interpolation function that returns rate when given time - "const" give step interpolation 
  step.doseinf <- approxfun(TIMEinf, RATEinf, method = "const")
  
  #Define times
  #units are min, L, mg - mg/L = ug/ml
  TIME <- sort(unique(c(seq(from=0,to=lasttime,by=0.5),0.001,Tinf-0.001,Tinf+0.001))) 
  
               
    #Function containing differential equations for amounts in compartments (A) - see help for lsoda
          DES <- function(T, A, THETAin)
          {
              #Infusion specifications - by approxfun interpolation
              RateIn <- step.doseinf(T)
                            
              dA <- vector(len=3)
              Qbody <- CO-Qhep
              dA[1] <- (RateIn -CO*A[1] +Qhep*A[2] +Qbody*A[3])/Vlung  
              dA[2] <- (Qhep*A[1] -Qhep*A[2] -CLhep*A[1])/Vhep  
              dA[3] <- (Qbody*A[1] -Qbody*A[3])/Vbody  
            
              list(dA)
          }

    #Set initial conditions - use names here to set names of output dataframe made by lsoda 
     A_0 <- c(A1=0,A2=0,A3=0) 

     paramlist <- c("RateIn"=RATEinf,"CO"=CO,"Qhep"=Qhep,"CLhep"=CLhep,"Vlung"=Vlung,"Vhep"=Vhep,"Vbody"=Vbody)

    #Run differential equation solver 
     sim.data <- lsoda(A_0, TIME, DES, paramlist)  
       
    #Process the simulated output 
      sim.data <- data.frame(sim.data)
      sim.data$Cart <- sim.data$A1
      sim.data$Chep <- sim.data$A2
      sim.data$Cbody <- sim.data$A3
      sim.data$Cart[sim.data$time==0] <- 0
                      
    #Draw the plot
     plotobj <- NULL
     titletext <- paste("AN EXAMPLE RECIRCULATORY MODEL\nRed=Arterial, Blue=Venous, Orange=Hepatic\n")
     plotobj <- ggplot(data=sim.data)
     plotobj <- plotobj + geom_line(aes(x=time, y=Cart), size=1, alpha=0.75, colour="red", size=2)  #Arterial blood
     plotobj <- plotobj + geom_line(aes(x=time, y=Cbody), size=1, alpha=0.75, colour="blue")        #Venous blood
     plotobj <- plotobj + geom_line(aes(x=time, y=Chep), size=1, alpha=0.75, colour="orange")       #Hepatic vein blood
     plotobj <- plotobj + scale_y_continuous("Concentration (ug/ml)")
     plotobj <- plotobj + scale_x_continuous("Time (min)")
     plotobj <- plotobj + annotate("text", x = max(sim.data$time)*0.8, y = max(sim.data$Cart)*0.8, label = paste("DoseRate =",Rate), colour = "black", size = 6)	
     plotobj <- plotobj + annotate("text", x = max(sim.data$time)*0.8, y = max(sim.data$Cart)*0.7, label = paste("CLhep =",CLhep), colour = "black", size = 6)	
     plotobj <- plotobj + annotate("text", x = max(sim.data$time)*0.8, y = max(sim.data$Cart)*0.6, label = paste("Qhep =",Qhep), colour = "black", size = 6)	 
     plotobj <- plotobj + ggtitle(titletext) 
     print(plotobj)
         
} #end


#Use the function to simulate a subject given the parameter values
  sim_3comp_recirc(Rate=100,Tinf=60,lasttime=400,CO=6,Qhep=2,CLhep=1,Vlung=2,Vhep=1,Vbody=25)

