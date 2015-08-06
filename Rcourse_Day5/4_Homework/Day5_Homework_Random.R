#Homework
#Please email/upload your answers to Dr Phelps

  #Copy the simulation script and give it a new name
  #Increase the number of subject to 2000  Hint:  Line 62
 
  plot(ETA1 ~ ETA2, data=thetadf)
  #Why aren't ETA1 and ETA2 correlated?

  plot(CL ~ V, data=thetadf)
  #Why are CL and V correlated?

  hist(ETA1)
  #Is ETA1 normally distributed?

  hist(CL)
  #Is CL normally distributed?  What distribution is it?  Hint: Look at Line 76

  plot(KA ~ as.factor(SEX), data=thetadf)
  #Is KA a population parameter?  Is it the same for males and females?




