#Homework
#Please email/upload your answers to Dr Phelps

  #Copy the simulation script and give it a new name
  #Increase the number of subject to 2000  Hint:  Line 62
 
  plot(ETA1 ~ ETA2, data=thetadf)
  #Why aren't ETA1 and ETA2 correlated?
  ##Answer
  #We did not specify a correlation structure in our model.  We could have used a covariance structure when defining ETA1 and ETA2 to do this

  plot(CL ~ V, data=thetadf)
  #Why are CL and V correlated?
  ##Answer
  #They are both affected by the common covariate body weight.

  
  hist(ETA1)
  #Is ETA1 normally distributed?
  ##Answer
  #Yes, by definition ETA1 will be normally distributed

  hist(CL)
  #Is CL normally distributed?  What distribution is it?  Hint: Look at Line 76
  ##Answer
  #CL will be approximately log-normally distributed.
  #It would be exactly log-normally distributed if there were no covariate effects, but the effect of weight on CL
  #will contribute additional "shape" to the distribution (e.g. imagine if there were only 2 weight values in the dataset - 1 and 100 kg)

  plot(KA ~ as.factor(SEX), data=thetadf)
  #Is KA a population parameter?  Is it the same for males and females?
  ##Answer
  #No, it does not have a stochastic component so it is a fixed effect parameter.
  #It is not the same between males and females because of a covariate effect.
  #Covariate influence on fixed-effect parameters is not common, by might be useful at times.  Allometric scaling being a key example.




