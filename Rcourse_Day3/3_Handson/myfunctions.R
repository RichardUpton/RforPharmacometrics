#My functions

BSAcalc <- function(heightcm,weightkg)
#Calculate BSA using the Mosteller formula 
#BSA (m²) = sqrt ( [height (cm) × weight (kg) ]/ 3,600 )
  {
   BSA <- sqrt(heightcm*weightkg/3600)
  }


BMIcalc <- function(heightcm,weightkg)
#BMI (kg/m²) = weight(kg)/(height(cm)/100)2
  {
   BMI <- weightkg/(heightcm/100)^2
  }
