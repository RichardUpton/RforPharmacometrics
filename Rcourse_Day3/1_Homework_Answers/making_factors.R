#There is more than 1 way to make a factor!

#factor is the main function for making factors
#as.factor coerces its argument to a factor. It is an abbreviated form of factor. (from the help page)

#A numeric code for route of administration
ROUTE <- c(0,1,1,0,1,1,0,1,0,1)


#Making ROUTE a factor method 1 - make the level names at once
Route <- factor(ROUTE, labels=c("iv","sc"))
Route
table(Route,ROUTE)


#Making ROUTE a factor method 2 - rename the level names in a second step
Route <- as.factor(ROUTE)
Route
levels(Route) <- c("iv","sc")
Route
table(Route,ROUTE)