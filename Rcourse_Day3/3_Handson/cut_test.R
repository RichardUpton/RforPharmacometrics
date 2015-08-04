#Testing the cut function

age <- c(0,1,1.9,2,2.1,2.9,3,3.1,3.9,4,4.1,5.9,6,6.1,11.9,12,12.1,17.9,18,18.1,49.9,50,51)

age.bins <- c(2,3,4,6,12,18,50)

age.group1 <- cut(age, breaks=age.bins, include.lowest=TRUE)

age.group2 <- cut(age, breaks=age.bins, include.lowest=FALSE)

age.group3 <- cut(age, breaks=age.bins, include.lowest=FALSE, right=FALSE)

age.group4 <- cut(age, breaks=age.bins, include.lowest=TRUE, right=FALSE)

result <- data.frame(age,age.group1,age.group2,age.group3,age.group4)

result

#Ages are:
#2 - <3
#3 - <4
#4 - <6
#6 - <12
#12 – <18
#18 (adults)

#Option 4 gives the right answer

    # age age.group1 age.group2 age.group3 age.group4
# 1   0.0       <NA>       <NA>       <NA>       <NA>
# 2   1.0       <NA>       <NA>       <NA>       <NA>
# 3   1.9       <NA>       <NA>       <NA>       <NA>
# 4   2.0      [2,5]       <NA>      [2,5)      [2,5)
# 5   2.1      [2,5]      (2,5]      [2,5)      [2,5)
# 6   4.9      [2,5]      (2,5]      [2,5)      [2,5)
# 7   5.0      [2,5]      (2,5]     [5,11)     [5,11)
# 8   5.1     (5,11]     (5,11]     [5,11)     [5,11)
# 9  10.9     (5,11]     (5,11]     [5,11)     [5,11)
# 10 11.0     (5,11]     (5,11]    [11,16)    [11,16)
# 11 11.1    (11,16]    (11,16]    [11,16)    [11,16)
# 12 15.9    (11,16]    (11,16]    [11,16)    [11,16)
# 13 16.0    (11,16]    (11,16]    [16,50)    [16,50]
# 14 16.1    (16,50]    (16,50]    [16,50)    [16,50]
# 15 49.9    (16,50]    (16,50]    [16,50)    [16,50]
# 16 50.0    (16,50]    (16,50]       <NA>    [16,50]
# 17 51.0       <NA>       <NA>       <NA>       <NA>
