# WORKSHOP SPATIAL ANALYSIS AND MAPPING

rm(list=ls())
setwd("C:/Users/maayk/Documents/spatial_analysis_and_mapping/")

austpop <- read.table("austpop.txt", header=TRUE)

austpop
names(austpop)
plot(ACT ~ Year, data=austpop, pch=16)

#?points

# DATAFRAMES 
elasticband <- data.frame(stretch=c(46,54,48,50,44,42,52),
                          distance=c(148,182,173,166,109,141,166))
elasticband <- edit(elasticband)


# HELPFULL COMMANDS
apropos('matrix') # lists all functions containing the word "matrix"
help.search('matrix') # lists all functions whose help pages have a title 
                      # or alias containing the word "matrix"

attach("usingR.RData")

plot(distance ~ stretch, data = elasticband, pch = 16)

snow_eurasia <- data.frame(year = c(1970:1979), 
                           snow.cover = c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2))
plot(snow.cover ~ year, data = snow_eurasia, pch = 16)
hist(snow_eurasia$snow.cover)

snow_eurasia['log.snow'] = log(snow_eurasia['snow.cover'])
plot(log.snow ~ year, data = snow_eurasia, pch = 16)
hist(snow_eurasia$log.snow)


## R AS A CALCULATOR 
2+2
sqrt(10)
2*3*4*5
1000*(1+0.075)^5 - 1000 # Interest on $1000, compounded annually
# at 7.5% p.a. for five years
pi # R knows about pi
2*pi*6378 #Circumference of Earth at Equator, in km; radius is 6378 km 
sin(c(30,60,90)*pi/180) # Convert angles to radians, then take sin() 


## SUMMARIZING DATA 
load("Maind_lex.RData") # Assumes Maind_lex.Rdata is in the working directory 
ls()

summary(hills)
pairs(hills)

cor(hills)
round(cor(hills), 3)
cor(log(hills))


## LINEAR REGRESSION
plot(distance ~ stretch,data=elasticband, pch=16)

elastic.lm <- lm(distance~stretch,data=elasticband) 
lm(distance ~stretch,data=elasticband)
summary(lm(distance~stretch,data=elasticband)) 

## Conversion table celsius fahrenheit 
celsius <- 25:30
fahrenheit <- 9/5*celsius+32
conversion <- data.frame(Celsius=celsius, Fahrenheit=fahrenheit) 
print(conversion)


## R OBJECTS 
save.image() # Save contents of workspace, into the file .RData 
save.image(file="archive.RData") # Save into the file archive.RData 
save(celsius, fahrenheit, file="tempscales.RData")

attach("tempscales.RData")
ls(pos=2)


# LOOPING
for (i in 1:10) print(i)
# Celsius to Fahrenheit
for (celsius in 25:30)
  print(c(celsius, 9/5*celsius + 32))

answer <- 0
for (j in c(31,51,91)){answer <- j+answer}
answer

sum(c(31,51,91))


# VECTORS 
c(2,3,5,2,7,1) # the c is short for concatenate 
3:10 # The numbers 3, 4, .., 10 
c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE) 
c("Canberra","Sydney","Newcastle","Darwin")

# combining of vectors
x <- c(2,3,5,2,7,1) 
y <- c(4,5,6)
z <- c(x,y)
z

# subsets of vectors
x <- c(3,11,8,15,12)
x[c(2,4)]
x[-c(2,3)]
x > 10
x[x > 10]

# factors
gender <- c(rep("female",691), rep("male",692))
gender <- factor(gender) # converts the above vector into a factor (1 for female, 2 for male)
levels(gender)

gender <- relevel(gender, ref="male") # switches the reference level to male 
gender <- factor(gender, levels=c("male","female")) # reference level = male 

gender <- factor(c(rep("female",691), rep("male",692)))
table(gender)

gender <- factor(gender, levels=c("male","female"))
table(gender)

gender <- factor(gender, levels=c("Male","female"))
table(gender)

attributes(gender)
rm(gender)


## DATAFRAMES 
Cars93.summary

type <- Cars93.summary$abbrev
type <- Cars93.summary[,4]
type <- Cars93.summary[,"abbrev"]
type <- Cars93.summary[[4]]   # in the fourth list element.

data() # provides a list of built-in datasets
summary(trees) # example of a built-in dataset

# some useful commands 
print() # Prints a single R object
cat() # Prints multiple objects, one after the other 
length() # Number of elements in a vector or of a list
mean()
median()
range()
unique() # Gives the vector of distinct values
diff() # Replace a vector by the vector of first differences
# N. B. diff(x) has one less element than x 
sort() # Sort elements into order, but omitting NAs 
order() # x[order(x)] orders elements of x, with NAs last 
cumsum()
cumprod()
rev() # reverse the order of vector elements

x <- c(1, 20, 2, NA, 22)
order(x)
x[order(x)]

# applying a function to all columns of a dataframe
sapply(rainforest, is.factor)
sapply(rainforest[,-7], range)   # The final column (7) is a factor
sapply(rainforest[,-7], range, na.rm=TRUE) 


# TABLES
install.packages("lattice", dep = T)

library(lattice) 
# The data frame barley accompanies lattice 
table(barley$year, barley$site)

x <- c(1,5,NA,8) 
x <- factor(x) 
x
factor(x,exclude=NULL) #NA's are by default excluded 

table(rainforest$species, !is.na(rainforest$branch))


## THE SEARCH LIST
search()
#install.packages("MASS", dep = T)
library(MASS)

search()
names(primates)
Bodywt
av <- with(primates, mean(Bodywt))


# FUNCTIONS IN R
miles.to.km <- function(miles)miles*8/5
miles.to.km(175)
miles.to.km(c(100,200,300))

attach(florida)
plot(BUSH, BUCHANAN, xlab="Bush", ylab="Buchanan") 

detach(florida)

plot.florida <- function(xvar="BUSH", yvar="BUCHANAN"){
  x <- florida[,xvar]
  y <- florida[,yvar]
  plot(x, y, xlab=xvar,ylab=yvar)
  mtext(side=3, line=1.75, "Votes in Florida, by county, in \nthe 2000 US Presidential election")
}
plot.florida(yvar="NADER") # yvar="NADER"" over-rides the default 

plot.florida(xvar="GORE", yvar="NADER")

