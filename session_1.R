# WORKSHOP SPATIAL ANALYSIS AND MAPPING

rm(list=ls())
setwd("C:/Users/maayk/Documents/spatial_analysis_and_mapping/")

vals <- c(4.3,7.1,6.3,5.2,3.2,2.1)

vals*2
sum(vals)
mean(vals)
vals[1]
vals[1:3]
sqrt(vals[1:3])
vals[c(5,3,2)]


## R PACKAGES
install.packages("tidyverse", dep = TRUE)
library(tidyverse)

cen.dat <- read.csv("census_data.csv")
## dimensions - rows and columns
dim(cen.dat)

## column / variable names
names(cen.dat)

## look at the first 6 rows and 7 columns
cen.dat[1:6,1:7]

## use the sumamry function for the first 7 columns
summary(cen.dat[,1:7])

# loading inary R data files, which are much faster than csv
load("census.rda")
## use ls to see what is loaded
ls()

# loading data directly from a URL
url <- url("http://www.people.fas.harvard.edu/~zhukov/Datasets.RData")
load(url)
ls()

# the repmis package makes it easy to store and get data from github as Rdata
install.packages('repmis')
library(repmis)
# load the data 
source_data("https://github.com/lexcomber/CAS_GW_Training/blob/master/Liudaogou.RData?raw=True")


write.csv(census, file = "new_census.csv")
write.csv(census, file = "test.csv", row.names = F)
save.image(file = "myworkspace.RData")
# this will save everything in the workspace
save(list = ls(), file = "MyData.RData")
# this will save just appling
save(list = "census", file = "MyData.RData")
# this will save vals and census
save(list = c("census", "vals"), file = "MyData.RData")


## TO MAKE SURE WE HAVE THE CORRECT DATA LOADED
rm(list = ls())
load("census.rda")
summary(census[,14:18])


## set the plot paramteters
# 1 row and 4 columns 
par(mfrow = c(1,4))
hist(census$OnePerson, xlim = c(0,1),
     xlab = "%", main = "One person households", col = "red")
hist(census$MarriedNoDependent, xlim = c(0,1),
     xlab = "%", main = "Married with dependents", col = "#FFFFBF")
hist(census$CohabitingCoupleNoDependent, xlim = c(0,1),
     xlab = "%", main = "Cohabiting with dependents", border = "#FDAE61")
hist(census$LoneParentDependent, xlim = c(0,1),
     xlab = "%", main = "Lone parent with dependents", border = "cyan")
# reset plot parameters
par(mfrow = c(1,1))

boxplot(census$PrivateRented, main = "The distribution of Private Rented")

census$social.class <- names(census[38:41])[apply(census[, 38:41], 1, which.max)]

par(mar = c(3,8,3,3))
# plot 1
boxplot(census$Owned~census$social.class,
        horizontal = T, outline = T, lwd = 0.5, las = 2,
        col = c("#D7191C","#FFFFBF", "#2B83BA"),
        xlab=expression(paste("Proportions of Owned property among social grades")))
# plot 2
boxplot(census$SocialRented~census$social.class,
        horizontal = T, outline = F, lwd = 0.5, las = 2,
        col = c("#D7191C","#FFFFBF", "#2B83BA"),
        xlab=expression(paste("Proportions of Owned property among social grades")))

round(cor(census[, c(24:28, 38:41)]), 3)

plot(census[,c(24:28, 38:41)], cex = .6)
