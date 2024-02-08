#########
#########
# Faustino Vazquez
# Alessandro Sapia
# Elshan Dashtiyev 
#########

#########
#Task 1.1 
# For the data file we had to change the format because we were having some problems when we downloaded the file, we rename it
# UnknownDistribution_corrected.csv in order to can code
#########

#Import data using UnknowDistribution.csv 

rm(list=ls())
setwd("C:\Users\Faus_\OneDrive\Documentos\KU-Ingolstadt\3rd_Semester\Software Tools for Simulation and Optimization\Simulation\Homework_1")
getwd()
UN <- read.csv("UnknownDistribution_corrected.csv", header = TRUE )# , sep = ";"
head(UN)

#Dataset
colnames(UN)<-"Observations"
UN$Observations

#Sorted Dataset
sort(UN$Observations)

#Number of Observations 
n <- nrow(UN)
n

#Minimum, Maximum
min <- min(UN$Observations)

min

max <- max(UN$Observations)
max

#Mean
mean <- mean(UN$Observations)
mean

#Corrected Standard Deviation 
sd <- sd(UN$Observations)
sd

#Median
median <- median(UN$Observations)
median

# Lamda
lamda <- 1/mean
summary(UN)

#########
#Task 1.2 
#########

#The summary statistics gives an overview of the central tendency and spread of the data. 
#By looking at the given data we can see the data might be a right-skewed distribution with heavy tails. 
#Based of that we could consider the exponential distribution as well as the triangle distribution
#due to its right-skewed nature.Another distribution could be the normal distribution as it 
#may approximate right-skewed Datasets. 

#########

#Task 1.3 a)


#########


# Histogram with default number of intervals
hist(UN$Observations, probability = F,
     main="Histogram of unknown distribution (default d)",
     xlab="x",
     breaks = 10,
     xlim = c(min,max))
#Assigning UN$Observations to variable x
x <- UN$Observations
d <- floor(1+log(n, base=2))
b <- (max(x)-min(x))/d

breaks <- min(x)+c(0:d)*b

#Plotting the histogram
hist(x, breaks = breaks, probability = T,
     main = "Histogram of unknown distribution (Sturges d)",
     ylab = "Density",
     xlab = "x",
     xlim = c(min,max)
     )


#########
#Task 1.3 b)
#########

###
#Exponential Distribution
###


#Create a histogram
hist(x, breaks=breaks, probability = TRUE,
     main = "Exponential Distribution",
     ylab = "Density",
     xlab = "x",
     xlim = c(min,max),  
     ylim = c(0,0.5))

#Overlaying the exponential distribution curve
curve(dexp(x, rate = 1/mean), add = T, col = "blue")


###
#Triangle distribution
###

library(triangle)

#Create a histogram
hist_data <- hist(x, breaks = breaks, probability = TRUE, col = "gray",
                  main = "Triangular Distribution",
                  xlab = "x", 
                  ylab = "Density",
                  xlim = c(min,max),
                  ylim = c(0,0.5)
                  )

#Extracting mode from histogram
mode <- hist_data$mids[which.max(hist_data$density)]
#Overlay the triangular distribution
curve(dtriangle(x, a = min, b = max, c = mode), add = TRUE, col = "green", lwd = 2)

###
# Normal Distribution
###

#Plot Histogramm 
hist(x, breaks = breaks,probability = TRUE, col = "grey", 
     main = "Normal Distribution",
     ylab = "Density",
     xlab = "x", 
     xlim = c(min,max),
     ylim = c(0,0.5)
     ) 

#Overlay the normal distribution
curve(dnorm(x, mean = mean, sd = sd), 
      add = TRUE, 
      col = "red")


####
#All them together
####

#Plot Histogramm 
hist(x, breaks = breaks,probability = TRUE, col = "grey", 
     main = "Distributions (Exponential, Normal, Triangular)",
     ylab = "Density",
     xlab = "x", 
     xlim = c(min,max),
     ylim = c(0,0.5)
) 

curve(dexp(x, rate = 1/mean), add = T, col = "blue")
curve(dnorm(x, mean = mean, sd = sd), add = TRUE, col = "red")
curve(dtriangle(x, a = min, b = max, c = mode), add = TRUE, col = "green", lwd = 2)


#########
#Task 1.3 c)
#########

# Exponential distirbution 
# The overlaid exponential distribution curve signifies a strong with some variability within the dataset. 
# Overall, the visual  suggests a good representation of the data . 

# Triangle distribution 
# The triangle shape does not indicate a central tendency which would 
# be usually the case for the distribution. 

# Normal distribution 
# The plot shows a unique profile deviating from the classic bell shape. 
# The initial increase followed by a curve suggests a derivation from symmetry. This asymmetric pattern may indicate a skewness


# Which two distribution fits the best the data: 
# By looking at the data we see two distributions with the highest accuracy of the curve and the plot are exponential and 
# the normal distribution. Even the normal distribution does not show the complete shape bell form, 
# it still fits better to the data then the triangle distribution
#########
#Task 1.3 d)
#########


##                              ##
#Testing Exponential Distribution#
##                              ##


#Importing the library for Kolmogorov-Smirnoff-Test
library(fitdistrplus)
library(MASS)


###Kolmogorov-Smirnoff-Test###


#Fitting the exponential distribution to the data
fit_exp <- fitdistr(x, "exponential")

#Empirical Distribution Function
ecdf_x <- ecdf(x)

#Cumulative Distribution Function
cdf_exp <- pexp(seq(min(x), max(x), length.out = 1000), rate = fit_exp$estimate["rate"])

#Test
ks_stat_exp <- ks.test(x, "pexp", rate = fit_exp$estimate["rate"])



##                           ##
#Testing Triangular Distribution#
##                           ##


###Kolmogorov-Smirnoff-Test###

library(triangle)

#Triangle
Subsets<-hist(x,plot=FALSE) #to determine the most frequent class
ks.test(x,"ptriangle",a=min(x), b=max(x), c=Subsets$mids[which.max(Subsets$counts)])



##                         ##
#Testing Normal Distribution#
##                         ##


###Kolmogorov-Smirnoff-Test###

#Test 
ks_stat_normal <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))


###Summary###

#Results for the exponential distribution
cat("Kolmogorov-Smirnov-Test for exponential distribution:\n")
print(ks_stat_exp)

#Results for the triangle distribution
cat("Kolmogorov-Smirnov-Test for triangle distribution:\n")

print(ks.test(x,"ptriangle",a=min(x), b=max(x), c=Subsets$mids[which.max(Subsets$counts)]))

#Results for the normal distribution
cat("Kolmogorov-Smirnov-Test for normal distribution:\n")
print(ks_stat_normal)

#In order to determine we need to look at the p-value´s results. 

#By comparing these we find the best fitting distribution for our given
#dataset is  exponential distribution


























































