#data.table available in library 'data.table'

#Q1
diamonds=read.table("diamonds.csv",header=T,sep=",")
df<-data.frame(diamonds)
mean(df$price[df$cut=="Ideal"])

#Q2
# Everything in R is stored in term of vectors, even the scalar type elements like x<-2.
# This helps R to perform faster calculations.
# Below code explains how to perform addition of two arrays with and without vectorization
a = (1:5)
b = (6:10)
#vectorized
result <- a+b 
#devectorized
result = rep(NA,length(a))
for(i in 1:length(a))
{
  result[i] <- a[i] + b[i]
}
# Interpreted languages are high level languages thus more readable
#     (as compared to machine level languages)
# Besides, interpreted languages like R helps us to achieve tremendous
#     increase in productivity at cost of decreased performance.

#Q3
# lapply, apply same arguments -> False
# apply() <- must be passed a matrix object, indicator for row/column for application
# similar to apply() but on each element of list -> lapply() and sapply()
# sapply() returns -> vector
# package containing superset of apply functions -> plyr
# apply function can be efficiently applied using loops -> False

#Q4
# 1D, homogeneous -> vector
# 1d, heterogeneous -> list
# 2D, homogeneous -> matrix
# nD, homogeneous -> array
# 2D, columns can contain different types -> data.frame
# hetrogeneous -> data frame

#Q5
# on-the-fly code -> interpreted
# machine-specific -> compiled
# Python, R, MATLAB -> interpreted
# C, Fortran, Java -> Compiled

#Q6
# faster, indexed -> data.table
# 
# d_ply -> data.frame
# aggregating -> data.table
# requires package -> data.table

#Q7
# fastest : vectorized
# moderate : devectorized with assigned space
# slowest : devectorized without assigned space
  
#Q8
# If Else cleaner/efficient than switch -> False
# Performance difference between If Else and switch is negligible -> True
# R has ability to perform conditional testing in vectorized way -> True
# Incrementing using vectors and withut vectors have similar runtimes -> False
# vectorized devectorized addition -> False
# Vector loop from index 0 -> False

#Q9
# CRAN packages, same quality, dependable -> False
# library/require to laod package -> True
# install.packages("package_name") to install packages -> True

#Q10
# Ways to assign value to a variable
#       x <- 2
#       x = 2
#       2 -> x
#       x <- y <- 2

#Q11
# hello.word(1,2,"test", 1, 10) will print 'Hello World 1 2'
# <- before function can be replaced by =

#Q12
# Generating vector of numbers from 1 to 5
#       c(1, 2, 3, 4, 5)
#       vec = 1     for(i in 2:5) vec[i] = i
#       c(1,c(2),c(c(3),4),5)
#       c(1:5)

#Q13
# Importing data in R
#       read.table can be used to read data from URL
#       

#Q14
# Data Types in R : Numeric, Integer, Character, Logical, Complex

#Q15
# Divisions are always stored in double

#Q16
# Offset from string to numeric
# Vectorization in function temp_to_k

#Q17
# Make team names colums 1 as row headers
# change in qplot parameters

#Q18
# convert to array so for loop can be applied
temp <- subset(D,D$numbers>=threshold)
sum = colSums(temp)
count=dim(temp)[1]
return(c(sum,count,sum/count))

#Q19
# Specify that input file contains column heaaders
data = read.table("diamonds.csv", sep=",", header=T)
mean(apply(as.matrix(data$price),1, get.profit, markup=1.25))

#20
# Message : Rcriminalwashere

#21
# To read data
wb = read.table("worldBank.csv",sep=',', header=T)
# Code works fine, but need to handle NA scenario
subset(wb, !is.na(wb$gdp_per_capita))

#22
# 
worldbank=read.table("worldbank.csv",header=T,sep=",")
plot(aggregate(worldbank$gdp/1000000000~substr(worldbank[,2],5,8), data=worldbank, FUN=sum, na.rm=TRUE),type='p',xlab="Years",ylab="GDP in Billions",lwd=2,col=3,main="World GDP by Year")

