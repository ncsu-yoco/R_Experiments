vectorized <- function()
{
  a<-c(1,1)
  b<-c(2,2)
  x<-c(NaN,NaN)
  for(i in 1:1000000)
  {
    x<-a+b
  }
  return()
}

devectorized <- function()
{
  a<-c(1,1)
  b<-c(2,2)
  x<-c(NaN,NaN)
  for(i in 1:1000000)
  {
    for(index in 1:2)
    {
      x[index]<-a[index]+b[index]
    }
  }
  return()
}

timeV <- function(N)
{
  timings <- rep(NA,N)
  for(itr in 1:N)
  {
    start <- Sys.time()
    vectorized()
    end <- Sys.time()
    timings[itr] <- end - start
  }
  return (timings)
}

timeD <- function(N)
{
  timings <- rep(NA,N)
  for(itr in 1:N)
  {
    start <- Sys.time()
    devectorized()
    end <- Sys.time()
    timings[itr] <- end - start
  }
  return(timings)
}

hello.world = function(arg1, arg2 = "blah")
{
  print(sprintf("Hello %s %s", arg1, arg2))
  return (1)
}

# Create a convenient lookup chart to convert from
# Degrees F to either Degrees C or K
library(ggplot2)
temp_f <- as.vector(runif(100,-100,200))
offset <- 32
factor <- 5/9
temp_conv <- function(temp_f)
{
  new_temp <- factor * (temp_f - offset)
  return(new_temp)
}
temp_c <- sapply(temp_f, temp_conv)
temp_to_k<-function(temp_c)
{
  cold_temp <- vector(mode="numeric", length=length(temp_c))
  cold_temp <-temp_c + 276
  return(cold_temp)
}
temp_k <- temp_to_k(temp_c)
temp_df <- data.frame(temp_f,temp_c,temp_k)
ggplot(temp_df,aes(temp_f,y="Temp_C,K",color=variable))+
  geom_point(aes(y=temp_c,col="Celsius"))+
  geom_point(aes(y=temp_k,col="Kelvin"))


criminal_data=read.table("winslosses.txt",header=T,sep=",")
teams=criminal_data[,1]
criminal_data=criminal_data[,-1]
rownames(criminal_data)=teams
qplot(criminal_data, x=teams, y=criminal_data$WINS,stat="identity")

criminal_data

ThresholdAverage <- function(numbers, threshold) {
 # sum=0; count=0;
  temp <- subset(D,D$numbers>=threshold)
  sum = colSums(temp)
  count=dim(temp)[1]
  return(c(sum,count,sum/count))
}
D <- data.frame(numbers=c(1,5,22,35,77,34,54,73,23,43,12,2,1,21));
results = ThresholdAverage(D,10)
print(paste("Threshold Sum:", results[1]))
print(paste("Threshold Count:", results[2]))
print(paste("Threshold Avg:", results[3]))

for(i in D){
  print(i)
  print(typeof(i))
}

get.profit <- function(){
  profit = (retail*markup + sucker_tax)-retail
  return(profit)
}
mean(apply(data$price, get.profit, markup=1.25))


key = c(5,18,5,8,19,1,23,12,1,14,9,13,9,18,3,18)
secretSauce = function(first, second, ...)
{
  if(first != second || second == second)
  {    
    first_tmp = first 
    first = second
    second = first_tmp
  } 
  return(c(first,second))
}
for (i in 1:length(key)){
  j = length(key)-(i-1)
  if (i < j){
    ret = secretSauce(key[i], key[j])
    key[i] = ret[1]
    key[j] = ret[2]
  }
}
message = letters[key]
message[1] = toupper(message[1])
paste(message, sep="", collapse="")


diamonds=read.table("diamonds.csv",header=T,sep=",")
df <- data.frame(diamonds)
mean(df$price[df$cut=="Ideal"])


criminal_data=read.table("winlosses.txt",header=T,sep=",") 
# Observe first column of data frame
head(criminal_data$team)
# Get team names
teams=criminal_data[,1]
#remove team names column from data
criminal_data=criminal_data[,-1]
#set rownames of data as team names
rownames(criminal_data)=teams
#plot desired graph
qplot(criminal_data, x=teams, y=criminal_data$WINS,stat="identity")