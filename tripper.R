# Set the woriking directory
setwd("/Users/francescocontin/Dropbox/rec/data")
# required libraries
library(package="bit64")
library(package=data.table)
# load the data about the hotel
trip<-fread("tripscore.csv")
# load the facebook data
fb<-fread("facebook.csv")
setnames(fb,c("id","category","page","date","pageId"))
# in order to be compatible with the other Users info
fb$id<-as.character(fb$id)
#create the function to associate to the useres a trip randomly choosen
### this function modifies lightly the preferences from what the hotel is offering
shuffler  <- function(x){
  # the hotel value for each feature is added by a random discrete Value
  x <-x+ round(rnorm(n=1,mean=0,sd=1),0)
  # test if the score is included in the proper values {0,1,2,3}
  x<-ifelse(test=x > 0 & x < 3,yes=x,no=ifelse(test=x<0,yes=0,no=3))
  return(x)
}

# this function creates the dataset with User VaCode (Unique code for every vacation) Preferences and hotel choosen
FunTripper<-function(trip){
  tripper<-matrix(NA,nrow=150,ncol=14)
  tripper<-as.data.frame(tripper)
    for(i in 1:150){
      # use the data.frame setting, more convinient know
      tripDF<-data.frame(trip)
      # this is sampling randomly a user 
      RandUser<-sample(x=fb$id,size=1)
      # random select a row of the hotel matrix
      hotel <- sample(x=1:dim(trip)[1],1) 
      tripper[i,1]<-as.character(RandUser) # random user saved in the output
      tripper[i,2]<-hotel # random hotel saved in the output
      tripper[i,3]<-i # index, indicating the VaCode  
      tripper[i,4:14]<-t(apply(tripDF[hot,6:16],1,shuffler)) # Shuffler function applied to the hotel data 
      # set the names to the new data frame
      setnames(tripper,c("User","Hotel","VaCode",names(tripDF[,6:16])))
    }
  return(tripper)
}

### create the user to be compared

FunTester <- function(trip){
  tripDF<-data.frame(trip)  
  test<-as.character(k)
  test<-c(test,t(apply(tripDF[hot,6:16],1,shuffler)))
  test<-as.numeric(as.character(test))
  names(test) <- c("User",names(tripDF[,6:16]))
  return(test)
}

test <- FunTester(trip)

## create 
tripper<-FunTripper(trip)
#function for computing the score,
tripScore <- function(tirp){
  # do not print the warnings when the correlation is 0
  options(warn=-1)
  # create the data table with: User VaCode Hotel Price and Correlation
  score <- data.table(User=tripper$User,
                      VaCode=tripper$VaCode,
                      HoteL=tripper$Hotel,
                      Price=tripper$price,
                      # this function computes the correlation between the preferences, doesent consider the price. the ifelse takes into account the 0 correlated records
                      cor=apply(tripper[,-c(1:3,14)],1,function(x)ifelse(test=is.na(cor(x,test[-c(1,12)],method="spea"))==F,yes=cor(x,test[-c(1,12)],method="spea"),no=0))
                      ) 
  # group by user the results and order it with decreasing correlation
  score <- score[,,by=User][order(User,-cor)]
  # takes only the first row for every use/ the one more correlated with the preferences of the user to be compared with.
  score  <- score[,.SD[1],by=User]
  return(score)
}

tripScore(tirp)

