###################################################################################################
############################## VANZA RECOMMEDNDER SYSTEM ##########################################
########################## Author: Francesco Contin, Foove ########################################
###################################################################################################
# Clean the memory
rm(list=ls())
#set working directory
setwd("/Users/francescocontin/Dropbox/rec/data")
#libraries
library(data.table)
# import the data
fb <- fread("facebook.csv")
# set he names to the data
setnames(fb,c("id","category","page","date","pageId"))

# function to compute the correlation with a randomly given user
fbScore<-function(fb,k=0){
  #create a tabele with as rows the users and as colomns the categories of pages likes
  tab<-table(id=fb$id,cat=fb$category)
  # this is sampling randomly a user 
  k<-which(sample(x=attr(tab,"dimnames")$id,1)==attr(tab,"dimnames")$id)
  #function for computing the score, input K=row numbmer of the user to be compared and facebook data
  score<-data.table(User=attr(tab[-k,],"dimnames")$id,
                    FBcor=apply(tab[-k,],1,function(x)cor(x,tab[k,],))
                    )
  # sorting the data
  score<-score[,,][order(-FBcor)]
  return(list(score,attr(tab,"dimnames")$id[k]))
                        
      }


scoreFb<-fbScore(fb)[[1]]
testUser<-fbScore(fb)[[2]]

