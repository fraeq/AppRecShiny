# load the required libraries
library(shiny)

rm(list=ls())
#library(devtools)
#devtools::install_github(repo="cran/bit64")
library(data.table)
library(bit64)

load("data.Rdata")
userNames$User<-as.character(userNames$User)
#create the function to associate to the useres a trip randomly choosen
### this function modifies lightly the preferences from what the hotel is offering
shuffler  <- function(x,std=1){
  # the hotel value for each feature is added by a random discrete Value
  x <-x+ round(rnorm(n=1,mean=0,sd=std),0)
  # test if the score is included in the proper values {0,1,2,3}
  x<-ifelse(test=x > 0 & x < 3,yes=x,no=ifelse(test=x<0,yes=0,no=3))
  return(x)
}

# this function creates the dataset with User VaCode (Unique code for every vacation) Preferences and hotel choosen
funTripper<-function(trip,rep=1000){
  # rep defines the number of travel to create, so high becouse we have 171 people on fb and I want at least one socre each
  tripper<-matrix(NA,nrow=rep,ncol=14)
  tripper<-as.data.frame(tripper)
  # use the data.frame setting, more convinient know
  tripDF<-data.frame(trip)
  for(i in 1:rep){
    # this is sampling randomly a user 
    RandUser<-sample(x=fb$id,size=1)
    # random select a row of the hotel matrix
    hotel <- sample(x=1:dim(trip)[1],1) 
    # create the data set
    tripper[i,1]<-as.character(RandUser) # random user saved in the output
    tripper[i,2]<-hotel # random hotel saved in the output
    tripper[i,3]<-i # index, indicating the VaCode  
    tripper[i,4:14]<-t(apply(tripDF[hotel,6:16],1,shuffler)) # Shuffler function applied to the hotel data 
    # set the names to the new data frame
    setnames(tripper,c("User","Hotel","VaCode",names(tripDF[,6:16])))
  }
  return(tripper)
}

### create the user to be compared 
##### AAAA THIS FUNCTION MUST BE RAN BEFORE THE PERSONALITY PART 
##### IT CREATES THE USER TO BE CHECKED

funTesterG <- function(trip,rep=10){
  TestUsers <- matrix(NA,nrow=rep,ncol=12)
  TestUsers <- as.data.frame(TestUsers)
  tripDF<-data.frame(trip)
  for(i in 1:rep){
    # this is sampling randomly a user 
    RandUser<-as.character(sample(x=fb$id,size=1))
    # random select a row of the hotel matrix
    hotel <- sample(x=1:dim(trip)[1],1) 
    TestUsers[i,1]<-RandUser # random user saved in the output
    TestUsers[i,2:12]<-t(apply(tripDF[hotel,6:16],1,shuffler)) # Shuffler function applied to the hotel data 
    }
  # set the names to the new data frame
  setnames(TestUsers,c("User",names(tripDF[,6:16])))
  TestUsers<-data.table(TestUsers)
  setkey(userNames,User)
  return(userNames[TestUsers])
}

###################################################################################################

####################################### PERSONALITY ###############################################

# function to compute the correlation with a randomly given user
fbScore<-function(fb,test=testUser){
  options(warn=-1)
  #create a tabele with as rows the users and as colomns the categories of pages likes
  tab<-table(id=fb$id,cat=fb$category)
  # this is the user sampled in the previous section
  k<-which(test[1]==attr(tab,"dimnames")$id)
  # use the user choose in the travel section
  #function for computing the score, input K=row numbmer of the user to be compared and facebook data
  score<-data.table(User=attr(tab[-k,],"dimnames")$id,FBcor=apply(tab[-k,],1,function(x)cor(x,tab[k,])))
  # sorting the data
  score<-score[,,][order(-FBcor)]
  #return(list(score,attr(tab,"dimnames")$id[k]))
  return(list(score,as.character(test[1])))
}

###################################################################################################
####################################### PREFERENCES ###############################################
#function for computing the score,
tripScore <- function(tripper,test=testUser){
  # do not print the warnings when the correlation is 0
  options(warn=-1)
  # create the data table with: User VaCode Hotel Price and Correlation
  score <- data.table(User=tripper$User,
                      VaCode=tripper$VaCode,
                      Hotel=tripper$Hotel,
                      Price=tripper$price,
                      # this function computes the correlation between the preferences, doesent consider the price. the ifelse takes into account the 0 correlated records
                      Tripcor=apply(tripper[,-c(1:3,14)],MARGIN=1,function(x)ifelse(test=is.na(cor(x,test,method="spea"))==F,yes=cor(x,test,method="spea"),no=0))
  ) 
  # group by user the results and order it with decreasing correlation
  score <- score[,,by=User][order(User,-Tripcor)]
  # takes only the first row for every use/ the one more correlated with the preferences of the user to be compared with.
  score  <- score[,.SD[1],by=User]
  return(score)
}

#### function to create a global score with equal weights as default. 

finalCor <- function(wF=0.5,wT=0.5,scoreFb,scoreTrip){
  # activate the warning function
  options(warn=-1)
  # check that the weights sum up to one, if they don't warn and rescale
  if(wF+wT!=1){ tot <- (wF+wT)
                wF=wF/tot
                wT=wT/tot
                #print(wF);print(wT)
                warning("The Weights have been rescaled",call.=F)
  }
  scoreFb <- data.table(scoreFb)
  scoreTrip <- data.table(scoreTrip)
  # set the database key for the table
  setkey(x=scoreFb,User)
  # match the correlations based on the score
  totScore <- scoreFb[scoreTrip]
  # compute the overall correlation and order it with decreasing correlation
  totScore <- totScore[,.(totCor=wF*FBcor+wT*Tripcor,VaCode,Hotel,Price,User)][order(-totCor)] 
  return(totScore)
}



### function that gives the final vacation as suggestion 
suggestion <- function(CorMat=finCor,price=testUser[12],num=5){
  # set the database key to match all the hotel with the desired price, cancel out the double, and take the num- most correlated
  setkey(CorMat,Price)
  Choice <- CorMat[Price==price,.(unique(Hotel)),][1:num]
  # set the data base key in order to find the code and name of the hotel suggested 
  setkey(trip,hotel)
  return(trip[Code==Choice$V1,.(Code,hotel,City=NatCity,Mountain=NatMount,
                                Sea=NatSea,Clubbing=EntClub,Kid=EntKid,Romantic=EntRom,
                                Sport=ActSpo,Shopping=ActSho,Cultural=ActCul,Adventure=ActAdv)])
}


# random user as comparison
# create the score for the personality 
#scoreFb<-fbScore(fb)[[1]]

# create the travel data
tripper <- funTripper(trip)


testUser <- funTesterG(trip)
user<-testUser[,1]

#finCor<-finalCor(wF=1,wT=10,scoreFb=scoreFB,scoreTrip)

#suggestion(price=1)

shinyServer(function(input,output,session){
  
  k<-funTesterG(trip)
  
#   NatCityUser<-reactive({
#   ifelse(test=input$UserKindVar==3,1,ifelse(input$UserKindVar==2,2,3))
#   })
#   output$NatCityUser<-renderText({
#     NatCityUser()
#   })
# function selecting which kind of user is to be selected 
  whichUser<-reactive({
    kind<-input$UserKindVar
   # return(kind)
    })

  #working check of the input variable
#   output$UserKind<-renderText({
#     whichUser()
#   })
#   

# update the values of the user choice
  observe({
    setkey(userNames,User)
    updateSelectInput(session,"userC",choices=userNames[k,Name])  
    })



# create the variables with the values
profile<-reactive({
if(input$UserKindVar==1){
  NatCityUser <- 0
  NatMountUser <- 3
  NatSeaUser <- 0
  EntClubUser <- 1
  EntKidUser <- 0
  EntRomUser <- 3
  ActSpoUser <- 1
  ActShoUser <- 3
  ActCulUser <- 3
  ActAdvUser <- 1
  }
else if(input$UserKindVar==2){
  NatCityUser <- 3
  NatMountUser <- 1
  NatSeaUser <- 3
  EntClubUser <- 0
  EntKidUser <- 3
  EntRomUser <- 1
  ActSpoUser <- 2
  ActShoUser <- 3
  ActCulUser <- 3
  ActAdvUser <- 0
}
else{
  NatCityUser <- 3
  NatMountUser <- 0
  NatSeaUser <- 3
  EntClubUser <- 3
  EntKidUser <- 0
  EntRomUser <- 0
  ActSpoUser <- 2
  ActShoUser <- 1
  ActCulUser <- 1
  ActAdvUser <- 3
}
c(NatCityUser,NatMountUser,NatSeaUser,EntClubUser,EntKidUser,EntRomUser,ActSpoUser,ActShoUser,ActCulUser,ActAdvUser)
})
 # functoin that given a choice for kind of user tunes all the parameters  
   observe({
     if(input$UserKindVar==1){
updateSliderInput(session,"NatCityUser",value=profile()[1])
updateSliderInput(session,"NatMountUser",value=profile()[2])
updateSliderInput(session,"NatSeaUser",value=profile()[3])
updateSliderInput(session,"EntClubUser",value=profile()[4])
updateSliderInput(session,"EntKidUser",value=profile()[5])
updateSliderInput(session,"EntRomUser",value=profile()[6])
updateSliderInput(session,"ActSpoUser",value=profile()[7])
updateSliderInput(session,"ActShoUser",value=profile()[8])
updateSliderInput(session,"ActCulUser",value=profile()[9])
updateSliderInput(session,"ActAdvUser",value=profile()[10])
}
    else if(input$UserKindVar==2){
     updateSliderInput(session,"NatCityUser",value=profile()[1])
     updateSliderInput(session,"NatMountUser",value=profile()[2])
     updateSliderInput(session,"NatSeaUser",value=profile()[3])
     updateSliderInput(session,"EntClubUser",value=profile()[4])
     updateSliderInput(session,"EntKidUser",value=profile()[5])
     updateSliderInput(session,"EntRomUser",value=profile()[6])
     updateSliderInput(session,"ActSpoUser",value=profile()[7])
     updateSliderInput(session,"ActShoUser",value=profile()[8])
     updateSliderInput(session,"ActCulUser",value=profile()[9])
     updateSliderInput(session,"ActAdvUser",value=profile()[10])
   }
    else{
  updateSliderInput(session,"NatCityUser",value=profile()[1])
   updateSliderInput(session,"NatMountUser",value=profile()[2])
   updateSliderInput(session,"NatSeaUser",value=profile()[3])
   updateSliderInput(session,"EntClubUser",value=profile()[4])
   updateSliderInput(session,"EntKidUser",value=profile()[5])
   updateSliderInput(session,"EntRomUser",value=profile()[6])
   updateSliderInput(session,"ActSpoUser",value=profile()[7])
   updateSliderInput(session,"ActShoUser",value=profile()[8])
   updateSliderInput(session,"ActCulUser",value=profile()[9])
   updateSliderInput(session,"ActAdvUser",value=profile()[10])
 }
})
  


# User code    
  output$userC<-renderText({
    setkey(userNames,Name)
   paste("The User code is: ",userNames[input$userC,User])
  })
# table for check  
  output$tripper<-renderTable({
     head(tripper)
  })
# obtaining the inserted info (check it is possible to do it in reactive table)


# summary of the choosen profile
output$profile<-renderTable({
      data.table(Name=userNames[input$userC,Name],
            Code=userNames[input$userC,User],
            City=input$NatCityUser,
            Mountains=input$NatMountUser,
            Sea=input$NatSeaUser,
            Clubbing=input$EntClubUser,
            Kid=input$EntKidUser,
            Romantic=input$EntRomUser,
            Sport=input$ActSpoUser,
            Shopping=input$ActShoUser,
            Culture=input$ActCulUser,
            Adventure=input$ActAdvUser
               )
            })
#   output$user<-renderTable({
#     k
#   })
# output$fbscore<-renderTable({
#   head(fbScore(fb,userNames[input$userC,User])[[1]])
# })

# output$fbscore<-renderTable({
# scoreFb <- head(fbScore(fb,userNames[input$userC,User])[[1]])
# })


userPref<-reactive(c(input$NatCityUser,input$NatMountUser,input$NatSeaUser,input$EntClubUser,input$EntKidUser,input$EntRomUser,input$ActSpoUser,input$ActShoUser,input$ActCulUser,input$ActAdvUser))

# output$tripscore<-renderTable({
# scoreTrip <- head(tripScore(tripper=tripper,test=userPref()))
# })

#  output$fbscore<-renderTable({
# # #   fbScore(fb,userNames[input$userC,User])[[2]]
# finalCorUs <- finalCor(wF=1,wT=10,scoreFb,scoreTrip)
# # # 
#     })
# finalCorUs <- finalCor(wF=10,wT=5,scoreFb,scoreTrip)
output$suggestion<-renderTable({
suggestion(CorMat=finalCor(wF=input$wFb,wT=input$wTr,fbScore(fb,userNames[input$userC,User])[[1]],
                           tripScore(tripper=tripper,test=userPref())),price=input$PriceUser,num=input$numCh)
})
})