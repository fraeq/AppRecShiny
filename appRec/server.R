# load the required libraries
library(shiny)
Sys.setlocale("LC_ALL",'C')
rm(list=ls())
#library(devtools)
#devtools::install_github(repo="cran/bit64")
library(data.table)
library(bit64)

load("data.Rdata")
userNames$User<-as.character(userNames$User)
#create the function to associate to the useres a trip randomly choosen
### this function modifies lightly the preferences from what the hotel is offering
shuffler  <- function(x,std=0.15){
  # the hotel value for each feature is added by a random discrete Value
  x <-x+ round(rnorm(n=1,mean=0,sd=std),0)
  # test if the score is included in the proper values {0,1,2,3}
  x<-ifelse(test=x > 0 & x < 3,yes=x,no=ifelse(test=x<0,yes=0,no=3))
  return(x)
}

# this function creates the dataset with User VaCode (Unique code for every vacation) Preferences and hotel choosen
funTripper<-function(trip,rep=2000){
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

# this function creates the dataset with User VaCode (Unique code for every vacation) Preferences and hotel choosen
funTripperRep<-function(trip,rep=1000,std=1){
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
    tripper[i,4:14]<-t(apply(tripDF[hotel,6:16],1,shuffler,std)) # Shuffler function applied to the hotel data 
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
  k<-which(as.character(test[1])==attr(tab,"dimnames")$id)
  # use the user choose in the travel section
  #function for computing the score, input K=row numbmer of the user to be compared and facebook data
  
  score<-data.table(User=attr(tab[-k,],"dimnames")$id,FBcor=apply(tab[-k,],1,function(x)cor(x,tab[k,])))
  # sorting the data
  score<-score[,,][order(-FBcor)]
  #char<-data.table(fb[id==test[1],length(page),by=category][order(-V1)][1:5])
  return(list(score,as.character(test[1])))
  
}
### if needed fast incliude this function over
fbChar<-function(fb,test=testUser){
  #function for computing the score, input K=row numbmer of the user to be compared and facebook data
  fb$id<-as.character(fb$id)
data.table(fb[id==test[1],length(page),by=category][order(-V1)][1:5])
  
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
                               Sport=ActSpo,Shopping=ActSho,Cultural=ActCul,Adventure=ActAdv,Text=text,Cat=Name)])

}


# random user as comparison
# create the score for the personality 
#scoreFb<-fbScore(fb)[[1]]

# create the travel data



testUser <- funTesterG(trip)
user<-testUser[,1]

#finCor<-finalCor(wF=1,wT=10,scoreFb=scoreFB,scoreTrip)

#suggestion(price=1)

shinyServer(function(input,output,session){
  tripper <-funTripperRep(trip)
  observeEvent(input$computeTravel,{cat("reco")})

#   tripper<- 
#   Retrip<-reactive({
#     if(input$computeTravel==0) funTripper(trip)
#     else funTripperRep(trip,std=input$variability)
#       #funTripperRep(trip,std=input$variability)
#   })
#   
# tripper <- Retrip()
  
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
  NatCityUser <- 1
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
  
# # summary of the choosen profile
# output$profile<-renderTable({
#   data.table(Name=userNames[input$userC,Name],
#    Code=userNames[input$userC,User],
#    City=input$NatCityUser,
#    Mountains=input$NatMountUser,
#    Sea=input$NatSeaUser,
#    Clubbing=input$EntClubUser,
#    Kid=input$EntKidUser,
#    Romantic=input$EntRomUser,
#    Sport=input$ActSpoUser,
#    Shopping=input$ActShoUser,
#    Culture=input$ActCulUser,
#    Adventure=input$ActAdvUser
#   )
# })

output$fbscore<-renderTable({
  fbScore(fb,userNames[input$userC,User])[[1]]
})

output$wei<-renderText({
  paste("FB: ",1-input$W,"Trip: ",1+input$W)
})
output$char<-renderTable({
  fbChar(fb,userNames[input$userC,User])
})
userPref<-reactive(c(input$NatCityUser,input$NatMountUser,input$NatSeaUser,input$EntClubUser,input$EntKidUser,input$EntRomUser,input$ActSpoUser,input$ActShoUser,input$ActCulUser,input$ActAdvUser))

# output$baubau<-renderUI({
# #   char<-fbChar(fb,userNames[input$userC,User])
#   HTML(paste0("<div class=\"panel panel-default\">
#                 <!-- Default panel contents -->
#                   <div class=\"panel-heading\">Facebook likes  <span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span> </div>
#                   <!-- Table -->
#                   <table class=\"table\">
#                     <tr>
#                       <td>",fbChar(fb,userNames[input$userC,User])[[1]][1],"</td>
#                       <td> <span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span> </td>
#                   </tr>
#             ",for(i in 1:5){print(
#                 paste0("
#                     <tr>
#                       <td>",fbChar(fb,userNames[input$userC,User])[[1]][i],"</td>
#                       <td>",fbChar(fb,userNames[input$userC,User])[[2]][i],"</td>
#                     </tr>")
#                 )
#               },"
#             
#           </table>
#       </div>")
#       )
# })
# paste0()
output$baubau<-renderUI({
 chart<-fbChar(fb,userNames[input$userC,User])
  top<-"<div class=\"panel panel-default\">
                <!-- Default panel contents -->
                  <div class=\"panel-heading\">Facebook likes  <span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span> </div>
                  <!-- Table -->
                  <table class=\"table\">"
bottom<-"</table>
      </div>"

char<-""
for(i in 1:5){
line<-paste0(
                      "<td>",chart[[1]][i],"</td>
                      <td>",chart[[2]][i]," </td>",
                      if(chart[[2]][i]==1){"<td><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></td>"}
                      else if(chart[[2]][i]==2){"<td><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></td>"}
                      else{"<td><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></td>"}
                      )
#line2<-paste0(rep("<td> <span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span> </td>",chart[[2]][i]))
char<-paste0("<tr>",char,line,"</tr>")
}
cat(char)
theHTML<-paste0(top,char,bottom)
return(HTML(theHTML))
})

output$sug2 <- renderUI({
  sugH<-suggestion(CorMat=finalCor(wF=(1-input$W),wT=(1+input$W),fbScore(fb,userNames[input$userC,User])[[1]],tripScore(tripper=tripper,test=userPref())),price=input$PriceUser,num=input$numCh)[,.(hotel,Text,Cat,City,Mountain,Sea,Clubbing,Kid,Romantic,Sport,Shopping,Cultural,Adventure)] 
  theHTML <- ""
  for(i in 1:input$numCh){
    suggestionHTML <- paste0("<div class=\"panel panel-default\">
                               <div class=\"panel-heading\">
                               <h3 class=\"panel-title\">"," #",i," ",sugH[i,hotel],
                                " -- ",sugH[i,Cat],
                                "</h3>
                               </div>
                                 <div class=\"panel-body\">
                              
                                 <h4>Environment</h4>
                                    <div class=\"progress\">
                                      <div class=\"progress-bar progress-bar-warning",if(input$NatCityUser==0 | sugH[i,City] < input$NatCityUser)(" off")," \" style=\"width:",sugH[i,City]/sum(sugH[i,City]+sugH[i,Mountain]+sugH[i,Sea])*100,"%\">
                                        ",ifelse(sugH[i,City]>0,paste0("City: ",sugH[i,City]),""),"
                                      </div>
                                      <div class=\"progress-bar progress-bar-success",if(sugH[i,Mountain] < input$NatMountUser){" off"}," \" style=\"width:",sugH[i,Mountain]/sum(sugH[i,City]+sugH[i,Mountain]+sugH[i,Sea])*100,"%\">
                                         ",ifelse(sugH[i,Mountain]>0,paste0("Mountain: ",sugH[i,Mountain]),""),"
                                      </div>
                                      <div class=\"progress-bar progress-bar-info",if(sugH[i,Sea] < input$NatSeaUser){" off"},"\" style=\"width:",sugH[i,Sea]/sum(sugH[i,City]+sugH[i,Mountain]+sugH[i,Sea])*100,"%\">
                                         ",ifelse(sugH[i,Sea]>0,paste0("Sea: ",sugH[i,Sea]),""),"
                                      </div>
                                    </div>
                                <h4>Entertainment</h4>
                                     <div class=\"progress\">
                                        <div class=\"progress-bar progress-bar-warning",if(sugH[i,Clubbing] < input$EntClubUser){" off"},"\" style=\"width:",sugH[i,Clubbing]/sum(sugH[i,Clubbing]+sugH[i,Kid]+sugH[i,Romantic])*100,"%\">
                                          ",ifelse(sugH[i,Clubbing]>0,paste0("Clubbing: ",sugH[i,Clubbing]),""),"
                                        </div>
                                        <div class=\"progress-bar progress-bar-success",if(sugH[i,Kid] < input$EntKidUser){" off"},"\" style=\"width:",sugH[i,Kid]/sum(sugH[i,Clubbing]+sugH[i,Kid]+sugH[i,Romantic])*100,"%\">
                                          ",ifelse(sugH[i,Kid]>0,paste0("Kid: ",sugH[i,Kid]),""),"
                                        </div>
                                        <div class=\"progress-bar progress-bar-danger",if(sugH[i,Romantic] < input$EntRomUser){" off"},"\" style=\"width:",sugH[i,Romantic]/sum(sugH[i,Clubbing]+sugH[i,Kid]+sugH[i,Romantic])*100,"%\">
                                          ",ifelse(sugH[i,Romantic]>0,paste0("Romantic: ",sugH[i,Romantic]),""),"
                                        </div>
                                      </div>
                              <h4>Activity</h4>
                                     <div class=\"progress\">
                                        <div class=\"progress-bar progress-bar-success",if(sugH[i,Sport] < input$ActSpoUser){" off"},"\" style=\"width:",sugH[i,Sport]/sum(sugH[i,Sport]+sugH[i,Shopping]+sugH[i,Cultural]+sugH[i,Adventure])*100,"%\">
                                          ",ifelse(sugH[i,Sport]>0,paste0("Sport: ",sugH[i,Sport]),""),"
                                        </div>
                                        <div class=\"progress-bar progress-bar-warning",if(sugH[i,Shopping] < input$ActShoUser){" off"}," \" style=\"width:",sugH[i,Shopping]/sum(sugH[i,Sport]+sugH[i,Shopping]+sugH[i,Cultural]+sugH[i,Adventure])*100,"%\">
                                          ",ifelse(sugH[i,Shopping]>0,paste0("Shopping: ",sugH[i,Shopping]),""),"
                                        </div>
                                        <div class=\"progress-bar progress-bar-info",if(sugH[i,Cultural] != input$ActCulUser){" off"},"\" style=\"width:",sugH[i,Cultural]/sum(sugH[i,Sport]+sugH[i,Shopping]+sugH[i,Cultural]+sugH[i,Adventure])*100,"%\">
                                          ",ifelse(sugH[i,Cultural]>0,paste0("Cultural: ",sugH[i,Cultural]),""),"
                                        </div>
                                        <div class=\"progress-bar progress-bar-danger",if(sugH[i,Adventure] != input$ActAdvUser){" off"},"\" style=\"width:",sugH[i,Adventure]/sum(sugH[i,Sport]+sugH[i,Shopping]+sugH[i,Cultural]+sugH[i,Adventure])*100,"%\">
                                          ",ifelse(sugH[i,Adventure]>0,paste0("Adventure: ",sugH[i,Adventure]),""),"
                                        </div>
                                      </div>
                                
                           
                               ",tags$div(sugH[i,Text]),"
                             </div>
                               </div>")
    theHTML  <-  paste0(theHTML, suggestionHTML)
  }
  return(HTML(theHTML))
  })
})
