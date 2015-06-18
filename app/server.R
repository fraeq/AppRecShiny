# load the required libraries
library(shiny)
library(data.table)

# load the Facebook data
fb <- fread("facebook.csv")
# set he names to the data
setnames(fb,c("id","category","page","date","pageId"))
# load the Hotel data 
trip<-fread("tripscore.csv")

shinyServer(function(input,output){})
