library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Vanza Recommender System"),
    sidebarLayout(
      sidebarPanel(
        h2("Personal Characteristics"),
        #actionButton(inputId="shuffle",label="start"),
        # the chance to select wich user to test about
        selectInput(inputId="userC",label="Choose the user for a recommendation:","",width="50%"),
        textOutput("userC"),
        h3("Preferences for the vacation"),
        h4("Which is your budget"),
        sliderInput(inputId="PriceUser",label="Which is your budget",min=1,max=3,value=sample(x=0:3,size=1),step=1,round=T,width="50%"),
        
        radioButtons(inputId="UserKindVar",label="Which Kind Of User are you?",
                     choices=list("All 1"=1,
                                  "All 2"=2,
                                  "All 3"=3),
                     selected=1,inline=T),
        textOutput("UserKind"),
        h4("Desired Environment"),
        sliderInput(inputId="NatCityUser",label="How much are you interested in staying in a city",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="NatMountUser",label="How much are you interested in staying in the mountains",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="NatSeaUser",label="How much are you interested in staying at the beach",min=0,max=3,value="",step=1,round=T,width="50%"),
        h4("Desired Enterteinment"),
        sliderInput(inputId="EntClubUser",label="How much are you interested in Clubbing",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="EntKidUser",label="How much are you interested in Kid enterteinment",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="EntRomUser",label="How much are you interested in Romantic stuff",min=0,max=3,value="",step=1,round=T,width="50%"),
        h4("Desired Activites"),
        sliderInput(inputId="ActSpoUser",label="How much are you interested in Sports",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="ActShoUser",label="How much are you interested in Shopping",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="ActCulUse",label="How much are you interested in Cultural Stuff",min=0,max=3,value="",step=1,round=T,width="50%"),
        sliderInput(inputId="ActAdvUser",label="How much are you interested in Adventure",min=0,max=3,value="",step=1,round=T,width="50%")
        ),
    mainPanel(
      tableOutput("tripper"),
      tableOutput("user"))
)))