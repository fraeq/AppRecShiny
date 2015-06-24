library(shiny)

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Vanza Recommender System"),
    sidebarLayout(
      sidebarPanel(width=3,
        tabsetPanel(
          tabPanel("Main Panel",
                   h2("Personal Characteristics"),
                   selectInput(inputId="userC",label="Choose the user for a recommendation:",choices="",width="100%"),
                   textOutput("userC"),
                   h3("Preferences for the vacation"),
                   h4("Which is your budget"),
                   sliderInput(inputId="PriceUser",label="Which is your budget",min=1,max=3,value=sample(x=0:3,size=1),step=1,round=T,width="100%"),
                   radioButtons(inputId="UserKindVar",label="Which Kind Of User are you?", choices=list("Couple"=1, "Family"=2, "Friends"=3), selected=1,inline=T),
                   textOutput("UserKind"),
                   numericInput(inputId="numCh",label="How many suggestion:",value=5,min=1,max=10,step=1)
                   #numericInput(inputId="variability",label="choose the variability in the travels",value=1,min=0.1,max=2,step=0.1)
                   
          ),
          tabPanel("Advanced",
                   h3("Preferences for the vacation"),
                   actionButton(inputId="computeTravel",label="Reshuffle the trip")
                   )
        )
      
    ),
      mainPanel(
        sidebarLayout(
          position="right",
            sidebarPanel(width=3,
              numericInput(inputId="wFb",label="Facebook weight",value=1,min=0,max=10,step=1),
              numericInput(inputId="wTr",label="Preferences weight",value=1,min=0,max=10,step=1),
              h4("Desired Environment"),
              sliderInput(inputId="NatCityUser",label="How much are you interested in staying in a city",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="NatMountUser",label="How much are you interested in staying in the mountains",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="NatSeaUser",label="How much are you interested in staying at the beach",min=0,max=3,value="",step=1,round=T,width="100%"),
              h4("Desired Entertainment"),
              sliderInput(inputId="EntClubUser",label="How much are you interested in Clubbing",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="EntKidUser",label="How much are you interested in Kid entertainment",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="EntRomUser",label="How much are you interested in Romantic stuff",min=0,max=3,value="",step=1,round=T,width="100%"),
              h4("Desired Activites"),
              sliderInput(inputId="ActSpoUser",label="How much are you interested in Sports",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="ActShoUser",label="How much are you interested in Shopping",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="ActCulUser",label="How much are you interested in Cultural Stuff",min=0,max=3,value="",step=1,round=T,width="100%"),
              sliderInput(inputId="ActAdvUser",label="How much are you interested in Adventure",min=0,max=3,value="",step=1,round=T,width="100%")
            ),
          mainPanel(
  #           textOutput("NatCityUser"),
  #           tableOutput("suggestion"),
  #           tableOutput("profile"),
  #           tableOutput("tripper"),
  #           tableOutput("user"),
  #           textOutput("userCheck"),
  #           tableOutput("fbscore"),
  #           tableOutput("tripscore")
              textOutput("woof"),
              uiOutput("sug2")  
              
          )
        )
        
      )
      )
    )
  )