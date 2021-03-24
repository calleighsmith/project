library(shiny)
library(maps)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(patchwork)

us = map_data("state")

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
} 

# Define UI
ui <- fluidPage(

    titlePanel("How States Communicate with Citizens about Their Beaches"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("method",
                        "Method of Communication:",
                        c("Posted on Internet" = "prob_internet",
                          "Posted at Beach" = "prob_beach",
                          "Announced in Newspaper" = "prob_newspaper",
                          "Announced on Radio" = "prob_radio",
                          "Announced on TV" = "prob_tv",
                          "Sent to Phone List" = "prob_phone",
                          "Results on Request" = "prob_request",
                          "Other" = "prob_other"))
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

states_comms$region <- stateFromLower(states_comms$state_code)
us_states_elec <- left_join(us, states_comms)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        p<- (ggplot(data=us_states_elec, aes_string(x="long", y="lat", group="group", fill=input$method)) +
        geom_polygon(color = "black", size = 0.1)+
        geom_point(data=environmental_literacy, aes(x=longitude, y=latitude, size = rating), color="black", inherit.aes = FALSE) +
        guides(size=guide_legend("Environmental Literacy Rating")) +
        scale_fill_gradientn(colours = rev(terrain.colors(10)), na.value = "lightgrey") +
        theme(strip.background = element_blank()) +
        theme_map() + 
        labs(fill = "Percent of Beaches Using this Method of Communication"))
        p
        
        
      
        
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
