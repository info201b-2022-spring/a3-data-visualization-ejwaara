library(shiny)
library(dplyr)
library(fmsb)

char_df <- read.csv("characters.csv")

# Define UI
ui <- fluidPage(
  selectInput(
    inputId = "char",
    label = "Select a character",
    choices = char_df$Character
  ),
  tableOutput(outputId = 'table'),
  plotOutput(outputId = "radar")
)

# Define server logic
server <- function(input, output) {
  
  make_radar_df <- function(char_name){
    rd_df <- select(char_df, -c( Character, Class ))
    
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(char_df, Character == char_name)
    data_pt <- select(data_pt,-c( Character, Class ))
    
    #lets glue all three dataframes together 
    return( do.call("rbind", list(max_df, min_df, data_pt)) )
    
  }
  
  output$table <- renderTable({
    #return(filter(char_df, Character == input$char))
    return( make_radar_df(input$char) )
  })
  
  output$radar <- renderPlot({
    radarchart(make_radar_df(input$char))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
