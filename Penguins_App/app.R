library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
data("penguins")


ui <- fluidPage(
  br(), 
  br(), 
  column(4, offset = 4, titlePanel("Palmer Penguins")),
  #Feature 1: Adding image to the UI to improve overall aesthetic of the app 
  tags$figure(
    align = "center", 
    tags$img(
      src = "palmer_penguins.jpeg", height = "50%", width = "50%")),
  br(), 
  #Guiding the user on how the app is used plus improving general aesthetic of app 
  tags$hr(style = "border-color: purple;"),
  h3("Exploring the Palmer Penguins Dataset:"),
  strong("This app displays a scatter plot of body mass/weight of penguins against flipper length. The weight range can be changed as per the user while using the checkboxes to select island and species."),
  tags$hr(style = "border-color: purple;"),
  #Feature 2 & 3: Sidebar Layout containing slider to manipulate weight range and check box to select species and island. 
  sidebarLayout(
    sidebarPanel(
      sliderInput("weightInput", "Weight", 2000,7000, c(3000,6000),post = "g"),
      checkboxGroupInput("penguinTypeInput","Species",
                         choices = c("Adelie", "Gentoo","Chinstrap"),
                         selected = "Adelie"),
      checkboxGroupInput("islandInput","Island",
                         choices = c("Torgersen","Biscoe","Dream"),
                         selected = "Torgersen"),
      #Feature 4: allows for users to see individual images of each species 
      selectInput("select", label = h4("Penguin Species"),
                  choices = c("Adelie", "Gentoo", "Chinstrap"), 
                  selected = "Adelie"),
      uiOutput("images")
    ),
    
    mainPanel(
      #Feature 5: Add text to show options 
      textOutput("text"),
      tabsetPanel(
        #Feature 6: Individual tabs for scatter plot, histogram and data table
        tabPanel("Scatter Plot", plotOutput("plot")),
        tabPanel("Histogram", plotOutput("weight_hist")),
        #Feature 7: Download Button to allow users to download the dataset to their computers
        tabPanel("Table",DT::dataTableOutput("data_table"), downloadButton("download_table", "Download Table")),
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <-reactive({penguins %>%
      #filter the data based on weight, species and island creating a reactive set for use
      filter(body_mass_g >= input$weightInput[1],
             body_mass_g <= input$weightInput[2],
             species %in% input$penguinTypeInput,
             island %in% input$islandInput)})
  #Codes for the scatter plot 
  output$plot <- renderPlot({filtered_data() %>%
      ggplot(aes(body_mass_g,flipper_length_mm)) +
      geom_point(aes(colour = species, shape = island),size = 3) +
      labs(x = "Weight (g)", y = "flipper length (mm)")})
  
  #Codes for the histogram
  output$weight_hist <- renderPlot({
    filtered_data() %>% 
      ggplot(aes(body_mass_g)) +geom_histogram(col=I("black"), fill="orange")
  })
  
  #codes for text to show how many results have been generated 
  output$text <- renderText(paste(nrow(filtered_data()), "results have been generated!"))
  
  #codes for image selection
  output$images <- renderUI({
    if(input$select == "Adelie") {img(src = "adelie_penguin.jpeg", height = "50%", width = "50%")}
    else if(input$select == "Gentoo") {img(src = "gentoo_penguin.jpeg", height = "50%", width = "50%")}
    else if(input$select == "Chinstrap") {img(src = "chinstrap_penguin.jpeg", height = "50%", width = "50%")}
  })
  
  #code for downloading the table 
  output$download_table <- downloadHandler( 
    filename = function(){
      paste('data_table', '.csv')
    },
    content = function(file){
      write.csv(filtered_data(), file)
    }
  )
  
  #Feature 8: code for interactive table allowing users to search and sort data as per their requirement. 
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    })
  
}
shinyApp(ui = ui, server = server)