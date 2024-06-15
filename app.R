library(tidyverse)
library(plotly)
library(shiny)
library(reactable)
library(bslib)

# grab the data set
clean_hike_data <- readRDS("clean_hike_data.rds")

# grab unique locations and features
locations_list <- unique(clean_hike_data$location_general)
features_list <- unique(unlist(clean_hike_data$features))
num_hikes <- nrow(clean_hike_data)

# theme to use throughout with colors from WTA webpage
setTheme <- bs_theme(
  primary = "#003029",
  secondary = "#4A7628",
  success = "#E9A82B",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

# Define UI
ui <- page_navbar( # set up navbar to link to different pages
  
  # adjust theme
  theme = setTheme,
  
  # main title in top bar
  title = "Washington Trails",

  nav_panel(title = "Description",
            theme = setTheme,
            
            p(paste("This app contains information on", num_hikes, "different trails in Washington. It allows you to explore 
                    this data set, and hopefully find a few hikes to your liking.")),
            
            p('The data originally comes from the ', 
              a(href = 'https://www.wta.org/', 'Washington Trails Association', .noWS = "outside"), 
              '. I got the idea to work with this data set from the ', 
              a(href = 'https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-11-24', 
                'TidyTuesday GitHub page', .noWS = "outside"), 
              ". I used their scraping and cleaning script, but I did edit it to grab a few more things, like the 
              link URL for each hike.",
              .noWS = c("after-begin", "before-end")),
              
              p("You can click on the tabs above to access the following widgets: "),
            
            HTML("<ul>
                 <li>A hike recommender, where you can input the attributes you'd like your hike to have (length, location, etc.)
                 and have one recommended</li>
                 <li>A table of all of the hikes in the data set, where you can sort by various attributes and use a search bar 
                 to search for something more specific</li>
                 <li>A page with interactive plots, where you can choose variables and see how they vary--e.g. see how user ratings
                 vary across different general locations</li>
                 </ul>"),
            
            p("Enjoy--and hopefully this can help you find something to do this summer!")
    ),
  
  
  
  nav_panel(title = "Hike recommender",
            theme = setTheme,
            
            p("Here you can have a trail recommended to you based on your specifications. Use the sliders and input boxes to 
              make your choices, hit 'Find a hike', and a suggestion will appear. Click the button again for more suggestions."),
            
            p("Note that I have truncated some of the sliders--e.g. the the longest trail in the data set is 295 miles, 
            but I have cut off the slider at 50 miles here to make it easier to use for most hikes. If you are looking 
            for longer hikes or with more elevation gain, try looking at the table of hikes tab and sorting the length 
            or gain columns from high to low."),
            
            p("All locations are selected by default. To remove locations, click one and hit delete or backspace. They can 
              then be selected again from the drop-down menu."),
            
            hr(),
            
            sidebarLayout(
              sidebarPanel(
                sliderInput("ratingC",
                            "Average user rating",
                            min = 0,
                            max = 5,
                            step = 0.1,
                            value = c(4, 4.5)),
                
                sliderInput("lengthC",
                        "Hike length (mi)",
                        min = 0,
                        max = 50,
                        value = c(5, 20)),
                
                sliderInput("gainC",
                        "Elevation gain (ft)",
                        min = 0,
                        max = 5000,
                        step = 100,
                        value = c(500, 3000)),
                
                sliderInput("highpointC",
                            "High point (ft)",
                            min = 0,
                            max = 5000,
                            step = 100,
                            value = c(500, 4000)),
                
                selectInput("locationC",
                        "General location",
                        multiple = TRUE,
                        selected = c("North Cascades", "Central Cascades", "Snoqualmie Region", "Eastern Washington",
                                     "Olympic Peninsula", "Issaquah Alps", "South Cascades", "Central Washington", 
                                     "Mount Rainier Area", "Southwest Washington", "Puget Sound and Islands"),
                        choices = locations_list),
                
                selectInput("featuresC",
                            "Features",
                            multiple = TRUE,
                            choices = features_list),
                
                width = 3
          
        ),

        
        mainPanel(
          
          actionButton("start", "Find a hike!"),

          p("Your recommended hike is ",
            textOutput(outputId = "name", inline = TRUE), ", with a length of ",
            textOutput(outputId = "length", inline = TRUE), ", an elevation gain of ",
            textOutput(outputId = "gain", inline = TRUE), "feet, and a high point of ",
            textOutput(outputId = "highpoint", inline = TRUE), "feet."),

          p("The general location is ",
            textOutput(outputId = "location", inline = TRUE), " and the average user rating is ",
            textOutput(outputId = "rating", inline = TRUE), " out of 5 stars."),

          p("A short description follows:"),

          textOutput("description"),

          br(),
          p("Find out more by visiting the link below!"),

          textOutput("url")
            
          
          
        )
    )),

  nav_panel(title = "Table of hikes",
            theme = setTheme,
    p("Here you can view all of the hikes available in the data set. Try clicking on column headers to sort by the highest 
      or lowest value--e.g. sort by 'user rating' to find the highest rated hikes."),
    
    p("You can also see a short description for each hike. Try using the search bar to filter for hikes that may have very 
      specific features that you're looking for. For example, try searching for 'bears', 'sunset', 'fishing', or whatever else
      you can think of."),
    reactableOutput("table")),
  
  nav_panel(title = "Plots",
            theme = setTheme,
    plotOutput("plot"),
    p("I'm planning to do more with the analysis of the data, looking at the associations between different variables. 
    I spent most of my time getting the recommendations to work... 
      Here is just an example looking at the distribution of the hike ratings by general location. I excluded hikes with 
      a zero rating because that typically meant that the hike did not have any ratings yet, which was skewing the data.")
))

hike_plot <- ggplot(clean_hike_data) + 
  facet_wrap(~location_general, scales = "free_x") +
  geom_density(mapping = aes(rating), color = "#4A7628", linewidth = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.1, NA)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Rating (out of five)", y = "") +
  theme(strip.background = element_blank()) +
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 24),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggtitle("Ratings distribution by location")

# define recommender function
recommender <- function(clean_hike_data, ratingC, lengthC, gainC, highpointC, locationC, featuresC) {
  
  saved <- 
    clean_hike_data %>% 
    filter(rating >= ratingC[1] & rating <= ratingC[2]) %>%
    filter(length_total >= lengthC[1] & length_total <= lengthC[2]) %>%
    filter(gain >= gainC[1] & gain <= gainC[2]) %>%
    filter(highpoint >= highpointC[1] & highpoint <= highpointC[2]) %>%
    filter(location_general %in% locationC) 
  
  return(saved)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected <- reactive(recommender(clean_hike_data, 
                                       input$ratingC, 
                                       input$lengthC, 
                                       input$gainC, 
                                       input$highpointC, 
                                       input$locationC, 
                                       input$featuresC))
  
  choice <- eventReactive(
    list(input$start, selected()),
    selected()[sample(1:length(selected()), 1), ]
  )
  
  output$name <- renderText(choice()$name)
  output$length <- renderText(choice()$length)
  output$trip <- renderText(choice()$trip)
  output$gain <- renderText(choice()$gain)
  output$highpoint <- renderText(choice()$highpoint)
  output$location <- renderText(choice()$location)
  output$url <- renderText(choice()$url)
  output$rating <- renderText(choice()$rating)
  output$description <- renderText(choice()$description)

  output$table <- renderReactable(
    # create the reactable
    reactable(clean_hike_data[, c(1:2, 11, 10, 4:8)],
              columns = list(
                name = colDef(name = "Hike name", width = 150),
                location = colDef(name = "Location", width = 200),
                length_total = colDef(name = "Total length (mi)", width = 150),
                trip = colDef(name = "Trip type", width = 100),
                gain = colDef(name = "Elevation gain (ft)", width = 175),
                highpoint = colDef(name = "High point (ft)", width = 150),
                rating = colDef(name = "User rating", width = 125),
                features = colDef(name = "Features", width = 200,
                                  format = colFormat(separators = TRUE)),
                description = colDef(name = "Description")),
              bordered = TRUE,
              highlight = TRUE,
              searchable = TRUE,
              sortable = TRUE))
  
  output$plot <- renderPlot(hike_plot,
                            height = 700)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)