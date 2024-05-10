
library(shiny) 


ui <- fluidPage(
  
    # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Custom styling for sidebar */
      .sidebar {
    border: 2px solid red;
    border-radius: 20px 20px 20px 20px; /* Adjust border-radius for all corners */
    padding: 10px;
    box-shadow: 5px 5px 5px #888888;
    width: 99%;
    margin: 0 auto; /* Center the sidebar horizontally */
      }
 .mainpanel {
    border: 2px solid white;
    border-radius: 20px 20px 20px 20px; /* Adjust border-radius for all corners */
    padding: 10px;
    box-shadow: 5px 5px 5px #888888;
    width: 99%;
    margin: 0 auto; /* Center the sidebar horizontally */
 }

  .center-table {
    margin: 0 auto; /* Center align the table */
  }
  
  /* Custom styling for pink slider */
      .priceRangeSlider .irs--shiny .irs-bar {
        top: 25px;
        height: 7px;
        border-top: 1px solid #428bca;
        border-bottom: 1px solid #428bca;
        background: #f86c6c;
        cursor: s-resize;
        z-index: 2;
      }
      
      
    "))
  ),

  
  # Application title
  titlePanel( HTML('<img src="data/images/logo.png" alt="Airbnb" style="height:50px; margin-right:10px;">'),
              "Airbnb Dashboard"
  ),
 
  # Page with sidebar layout
    fluidRow(
    # Filters row
    fluidRow(
      column(
        width = 3,
        dateRangeInput("dateRange", "Date", 
                       start = min_date,  
                       end = max_date,    
                       min = min_date,    
                       max = max_date,    
                       format = "yyyy-mm-dd",  
                       startview = "month")
      ),
      # Add the price range filter
      column(
        width = 3,
        sliderInput("priceRange", "Price Range",
                    min = 0,  # Minimum price
                    max = 1000,  # Maximum price
                    value = c(0, 1000),  # Initial price range
                    step = 10,  # Step size for slider
                    dragRange = TRUE)  # Step size for slider
      ),
      column(
        width = 3,
        selectInput("propertyType", "Property Type", choices = c("All", unique(air_bnb_final$property_type)))
      ),
      column(
        width = 3,
        selectInput("neighborhood", "Neighborhood", choices = c("All", unique(air_bnb_final$host_neighbourhood)))
      ),
     
      class="sidebar"
    ),
    h3(""),
    h3(""),
    
   
    # Main panel
    mainPanel(  
      
      tabsetPanel( 
     
        tabPanel("Performance and Trends", 
                 br(),
                 # Flash Cards
                 fluidRow(
                   style = "background-color: white; padding-top: 20px; padding-bottom: 20px; margin-left: 0px; margin-right: 0px;",
                   column(width = 3,
                          style = "border-right: 5px solid pink;",
                          wellPanel(
                            title = "Flashcard 1",
                            p("Content for Flashcard 1")
                          )
                   ),
                   column(width = 3,
                          style = "border-right: 5px solid pink;",
                          wellPanel(
                            title = "Flashcard 2",
                            p("Content for Flashcard 2")
                          )
                   ),
                   column(width = 3,
                          style = "border-right: 5px solid pink;",
                          wellPanel(
                            title = "Flashcard 3",
                            p("Content for Flashcard 3")
                          )
                   ),
                   column(width = 3,
                          wellPanel(
                            title = "Flashcard 4",
                            p("Content for Flashcard 4")
                          )
                   )
                 ),
                 
                 # Add space between the flashcards and tab headers
                 br(),
                 
                 
                 fluidRow(
                   
                   column(12, 
                          h3("Seasonal Trends"),
                          plotOutput("seasonalTrends"),
                          style = "text-align: center;"
                   )
                 ),
                 fluidRow(
                   # column(6, 
                   #        h3("Listing Performance"),
                   #        plotOutput("listingPerformance")
                   # ),
                   column(6, 
                          h3("Uncovering the Earning Potential at AirBnb"),
                          tableOutput("marketGrowth")
                          #div(id = "marketGrowth", class = "center-table", tableOutput("marketGrowth"))
                          
                   ),
                   column(6, 
                          h3("Superhost Impact"),
                          plotOutput("superhostImpact")
                   )
                 ),
                 sliderInput("num_weeks", "Number of Weeks to Display:",
                             min = 1,
                             max = 100,  # Maximum number of weeks based on available data
                             value = 4,
                             step = 1),
                 plotOutput("WeekTrends")
        ),
        tabPanel("Location and Neighbourhood" ,
                 fluidRow(
                   column(12, 
                          h3("Location and Neighborhood"),
                          leafletOutput("locationmap")
                   )
                 )
        ),
        
        tabPanel("Pricing and Optimization",
                 plotOutput("Costing"),  # Plot for displaying neighborhood-wise average costing
                 plotOutput("superHostPricing"),
                 plotOutput("correlationHeatmap"),
                 plotOutput("avgPriceByPropertyTypeBubbleplot")
        ),
        tabPanel("Guest preference and Targeting", 
                 plotOutput("Price_vs_amenities"),
                 plotOutput("popularamenities")
        ),
      ), class="mainpanel",
      style = "margin-left: 0px; margin-right: 0px;"  # Adjust margin to ensure the white strip stays within the tab panel
      
    )
  ),
  style = "width: 100%;"  
)


