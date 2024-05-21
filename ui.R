
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
      
      .well {
    min-height: 20px;
    height:130px;
    font-size:24px;
    font:bold;
    margin-bottom: 0px;
    background-color: rgb(245, 245, 245);
    box-shadow: rgba(0, 0, 0, 0.05) 0px 1px 1px inset;
    padding: 19px;
    border-width: 1px;
    border-style: solid;
    border-color: rgb(227, 227, 227);
    border-image: initial;
    border-radius: 4px;
    font-weight: 700;
      }

.avg-price {
  font-size: 35px;
  font-weight: bold;
  
}
      
    "))
  ),

  
  # Application title
  #tags$img(src = "D:/MPS ANALYTICS/Q2/ALY 6070/Final Project/ShinyApp/AirbnbShinyDashboard/www/images/logo.png"),
  # titlePanel( HTML('<img src="D:/MPS ANALYTICS/Q2/ALY 6070/Final Project/ShinyApp/AirbnbShinyDashboard/www/images/logo.png" alt="Airbnb" style="height:50px; margin-right:10px;">'),
  #             "Airbnb Dashboard"
  # ),
  
  titlePanel(
    title=div(img(src="images/logo1.jpg", height=90,width=120),"Airbnb Your Path to Profitability in Austin",style="color: #FF5A5F;font-weight: bold;font-family: inherit;
    font-size: 40px;")
    # HTML('<img  alt="Airbnb at Austin, Texas" style="height:50px; margin-right:10px;">'),
    #           Airbnb Dashboard"
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
      
      class = "sidebar"
    ),
    h3(""),
    h3(""),
  ),
   
    # Main panel
  fluidRow(
    column(
      width = 12,
    mainPanel(  
      
      tabsetPanel( 
     
        tabPanel("Performance and Trends", 
                 br(),
                 # Flash Cards
                 fluidRow(
                   style = "background-color:white ;padding:10px; margin-bottom:0px; ",
                   column(width=2),
                   column(width = 2,
                          style = "border-right: 5px solid pink; text-align: left;",
                          wellPanel(
                            class="well",
                            title = "Flashcard 1",
                            textOutput("flashcard_hosts"),
                            p(),
                            p("Hosts on Airbnb", style="font:initial;font-size:15px;"),
                            p("*as of December 31, 2015", style = "color: lightgrey; text-align: left;font-size:12px;")
                          )
                   ),
                  
                   column(width = 2,
                          style = "border-right: 5px solid pink; text-align: left;",
                          wellPanel(
                            title = "Flashcard 2",
                            textOutput("flashcard_listings"),p(),
                            p("Active Listings in Austin,TX", style="font:initial;font-size:15px;"),
                            p("*as of December 31, 2015", style = "color: lightgrey; text-align: left;font-size:12px;")
                          )
                   ),
                   column(width = 2,
                          style = "border-right: 5px solid pink; text-align: left;",
                          wellPanel(
                            title = "Flashcard 2",
                            textOutput("total_towns"),p(),
                            p("regions with Airbnb listings", style="font:initial;font-size:15px;"),
                            p("*as of December 31, 2015", style = "color: lightgrey; text-align: left;font-size:12px;")
                          )
                   ),
                   column(width = 2,
                          style = "border-right: 5px solid pink; text-align: left;",
                          wellPanel(
                            title = "Flashcard 2",
                            textOutput("total_earnings"),p(),
                            p("earned by Hosts, all time", style="font:initial;font-size:15px;"),
                            p("*as of December 31, 2015", style = "color: lightgrey; text-align: left;font-size:12px;")
                          )
                   ),
                   column(width=2)
                 ),
                 
                 # Add space between the flashcards and tab headers
                 br(),
                 
                 
                 fluidRow(
                   
                   column(12, 
                          h3("Exploring Monthly Revenue: Airbnb (2015-16) in Austin, TX"),
                          plotOutput("seasonalTrends"),
                          style = "text-align: center;"
                   )
                  
                  
                 ),
                h3("Weekly Earnings Analysis"),
                 sliderInput("num_weeks", "Number of Weeks to Display:",
                             min = 1,
                             max = 200, 
                             value = 4,
                             step = 1),
                 
                 plotOutput("WeekTrends"),
                style = "text-align: center;"
        ),
        
        tabPanel("Location and Neighbourhood" ,
                 fluidRow(
                   column(8, 
                          h3("Estimated Earnings by Location"),
                          leafletOutput("locationmap"),
                          style = "text-align: center;"
                   ),
                   column(4,
                          tags$p(
                            style = "font-size: 40px; color: red; font-weight: bold;",
                            "Airbnb it."
                          ),
                          p(
                            style = "font-size: 30px;",
                            "You could earn"
                          ),
                          p(
                            
                            #style = "font-size: 25px; text-decoration: underline;",  # Add underline
                            htmlOutput("avgPrice"),
                            
                          ),
                          p(
                            htmlOutput("avgPrice_aNight")
                          ),
                          style = "text-align: center;    padding-top: 150px;"
                   )
                   )
                  
        ),
        
        tabPanel("Pricing and Optimization",
                 fluidRow(
                   column(12, 
                          h3("Average Price by Property Type"),
                          
                          plotOutput("avgPriceByPropertyTypeBubbleplot"),
                          style = "text-align: center;"
                   ),
                   column(6,
                          h3("Hosts To Superhosts"),
                          plotOutput("roomTypePlot"),
                          style = "text-align: center;"
                          
                   ),
                   column(6,
                          h3("Top 10 Neighborhoods by Average Cost"),
                          plotOutput("Costing"),
                          style = "text-align: center;"
                          ),
                   
                  
                 )
                 
                 
                 
        ),
        tabPanel("Guest preference and Targeting",
                 fluidRow(
                   column(12,
                          h3("Guest Satisfaction Ratings"),
                          plotOutput("RatingCount"),
                          style = "text-align: center;"
                          ),
                   column(12, 
                          h3("Most Popular Amenities"),
                          plotOutput("popularamenities"),
                          style = "text-align: center;"
                          )
                 )
                 
                 
        ),
        ), class="mainpanel",
      style = "margin-left: 0px; margin-right: 0px;"  # Adjust margin to ensure the white strip stays within the tab panel
    )
    )
  ),
  style = "width: 100%;"  
)



