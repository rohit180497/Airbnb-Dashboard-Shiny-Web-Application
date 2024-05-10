
library("shiny")
# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

library("plyr")
library("ggplot2")


## shinyServer( --------

function(input, output) { # server is defined within these parentheses
  
  # Convert price to numeric
  air_bnb_final$price <- as.numeric(gsub("[$,]", "", air_bnb_final$price))
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    dataTemp <- air_bnb_final
    
    # Filter by price range
    priceRange <- input$priceRange
    dataTemp <- dataTemp[dataTemp$price >= priceRange[1] & dataTemp$price <= priceRange[2], ]
    
   
    
    # Filter by property type
    if (input$propertyType != "All") {
      dataTemp <- dataTemp[dataTemp$property_type == input$propertyType, ]
    }
    
    # Filter by neighborhood
    if (input$neighborhood != "All") {
      dataTemp <- dataTemp[dataTemp$host_neighbourhood == input$neighborhood, ]
    }
    
    if(length(input$dateRange) > 1){
      # Extract start and end dates from the date range input
      startDate <- as.Date(input$dateRange[1])
      endDate <- as.Date(input$dateRange[2])
      
      # Filter data between start and end dates
      dataTemp <- dataTemp[dataTemp$date >= startDate & dataTemp$date <= endDate, ]
    }
    
    return(dataTemp)
  })
  
  ##FLASH CARDS
  
  
  
  # Render plot for property type distribution
  output$propertyTypePlot <- renderPlot({
    filtered_data <- subset(air_bnb_final, property_type == input$propertyType)
    if (nrow(filtered_data) > 0) {
      hist(filtered_data$property_type, main = "Property Type Distribution", xlab = "Property Type")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No Data Available", type = "n", xlab = "", ylab = "")
    }
  })
  
  # Create a summary table
  output$summaryTable <- renderTable({
    # Get filtered data
    filtered_data <- filtered_data()
    summary_table <- table(filtered_data$property_type)
    data.frame(Property_Type = names(summary_table), Count = as.integer(summary_table))
  }) 
  
  # Calculate average price
  output$avgPrice <- renderText({
    avg_price <- mean(filtered_data()$price)
    paste("Average Price: $", round(avg_price, 2))
  })

  
  # Calculate percentage of superhosts
  output$superhosts <- renderText({
    superhost_percent <- (sum(filtered_data()$host_is_superhost == "t") / nrow(filtered_data())) * 100
    paste("Superhosts: ", round(superhost_percent, 2), "% of listings")
  })
  
  
  # Market Growth
  output$marketGrowth <- renderTable({
    yearly_data <- filtered_data() %>%
      summarize(Listings_Count = n_distinct(listing_id), Totally_Revenue = as.integer(sum(price)))
    
    # Assign custom column names
    names(yearly_data) <- c("Number of Listings", "Total Revenue")
    
    # Return the modified data frame
    yearly_data
  }, 
  
  striped = TRUE,
  spacing = "l",
  align = "c",
  digits = 4,
  width = "20%",
  colnames = TRUE,
 # caption = "Market Growth Summary"
  )
  
  
  # Seasonal Trends
  output$seasonalTrends <- renderPlot({
    
    # Get filtered data
    filtered_data <- filtered_data()
    filtered_data$price_x <- as.numeric(sub("\\$", "", filtered_data$price_x))
    monthly_revenue <- aggregate(price_x ~ month, filtered_data, sum)
    monthly_revenue$Total_Revenue_Million <- monthly_revenue$price_x / 1e6
    
    max_revenue_month <- which.max(monthly_revenue$Total_Revenue_Million)
    
    # Convert numeric month to month abbreviation
    monthly_revenue$month <- month.abb[monthly_revenue$month]
    
    # Convert max_revenue_month to month abbreviation
    max_revenue_month <- month.abb[max_revenue_month]
    
    ggplot(monthly_revenue, aes(x = factor(month), y = Total_Revenue_Million, group = 1)) +
      geom_line() +
      geom_point() +  # Add points for each data point
      scale_fill_manual(values = c("grey", "cyan"), guide = FALSE) +  # Highlight the bar with highest revenue in red, others in grey
      labs(x = "Month", y = "Total Revenue (Million)", title = "Monthly Revenue") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Remove major gridlines
            panel.grid.minor = element_blank())
    
  })
  

   # Listing Performance
   output$listingPerformance <- renderPlot({
     
     # Get filtered data
     filtered_data <- filtered_data()
     filtered_data$price_x <- as.numeric(sub("\\$", "", filtered_data$price_x))
     listing_revenue <- aggregate(price_x ~ listing_id, filtered_data, sum)
     listing_revenue$Total_Revenue_Million <- listing_revenue$price_x / 1e6
     
     ggplot(listing_revenue, aes(x = Total_Revenue_Million, y = price_x)) +
       geom_point(color = "orange") +  # Scatter plot instead of histogram
       labs(x = "Total Revenue (Million)", y = "Frequency") +
       ggtitle("Listing Performance") +
       theme_minimal() +
       theme(
         panel.grid.major = element_blank(),  # Remove major gridlines
         panel.grid.minor = element_blank(),  # Remove minor gridlines
         plot.title = element_text(hjust = 0.5)  # Center the title
       ) +
       scale_y_continuous(breaks = seq(0, 1000, by = 200))  # Set y-axis labels
     
   })
   
   
  # Superhost Impact
  output$superhostImpact <- renderPlot({
    filtered_data <- filtered_data() 
    
    # ggplot(filtered_data, aes(x = host_is_superhost, y = mean(price), fill = host_is_superhost)) +
    #   geom_bar(stat = "identity") +
    #   labs(x = "Superhost", y = "Average Revenue", fill = "Superhost") +
    #   ggtitle("Superhost Impact") +
    #   theme_minimal() +
    #   theme(
    #     panel.grid.major = element_blank(),  # Remove major gridlines
    #     panel.grid.minor = element_blank(),  # Remove minor gridlines
    #     plot.title = element_text(hjust = 0.5)  # Center the title
    #   )
    
    # Calculate the count of hosts for each category
    # Data
    unique_df <- filtered_data %>% distinct(host_id, .keep_all = TRUE)
    host_counts <- table(unique_df$host_is_superhost)
    host_counts_df <- as.data.frame(host_counts)
    names(host_counts_df) <- c("Superhost", "Count")
    
    # Colors
    airbnb_colors <- c("#FF5A5F", "#008489")  # Example Airbnb colors
    
    # Plot
    ggplot(host_counts_df, aes(x = "", y = Count, fill = Superhost)) +
      geom_col(width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = airbnb_colors) +
      labs(x = NULL, y = NULL, fill = "Superhost") +
      ggtitle("Superhost Distribution") +
      theme_void() +
      geom_text(aes(label = scales::percent(Count / sum(Count)), y = Count), 
                position = position_stack(vjust = 0.5),
                color = "white", size = 4)
    
  })
  
  
  # Generate plot based on selected number of weeks
  output$WeekTrends <- renderPlot({
    filtered_data <- filtered_data()
    
    data <- 
      aggregate(price_x ~ date, filtered_data, mean)
    
    # Generate sequence of dates with weekly intervals
    start_date <- min(data$date)
    end_date <- max(data$date)
    weekly_dates <- seq(start_date, end_date, by = "week")
    
    # Calculate end date based on selected number of weeks
    end_display_date <- start_date + (input$num_weeks * 7) - 1
    selected_data <- data[data$date >= start_date & data$date <= end_display_date, ]
    
    ggplot(selected_data, aes(x = date, y = price_x)) +
      geom_line() +
      geom_point(data = filter(selected_data, weekdays(date) %in% c("Saturday", "Sunday")), aes(color = weekdays(date)), size = 3) +  # Highlight weekend points
      scale_x_date(date_breaks = "1 week", date_labels = "%m/%d/%Y") +
      labs(x = "Date", y = "Average Price", color = "Day of Week") +
      theme_minimal() +
      scale_color_manual(values = c("Saturday" = "green", "Sunday" = "red")) +
      guides(color = guide_legend(title = "Day of Week"))
    
    
  })
  
  
  ##Location and neighbourhood
  output$locationmap <- renderLeaflet({
    filtered_data <- filtered_data()
    unique_df <- filtered_data %>% distinct(listing_id, .keep_all = TRUE)
    #view(unique_df)
    airbnb <- unique_df %>% 
      select(latitude, longitude, name, property_type, price, room_type, listing_id) %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    popup_info <- paste("<b>Property Name:</b> ", airbnb$name,
                        "<br><b>Property Type:</b> $", airbnb$property_type,
                        "<br><b>Room Type:</b> $", airbnb$room_type,
                        "<br><b>Price:</b> $", airbnb$price)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = airbnb,
                       popup = popup_info,
                       fillOpacity = 0.8,
                       radius = 5)
  })
  
  

  # Calculate neighborhood-wise average costing
  neighborhood_avg_costing <- reactive({
    filtered_data() %>%
      group_by(host_neighbourhood) %>%
      summarise(avg_costing = mean(price_x, na.rm = TRUE)) %>%
      arrange(desc(avg_costing)) %>%
      head(15)  # Select top 15 neighborhoods
  })
  
  # Costing Plot
  output$Costing <- renderPlot({
    
    # Get filtered data
    filtered_data <- filtered_data()
    filtered_data$price_x <- as.numeric(sub("\\$", "", filtered_data$price_x))
    
    # Aggregate data by neighborhood
    neighborhood_cost <- aggregate(price_x ~ host_neighbourhood, filtered_data, mean)
    
    # Reorder the levels of host_neighbourhood based on average cost in descending order
    neighborhood_cost <- neighborhood_cost[order(-neighborhood_cost$price_x), ]
    neighborhood_cost$host_neighbourhood <- factor(neighborhood_cost$host_neighbourhood, levels = neighborhood_cost$host_neighbourhood)
    
    # Select top 15 neighborhoods
    top_15_neighborhoods <- head(neighborhood_cost, 15)
    
    ggplot(top_15_neighborhoods, aes(x = host_neighbourhood, y = price_x)) +
      geom_bar(stat = "identity") +
      labs(x = "Neighborhood", y = "Average Cost", title = "Top 15 Neighborhoods by Average Cost") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis label font size
            axis.text.y = element_text(size = 12),  # Increase y-axis label font size
            axis.title.x = element_text(size = 14),  # Increase x-axis title font size
            axis.title.y = element_text(size = 14),  # Increase y-axis title font size
            plot.title = element_text(hjust = 0.5, size = 16),  # Increase title font size and center it
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
  # Super Host Pricing Strategy
  output$superHostPricing <- renderPlot({
    filtered_data <- filtered_data()
    
    ggplot(filtered_data, aes(x = host_is_superhost, y = price_x, fill = host_is_superhost)) +
      geom_boxplot() +
      labs(x = "Super Host", y = "Price", fill = "Super Host") +
      ggtitle("Super Host Pricing Strategy") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),  # Increase title font size
        axis.text.x = element_text(size = 14),  # Increase x-axis label font size
        axis.text.y = element_text(size = 14),  # Increase y-axis label font size
        axis.title.x = element_text(size = 14),  # Increase x-axis title font size
        axis.title.y = element_text(size = 14)  # Increase y-axis title font size
      )
  })
  
  
  # Inside your server function in server.R
  output$avgPriceByPropertyTypeBubbleplot <- renderPlot({
    # Load required libraries
    library(ggplot2)
    
    # Calculate average price by property type
    avg_price <- aggregate(price ~ property_type, data = air_bnb_final, FUN = mean)
    
    # Create bubble plot of average price by property type
    ggplot(avg_price, aes(x = property_type, y = price, size = price, fill = property_type)) +
      geom_point(shape = 21, color = "black") +
      scale_size_continuous(range = c(3, 15)) +  # Adjust bubble size range
      labs(title = "Average Price by Property Type",
           x = "Property Type", y = "Average Price",
           size = "Average Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Increase font size for x-axis labels
            axis.text.y = element_text(size = 10),  # Increase font size for y-axis labels
            plot.title = element_text(hjust = 0.5, size = 20)) +  # Center title and increase font size
      guides(fill = FALSE, size = FALSE)  # Remove the legend for fill and size
  })
  
  
  ##Amenities vs Price
  output$Price_vs_amenities <- renderPlot({ 
    filtered_data <- filtered_data() 
    
    # Convert price_x column to numeric
    filtered_data$price_x <- as.numeric(gsub("[$,]", "", filtered_data$price_x))
    
    # Plot amenities vs price
    ggplot(filtered_data, aes(x = total_amenities, y = price_x)) +
      geom_point() +
      labs(x = "Total Amenities", y = "Price") +
      ggtitle("Amenities vs Price")
    
  })
  
  
  ##Popular amenities
  output$popularamenities <- renderPlot({ 
    filtered_data <- filtered_data()
    
    # Split amenities into individual items
    amenities_split <- str_split(filtered_data$amenities, ",") 
    
    # Flatten the list of lists into a single list
    amenities_flat <- unlist(amenities_split)
    
    # Count the frequency of each amenity
    amenity_counts <- table(amenities_flat)
    
    # Convert to data frame
    amenity_df <- data.frame(Amenity = names(amenity_counts), Count = as.numeric(amenity_counts))
    
    # Sort amenities by count
    amenity_df <- amenity_df %>% arrange(desc(Count))
    
    # Plot top N popular amenities
    top_n <- 10  # Number of top amenities to plot
    top_amenities <- head(amenity_df, top_n)
    
    # Plot
    ggplot(top_amenities, aes(x = reorder(Amenity, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(x = "Amenity", y = "Count") +
      ggtitle("Top 10 Popular Amenities")
    
  })
  
}




