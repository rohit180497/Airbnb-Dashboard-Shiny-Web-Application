
library("shiny")
library("plyr")
library("ggplot2")


## shinyServer( --------

function(input, output) { # server is defined within these parentheses
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    
    # Convert price to numeric
    air_bnb_final$price <- as.numeric(gsub("[$,]", "", air_bnb_final$price))
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
  
  # Reactive expression for filtered data
  filtered_data_listings <- reactive({
    
    dataListingsTemp <- listings
    
    # # Convert price to numeric
    # dataListingsTemp$price <- as.numeric(gsub("[$,]", "", dataListingsTemp$price))
    # 
    # 
    # # Filter by price range
    # priceRange <- input$priceRange
    # dataListingsTemp <- dataListingsTemp[dataListingsTemp$price >= priceRange[1] & dataListingsTemp$price <= priceRange[2], ]

    if (input$propertyType != "All") {
      # Check if input property type exists in the data frame
      if (tolower(input$propertyType) %in% tolower(dataListingsTemp$property_type)) {
        # Filter by property type
        dataListingsTemp <- dataListingsTemp[tolower(dataListingsTemp$property_type) == tolower(input$propertyType), ]
        print(dim(dataListingsTemp))
        print(input$propertyType)
      } else {
        # If input property type doesn't exist, return an empty data frame
        dataListingsTemp <- dataListingsTemp[FALSE, ]
      }
    }
    
    # # Filter by neighborhood
    # if (input$neighborhood != "All") {
    #   if (tolower(input$neighborhood) %in% tolower(dataListingsTemp$host_neighbourhood)) {
    #     dataListingsTemp <- dataListingsTemp[tolower(dataListingsTemp$host_neighbourhood) == tolower(input$neighborhood), ]
    #   } else {
    #     dataListingsTemp <- dataListingsTemp[FALSE, ]
    #   }
    # }
    # 
    # # Filter by date range
    # if(length(input$dateRange) > 1){
    #   # Extract start and end dates from the date range input
    #   startDate <- as.Date(input$dateRange[1])
    #   endDate <- as.Date(input$dateRange[2])
    #   
    #   # Filter data between start and end dates
    #   dataListingsTemp <- dataListingsTemp[dataListingsTemp$date >= startDate & dataListingsTemp$date <= endDate, ]
    # }
    
    return(dataListingsTemp)
  })
  
 
  
  
  ##FLASH CARDS
 
  # Pass total number of hosts and listings to UI
  output$flashcard_hosts <- renderText({
    shorten_number(n_distinct(filtered_data()$host_id))
  })
  
  output$flashcard_listings <- renderText({
    shorten_number(n_distinct(filtered_data()$listing_id))
  })
  
  output$total_towns <- renderText({
      shorten_number(n_distinct(filtered_data()$neighbourhood_cleansed))
    })
  
  output$total_earnings <- renderText({
    shorten_number(sum(filtered_data()$price))
  })
  
  # Function to convert numbers into a shortened format
  shorten_number <- function(num) {
    if (num >= 1e6) {
      return(paste0(round(num/1e6), "M+"))  # Convert to millions
    } else if (num >= 1e3) {
      return(paste0(round(num/1e3), "K+"))   # Convert to thousands
    } else {
      return(num)  # Return the number as is if less than 1000
    }
  }
  
  
  
  # Render plot for property type distribution
  output$propertyTypePlot <- renderPlot({
    filtered_data <- subset(air_bnb_final, property_type == input$propertyType)
    if (nrow(filtered_data) > 0) {
      hist(filtered_data$property_type, main = "Property Type Distribution", xlab = "Property Type")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No Data Available", type = "n", xlab = "", ylab = "")
    }
  })
  
  
  
  # Seasonal Trends
  output$seasonalTrends <- renderPlot({
    
    # Get filtered data
    filtered_data <- filtered_data()
    filtered_data$price_x <- as.numeric(sub("\\$", "", filtered_data$price_x))
    monthly_revenue <- aggregate(price_x ~ month, filtered_data, sum)
    monthly_revenue$Total_Revenue_Million <- monthly_revenue$price_x / 1e6
    
    # Convert numeric month to month abbreviation
    monthly_revenue$month <- factor(month.abb[monthly_revenue$month], levels = month.abb)
    
    # Convert max_revenue_month to month abbreviation
    print(monthly_revenue)
    ggplot(monthly_revenue, aes(x = as.factor(month), y = Total_Revenue_Million, group = 1)) +
      geom_line() +
      geom_point() +  # Add points for each data point
      labs(x = "Month", y = "Host Income (Million)") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Remove major gridlines
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12))
    
    
    
    
  })
  

   # # Listing Performance
   # output$listingPerformance <- renderPlot({
   #   
   #   # Get filtered data
   #   filtered_data <- filtered_data()
   #   filtered_data$price_x <- as.numeric(sub("\\$", "", filtered_data$price_x))
   #   listing_revenue <- aggregate(price_x ~ listing_id, filtered_data, sum)
   #   listing_revenue$Total_Revenue_Million <- listing_revenue$price_x / 1e6
   #   
   #   ggplot(listing_revenue, aes(x = Total_Revenue_Million, y = price_x)) +
   #     geom_point(color = "orange") +  # Scatter plot instead of histogram
   #     labs(x = "Total Revenue (Million)", y = "Frequency") +
   #     ggtitle("Listing Performance") +
   #     theme_minimal() +
   #     theme(
   #       panel.grid.major = element_blank(),  # Remove major gridlines
   #       panel.grid.minor = element_blank(),  # Remove minor gridlines
   #       plot.title = element_text(hjust = 0.5)  # Center the title
   #     ) +
   #     scale_y_continuous(breaks = seq(0, 1000, by = 200))  # Set y-axis labels
   #   
   # })
   
   
  # # Superhost Impact
  # output$superhostImpact <- renderPlot({
  #   filtered_data <- filtered_data() 
  #   
  #   # ggplot(filtered_data, aes(x = host_is_superhost, y = mean(price), fill = host_is_superhost)) +
  #   #   geom_bar(stat = "identity") +
  #   #   labs(x = "Superhost", y = "Average Revenue", fill = "Superhost") +
  #   #   ggtitle("Superhost Impact") +
  #   #   theme_minimal() +
  #   #   theme(
  #   #     panel.grid.major = element_blank(),  # Remove major gridlines
  #   #     panel.grid.minor = element_blank(),  # Remove minor gridlines
  #   #     plot.title = element_text(hjust = 0.5)  # Center the title
  #   #   )
  #   
  #   # Calculate the count of hosts for each category
  #   # Data
  #   unique_df <- filtered_data %>% distinct(host_id, .keep_all = TRUE)
  #   host_counts <- table(unique_df$host_is_superhost)
  #   host_counts_df <- as.data.frame(host_counts)
  #   names(host_counts_df) <- c("Superhost", "Count")
  #   
  #   # Colors
  #   airbnb_colors <- c("#FF5A5F", "#008489")  # Example Airbnb colors
  #   
  #   # Plot
  #   ggplot(host_counts_df, aes(x = "", y = Count, fill = Superhost)) +
  #     geom_col(width = 1) +
  #     coord_polar("y") +
  #     scale_fill_manual(values = airbnb_colors) +
  #     labs(x = NULL, y = NULL, fill = "Superhost") +
  #     #ggtitle("Superhost Distribution") +
  #     theme_void() +
  #     geom_text(aes(label = scales::percent(Count / sum(Count)), y = Count), 
  #               position = position_stack(vjust = 0.5),
  #               color = "white", size = 4)
  #   
  # })
  
  
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
      theme(
        axis.title = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 12)
        
      )+
      scale_color_manual(values = c("Saturday" = "green", "Sunday" = "red")) +
      guides(color = guide_legend(title = "Day of Week"))
    
    
  })
  
  
  ##Location and neighbourhood
  output$locationmap <- renderLeaflet({
    filtered_data <- filtered_data()
    unique_df <- filtered_data %>% distinct(listing_id, .keep_all = TRUE)
    #view(unique_df)
    airbnb <- unique_df %>% 
      select(latitude, longitude, name, property_type, price, room_type, listing_id,host_neighbourhood) %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    popup_info <- paste("<b>Property Name:</b> ", airbnb$name,
                        "<b>Neighborhood:</b> ", airbnb$host_neighbourhood,
                        "<br><b>Property Type:</b>", airbnb$property_type,
                        "<br><b>Room Type:</b>", airbnb$room_type,
                        "<br><b>Price:</b> $", airbnb$price)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = airbnb,
                       popup = popup_info,
                      # fillOpacity = 0.8,
                      label = ~paste("$",price),
                       radius = 5)
  })
  
  #Airbnb it .
 
  
  output$avgPrice <- renderUI({
    avg_price <- round(mean(filtered_data()$price)*30, 0)
    HTML(paste("<div class='avg-price'>", "$", avg_price, "</div>"))
  })
  
  output$avgPrice_aNight <- renderUI({
    avg_price_night <- round(mean(filtered_data()$price), 0)
    HTML(paste("<div class='avg-price-aNight'>", "30 Nights at an estimated $", avg_price_night, "a night </div>"))
  })
  

  # Calculate neighborhood-wise average costing
  neighborhood_avg_costing <- reactive({
    filtered_data() %>%
      group_by(host_neighbourhood) %>%
      summarise(avg_costing = mean(price_x, na.rm = TRUE)) %>%
      arrange(desc(avg_costing)) %>%
      head(15)  # Select top 15 neighborhoods
  })
  # Define a custom palette from light green to dark blue
  my_palette <- colorRampPalette(c("#00008B", "#90EE90"))(10)
  
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
    
    # Select top 10 neighborhoods
    top_10_neighborhoods <- head(neighborhood_cost, 10)
    
    ggplot(top_10_neighborhoods, aes(x = host_neighbourhood, y = price_x)) +
      geom_bar(stat = "identity", fill = my_palette) +
      labs(x = "Neighborhood", y = "Average Cost") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis label font size
            axis.text.y = element_text(size = 12),  # Increase y-axis label font size
            axis.title= element_text(size = 14,face = "bold"),  # Increase axis title font size
            
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
  
  
  
  
  # Super Host Pricing Strategy
  output$superHostPricing <- renderPlot({
    filtered_data <- filtered_data()
    
    # Calculate average price for superhosts and non-superhosts
    avg_prices <- aggregate(price_x ~ host_is_superhost, data = filtered_data, FUN = mean)
    
    # Plotting a grouped bar chart with enhancements
    ggplot(avg_prices, aes(x = host_is_superhost, y = price_x, fill = host_is_superhost)) +
      geom_bar(stat = "identity", color = "black", size = 0.1) +  # Adjust the outline of the bars
      geom_text(aes(label = paste0("$", round(price_x, 2))), vjust = -0.5, size = 4) +  # Add data labels above the bars
      labs(x = "Super Host", y = "Average Price", fill = "Super Host") +
      ggtitle("Super Host Pricing Strategy") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),  # Increase title font size
        axis.text  = element_text(size = 14),  # Increase x-axis label font size
        axis.title = element_text(size = 14, face="bold"),  # Increase x-axis title font size
        legend.position = "none"  # Remove legend
      )
  })
  
  output$roomTypePlot <- renderPlot({
    # Subset data for super hosts
    super_host_counts <- aggregate(host_id ~ room_type + host_is_superhost, air_bnb_final, function(x) length(unique(x)))
    
    # Plot
    ggplot(super_host_counts, aes(x = room_type, y = host_id, fill = host_is_superhost)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = host_id), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add labels on the bars
      labs(x = "Room Type", y = "Count", fill = "Superhost") +
      scale_fill_manual(values = c("skyblue", "#FF5733"), labels = c("No", "Yes")) +
      theme_minimal() +
      theme(axis.text.x = element_text( hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14, face="bold"),
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "right")
  })
  

  output$avgPriceByPropertyTypeBubbleplot <- renderPlot({
    
    # Calculate average price by property type
    avg_price <- aggregate(price ~ property_type, data = filtered_data(), FUN = mean)
    
    # Create bubble plot of average price by property type
    ggplot(avg_price, aes(x = property_type, y = price, size = price, fill = property_type)) +
      geom_point(shape = 21) +
      scale_size_continuous(range = c(5, 30))+
    labs(
           x = "Property Type", y = "Average Price",
           size = "Average Price") +
      theme_minimal() +  # Use a minimal theme without gridlines
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Increase font size for x-axis labels
            axis.text = element_text(size = 12),  # Increase font size for axis labels
            axis.title = element_text(size = 15),
            plot.title = element_text(hjust = 0.5, size = 20)) +  # Center title and increase font size
      guides(fill = FALSE, size = FALSE) +  # Remove the legend for fill and size
      scale_fill_manual(values = c("#FF5733", "#FFD700", "#7FFF00", "#00FFFF", "#FF69B4", "#8A2BE2", "#00FF00", "#808000", "#FFA500", "#4B0082", "#800000", "#00FF7F", "#8B4513", "#9400D3", "#800080"))  # Set custom fill colors using hexadecimal codes
  
    })
  
 
  #
  
  output$RatingCount <- renderPlot({

    # listings_filtered <- filtered_data_listings()
    # print(dim(listings_filtered))
    # # Subset the data to include only the relevant review score variables
    # reviews_subset <- listings_filtered  %>%
    #   select(index, review_scores_accuracy:review_scores_value)
    # print(dim(reviews_subset))
    # 
    # # Melt the data for easy plotting
    # melted_reviews <- melt(reviews_subset, id.vars = "index")
    # 
    # melted_reviews <-  melted_reviews %>% na.omit(melted_reviews)
    # print(dim(melted_reviews))
    # 
    # # Function to categorize review scores
    # categorize_reviews <- function(scores) {
    #   ifelse(scores <= 5, "Poor",
    #          ifelse(scores <= 8, "Good", "Excellent"))
    # }
    # 
    # # Categorize review scores and add as a new column
    # melted_reviews$category <- categorize_reviews(melted_reviews$value)
    # dim(melted_reviews)
    
    
    listings_filtered <- filtered_data_listings()
    print(dim(listings_filtered))
    # Subset the data to include only the relevant review score variables
    reviews_subset <- listings_filtered %>%
      select(index, review_scores_accuracy:review_scores_value)
    print(dim(reviews_subset))
    
    # Melt the data for easy plotting
    melted_reviews <- reviews_subset %>%
      pivot_longer(cols = -index, names_to = "variable", values_to = "value")
    
    melted_reviews <-  melted_reviews %>% na.omit(melted_reviews)
    print(dim(melted_reviews))
    
    # Function to categorize review scores
    categorize_reviews <- function(scores) {
      ifelse(scores <= 5, "Poor",
             ifelse(scores <= 8, "Good", "Excellent"))
    }
    
    # Categorize review scores and add as a new column
    melted_reviews$category <- categorize_reviews(melted_reviews$value)
    dim(melted_reviews)
    
    
    # Plot
    ggplot(melted_reviews, aes(x = variable, fill = category)) +
      geom_bar(position = "dodge") +
      scale_x_discrete(labels = c(
        "review_scores_accuracy" = "Accuracy",
        "review_scores_cleanliness" = "Cleanliness",
        "review_scores_checkin" = "Check-in",
        "review_scores_communication" = "Communication",
        "review_scores_location" = "Location",
        "review_scores_value" = "Value"
      )) +
      #geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
      labs(x = "Review Type", y = "Ratings Count") +
      theme_minimal()+
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15, face="bold"),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        plot.title = element_text(hjust = 0.5)  # Center the title
      )

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
    filtered_data_listings <- filtered_data_listings()
    
    # Split amenities into individual items
    amenities_split <- str_split(filtered_data_listings$amenities, ",") 
    
    # Flatten the list of lists into a single list
    amenities_flat <- unlist(amenities_split)
    
    # Apply gsub to remove special characters from each element of the list
    amenities_clean <- lapply(amenities_flat, function(x) gsub("[[:punct:]]", "", x))
    
    # Optionally, you can also remove leading and trailing whitespace
    amenities_clean <- lapply(amenities_clean, trimws)
    
    # Convert back to a character vector if needed
    amenities_clean <- unlist(amenities_clean)
    
    # Count the frequency of each amenity
    amenity_counts <- table(amenities_clean)
    
    # Convert to data frame
    amenity_df <- data.frame(Amenity = names(amenity_counts), Count = as.numeric(amenity_counts))
    
    # Sort amenities by count
    amenity_df <- amenity_df %>% arrange(desc(Count))
    
    # Plot top N popular amenities
    top_n <- 10  # Number of top amenities to plot
    top_amenities <- head(amenity_df, top_n)
   # print(top_amenities)
    # Plot
    my_palette <- colorRampPalette(c("#725168","#ba9bb1"))(10)
    
    ggplot(top_amenities, aes(x = reorder(Amenity, Count), y = Count)) +
      geom_bar(stat = "identity", fill = my_palette) +
      scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
      coord_flip() +
      theme(axis.title.y = element_blank(),  
            axis.text.y = element_text(size = 16),  
            plot.title = element_text(hjust = 0.5), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      labs(x = "", y="")
    
    # ggplot(top_amenities, aes(x = reorder(Amenity, Count), y = Count)) +
    #   geom_bar(stat = "identity", fill = my_palette) +
    #   scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    #   coord_flip() +
    #   theme(axis.title.y = element_blank(),  # Remove y-axis label
    #         axis.text.x = element_text(size = 16),  # Increase x-axis label font size
    #         plot.title = element_text(hjust = 0.5),  # Center the title
    #         panel.grid.major = element_blank(),  # Remove major gridlines
    #         panel.grid.minor = element_blank()) +
    #   labs(x = "Amenities", y = "")
    
    
    
  })
  
}




