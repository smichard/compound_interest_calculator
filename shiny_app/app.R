# Load libraries
library(dplyr)
library(ggplot2)
#library(gridExtra)
library(markdown)
library(lubridate)
library(shiny)
library(tibble)

# define UI
ui <- fluidPage(
  titlePanel("Capital building calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("start_date", "Start Year:", 
                  choices = 2000:2040, selected = 2024),
      numericInput("initial_capital", "Initial Capital:", value = 1000),
      numericInput("savings_rate", "Savings Rate:", value = 100),
      selectInput("savings_intervall", "Savings Intervall:", 
                  choices = c("monthly", "yearly")),
      numericInput("investment_period", "Investment Period (in years):", value = 20),
      sliderInput("interest_rate", "Interest Rate [ % ]:", 
                  min = 0, max = 50, value = 7.5, step = 0.5),
      sliderInput("adjustment_rate", "Adjustment Rate [ % ]:", 
                  min = 0, max = 20, value = 0, step = 0.5),
      numericInput("savings_suspension", 
                   "Savings Suspension after x years:", value = NA, min = 1),
      numericInput("target_value", "Target Value:", value = NA),
      actionButton("goButton", "Calculate")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotOutput("plot1")),
        tabPanel("Distribution", plotOutput("plot2"), tableOutput("table1")),
        tabPanel("Savings Rate", plotOutput("plot3"), textOutput("crosspoint_message")),
        tabPanel("Normalized Values", plotOutput("plot4"), textOutput("crosspoint_message_2")),
        tabPanel("Goals", plotOutput("plot5"), plotOutput("plot6"), plotOutput("plot7"), textOutput("target_message")),
        tabPanel("Values", tableOutput("table2")),
        tabPanel("Instructions", includeMarkdown("instructions.md"))
      )
    )
  )
)

# define server
server <- function(input, output) {
  input_values <- reactiveVal(NULL)

  observeEvent(input$goButton, {
    input_values(list(
      start_date = ymd(paste0(input$start_date, "-01-01")),
      adjustment_rate = input$adjustment_rate,
      interest_rate = input$interest_rate,
      initial_capital = input$initial_capital,
      savings_rate = input$savings_rate,
      savings_intervall = input$savings_intervall,
      investment_period = input$investment_period,
      savings_suspension = input$savings_suspension,
      target_value = input$target_value
    ))
    
    start_date <- input_values()$start_date
    adjustment_rate <- input_values()$adjustment_rate/100
    interest_rate <- input_values()$interest_rate/100
    initial_capital <- input_values()$initial_capital
    savings_rate <- input_values()$savings_rate
    savings_intervall <- input_values()$savings_intervall
    investment_period <- input_values()$investment_period
    savings_suspension <- input_values()$savings_suspension
    target_value <- input_values()$target_value
    intersection_year <- NA
    
    results <- tibble(
      year = year(start_date) + 0:(investment_period - 1),
      capital_start = NA_real_,
      savings_anount = NA_real_,
      interest = NA_real_,
      accumulated_capital = NA_real_,
      growth = NA_real_,
      savings_anount_normalized = NA_real_,
      interest_normalized = NA_real_,
      capital_end = NA_real_
    )
    
    accumulated_savings_anount <- 0
    
    for (i in 1:investment_period) {
      if (i == 1) {
        capital_start <- initial_capital
      } else {
        capital_start <- results$capital_end[i - 1]
      }
      
      if (!is.na(savings_suspension) && i > savings_suspension && savings_suspension < investment_period) {
        savings_anount <- 0
      } else {
        if (savings_intervall == "monthly") {
          savings_anount <- savings_rate * (1 + adjustment_rate)^(i - 1) * 12
        } else {
          savings_anount <- savings_rate * (1 + adjustment_rate)^(i - 1)
        }
      }
      
      if (savings_intervall == "monthly") {
        monthlyer_interest_rate <- 100*((1 + interest_rate / 100)^(1/12) - 1)
        monthly_savings <- savings_anount / 12
        total_interest <- 0
        current_capital <- capital_start
        
        for (m in 1:12) {
          monthly_interest = current_capital * monthlyer_interest_rate
          total_interest <- total_interest + monthly_interest
          current_capital <- current_capital + monthly_interest + monthly_savings
        }
        
        interest <- total_interest
      } else {
        interest <- capital_start * interest_rate
      }
      
      accumulated_savings_anount <- accumulated_savings_anount + savings_anount
      accumulated_capital <- initial_capital + accumulated_savings_anount
      growth <- savings_anount + interest
      savings_anount_normalized <- 100 * savings_anount / growth
      interest_normalized <- 100 * interest / growth
      capital_end <- capital_start + growth
      
      results$capital_start[i] <- round(capital_start, 2)
      results$savings_anount[i] <- round(savings_anount, 2)
      results$interest[i] <- round(interest, 2)
      results$accumulated_capital[i] <- round(accumulated_capital, 2)
      results$growth[i] <- round(growth, 2)
      results$savings_anount_normalized[i] <- round(savings_anount_normalized, 2)
      results$interest_normalized[i] <- round(interest_normalized, 2)
      results$capital_end[i] <- round(capital_end, 2)
    }
    
    # add sum row
    complete_data <- results
    complete_data <- rbind(complete_data, c('Sum', NA, round(sum(complete_data$savings_anount), 2), round(sum(complete_data$interest), 2), NA, NA, NA, NA, round(complete_data$capital_end[investment_period], 2)))
    
    # Calculate data for the pie chart
    total_savings_anount <- sum(results$savings_anount) + initial_capital
    total_interest <- sum(results$interest)
    total_capital <- total_savings_anount + total_interest
    
    # 1. Find the Intersection Point
    for (i in 1:(nrow(results) - 1)) {
      if (results$savings_anount[i] > results$interest[i] && results$savings_anount[i+1] <= results$interest[i+1]) {
        # Interpolate the exact year of intersection
        x1 <- results$year[i]
        x2 <- results$year[i+1]
        y1_savings <- results$savings_anount[i]
        y2_savings <- results$savings_anount[i+1]
        y1_interest <- results$interest[i]
        y2_interest <- results$interest[i+1]
        
        intersection_year <- x1 + (x2 - x1) * (y1_savings - y1_interest) / ((y2_interest - y1_interest) - (y2_savings - y1_savings))
        break
      }
    }
    
    # 2. Generate the Cross Point String
    if (!is.na(intersection_year)) {
      crosspoint_string <- paste("From", floor(intersection_year), "the growth is mainly driven by the generated interests")
    } else {
      crosspoint_string <- NA
    }
    
    # Check if target_value is not NA
    if (!is.na(target_value)) {
      # Interpolate to find the year when target_value is reached
      interpolated_target <- approx(results$capital_end, results$year, xout = target_value)
      
      # Date when target_value is reached
      year_at_target <- interpolated_target$y 
    }

    output$plot1 <- renderPlot({
      ggplot(results, aes(x = year)) +
        geom_line(aes(y = accumulated_capital, color = "Savings"), size = 1.2) +
        geom_line(aes(y = capital_end, color = "Total Capital"), size = 1.2) +
        labs(title = "Capital growth over time", x = "Year", y = "Amount [ € ]") +
        scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        scale_color_manual(values = c("Savings" = "steelblue", "Total Capital" = "lightgreen"),
                           breaks = c("Total Capital", "Savings")) + # Change order in the legend
        guides(color = guide_legend(title = NULL)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 14), # Change size of axis labeling
          axis.title = element_text(size = 16)  # Change size of axis title
        )
    })
    
    output$plot2 <- renderPlot({
      # Create data for the pie chart
      pie_data <- data.frame(
        Kategorie = c("Total Savings", "Total Interest"),
        Wert = c(total_savings_anount, total_interest)
      )
      
      ggplot(pie_data, aes(x = "", y = Wert, fill = Kategorie)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(round(Wert / total_capital * 100, 1), "%")), 
                  position = position_stack(vjust = 0.5), size = 6) +
        labs(title = "Distribution of savings and interest") +
        scale_fill_manual(values = c("Total Interest" = "lightgreen", "Total Savings" = "steelblue")) +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.title = element_blank(),
          legend.text = element_text(size = 16)
        )
      
    })
    
    output$plot3 <- renderPlot({
      max_savings_anount <- max(results$savings_anount, na.rm = TRUE)
      max_interest <- max(results$interest, na.rm = TRUE)
      y_max <- max(max_savings_anount, max_interest)
      plot_savings <- ggplot(results, aes(x = year)) +
        geom_line(aes(y = savings_anount, color = "Savings Rate"), size = 1.2) +
        geom_line(aes(y = interest, color = "Generated Interests"), size = 1.2) +
        labs(
          title = "Development of the savings rate and generated interest per year",
          x = "Year",
          y = "Amount [ € ]"
        ) +
        scale_color_manual(values = c("Savings Rate" = "steelblue", "Generated Interests" = "lightgreen")) +
        scale_y_continuous(limits = c(0, y_max), labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )
      
      if (!is.na(intersection_year)) {
        plot_savings <- plot_savings +
          geom_vline(aes(xintercept = intersection_year), linetype="dashed", size=1.0, color="orange") +
          annotate("text", x = intersection_year, y = min(results$savings_anount), label = as.character(floor(intersection_year)), size = 5, hjust = 1.2, vjust = 7.8 )
      }
      plot_savings
    })
    
    output$plot4 <- renderPlot({
      plot_normalized <- ggplot(results, aes(x = year)) +
        geom_line(aes(y = savings_anount_normalized, color = "Savings Rate"), size = 1.2) +
        geom_line(aes(y = interest_normalized, color = "Generated Interests"), size = 1.2) +
        labs(
          title = "Normalized savings rate and generated interests per year",
          x = "Year",
          y = "Normalized Value [ % ]"
        ) +
        scale_color_manual(values = c("Savings Rate" = "steelblue", "Generated Interests" = "lightgreen")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )
      
      if (!is.na(intersection_year)) {
        plot_normalized <- plot_normalized +
          geom_vline(aes(xintercept = intersection_year), linetype="dashed", size=1.0, color="orange") +
          annotate("text", x = intersection_year, y = min(results$savings_anount_normalized), label = as.character(floor(intersection_year)), size = 5, hjust = 1.2, vjust = 5.0 )
      }
      plot_normalized
    })
    
    output$plot5 <- renderPlot({
      # Create diagram
      # Determine the years when the capital_end line reaches the threshold values
      thresholds <- seq(100000, 1000000, by = 100000)
      years_at_threshold <- numeric(length(thresholds))
      
      for (i in 1:length(thresholds)) {
        interpolated <- approx(results$capital_end, results$year, xout = thresholds[i])
        years_at_threshold[i] <- interpolated$y
      }
      
      if (!all(is.na(years_at_threshold))) {
        # Create a dataframe for the segments
        segment_data <- data.frame(
          x = rep(min(results$year), length(thresholds)),
          xend = years_at_threshold,
          y = thresholds,
          yend = thresholds
        )
        
        segment_data <- na.omit(segment_data)
        
        # Calculate difference between the years
        segment_data$diff <- c(NA, diff(segment_data$xend))
        
        # Calculate period until reaching the first 100,000 Euros
        time_to_first_threshold <- segment_data$xend[1] - segment_data$x[1]
        
        # Create diagram
        ggplot(results, aes(x = year, y = capital_end)) +
          geom_line(color = "lightgreen", size= 1.2) +
          geom_segment(data = segment_data, aes(x = xend, xend = xend, y = 0, yend = y), linetype = "dashed", color = "orange") +
          geom_text(data = segment_data[1, ], aes(x = xend, y = max(results$capital_end) * 0.05, label = sprintf("%.1f", time_to_first_threshold)), check_overlap = TRUE, size = 5, hjust = 1.2, vjust = 0.6) +  # Add text for the first threshold value
          geom_text(data = segment_data[-1, ], aes(x = xend, y = max(results$capital_end) * 0.05, label = sprintf("%.1f", diff)), check_overlap = TRUE, size = 5, hjust = 1.2, vjust = 0.6) +  # Add text for the other threshold values
          labs(title = "Development of total capital and consideration of characteristic anchor points", x = "Year", y = "Value [ € ]") +
          scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.title.position = "plot",
            legend.text = element_text(size = 16),
            legend.title = element_blank(),  # Hide the title of the legend
            axis.text = element_text(size = 14), # Change size of axis labeling
            axis.title = element_text(size = 16)  # Change size of axis title
            )  
      }
    })
    
    output$table1 <- renderTable({
      table_summary <- data.frame(
        Category = c("Total Capital [ € ]", "Total Savings [ € ]", "Total Interests [ € ]"),
        Value = c(format(round(total_capital), big.mark = ".", decimal.mark = ",", scientific = FALSE),
                  format(round(total_savings_anount), big.mark = ".", decimal.mark = ",", scientific = FALSE),
                  format(round(total_interest), big.mark = ".", decimal.mark = ",", scientific = FALSE))
      )
      table_summary
    }, colnames = FALSE)
    
    output$plot6 <- renderPlot({
      # Calculate dynamic target values based on the initial capital and capital_end values
      doubling_factor <- 2
      max_target_value <- max(results$capital_end)  # You can adjust this as needed
      
      # Define a function to calculate the year when capital doubles based on a target value
      calculateDoublingYear <- function(target_value, results) {
        interpolated_target <- approx(results$capital_end, results$year, xout = target_value)
        return(interpolated_target$y)
      }
      
      double_target <- numeric()
      current_target <- initial_capital
      
      while (current_target <= max_target_value) {
        # Find the next target by doubling the current one
        next_target <- current_target * doubling_factor
        
        # Check if the next target is achievable based on capital_end
        if (next_target <= max(results$capital_end)) {
          double_target <- c(double_target, next_target)
          current_target <- next_target
        } else {
          break  # Stop if doubling is no longer possible
        }
      }
      
      if (length(double_target) != 0) {
        # double_target is not empty, calculate values and plot diagram
        # Calculate the years when capital doubles for each dynamically calculated target value
        doubling_years <- sapply(double_target, function(target) {
          calculateDoublingYear(target, results)
        })
        
        # Create a data frame for labeling doubling years
        label_data <- data.frame(year = doubling_years, label = floor(doubling_years))
        
        # Create the original plot
        ggplot(results, aes(x = year, y = capital_end)) +
          geom_line(color = "lightgreen", size = 1.2) +
          geom_vline(xintercept = doubling_years, linetype = "dashed", color = "orange", size = 1) +
          geom_text(data = label_data, aes(x = year, y = max(results$capital_end) * 0.05, label = label), size = 5, color = "black", hjust = 1.1) +  # Add text labels for doubling years
          labs(title = "Capital Doubling from Initial Investment", x = "Year", y = "Value [ € ]") +
          scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.title.position = "plot",
            legend.text = element_text(size = 16),
            legend.title = element_blank(),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16)
          )
      } 
    })
    
    output$plot7 <- renderPlot({
      # Check if target_value is not NA
      if (!is.na(target_value)) {
        if (!is.na(year_at_target)) {
        
        ggplot(results, aes(x = year, y = capital_end)) +
          geom_line(color = "lightgreen", size= 1.2) +
          geom_hline(yintercept = target_value, linetype = "dashed", color = "orange", size = 1) +
          geom_vline(xintercept = year_at_target, linetype = "dashed", color = "orange", size = 1) +
          geom_text(aes(x = year_at_target, y = 0, label = as.character(floor(year_at_target))), size = 5, hjust = 1.2, vjust = 0.6, color = "black") +
          labs(title = "Total capital and point in time of target value", x = "Year", y = "Value [ € ]") +
          scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.title.position = "plot",
            legend.text = element_text(size = 16),
            legend.title = element_blank(),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16)
          )
        }
      }
    })
    
    output$crosspoint_message <- renderText({
      if (!is.na(crosspoint_string)) {
        crosspoint_string
      }
    })
    
    output$crosspoint_message_2 <- renderText({
      if (!is.na(crosspoint_string)) {
        crosspoint_string
      }
    })
    
    output$target_message <- renderText({
      if (!is.na(target_value)) {
        if (!is.na(year_at_target)) {
          message <- sprintf("The target value of %s € will likely be achieved by %d.", format(target_value, big.mark = ".", decimal.mark = ",", scientific = FALSE), floor(year_at_target))
        } else {
          message <- sprintf("The target value of %s € will not be achieved within the chosen observation period.", format(target_value, big.mark = ".", decimal.mark = ",", scientific = FALSE))
        }
      }
    })
    
    output$table2 <- renderTable({
      table_all_values <- complete_data %>%
        select(year, capital_start, savings_anount, interest, capital_end) %>%
        mutate(
          `Capital Beginning of the Year [€]` = format(as.numeric(capital_start), nsmall = 2, big.mark = '.', decimal.mark = ','),
          `Savings Amount per Year [€]` = format(as.numeric(savings_anount), nsmall = 2, big.mark = '.', decimal.mark = ','),
          `Generated Interest per Year [€]` = format(as.numeric(interest), nsmall = 2, big.mark = '.', decimal.mark = ','),
          `Capital at the end of the Year [€]` = format(as.numeric(capital_end), nsmall = 2, big.mark = '.', decimal.mark = ',')
        ) %>%
        select(year, `Capital Beginning of the Year [€]`, `Savings Amount per Year [€]`, `Generated Interest per Year [€]`, `Capital at the end of the Year [€]`)
      
      table_all_values$`Capital Beginning of the Year [€]`[nrow(table_all_values)] <- ""
      
      table_all_values
    })
    
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
