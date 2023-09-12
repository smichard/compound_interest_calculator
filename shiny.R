# Bibliotheken laden
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(shiny)
library(tibble)

# UI definieren
ui <- fluidPage(
  titlePanel("Capital building calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("start_date", "Start Year:", 
                  choices = 2010:2030, selected = 2020),
      numericInput("initial_capital", "Initial Capital:", value = 1000),
      numericInput("savings_rate", "Savings Rate:", value = 100),
      selectInput("savings_intervall", "Savings Intervall:", 
                  choices = c("monthly", "yearly")),
      numericInput("investment_period", "Investment Perios (in Years):", value = 20),
      sliderInput("interest_rate", "Interest Rate [ % ]:", 
                  min = 0, max = 50, value = 7.5, step = 0.5),
      sliderInput("adjustment_rate", "Adjustment Rate [ % ]:", 
                  min = 0, max = 20, value = 0, step = 0.5),
      numericInput("savings_suspension", 
                   "Savings Suspension after x years:", value = NA, min = 1),
      numericInput("target_value", "Target Value:", value = NA),
      actionButton("goButton", "Berechnen")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gesamtkapital", plotOutput("plot1")),
        tabPanel("Verteilung", plotOutput("plot2"), tableOutput("table1")),
        tabPanel("Sparbeträge und Zinsen", plotOutput("plot3")),
        tabPanel("Normierte Werte", plotOutput("plot4")),
        tabPanel("Kapitalentwicklung", plotOutput("plot5")),
        tabPanel("Tabelle", tableOutput("table2"))
      )
    )
  )
)

# Server definieren
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
        monthlyer_interest_rate <- interest_rate / 12
        interest <- sum(sapply(1:12, function(m) (capital_start + (m - 1) * savings_anount * (1 + adjustment_rate)^(i - 1)) * monthlyer_interest_rate))
      } else {
        interest <- capital_start * interest_rate
      }
      
      accumulated_savings_anount <- accumulated_savings_anount + savings_anount
      accumulated_capital <- initial_capital + accumulated_savings_anount
      growth <- savings_anount + interest
      savings_anount_normalized <- savings_anount / growth
      interest_normalized <- interest / growth
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
    
    # Summenzeile hinzufügen, TO DO: Vergleich Results und complete data
    complete_data <- results
    complete_data <- rbind(complete_data, c('Sum', NA, round(sum(complete_data$savings_anount), 2), round(sum(complete_data$interest), 2), NA, NA, NA, NA, round(complete_data$capital_end[investment_period], 2)))
    #results <- rbind(results, c(NA, NA, round(sum(results$savings_anount), 2), round(sum(results$interest), 2), NA, NA, NA, NA, round(results$capital_end[investment_period], 2)))
    
    

    output$plot1 <- renderPlot({
      ggplot(results, aes(x = year)) +
        geom_line(aes(y = accumulated_capital, color = "Savings"), size = 1.2) +
        geom_line(aes(y = capital_end, color = "Total Capital"), size = 1.2) +
        labs(title = "Capital growth over time", x = "Year", y = "Amount [ € ]") +
        scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        scale_color_manual(values = c("Savings" = "steelblue", "Total Capital" = "lightgreen"),
                           breaks = c("Total Capital", "Savings")) + # Reihenfolge in der Legende ändern
        guides(color = guide_legend(title = NULL)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 14), # Größe der Achsenbeschriftung ändern
          axis.title = element_text(size = 16)  # Größe des Achsentitels ändern
        )
    })
    
    output$plot2 <- renderPlot({
      # Daten für den Pie-Chart berechnen
      total_savings_anount <- sum(results$savings_anount) + initial_capital
      total_interest <- sum(results$interest)
      total_capital <- total_savings_anount + total_interest
      
      # Daten für den Pie-Chart erstellen
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
      # Diagramm erstellen
      # Höchsten Werte für savings_anount und interest ermitteln
      max_savings_anount <- max(results$savings_anount, na.rm = TRUE)
      max_interest <- max(results$interest, na.rm = TRUE)
      y_max <- max(max_savings_anount, max_interest)
      
      # Diagramm erstellen
      ggplot(results, aes(x = year)) +
        geom_line(aes(y = savings_anount, color = "Savings Rate"), size = 1.2) +
        geom_line(aes(y = interest, color = "Interest Rate"), size = 1.2) +
        labs(
          title = "Development of the savings rate and interest rate over the years",
          x = "Year",
          y = "Amount [ € ]"
        ) +
        scale_color_manual(values = c("Savings Rate" = "steelblue", "Interest Rate" = "lightgreen")) +
        scale_y_continuous(limits = c(0, y_max), labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          legend.title = element_blank(),  # Titel der Legende ausblenden
          axis.text = element_text(size = 14), # Größe der Achsenbeschriftung ändern
          axis.title = element_text(size = 16)  # Größe des Achsentitels ändern
        )
      
    })
    
    output$plot4 <- renderPlot({
      # Diagramm erstellen
      ggplot(results, aes(x = year)) +
        geom_line(aes(y = savings_anount_normalized, color = "Savings Rate"), size = 1.2) +
        geom_line(aes(y = interest_normalized, color = "Interest Rate"), size = 1.2) +
        labs(
          title = "Normalized savings rate and interest rate over the years",
          x = "Year",
          y = "Normalized Value [ % ]"
        ) +
        scale_color_manual(values = c("Savings Rate" = "steelblue", "Interest Rate" = "lightgreen")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.title.position = "plot",
          legend.text = element_text(size = 16),
          legend.title = element_blank(),  # Titel der Legende ausblenden
          axis.text = element_text(size = 14), # Größe der Achsenbeschriftung ändern
          axis.title = element_text(size = 16)  # Größe des Achsentitels ändern
        )
      
    })
    
    output$plot5 <- renderPlot({

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
      # Umbenennung der Spalten
      #colnames(table_render) <- c("Year", "Capital Beginning of the Year [€]", "Savings Amount per Year [€]", "Generated Interest per Year [€]", "Capital at the end of the Year [€]")
      
      table_all_values
    })
    
  })
}

# Shiny-App ausführen
shinyApp(ui = ui, server = server)
