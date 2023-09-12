# Load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tibble)

# Input parameters
start_date <- ymd("2024-01-01")
initial_capital <- 5000
savings_rate <- 100
savings_intervall <- "monthly"  # oder "yearly"
adjustment_rate <- 0  # 5% als Dezimalzahl
interest_rate <- 0.1  # 3% als Dezimalzahl
investment_period <- 20  # in yearen
savings_suspension <- NA  # Optionaler Wert
target_value <- NA  # Optionaler Wert

# Create table
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

# Diagramm erstellen
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

table_all_values <- complete_data %>%
  select(year, capital_start, savings_anount, interest, capital_end) %>%
  mutate(
    `Capital Beginning of the Year [€]` = format(as.numeric(capital_start), nsmall = 2, big.mark = '.', decimal.mark = ','),
    `Savings Amount per Year [€]` = format(as.numeric(savings_anount), nsmall = 2, big.mark = '.', decimal.mark = ','),
    `Generated Interest per Year [€]` = format(as.numeric(interest), nsmall = 2, big.mark = '.', decimal.mark = ','),
    `Capital at the end of the Year [€]` = format(as.numeric(capital_end), nsmall = 2, big.mark = '.', decimal.mark = ',')
  ) %>%
  select(year, `Capital Beginning of the Year [€]`, `Savings Amount per Year [€]`, `Generated Interest per Year [€]`, `Capital at the end of the Year [€]`)

# Umbenennung der Spalten
table_all_values$`Capital Beginning of the Year [€]`[nrow(table_all_values)] <- ""
#colnames(table_render) <- c("Year", "Capital Beginning of the Year [€]", "Savings Amount per Year [€]", "Generated Interest per Year [€]", "Capital at the end of the Year [€]")

table_all_values


# Summary Table
table_summary <- data.frame(
  Category = c("Total Capital [ € ]", "Total Savings [ € ]", "Total Interests [ € ]"),
  Value = c(format(round(total_capital), big.mark = ".", decimal.mark = ",", scientific = FALSE),
            format(round(total_savings_anount), big.mark = ".", decimal.mark = ",", scientific = FALSE),
            format(round(total_interest), big.mark = ".", decimal.mark = ",", scientific = FALSE))
)

# Ausgabe der Tabelle ohne Spaltenüberschriften und ohne NULL
invisible(apply(table_summary, 1, function(row) cat(paste(row, collapse = "\t"), "\n")))

# Diagramm erstellen
# Bestimmen Sie die yeare, in denen die Linie capital_end die Schwellenwerte erreicht
thresholds <- seq(100000, 1000000, by = 100000)
years_at_threshold <- numeric(length(thresholds))

for (i in 1:length(thresholds)) {
  years_at_threshold[i] <- min(results$year[results$capital_end >= thresholds[i]], na.rm = TRUE)
}

# Erstellen Sie ein Dataframe für die Segmente
segment_data <- data.frame(
  x = rep(min(results$year), length(thresholds)),
  xend = years_at_threshold,
  y = thresholds,
  yend = thresholds
)

# Diagramm erstellen, TO DO: fertigstellen
ggplot(results, aes(x = year, y = capital_end)) +
  geom_line(color = "blue") +
  geom_segment(data = segment_data, aes(x = x, xend = xend, y = y, yend = yend), linetype = "dashed", color = "red") +
  geom_segment(data = segment_data, aes(x = xend, xend = xend, y = 0, yend = y), linetype = "dashed", color = "red") +
  labs(title = "Entwicklung des Kapitals über die Zeit", x = "year", y = "Betrag in €") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.title.position = "plot",
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 14), # Größe der Achsenbeschriftung ändern
    axis.title = element_text(size = 16)  # Größe des Achsentitels ändern
  )

