# Bibliotheken laden
library(lubridate)
library(tibble)
library(ggplot2)

# Eingangsparameter
start_datum <- ymd("2024-01-01")
anfangskapital <- 20000
sparrate <- 1000
sparintervall <- "monatlich"  # oder "jährlich"
dynamik <- 0.05  # 5% als Dezimalzahl
zinssatz <- 0.125  # 3% als Dezimalzahl
Dauer <- 20  # in Jahren
Aussetzen_des_Sparbetrags <- NA  # Optionaler Wert
Zielwert <- NA  # Optionaler Wert

# Tabelle erstellen
ergebnisse <- tibble(
  Jahr = year(start_datum) + 0:(Dauer - 1),
  Kapital_Beginn = NA_real_,
  Sparbetrag = NA_real_,
  Zinsen = NA_real_,
  Angespartes_Kapital = NA_real_,
  Wachstum = NA_real_,
  Sparbetrag_normiert = NA_real_,
  Zinsen_normiert = NA_real_,
  Kapital_Ende = NA_real_
)

kumulierter_sparbetrag <- 0

for (i in 1:Dauer) {
  if (i == 1) {
    kapital_beginn <- anfangskapital
  } else {
    kapital_beginn <- ergebnisse$Kapital_Ende[i - 1]
  }
  
  if (!is.na(Aussetzen_des_Sparbetrags) && i > Aussetzen_des_Sparbetrags && Aussetzen_des_Sparbetrags < Dauer) {
    sparbetrag <- 0
  } else {
    if (sparintervall == "monatlich") {
      sparbetrag <- sparrate * (1 + dynamik)^(i - 1) * 12
    } else {
      sparbetrag <- sparrate * (1 + dynamik)^(i - 1)
    }
  }
  
  if (sparintervall == "monatlich") {
    monatlicher_zinssatz <- zinssatz / 12
    zinsen <- sum(sapply(1:12, function(m) (kapital_beginn + (m - 1) * sparbetrag * (1 + dynamik)^(i - 1)) * monatlicher_zinssatz))
  } else {
    zinsen <- kapital_beginn * zinssatz
  }
  
  kumulierter_sparbetrag <- kumulierter_sparbetrag + sparbetrag
  angespartes_kapital <- anfangskapital + kumulierter_sparbetrag
  wachstum <- sparbetrag + zinsen
  sparbetrag_normiert <- sparbetrag / wachstum
  zinsen_normiert <- zinsen / wachstum
  kapital_ende <- kapital_beginn + wachstum
  
  ergebnisse$Kapital_Beginn[i] <- round(kapital_beginn, 2)
  ergebnisse$Sparbetrag[i] <- round(sparbetrag, 2)
  ergebnisse$Zinsen[i] <- round(zinsen, 2)
  ergebnisse$Angespartes_Kapital[i] <- round(angespartes_kapital, 2)
  ergebnisse$Wachstum[i] <- round(wachstum, 2)
  ergebnisse$Sparbetrag_normiert[i] <- round(sparbetrag_normiert, 2)
  ergebnisse$Zinsen_normiert[i] <- round(zinsen_normiert, 2)
  ergebnisse$Kapital_Ende[i] <- round(kapital_ende, 2)
}

# Summenzeile hinzufügen
ergebnisse <- rbind(ergebnisse, c(NA, NA, round(sum(ergebnisse$Sparbetrag), 2), round(sum(ergebnisse$Zinsen), 2), NA, NA, NA, NA, round(ergebnisse$Kapital_Ende[Dauer], 2)))

# Diagramm erstellen
ggplot(ergebnisse, aes(x = Jahr)) +
  geom_line(aes(y = Kapital_Ende, color = "Gesamtkapital")) +
  geom_line(aes(y = Angespartes_Kapital, color = "Angespartes Geld")) +
  labs(title = "Entwicklung des Kapitals über die Zeit", x = "Jahr", y = "Betrag in €", color = "Legende") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

# Daten für den Pie-Chart berechnen
gesamter_sparbetrag <- sum(ergebnisse$Sparbetrag) + anfangskapital
gesamte_zinsen <- sum(ergebnisse$Zinsen)
gesamt <- gesamter_sparbetrag + gesamte_zinsen

# Daten für den Pie-Chart erstellen
pie_data <- data.frame(
  Kategorie = c("Gesamter Sparbetrag", "Gesamte Zinsen"),
  Wert = c(gesamter_sparbetrag, gesamte_zinsen)
)

# Pie-Chart erstellen
ggplot(pie_data, aes(x = "", y = Wert, fill = Kategorie)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Wert / gesamt * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Verteilung von Sparbeträgen und Zinsen", fill = "Kategorie") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

# Diagramm erstellen
# Höchsten Werte für Sparbetrag und Zinsen ermitteln
max_sparbetrag <- max(ergebnisse$Sparbetrag[-nrow(ergebnisse)], na.rm = TRUE)
max_zinsen <- max(ergebnisse$Zinsen[-nrow(ergebnisse)], na.rm = TRUE)
y_max <- max(max_sparbetrag, max_zinsen)

# Diagramm erstellen
ggplot(ergebnisse, aes(x = Jahr)) +
  geom_line(aes(y = Sparbetrag, color = "Sparbetrag"), size = 1) +
  geom_line(aes(y = Zinsen, color = "Zinsen"), size = 1) +
  labs(
    title = "Entwicklung von Sparbeträgen und Zinsen über die Jahre",
    x = "Jahr",
    y = "Betrag in €",
    color = "Kategorie"
  ) +
  scale_y_continuous(limits = c(0, y_max), labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

# Diagramm erstellen
ggplot(ergebnisse, aes(x = Jahr)) +
  geom_line(aes(y = Sparbetrag_normiert, color = "Sparbetrag_normiert"), size = 1) +
  geom_line(aes(y = Zinsen_normiert, color = "Zinsen_normiert"), size = 1) +
  labs(
    title = "Entwicklung von normierten Sparbeträgen und Zinsen über die Jahre",
    x = "Jahr",
    y = "Normierter Wert",
    color = "Kategorie"
  ) +
  theme_minimal()

# Diagramm erstellen
# Bestimmen Sie die Jahre, in denen die Linie Kapital_Ende die Schwellenwerte erreicht
thresholds <- seq(100000, 1000000, by = 100000)
years_at_threshold <- numeric(length(thresholds))

for (i in 1:length(thresholds)) {
  years_at_threshold[i] <- min(ergebnisse$Jahr[ergebnisse$Kapital_Ende >= thresholds[i]], na.rm = TRUE)
}

# Erstellen Sie ein Dataframe für die Segmente
segment_data <- data.frame(
  x = rep(min(ergebnisse$Jahr), length(thresholds)),
  xend = years_at_threshold,
  y = thresholds,
  yend = thresholds
)

# Diagramm erstellen
ggplot(ergebnisse, aes(x = Jahr, y = Kapital_Ende)) +
  geom_line(color = "blue") +
  geom_segment(data = segment_data, aes(x = x, xend = xend, y = y, yend = yend), linetype = "dashed", color = "red") +
  geom_segment(data = segment_data, aes(x = xend, xend = xend, y = 0, yend = y), linetype = "dashed", color = "red") +
  labs(title = "Entwicklung des Kapitals über die Zeit", x = "Jahr", y = "Betrag in €") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()