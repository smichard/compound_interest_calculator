# Bibliotheken laden
library(lubridate)
library(tibble)
library(ggplot2)

# Eingangsparameter
start_datum <- ymd("2023-01-01")
anfangskapital <- 1000
sparrate <- 100
sparintervall <- "monatlich"  # oder "jährlich"
dynamik <- 0  # 5% als Dezimalzahl
zinssatz <- 0.15  # 3% als Dezimalzahl
ansparzeit <- 20  # in Jahren

# Tabelle erstellen
ergebnisse <- tibble(
  Jahr = year(start_datum) + 0:(ansparzeit - 1),
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

for (i in 1:ansparzeit) {
  if (i == 1) {
    kapital_beginn <- anfangskapital
  } else {
    kapital_beginn <- ergebnisse$Kapital_Ende[i - 1]
  }
  
  if (sparintervall == "monatlich") {
    sparbetrag <- sparrate * (1 + dynamik)^(i - 1) * 12
    monatlicher_zinssatz <- zinssatz / 12
    zinsen <- sum(sapply(1:12, function(m) (kapital_beginn + (m - 1) * sparrate * (1 + dynamik)^(i - 1)) * monatlicher_zinssatz))
  } else {
    sparbetrag <- sparrate * (1 + dynamik)^(i - 1)
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
ergebnisse <- rbind(ergebnisse, c(NA, NA, round(sum(ergebnisse$Sparbetrag), 2), round(sum(ergebnisse$Zinsen), 2), NA, NA, NA, NA, round(ergebnisse$Kapital_Ende[ansparzeit], 2)))

# Diagramm erstellen
ggplot(ergebnisse, aes(x = Jahr)) +
  geom_line(aes(y = Kapital_Ende, color = "Gesamtkapital")) +
  geom_line(aes(y = Angespartes_Kapital, color = "Angespartes Geld")) +
  labs(title = "Entwicklung des Kapitals über die Zeit", x = "Jahr", y = "Betrag in €", color = "Legende") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

