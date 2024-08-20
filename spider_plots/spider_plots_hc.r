# Notwendige Pakete laden
library(tidyr)
library(dplyr)
library(highcharter)
library(htmlwidgets)

# Originaldaten
data <- data.frame(
  rowname = c("Technische Universität Darmstadt", "Universität Hamburg", "Universität Stuttgart"),
  Robotik = c(3.0662983, 2.8363047, 0.6152945),
  Kryptologie = c(0.2486188, 0.0000000, 0.7617931),
  Bildverarbeitung = c(1.381215, 1.620746, 2.549077),
  Bioinformatik = c(0.3314917, 8.2658023, 0.3515968),
  Echtzeitsysteme = c(0.0000000, 0.0000000, 0.5273953)
)

# Datensatz in langer Form umwandeln
data_long <- data %>%
  pivot_longer(cols = -rowname, names_to = "Fachbereich", values_to = "Wert")

# Daten für Spiderplot vorbereiten
data_wide <- data_long %>%
  pivot_wider(names_from = Fachbereich, values_from = Wert) %>%
  select(rowname, Robotik, Kryptologie, Bildverarbeitung, Bioinformatik, Echtzeitsysteme)

# Spiderplots für jede Hochschule erstellen
plots <- data_wide %>%
  group_by(rowname) %>%
  group_map(~{
    highchart() %>%
      hc_chart(polar = TRUE, type = "line") %>%
      hc_title(text = .y$rowname) %>%
      hc_xAxis(categories = names(.x)[-1], tickmarkPlacement = 'on', lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, 
      #min = 0, max = 8, 
      tickInterval = 1, title = list(text = "Prozent")) %>%
      hc_series(
        list(
          name = .y$rowname,
          data = as.numeric(.x[1, -1]),
          pointPlacement = 'on'
        )
      )
  }, .keep = TRUE)

# Gridview erstellen
grid <- hw_grid(plots, ncol = 2, rowheight = 300)

# Gridview anzeigen
grid
