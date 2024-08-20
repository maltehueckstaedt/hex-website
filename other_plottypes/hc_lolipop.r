# Notwendige Pakete laden
library(tidyr)
library(dplyr)
library(highcharter)
library(htmlwidgets)
library(htmltools)

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

# Lollipop Plots für jede Hochschule erstellen
plots <- data_long %>%
  group_by(rowname) %>%
  group_map(~{
    hchart(.x, "lollipop", hcaes(x = Fachbereich, y = Wert)) %>%
      hc_title(text = .y$rowname) %>%
      hc_yAxis(title = list(text = "Prozent"), min = 0, max = 10) %>%
      hc_xAxis(title = list(text = "Fachbereich"))
  })

# Gridview erstellen
grid <- hw_grid(plots, ncol = 2, rowheight = 300)

# HTML-Inhalte erzeugen
# html_content <- tagList(grid)

# # HTML-Inhalte in eine HTML-Datei speichern
# save_html(html_content, file = "plots.html")
