library(highcharter)
library(tidyverse)
library(svVis)
 
.libPaths()
# Funktion, die ein Highcharts Balkendiagramm für Kursformate erstellt
plot_course_formats <- function(data_frame) {

  # Initialisiere das Highchart-Objekt
  hchart <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Kursformate 2022") %>%
    hc_xAxis(
      categories = data_frame$name,
      labels = list(enabled = FALSE),
      title = list(text = "", style = list(fontWeight = "bold"))
    ) %>%
    hc_yAxis(title = list(text = "Anzahl", style = list(fontWeight = "bold"))) %>%
    hc_plotOptions(
      column = list(
        pointPadding = 0.2,
        groupPadding = 0.1
      )
    ) %>%
    hc_legend(enabled = TRUE)
  
  # Füge jede Serie basierend auf den Kategorien hinzu
  for (i in seq_along(data_frame$name)) {
    hchart <- hchart %>%
      hc_add_series(
        name = data_frame$name[i],
        data = list(list(y = data_frame$value[i]))
      )
  }
  
  # Füge Tooltip hinzu
  hchart %>%
    hc_tooltip(
      headerFormat = '',
      pointFormatter = JS(
        "function() { 
           return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; 
         }"
      )
    )
}

# Beispiel Nutzung
df <- data.frame(
  name = c("Seminar", "Vorlesung", "Sonstiges", "Übung", "Erfahrung", "Austausch", "Sprachkurs"),
  value = c(24085, 11305, 11005, 10199, 3897, 3098, 1570)
)

plot_course_formats(df)
