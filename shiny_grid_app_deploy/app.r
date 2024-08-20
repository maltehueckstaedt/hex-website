#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Load Packages -----------------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(shiny)
library(highcharter)
library(xts)
library(htmlwidgets)
library(svVis)

# Filter Bar Function
hc_filter_bar <- function(data_frame, theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL, 
tooltip_suffix = " Kurse", invisible_categories = c()) {
  
  selected_theme <- svVis:::manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)
  
  # Initialisiere das Highchart-Objekt
  hchart <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_colors(colors = selected_theme$color) |> 
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
  
  # Erstelle eine Hilfsvariable zur Verwaltung der Sichtbarkeit
  visibility_status <- !(data_frame$name %in% invisible_categories)
  
  # Füge jede Serie basierend auf den Kategorien hinzu
  for (i in seq_along(data_frame$name)) {
    hchart <- hchart %>%
      hc_add_series(
        name = data_frame$name[i],
        data = list(list(y = data_frame$value[i])),
        visible = visibility_status[i]
      )
  }
  
  # Füge Tooltip hinzu
  hchart %>%
    hc_tooltip(
      headerFormat = '',
      pointFormatter = JS(
        sprintf("function() { 
           return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + '%s'; 
         }", tooltip_suffix)
      )
    )
}


create_lineplot_interactive_highcharter <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    group_color = NULL, group_linetype = NULL, group_shape = NULL,
    highlight_x_axis = FALSE, highlight_y_axis = FALSE,
    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
    xlabel_text = "", ylabel_text = "",
    legend_title = "",
    invisible_categories = c()) {

  test_group_color <-  !is.null(rlang::quo_get_expr(rlang::enquo(group_color)))
  test_group_shape <-  !is.null(rlang::quo_get_expr(rlang::enquo(group_shape)))
  test_group_linetype <-  !is.null(rlang::quo_get_expr(rlang::enquo(group_linetype)))

  df$group_color_var <- if (test_group_color) df[[rlang::as_name(rlang::enquo(group_color))]] else {""}  
  df$group_shape_var <- if (test_group_shape) df[[rlang::as_name(rlang::enquo(group_shape))]] else {""} 
  df$group_linetype_var <- if (test_group_linetype) df[[rlang::as_name(rlang::enquo(group_linetype))]] else {""}

  x_var <- ensym(x_var)
  y_var <- ensym(y_var)

  # Themes

  selected_theme <- svVis:::manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  selected_theme_hc <- hc_theme(
    chart = list(
      backgroundColor = "#ffffff",
      style = list(
        fontFamily = selected_theme$theme$text$family
      )
    )
  )

  hc <- highchart() %>%
    hc_add_theme(selected_theme_hc) %>%
    hc_chart(type = "line") %>%
    hc_title(text = plot_title) %>%
    hc_xAxis(title = list(text = xlabel_text)) %>%
    hc_yAxis(title = list(text = ylabel_text)) %>%
    hc_legend(title = list(text = legend_title)) %>%
    hc_credits(enabled = TRUE, text = if(is.na(custom_caption)) paste0("Quelle: ", source, ".") else custom_caption)

  # Serien hinzufügen und Sichtbarkeit basierend auf `invisible_categories` steuern
  if (test_group_color) {
    unique_groups <- unique(df$group_color_var)
    
    for (group in unique_groups) {
      group_data <- df[df$group_color_var == group,]
      hc <- hc %>%
        hc_add_series(
          name = group,
          data = list_parse2(group_data %>% transmute(x = !!x_var, y = !!y_var)),
          visible = !(group %in% invisible_categories),
          marker = list(symbol = "circle")
        )
    }
  }

  # Weitere Konfiguration für Linien- und Formgruppierungen

  if (test_group_linetype & !test_group_color) {
    custom_dashes <- c('Dash', 'DashDot', 'Dot', 'LongDash', 'LongDashDot', 'LongDashDotDot', 'ShortDash', 'ShortDashDot', 'ShortDashDotDot', 'ShortDot', 'Solid')
    custom_dashes <- custom_dashes[1:length(unique(df$group_linetype_var))]
    hc <- hc %>%
      hc_add_series(
        name = "group_linetype_var",
        data = list_parse2(df %>% transmute(x = !!x_var, y = !!y_var)),
        dashStyle = custom_dashes,
        visible = TRUE,  # Da `invisible_categories` auf `group_color` abzielt, wird hier `visible` auf TRUE gesetzt
        marker = list(symbol = "circle")
      )
  }

  if (test_group_shape & !test_group_color & !test_group_linetype) {
    hc <- hc %>%
      hc_add_series(
        name = "group_shape_var",
        data = list_parse2(df %>% transmute(x = !!x_var, y = !!y_var)),
        visible = TRUE,  # Da `invisible_categories` auf `group_color` abzielt, wird hier `visible` auf TRUE gesetzt
        marker = list(symbol = "circle")
      )
  }

  # Common settings

  hc <- hc %>%
    hc_add_theme(selected_theme_hc) %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
    hc_xAxis(title = list(text = xlabel_text), gridLineWidth = ifelse(highlight_x_axis, 2, 1)) %>%
    hc_yAxis(title = list(text = ylabel_text), gridLineWidth = ifelse(highlight_y_axis, 2, 1)) %>%
    hc_title(text = plot_title, align = "center") %>%
    hc_subtitle(text = plot_subtitle, align = "center") %>%
    hc_credits(enabled = TRUE, text = if(is.na(custom_caption)) paste0("Quelle: ", source, ".") else custom_caption) %>%
    hc_colors(colors = selected_theme$color) %>%
    hc_add_theme(selected_theme_hc)

  
  # Highlight X/Y

  max_x_value <- max(df[[rlang::as_name(x_var)]])
  max_y_value <- max(df[[rlang::as_name(y_var)]])

  min_x_value <- min(df[[rlang::as_name(x_var)]])
  min_y_value <- min(df[[rlang::as_name(y_var)]])

  if (highlight_x_axis == TRUE & highlight_y_axis == TRUE) {
    
    hc <- hc  %>%
    hc_annotations(list(
      shapes = list(
        list(
          type = 'path',
          strokeWidth = 2,
          stroke = '#000000',
          dashStyle = 'Dash',
          points = list(
            list(xAxis = 0, yAxis = 0, x =  0, y = min_y_value*2),    # Vertikale Linie Starpunkt
            list(xAxis = 0, yAxis = 0, x = 0, y = max_y_value*2)   # Vertikale Linie Endpunkt
          )
        ),
        list(
          type = 'path',
          strokeWidth = 2,
          stroke = '#000000',   
          dashStyle = 'Dash',
          points = list(
            list(xAxis = 0, yAxis = 0, x = min_x_value, y = 0),   # Horizontaler Linie Startpunkt
            list(xAxis = 0, yAxis = 0, x = max_x_value*2, y =0)   # Horizontaler Linie Endpunkt
          )
        )
      )
    ))

  } else if (highlight_x_axis == TRUE) { 

    hc <- hc %>%
    hc_annotations(list(
      shapes = list(
        list(
          type = 'path',
          strokeWidth = 2,
          stroke = '#000000',   
          dashStyle = 'Dash',
          points = list(
            list(xAxis = 0, yAxis = 0, x = min_x_value*2, y = 0),   # Horizontaler Linie Startpunkt
            list(xAxis = 0, yAxis = 0, x = max_x_value*2, y =0)   # Horizontaler Linie Endpunkt
          )
        )
      )
    ))


  } else if (highlight_y_axis == TRUE) {
      
    max_y_value <- max(df[[rlang::as_name(y_var)]])

    hc <- hc %>%
    hc_annotations(list(
      shapes = list(
        list(
          type = 'path',
          strokeWidth = 2,
          stroke = '#000000',
          dashStyle = 'Dash',
          points = list(
            list(xAxis = 0, yAxis = 0, x =  0, y = min_y_value*2),    # Vertikale Linie Startpunkt
            list(xAxis = 0, yAxis = 0, x = 0, y = max_y_value*2)   # Vertikale Linie Endpunkt
          )
        )
      )
    ))
  }

  # LEGEND TITLE

  if (!is.null(legend_title)) {
  hc <- hc %>%
    hc_legend(title = list(text = legend_title))
  }

  if (!is.null(xmin) | !is.null(xmax) | !is.null(ymin) | !is.null(ymax)) {

  warning("Automated zoom function is switched off by limiting the x- or y-axis.",  call. = FALSE)

  # X/Y MIN/MAX
  x_lower <- if (is.null(xmin)) min_x_value else xmin
  x_upper <- if (is.null(xmax)) max_x_value else xmax
  y_lower <- if (is.null(ymin)) min_y_value else ymin
  y_upper <- if (is.null(ymax)) max_y_value else ymax

  hc <- hc %>% 
    hc_xAxis(min = x_lower, max = x_upper) %>%
    hc_yAxis(min = y_lower, max = y_upper)
   }

  return(hc)
}


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Load + Prepare Data -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

setwd("C:/Users/mhu/Documents/gitlab/hex-website/shiny_grid_app_deploy")

df_format_2022 <- read_rds("daten/grunddata_tabelle_format.rds") |> 
  filter(sem_jahr == "10/2022") |>
  group_by(kursformat) |>
  summarise(total_sum = sum(Kurs_format_sum, na.rm = TRUE)) |>
  na.omit() |> 
  rename(name = kursformat, value = total_sum) |>
  arrange(desc(value))

df_format_17_22 <- read_rds("daten/grunddata_tabelle_format.rds") |> 
  filter(sem_jahr %in% c("10/2017", "10/2018",
                         "10/2019", "10/2020",
                         "10/2021", "10/2022")) |>
  group_by(sem_jahr,kursformat) |>
  summarise(total_sum = sum(Kurs_format_sum, na.rm = TRUE))  |>
  na.omit() |>
  rename(name=kursformat,
         value = total_sum) 

df_sprache_2022 <- read_rds("daten/grunddata_tabelle_sprache.rds") |>
  filter(sem_jahr == "10/2022") |>
  group_by(primaersprache) |>
  summarise(total_sum = sum(Kurs_sprache_sum, na.rm = TRUE)) |>
  na.omit() |> 
  rename(name = primaersprache, value = total_sum) |>
  arrange(desc(value)) %>%
  arrange(name == "Sonstiges", name) # Stelle sonstiges ans Ende


df_sprache_17_22 <- read_rds("daten/grunddata_tabelle_sprache.rds") |> 
  filter(sem_jahr %in% c("10/2017", "10/2018",
                         "10/2019", "10/2020",
                         "10/2021", "10/2022"))  %>%
  # mutate(primaersprache = case_when(
  #   primaersprache %in% c("Deutsch", "Englisch") ~ primaersprache,
  #   TRUE ~ "Sonstiges"
  # ))  |> 
  group_by(sem_jahr,primaersprache) |>
  summarise(total_sum = sum(Kurs_sprache_sum, na.rm = TRUE))  |>
  na.omit() |>
  rename(name=primaersprache,
         value = total_sum)
# Laden und aufbereiten Daten


df_ent_17_22_mean <- read_rds("daten/dat_ent.rds") |>
  mutate(jahr=as.character(jahr)) |>
  mutate(jahr = str_replace(jahr, "^", "10/")) |>
  group_by(jahr) %>%
  summarise(hochschule = "Durchschnitt",
            mean.sum_ENT = mean(sum_ENT, na.rm = TRUE),
            sd.sum_ENT = sd(sum_ENT, na.rm = TRUE),
            n.sum_ENT = n()) %>%
  mutate(se.sum_ENT = sd.sum_ENT / sqrt(n.sum_ENT),
         lower.ci.sum_ENT = mean.sum_ENT - qt(1 - (0.05 / 2), n.sum_ENT - 1) * se.sum_ENT,
         upper.ci.sum_ENT = mean.sum_ENT + qt(1 - (0.05 / 2), n.sum_ENT - 1) * se.sum_ENT)

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Highcharter Plot -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

# Highcharts-Theme mit Brandon Text Regular-Schriftart erstellen
sv_theme <- hc_theme(
  chart = list(
    style = list(
      fontFamily = "Brandon Text Regular"
    )
  ),
  title = list(
    style = list(
      fontFamily = "Brandon Text Regular"
    )
  ),
  subtitle = list(
    style = list(
      fontFamily = "Brandon Text Regular"
    )
  ),
  xAxis = list(
    labels = list(
      style = list(
        fontFamily = "Brandon Text Regular"
      )
    )
  ),
  yAxis = list(
    labels = list(
      style = list(
        fontFamily = "Brandon Text Regular"
      )
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Brandon Text Regular"
    )
  ),
  tooltip = list(
    style = list(
      fontFamily = "Brandon Text Regular"
    )
  )
)


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc1_1: Kursformate 2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

hc1_1 <- hc_filter_bar(df_format_2022, tooltip_suffix = " Kurse", invisible_categories = c("Seminar","Vorlesung")) %>%
  hc_add_theme(sv_theme) %>%
  hc_exporting(enabled = TRUE, filename = "Formate_2022")|> 
  hc_caption(
    text = "<b>Information:</b> Übungsformate: z.B. Tutorium Repetitorium; Erfahrungsorientierte Formate: z.B. Praktikum,
    Praxisorientierte Lehrveranstaltung; Austauschformate: z.B.: Kolloquium, Arbeitsgemeinschaften; Sprachlernformate: z.B. Sprachkurs,
    Sprachlehrveranstaltung; Sonstige: z.B. Informationsveranstaltung",
    style = list(fontSize = "9px")
  )


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc1_2: Kursformate 2017-2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


hc1_2 <- create_lineplot(
  df_format_17_22,
  sem_jahr,
  value,
  group_color = name,
  plot_title =  "Kursformate 2017-2022",
  #plot_subtitle = "subtitle",
  #source = "source",
  plot_type = "highcharter"
) %>%
  hc_add_theme(sv_theme) %>%
  hc_xAxis(gridLineWidth = 0) %>%  # Entfernt die vertikalen Gitternetzlinien
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; 
       }"
    )
  ) %>%  
  hc_exporting(enabled = TRUE, filename = "Kursformate_2017_22")


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc2_1: Sprachen 2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


hc2_1 <- hc_filter_bar(df_sprache_2022, tooltip_suffix = " Kurse", 
invisible_categories = c("Deutsch","Englisch", "Sonstiges")) %>%
  hc_add_theme(sv_theme) %>%
  hc_exporting(enabled = TRUE, filename = "Sprachen_2022")|> 
  hc_caption(
    text = "<b>Information:</b> Übungsformate: z.B. Tutorium Repetitorium; Erfahrungsorientierte Formate: z.B. Praktikum,
    Praxisorientierte Lehrveranstaltung; Austauschformate: z.B.: Kolloquium, Arbeitsgemeinschaften; Sprachlernformate: z.B. Sprachkurs,
    Sprachlehrveranstaltung; Sonstige: z.B. Informationsveranstaltung",
    style = list(fontSize = "9px")
  )

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc2_2: Sprachen 2017-2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

df_sprache_17_22 <- as.data.frame(df_sprache_17_22)

hc2_2 <- create_lineplot_interactive_highcharter(
  df= df_sprache_17_22,
  x_var= sem_jahr,
  y_var = value,
  group_color = name,
   plot_title =  "Kurssprachen 2017-2022", 
   invisible_categories = c("Deutsch","Englisch","Sonstiges")
) %>%
  hc_add_theme(sv_theme) %>%
  hc_xAxis(gridLineWidth = 0) %>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Kurse'; 
       }"
    )
  ) %>%
  hc_exporting(enabled = TRUE, filename = "Kurssprachen_2017_22") 

 

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc3: Platzhalter -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

hc3_2 <- hc2_2


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc4_1: Studierende in Fächern 2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


set.seed(42)  

# Jahre und spezifische Fächergruppen definieren
years <- c("10/2017","10/2018","10/2019","10/2020","10/2021","10/2022")
subject_groups <- c("Geisteswissenschaften", "Sport", "Rechts-, Wirtschafts- und Sozialwissenschaften",
                    "Mathematik, Naturwissenschaften", "Humanmedizin/Gesundheitswissenschaften",
                    "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin", "Ingenieurwissenschaften",
                    "Kunst, Kunstwissenschaft")

# Daten generieren
data_studierende <- expand_grid(Year = years, Subject_Group = subject_groups) %>%
  mutate(Number_of_Students = sample(500:5000, n(), replace = TRUE))


data_studierende_2022 <- data_studierende |> filter(Year=="10/2017")

hc4_1 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Studierende 2022") %>%
  hc_xAxis(
    categories = data_studierende_2022$Subject_Group                                                 ,
    labels = list(enabled = FALSE),
    title = list(text = "", style = list(fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Studierenden", style = list(fontWeight = "bold"))) %>%
  hc_plotOptions(
    column = list(
      pointPadding = 0.2,   # Reduziert den Abstand zwischen den Balken
      groupPadding = 0.1    # Reduziert den Abstand zwischen den Gruppen
    )
  ) %>%
  hc_legend(enabled = TRUE) %>%
  hc_add_series(
    name = "Geisteswissenschaften",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Geisteswissenschaften"],
    color = "#195365",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Sport",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Sport"],
    color = "#6896A8",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Rechts-, Wirtschafts- und Sozialwissenschaften",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Rechts-, Wirtschafts- und Sozialwissenschaften"],
    color = "#91BEA0"
  ) %>%
  hc_add_series(
    name = "Mathematik, Naturwissenschaften",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Mathematik, Naturwissenschaften"],
    color = "#B5BFC5"
  ) %>%
  hc_add_series(
    name = "Humanmedizin/Gesundheitswissenschaften",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Humanmedizin/Gesundheitswissenschaften"],
    color = "#E73F0C"
  ) %>%
  hc_add_series(
    name = "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin"],
    color = "#9650EB"
  ) %>%
  hc_add_series(
    name = "Ingenieurwissenschaften",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Ingenieurwissenschaften"],
    color = "#AFD700",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Kunst, Kunstwissenschaft",
    data = data_studierende_2022$Number_of_Students[data_studierende_2022$Subject_Group == "Kunst, Kunstwissenschaft"],
    color = "#ADA58B",
    visible = TRUE  # Standardmäßig abgewählt
  )%>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Studierende'; 
       }"
    )
  ) %>%
  hc_add_theme(sv_theme) %>% # für die font
  hc_exporting(enabled = TRUE, filename = "Studierende_2022")

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc4_2: Studierende in Fächern 2017-2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

hc4_2 <- create_lineplot(
  data_studierende,
  Year,
  Number_of_Students,
  group_color = Subject_Group,
  plot_title =  "Studierende 2017-2022",
  #plot_subtitle = "subtitle",
  #source = "source",
  plot_type = "highcharter"
) %>%
  hc_add_theme(sv_theme) %>%
  hc_xAxis(gridLineWidth = 0) %>%  # Entfernt die vertikalen Gitternetzlinien
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Studierende'; 
       }"
    )
  ) %>%  
  hc_exporting(enabled = TRUE, filename = "Sprachen_2022")


#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc5_1: Proffs in Fächern 2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


set.seed(42)  

# Jahre und spezifische Fächergruppen definieren
years <- c("10/2017","10/2018","10/2019","10/2020","10/2021","10/2022")
subject_groups <- c("Geisteswissenschaften", "Sport", "Rechts-, Wirtschafts- und Sozialwissenschaften",
                    "Mathematik, Naturwissenschaften", "Humanmedizin/Gesundheitswissenschaften",
                    "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin", "Ingenieurwissenschaften",
                    "Kunst, Kunstwissenschaft")

# Daten generieren
data_profs <- expand_grid(Year = years, Subject_Group = subject_groups) %>%
  mutate(Number_of_Students = sample(500:5000, n(), replace = TRUE))


data_profs_2022 <- data_profs |> filter(Year=="10/2017")

hc5_1 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Professor*innen 2022") %>%
  hc_xAxis(
    categories = data_profs_2022$Subject_Group                                                 ,
    labels = list(enabled = FALSE),
    title = list(text = "", style = list(fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Studierenden", style = list(fontWeight = "bold"))) %>%
  hc_plotOptions(
    column = list(
      pointPadding = 0.2,   # Reduziert den Abstand zwischen den Balken
      groupPadding = 0.1    # Reduziert den Abstand zwischen den Gruppen
    )
  ) %>%
  hc_legend(enabled = TRUE) %>%
  hc_add_series(
    name = "Geisteswissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Geisteswissenschaften"],
    color = "#195365",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Sport",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Sport"],
    color = "#6896A8",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Rechts-, Wirtschafts- und Sozialwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Rechts-, Wirtschafts- und Sozialwissenschaften"],
    color = "#91BEA0"
  ) %>%
  hc_add_series(
    name = "Mathematik, Naturwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Mathematik, Naturwissenschaften"],
    color = "#B5BFC5"
  ) %>%
  hc_add_series(
    name = "Humanmedizin/Gesundheitswissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Humanmedizin/Gesundheitswissenschaften"],
    color = "#E73F0C"
  ) %>%
  hc_add_series(
    name = "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin"],
    color = "#9650EB"
  ) %>%
  hc_add_series(
    name = "Ingenieurwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Ingenieurwissenschaften"],
    color = "#AFD700",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Kunst, Kunstwissenschaft",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Kunst, Kunstwissenschaft"],
    color = "#ADA58B",
    visible = TRUE  # Standardmäßig abgewählt
  )%>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Professor*innen'; 
       }"
    )
  ) %>%
  hc_add_theme(sv_theme) %>% # für die font
  hc_exporting(enabled = TRUE, filename = "Studierende_2022")

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc5_2: Profs in Fächern 2017-2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

hc5_2 <- create_lineplot(
  data_profs,
  Year,
  Number_of_Students,
  group_color = Subject_Group,
  plot_title =  "Professor*innen 2017-2022",
  #plot_subtitle = "subtitle",
  #source = "source",
  plot_type = "highcharter"
) %>%
  hc_add_theme(sv_theme) %>%
  hc_xAxis(gridLineWidth = 0) %>%  # Entfernt die vertikalen Gitternetzlinien
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Professor*innen'; 
       }"
    )
  ) %>%  
  hc_exporting(enabled = TRUE, filename = "Sprachen_2022")



#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc6_1: WissPerso in Fächern 2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////


set.seed(42)  

# Jahre und spezifische Fächergruppen definieren
years <- c("10/2017","10/2018","10/2019","10/2020","10/2021","10/2022")
subject_groups <- c("Geisteswissenschaften", "Sport", "Rechts-, Wirtschafts- und Sozialwissenschaften",
                    "Mathematik, Naturwissenschaften", "Humanmedizin/Gesundheitswissenschaften",
                    "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin", "Ingenieurwissenschaften",
                    "Kunst, Kunstwissenschaft")

# Daten generieren
data_profs <- expand_grid(Year = years, Subject_Group = subject_groups) %>%
  mutate(Number_of_Students = sample(500:5000, n(), replace = TRUE))


data_profs_2022 <- data_profs |> filter(Year=="10/2017")

hc6_1 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Professor*innen 2022") %>%
  hc_xAxis(
    categories = data_profs_2022$Subject_Group                                                 ,
    labels = list(enabled = FALSE),
    title = list(text = "", style = list(fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Studierenden", style = list(fontWeight = "bold"))) %>%
  hc_plotOptions(
    column = list(
      pointPadding = 0.2,   # Reduziert den Abstand zwischen den Balken
      groupPadding = 0.1    # Reduziert den Abstand zwischen den Gruppen
    )
  ) %>%
  hc_legend(enabled = TRUE) %>%
  hc_add_series(
    name = "Geisteswissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Geisteswissenschaften"],
    color = "#195365",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Sport",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Sport"],
    color = "#6896A8",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Rechts-, Wirtschafts- und Sozialwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Rechts-, Wirtschafts- und Sozialwissenschaften"],
    color = "#91BEA0"
  ) %>%
  hc_add_series(
    name = "Mathematik, Naturwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Mathematik, Naturwissenschaften"],
    color = "#B5BFC5"
  ) %>%
  hc_add_series(
    name = "Humanmedizin/Gesundheitswissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Humanmedizin/Gesundheitswissenschaften"],
    color = "#E73F0C"
  ) %>%
  hc_add_series(
    name = "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin"],
    color = "#9650EB"
  ) %>%
  hc_add_series(
    name = "Ingenieurwissenschaften",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Ingenieurwissenschaften"],
    color = "#AFD700",
    visible = TRUE  # Standardmäßig abgewählt
  ) %>%
  hc_add_series(
    name = "Kunst, Kunstwissenschaft",
    data = data_profs_2022$Number_of_Students[data_profs_2022$Subject_Group == "Kunst, Kunstwissenschaft"],
    color = "#ADA58B",
    visible = TRUE  # Standardmäßig abgewählt
  )%>%
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Professor*innen'; 
       }"
    )
  ) %>%
  hc_add_theme(sv_theme) %>% # für die font
  hc_exporting(enabled = TRUE, filename = "Studierende_2022")

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
## hc6_2: WissPerso in Fächern 2017-2022 -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

hc6_2 <- create_lineplot(
  data_profs,
  Year,
  Number_of_Students,
  group_color = Subject_Group,
  plot_title =  "Professor*innen 2017-2022",
  #plot_subtitle = "subtitle",
  #source = "source",
  plot_type = "highcharter"
) %>%
  hc_add_theme(sv_theme) %>%
  hc_xAxis(gridLineWidth = 0) %>%  # Entfernt die vertikalen Gitternetzlinien
  hc_tooltip(
    headerFormat = '',
    pointFormatter = JS(
      "function() { 
         return '<b>' + this.series.name + '</b>: ' + Highcharts.numberFormat(this.y, 0, ',', '.') + ' Professor*innen'; 
       }"
    )
  ) %>%  
  hc_exporting(enabled = TRUE, filename = "Sprachen_2022")

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# UI -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Brandon Text Regular:wght@400;700&display=swap"),
    tags$style(HTML("
    
      @font-face {
        font-family: 'Brandon Text Regular';
        src: url('shiny_grid_app_deploy/www/HVD Fonts - BrandonText-Regular.ttf') format('truetype');
        font-display: swap;
      }
      body, html {
        font-family: 'Brandon Text Regular', sans-serif;
        margin: 0;
        padding: 0;
        height: 100%;
        width: 100%;
        overflow: auto;
      }

      highcharts-title {
      font-family: 'Montserrat', sans-serif;
      }

      .switch {
        background-color: white;
        border: 1px solid #4a4a4a;
        color: #4a4a4a;
        padding: 5px 5px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 10px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 40px;
        width: 150px;
        height: 40px;
        box-shadow: none;
        outline: none; 
      }

      .switch:active {
        transform: translateY(2px);
      }

      .switch:hover {
        background-color: #f9f9f9;
      }

      .row-flex {
        display: flex;
        flex-wrap: wrap;
        align-items: flex-start; /* Aligns items to the start of the flex container */
      }
      @media (max-width: 768px) {
        .row-flex {
          flex-direction: column-reverse; /* Stacks the columns vertically and reverses the order */
        }
      }
    "))
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch1", "Kursformate im Zeitverlauf", class = "switch"),
                  highchartOutput("chart1", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."), class = "text-content")
           )
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch2", "Sprachen im Zeitverlauf", class = "switch"),
                  highchartOutput("chart2", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum"), class = "text-content")
           )
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch3", HTML("Entrepreneurship <br> im Zeitverlauf"), class = "switch"),
                  highchartOutput("chart3", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum"), class = "text-content")
           )
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch4", HTML("Studierendenzahlen <br> im Zeitverlauf"), class = "switch"),
                  highchartOutput("chart4", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum"), class = "text-content")
           )
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch5", HTML("Professor*innen <br> im Zeitverlauf"), class = "switch"),
                  highchartOutput("chart5", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum"), class = "text-content")
           )
  ),
  fluidRow(class = "row-flex",
           column(6,
                  actionButton("switch6", HTML("WissPer <br> im Zeitverlauf"), class = "switch"),
                  highchartOutput("chart6", height = "400px")
           ),
           column(6,
                  div(h3("Platzhalter Text"), p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum"), class = "text-content")
           )
  )
)

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Server -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

server <- function(input, output, session) {
  current_chart1 <- reactiveVal(hc1_1)
  current_chart2 <- reactiveVal(hc2_1)
  current_chart3 <- reactiveVal(hc1_1)
  current_chart4 <- reactiveVal(hc4_1)
  current_chart5 <- reactiveVal(hc5_1)
  current_chart6 <- reactiveVal(hc6_1)
  
  observeEvent(input$switch1, {
    if (identical(current_chart1(), hc1_1)) {
      current_chart1(hc1_2)
      updateActionButton(session, "switch1", label = "Kursformate im <br> Studienjahr 2022")
    } else {
      current_chart1(hc1_1)
      updateActionButton(session, "switch1", label = "Kursformate im Zeitverlauf")
    }
  })
  
  observeEvent(input$switch2, {
    if (identical(current_chart2(), hc2_1)) {
      current_chart2(hc2_2)
      updateActionButton(session, "switch2", label = "Sprachen im <br> Studienjahr 2022")
    } else {
      current_chart2(hc2_1)
      updateActionButton(session, "switch2", label = "Sprachen im Zeitverlauf")
    }
  })
  
  observeEvent(input$switch3, {
    if (identical(current_chart3(), hc1_1)) {
      current_chart3(hc3_2)
      updateActionButton(session, "switch3", label = HTML("Entrepreneurship im<br> Studienjahr 2022"))
    } else {
      current_chart3(hc1_1)
      updateActionButton(session, "switch3", label = HTML("Entrepreneurship <br> im Zeitverlauf"))
    }
  })
  
  observeEvent(input$switch4, {
    if (identical(current_chart4(), hc4_1)) {  # Ändere current_chart3 zu current_chart4
      current_chart4(hc4_2)  # Ändere current_chart3 zu current_chart4
      updateActionButton(session, "switch4", label = HTML("Studierendenzahlen im<br> Studienjahr 2022"))
    } else {
      current_chart4(hc4_1)  # Ändere current_chart3 zu current_chart4
      updateActionButton(session, "switch4", label = HTML("Studierendenzahlen<br> im Zeitverlauf"))
    }
  })
  
  observeEvent(input$switch5, {
    if (identical(current_chart5(), hc5_1)) {  # Ändere current_chart3 zu current_chart5
      current_chart5(hc5_2)  # Ändere current_chart3 zu current_chart5
      updateActionButton(session, "switch5", label = HTML("Professor*innen im<br> Studienjahr 2022"))
    } else {
      current_chart5(hc5_1)  # Ändere current_chart3 zu current_chart5
      updateActionButton(session, "switch5", label = HTML("Professor*innen <br> im Zeitverlauf"))
    }
  })
  
  observeEvent(input$switch6, {
    if (identical(current_chart6(), hc6_1)) { 
      current_chart6(hc5_2)  
      updateActionButton(session, "switch6", label = HTML("WissPerso im<br> Studienjahr 2022"))
    } else {
      current_chart6(hc5_1)   
      updateActionButton(session, "switch6", label = HTML("WissPerso<br> im Zeitverlauf"))
    }
  })
  
  output$chart1 <- renderHighchart({
    current_chart1()
  })
  
  output$chart2 <- renderHighchart({
    current_chart2()
  })
  
  output$chart3 <- renderHighchart({
    current_chart3()
  })
  
  output$chart4 <- renderHighchart({
    current_chart4()
  })
  
  output$chart5 <- renderHighchart({
    current_chart5()
  })
  
  output$chart6 <- renderHighchart({
    current_chart6()
  })
}

#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Start App -----------------------------------------------------------------
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////

shinyApp(ui, server)