###################################
# Load Librarys ----
###################################

library(tidyverse)
library(openxlsx)
library(fmsb)
library(showtext)
library(ggtext)

###################################
# Add Bradon Font ----
###################################

font.add("Brandon", 
         regular = "C:/Users/mhu/Documents/gitlab/hex-website/static_plots_for_sliders/HVD Fonts - BrandonText-Regular.otf", 
         bold="C:/Users/mhu/Documents/gitlab/hex-website/static_plots_for_sliders/HVD Fonts - BrandonText-Bold.otf",    
         italic = "C:/Users/mhu/Documents/gitlab/hex-website/static_plots_for_sliders/HVD Fonts - BrandonText-RegularItalic.otf")
showtext.auto() 
font_families()

###################################
# Data Wrangling ----
###################################

###################################
## Percent der Topics ----
###################################

# data <- read.xlsx("C:/Users/mhu/Documents/gitlab/hex-website/spider_plots/hex_info_with_topics.xlsx") %>% as_tibble()

# df_grp <-  data %>%
#   group_by(hochschule, Topic_Name) %>% # gruppiere nach HS und Topic
#   summarise(n = n()) %>% # zähle Kurse einer HS eines Topics und speichere in n
#   group_by(hochschule) %>% # gruppiere nach HS
#   mutate(total = sum(n), # berechne Gesamtzahl aller Kurse einer HS
#          relative_frequency = n / total * 100) %>% # Berechne realtiven Anteil eines Topics am Gesamt-Kursangebot
#   select(hochschule, Topic_Name, n, relative_frequency)
# #write.xlsx(df_grp,"C:/Users/mhu/Documents/gitlab/hex-informatik-analysen/plots_sv_website/df_grp.xlsx")

###################################
## Relabel Topics ----
###################################

df_grp <- read.xlsx("spider_plots/df_grp.xlsx")

df_grp <- df_grp %>%
  mutate(New_Topic_Name = case_when(
    grepl('algorithms|maschinelles lernen|artificial intelligence', Topic_Name) ~ "Künstliche Intelligenz",
    grepl('android app bereich sensordatenanalyse|sensorknoten|sensordatenanalyse', Topic_Name) ~ "Sensordatenanalyse",
    grepl('angewandte bioinformatik|einführung bioinformatik|bioinformatics', Topic_Name) ~ "Bioinformatik",
    grepl('betrieblicher prozesse informationstechnologie|informationstechnologie', Topic_Name) ~ "Informationstechnologie",
    grepl('theory|beweistheorie', Topic_Name) ~ "Beweistheorie",
    grepl('compilerbau|compiler wesentlicher|compilers', Topic_Name) ~ "Compilerbau",
    grepl('digital innovation|smart', Topic_Name) ~ "Digitale Innovation",
    grepl('eingebetteter echtzeitsysteme|realtime systems', Topic_Name) ~ "Echtzeitsysteme",
    grepl('virtual reality|augmented reality', Topic_Name) ~ "Virtual Reality",
    grepl('knowledge graphs|graphs knowledge', Topic_Name) ~ "Semantisches Netzwerke",
    grepl('design thinking|hauptseminar design', Topic_Name) ~ "Design Thinking",
    grepl('informatikunterricht|tutorien', Topic_Name) ~ "Informatikunterricht",
    grepl('robotik|robotersysteme|lernende roboter', Topic_Name) ~ "Robotik",
    grepl('medizinische bildverarbeitung|image processing|imaging', Topic_Name) ~ "Bildverarbeitung",
    grepl('openssl|kryptologie', Topic_Name) ~ "Kryptologie",
    grepl('optimierungsalgorithmen|optimierungsprobleme', Topic_Name) ~ "Optimierungsalgorithmen",
    grepl('photophosphorylierung|pigmente|photosynthese', Topic_Name) ~ "Photosynthese",
    grepl('programmierung|systeme', Topic_Name) ~ "Programmierung",
    grepl('protocol design|kommunikationsnetze', Topic_Name) ~ "Netzwerkprotokolle",
    TRUE ~ "Sonstiges"
  )) %>% 
  select(-Topic_Name) %>%
  mutate(TU = ifelse(df_grp$hochschule %in% c("Rheinisch-Westfälische Technische Hochschule Aachen", 
                                                            "Technische Universitaet München", 
                                                            "Technische Universität Darmstadt"), "Techn_+", "Tech_-"))

#berechne Mittelwerte für technische Unis, für nicht-technische und alle anderen
mean_tus <- df_grp %>%
  group_by(TU,New_Topic_Name) %>%
  summarise(Mittelwert = mean(relative_frequency))

#berechne Mittelwerte für alle HS
mean_all <- df_grp %>%
  group_by(New_Topic_Name) %>%
  summarise(Mittelwert = mean(relative_frequency)) |>
  mutate(TU=rep("alle", 20))

# Füge die Daten zusammen
df_grp_spider <-  bind_rows(mean_tus,mean_all) |>
  filter(New_Topic_Name %in% c("Bioinformatik", "Robotik", 
  "Digitale Innovation", "Künstliche Intelligenz", 
  "Echtzeitsysteme", "Optimierungsalgorithmen")) %>%
  rename(Value = Mittelwert, Topic = New_Topic_Name) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Topic, values_from = Value, values_fill = 0) |>
  column_to_rownames("TU")

 
#write.xlsx(df_grp, "df_grp.xlsx")
###################################
## Select Data ----
###################################
 
# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  "Bioinformatik" = c(3.5, 0), "Robotik" = c(3.5, 0), "Digitale Innovation" = c(3.5, 0),
  "Künstliche Intelligenz" = c(3.5, 0), "Echtzeitsysteme" = c(3.5, 0), "Optimierungsalgorithmen" = c(3.5, 0)
)
rownames(max_min) <- c("Max", "Min")
colnames(max_min) <-  colnames(df_grp_spider)
# Bind the variable ranges to the data
df <- rbind(max_min, df_grp_spider)
colnames(max_min)
colnames(df_grp_spider)

# reorder Kategorien 

df_grp_spider <- df_grp_spider %>%
  slice(c(2, 1, 3))
 
###################################
# Create Single Spider ----
###################################

create_beautiful_radarchart <- function(data, color = "#ff0202", 
                                        vlabels = colnames(data), vlcex = 1.4,
                                        caxislabels = NULL, calcex = 1.4, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.1), plwd = 3, plty = 1,
    pty = 32, #symboltyp 
    # Customize the grid
    cglcol = "#414141", cglty = 1, cglwd = .3,
    # Customize the axis
    axislabcol = "#8b8b8b", calcex = calcex, #lable-color
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Reduce plot margin using par()
op <- par(mar = c(5, 4, 4, 2) + 0.1, family = "Brandon")
par(mfrow = c(1, 1))

# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c("0 %", "1 %", "2 %", "3 %", "4%"),
  color = c("#195365", "#6896A8", "#ADA58B")
)

# Add a horizontal legend
new_labels <- c("Technische Universitäten", "nicht-Technische Universitäten", "alle Universitäten")  # Erstellen Sie einen Vektor mit Ihren gewünschten Labels
legend(
  x = "bottomright", legend = new_labels, horiz = FALSE,
  bty = "n", pch = 20, col = c("#195365", "#6896A8", "#ADA58B"),
  text.col = "black", cex = 1, pt.cex = 3
)

# Add title
title(main = "Themengebiete der Informatiklehre", cex.main = 1.5)

# Add right-aligned caption
mtext("Hinweis: Die Analyse umfasst sämtliche im Informatikbereich angebotenen Kurse der HEX-Datenbank, unabhängig vom\nSemester oder anderen Einschränkungen.",
      side = 1, line = 4, adj = 0, cex = 1) # Adjust 'line' as needed for positioning

# Restore original parameters
par(op)