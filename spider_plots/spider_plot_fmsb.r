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
  select(-Topic_Name)

#write.xlsx(df_grp, "df_grp.xlsx")
###################################
## Select Data ----
###################################

df_grp_spider <- df_grp %>% 
  filter(hochschule %in% c("Universität zu Köln", "Freie Universität Berlin", "Heinrich-Heine-Universität Düsseldorf")) %>%
  filter(New_Topic_Name %in% c("Bioinformatik", "Robotik", "Digitale Innovation", "Künstliche Intelligenz", "Echtzeitsysteme", "Optimierungsalgorithmen")) %>%
  select(hochschule, relative_frequency, New_Topic_Name) %>%
  rename(Value = relative_frequency, Topic = New_Topic_Name) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Topic, values_from = Value, values_fill = 0) |>
  column_to_rownames("hochschule")


# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  "Bioinformatik" = c(6.5, 0), "Robotik" = c(6.5, 0), "Digitale Innovation" = c(6.5, 0),
  "Künstliche Intelligenz" = c(6.5, 0), "Echtzeitsysteme" = c(6.5, 0), "Optimierungsalgorithmen" = c(6.5, 0)
)
rownames(max_min) <- c("Max", "Min")
colnames(max_min) <-  colnames(df_grp_spider)
# Bind the variable ranges to the data
df <- rbind(max_min, df_grp_spider)
colnames(max_min)
colnames(df_grp_spider)
 
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
  data = df, caxislabels = c("0 %", "2 %", "4 %", "6 %", "8 %"),
  color = c("#195365", "#6896A8", "#ADA58B")
)
# Add a horizontal legend
new_labels <- c("Universität Nr.1", "Universität Nr.2", "Universität Nr.3")  # Erstellen Sie einen Vektor mit Ihren gewünschten Labels
legend(
  x = "bottomright", legend = new_labels, horiz = FALSE,
  bty = "n", pch = 20 , col = c("#195365", "#6896A8", "#ADA58B"),
  text.col = "black", cex = 1, pt.cex = 3
)
# Add title and caption
title(main = "Themengebiete der Informatiklehre", sub = "Hinweis: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, \n sed diam nonumy eirmod tempor invidunt ut labore et \n dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam.", 
cex.main = 1.5, 
cex.sub = 1)
par(op)

###################################
# Create Gridview Spider ----
###################################

df_grp_spider <- df_grp %>% 
  filter(hochschule %in% c("Heinrich-Heine-Universität Düsseldorf", "Freie Universität Berlin", "Christian-Albrechts-Universität zu Kiel", "Universität zu Köln")) %>%
  filter(New_Topic_Name %in% c("Bioinformatik", "Robotik", "Digitale Innovation", "Künstliche Intelligenz", "Echtzeitsysteme", "Optimierungsalgorithmen")) %>%
  select(hochschule, relative_frequency, New_Topic_Name) %>%
  rename(Value = relative_frequency, Topic = New_Topic_Name) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Topic, values_from = Value, values_fill = 0) |>
  column_to_rownames("hochschule")

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  "Bioinformatik" = c(6.5, 0), "Robotik" = c(6.5, 0), "Digitale Innovation" = c(6.5, 0),
  "Künstliche Intelligenz" = c(6.5, 0), "Echtzeitsysteme" = c(6.5, 0), "Optimierungsalgorithmen" = c(6.5, 0)
)
rownames(max_min) <- c("Max", "Min")
colnames(max_min) <-  colnames(df_grp_spider)
# Bind the variable ranges to the data
df <- rbind(max_min, df_grp_spider)
df

# Define colors and titles
colors <- c("#195365", "#6896A8", "#ADA58B", "#9650EB")
titles <- c("Universität Nr.1", "Universität Nr.2", "Universität Nr.3", "Universität Nr.4")
rownames(df)

# Reduce plot margin using par()
# Split the screen in 2 rows and 2 columns
op <- par(mar = c(1, 1, 1, 1), oma = c(4, 4, 4, 4), family = "Brandon")
par(mfrow = c(2, 2))

# Create the radar chart
for(i in 1:4){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ], caxislabels = c("0 %", "2 %", "4 %", "6 %", "8 %"),
    color = colors[i], title = titles[i],
    cex.main = 1.5
  )
}

# Add a common title
title <- "Gemeinsamer Titel"
mtext(title, side = 3, outer = TRUE, line = 2, cex = 1.5)

# Add a common caption
caption <- "Gemeinsame Bildunterschrift"
mtext(caption, side = 1, outer = TRUE, line = 2, cex = 1)

# Reset plotting parameters
par(op)
