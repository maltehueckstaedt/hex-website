library(ggpubr)
library(tidyverse)
library(openxlsx)

#write.xlsx(df_grp,"C:/Users/mhu/Documents/gitlab/hex-informatik-analysen/plots_sv_website/df_grp.xlsx")
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
  select(-Topic_Name) |> mutate(Name_Anonym = case_when(
    hochschule == "Christian-Albrechts-Universität zu Kiel" ~ "Hochschule Nr. 1",
    hochschule == "Freie Universität Berlin" ~ "Hochschule Nr. 2",
    hochschule == "Friedrich-Alexander-Universität Erlangen-Nürnberg" ~ "Hochschule Nr. 3",
    hochschule == "Heinrich-Heine-Universität Düsseldorf" ~ "Hochschule Nr. 4",
    hochschule == "Humboldt-Universität zu Berlin" ~ "Hochschule Nr. 5",
    hochschule == "Johann Wolfgang Goethe-Universität Frankfurt am Main" ~ "Hochschule Nr. 6",
    hochschule == "Johannes Gutenberg-Universität Mainz" ~ "Hochschule Nr. 7",
    hochschule == "Rheinisch-Westfälische Technische Hochschule Aachen" ~ "Hochschule Nr. 8",
    hochschule == "Ruhr-Universität Bochum" ~ "Hochschule Nr. 9",
    hochschule == "Technische Universitaet München" ~ "Hochschule Nr. 10",
    hochschule == "Technische Universität Darmstadt" ~ "Hochschule Nr. 11",
    hochschule == "Universität Duisburg-Essen" ~ "Hochschule Nr. 12",
    hochschule == "Universität Hamburg" ~ "Hochschule Nr. 13",
    hochschule == "Universität Stuttgart" ~ "Hochschule Nr. 14",
    hochschule == "Universität zu Köln" ~ "Hochschule Nr. 15",
    TRUE ~ hochschule  # falls es andere Namen gibt, bleiben sie unverändert
  ))

table(df_grp$hochschule)
df_grp_spider <- df_grp %>% 
  #filter(hochschule %in% c("Technische Universität Darmstadt", "Universität Hamburg", "Universität Stuttgart")) %>%
  filter(New_Topic_Name %in% c("Robotik", "Kryptologie", "Echtzeitsysteme", "Bildverarbeitung", "Echtzeitsysteme", "Bioinformatik")) %>%
  select(Name_Anonym, relative_frequency, New_Topic_Name) %>%
  rename(Value = relative_frequency, Topic = New_Topic_Name) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Topic, values_from = Value, values_fill = 0) |>
  column_to_rownames("Name_Anonym") %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

ggballoonplot(df_grp_spider, fill = "value")+
  scale_fill_viridis_c(option = "C") + labs(
    title = "Themengebiete der Informatiklehre\nan ausgewählten deutschen Universitäten",
    x = "Jahr",
    y = "Anzahl",
    fill = "Anteil eines Themengebietes an\nallen Kursen der Informatik\neiner Universität in Prozent"
  ) +
  guides(size = FALSE)+
  gradient_fill(c("#ffffff", "#ff0000"))+
  theme(
    legend.position = "bottom", # Legendenposition unten
    legend.title = element_text(size = 8), # Anpassung des Legendentitels
    legend.text = element_text(size = 8) # Anpassung des Legendentextes
  )

