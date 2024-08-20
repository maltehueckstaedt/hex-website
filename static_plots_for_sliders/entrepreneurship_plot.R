###################################
# Load Librarys ----
###################################

library(tidyverse) 
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
# Prepare Data ---- 
###################################

## Raw Data ---- 

df_ent_17_22_raw <- read_rds("shiny_grid_app_deploy/daten/dat_ent.rds") |>
  mutate(jahr = as.character(jahr)) |>
  mutate(jahr = str_replace(jahr, "^", "10/")) |>
  filter(jahr %in% c("10/2017", "10/2018", "10/2019", "10/2020", "10/2021", "10/2022")) |>
  filter(hochschule == "Universität Stuttgart" | 
         hochschule == "Technische Universität München" |
         hochschule == "Freie Universität Berlin") |>
  mutate(
    hochschule = str_replace(hochschule, "Universität Stuttgart", "Uni_1"),
    hochschule = str_replace(hochschule, "Freie Universität Berlin", "Uni_2"),
    hochschule = str_replace(hochschule, "Technische Universität München", "Uni_3")
  ) |>
  mutate(jahr=as.factor(jahr))|>
  mutate(jahr=as.numeric(jahr))

## Mean Data ---- 

df_ent_17_22_mean <- read_rds("shiny_grid_app_deploy/daten/dat_ent.rds") |>
  mutate(jahr = str_replace(as.character(jahr), "^", "10/")) |>
  group_by(jahr) |>
  summarise(
    hochschule = "Durchschnitt",
    mean.ant_ENT = mean(ant_ent, na.rm = TRUE),
    sd.ant_ent = sd(ant_ent, na.rm = TRUE),
    n.ant_ent = n(),
    .groups = 'drop'
  ) |>
  mutate(
    se.ant_ent = sd.ant_ent / sqrt(n.ant_ent),
    lower.ci.ant_ent = mean.ant_ENT - qt(1 - (0.05 / 2), n.ant_ent - 1) * se.ant_ent,
    upper.ci.ant_ent = mean.ant_ENT + qt(1 - (0.05 / 2), n.ant_ent - 1) * se.ant_ent,
    jahr = as.numeric(as.factor(jahr))
  ) |> 
  mutate(ant_ent=mean.ant_ENT)
 
 ## Combine Data ---- 

 df <- bind_rows(df_ent_17_22_raw, df_ent_17_22_mean) |> 
       mutate(hochschule=factor(hochschule, levels = c("Uni_1", "Uni_2", "Uni_3","Durchschnitt")))

###################################
# Entrepeneurship-Plot ---- 
###################################
 
subtitle <- "Most cars are powered by a <b style = 'color:#4E598A;'>four</b> or <br> <b style = 'color:#598A4E;'>six</b> cylinder engine, while most trucks have a <b style = 'color:#598A4E;'>six</b> or <b style = 'color:#8A4E59;'>eight</b> cylinder. The more cylinders in an engine, the more combustion that occurs, creating more movement to turn the crankshaft and power to move the car. <span style = 'color:#473504;'>However, more cylinders also require more gasoline to make the combustion necessary to drive the car and thus are not as efficient.</span> - Eagle Ridge"

caption <- "<span style=/"color: #525252;/"><b>Information:</b></span> <span style=/"color: #666666;/">In Kurstiteln, -beschreibungen und Lernzielen wurde nach den Keywords <em> entrepreneurship, entrepreneur, entepreneurial, geschäftsaufbau, business toolkit, management werkzeuge, idea competition, unternehmensgründung, start-up, startup, bussiness-plan, businessplan, existenzgründung, existenzgründer, unternehmertum, selbstständigkeit</em></span><br><span style=/"color: #525252;/"><b>*</b></span><span style=/"color: #666666;/">Das Konfidenzintervall gibt jenen Bereich an, in dem der wahre Mittelwert mit einer Wahrscheinlichkeit von 95% liegt. Es zeigt die Unsicherheit der Mittelwertsschätzung an und hilft zu beurteilen, wie präzise diese Schätzung ist.</em></span>"


ggplot(df, aes(x = jahr, y = ant_ent, color = hochschule, group = hochschule)) +
  geom_ribbon(data = df_ent_17_22_mean, 
              aes(y = mean.ant_ENT, ymin = lower.ci.ant_ent, ymax = upper.ci.ant_ent), 
              fill = "#B5BFC5", 
              alpha = 0.3,
              size = .2) +
  geom_line(data = df %>% filter(hochschule != "Durchschnitt"), size = 1) +
  geom_line(data = df %>% filter(hochschule == "Durchschnitt"), linetype = "dashed", size = 0.5, show.legend = TRUE) +
  geom_point(data = df %>% filter(hochschule != "Durchschnitt"), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("#195365", "#6896A8", "#ADA58B", "#B5BFC5"),
                     labels = c("Universität Nr.1", "Universität Nr.2", "Universität Nr.3", "Durchschnittswert/naller Universitäten/n(95% Konfidenzintervall*)")) +
  labs(
    title = "Kursangebote im Themengebiet *Entrepreneurship* – 2017 bis 2022",
    x = "Jahr",
    y = "prozentualer Anteil",
    caption = caption
  ) +
  theme_minimal() +  # Ein einfaches und sauberes Thema verwenden
  scale_x_continuous(
    breaks = 1:6, 
    labels = c("10/2017", "10/2018", "10/2019", "10/2020", "10/2021", "10/2022")
  ) +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(family = "Brandon"),
    plot.title = element_markdown(size = 16,
                                  color = '#525252',
                                  lineheight = 1.2),
    plot.caption = element_textbox_simple(
      size = 7,
      lineheight = 1.2,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    )
  )
  
  # df_sprache_2022 <- df_sprache_2022 |>
#                    mutate(color = c("#195365", "#6896A8", "#ADA58B", "#91BEA0", "#B5BFC5",
#                                     "#E73F0C", "#9650EB", "#AFD700"))