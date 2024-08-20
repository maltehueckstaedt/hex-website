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

df_covid <- read_rds("data/dat_covid.rds")  
 
df_covid$sem <- paste(rep(c("SS", "WS"), each = 1, times = 5), df_covid$jahr)
df_covid$axes <- 1:10
###################################
# Entrepeneurship-Plot ---- 
###################################
 
subtitle <- "Most cars are powered by a <b style = 'color:#4E598A;'>four</b> or <br> <b style = 'color:#598A4E;'>six</b> cylinder engine, while most trucks have a <b style = 'color:#598A4E;'>six</b> or <b style = 'color:#8A4E59;'>eight</b> cylinder. The more cylinders in an engine, the more combustion that occurs, creating more movement to turn the crankshaft and power to move the car. <span style = 'color:#473504;'>However, more cylinders also require more gasoline to make the combustion necessary to drive the car and thus are not as efficient.</span> - Eagle Ridge"

caption <- "<span style=\"color: #525252;\"><b>Information:</b></span> <span style=\"color: #666666;\">In Kurstiteln, -beschreibungen und Lernzielen wurde nach den Keywords <em> entrepreneurship, entrepreneur, entepreneurial, geschäftsaufbau, business toolkit, management werkzeuge, idea competition, unternehmensgründung, start-up, startup, bussiness-plan, businessplan, existenzgründung, existenzgründer, unternehmertum, selbstständigkeit</em></span>"
195365

ggplot(df_covid, aes(x = axes, y = sum_COVID)) +
  geom_line(color = "#195365", size = 1, alpha = 0.8) +  # Gruppieren nach Semester
  geom_point(size = 3, alpha = 0.8, color = "#195365") +
  scale_x_continuous( 
    labels = unique(df_covid$sem),
    breaks = df_covid$axes
  )  +  # Punkte hinzufügen
  geom_vline(xintercept = 4.1, linetype = "dashed", color = "#E73F0C") +
  labs(
    title = "Kursangebote mit Long COVID-Bezug im Zeitverlauf",
    x = "Jahr",
    y = "Anzahl der Kurse",
    caption = "Datenquelle: The Lancet. Facing up to Long COVID.\n Lancet. 2020 Dec 12;396(10266):1861. doi: 10.1016/S0140-6736(20)32662-3.\nPMID: 33308453; PMCID: PMC7834723."
  ) +
  theme_minimal() +
  annotate('text', x = 2.5, y = 1.5, label = 'erste, wissenschaftliche Publikation\n im Themengebiet Long COVID', size = 3, family = "Brandon") +
  annotate('curve', x = 2.5, y = 1.4, xend = 3.8, yend = 1.2, curvature = 0.1, arrow = arrow(length = unit(0.13, "inches")), color = "black", size=.2) +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(family = "Brandon"),
    plot.title = element_text(size = 16, color = '#525252', lineheight = 1.2),
    plot.caption = element_text(size = 8, lineheight = 1.2)
  )

?annotate

  # df_sprache_2022 <- df_sprache_2022 |>
#                    mutate(color = c("#195365", "#6896A8", "#ADA58B", "#91BEA0", "#B5BFC5",
#                                     "#E73F0C", "#9650EB", "#AFD700"))