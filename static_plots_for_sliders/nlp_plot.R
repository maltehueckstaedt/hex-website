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

nlp_data <- read_rds("data/dat_nlp.rds") %>%
  group_by(jahr) %>%
  summarise(
    sum_transformer = sum(sum_transformer),
    sum_pre_transformer = sum(sum_pre_transformer)) |> 
    #sum_nlp = sum(sum_nlp)) |> 
    filter(jahr >= 2017 & jahr <= 2022)

nlp_long <- nlp_data %>%
  pivot_longer(cols = c(sum_transformer, sum_pre_transformer),
               names_to = "variable", values_to = "value")
 
table(nlp_long$variable)
###################################
# Entrepeneurship-Plot ---- 
###################################
 
subtitle <- "Most cars are powered by a <b style = 'color:#4E598A;'>four</b> or <br> <b style = 'color:#598A4E;'>six</b> cylinder engine, while most trucks have a <b style = 'color:#598A4E;'>six</b> or <b style = 'color:#8A4E59;'>eight</b> cylinder. The more cylinders in an engine, the more combustion that occurs, creating more movement to turn the crankshaft and power to move the car. <span style = 'color:#473504;'>However, more cylinders also require more gasoline to make the combustion necessary to drive the car and thus are not as efficient.</span> - Eagle Ridge"

caption <- "<span style='color: #525252;'><b>Information:</b></span> <span style='color: #666666;'> Die Klassifizierung der Kurse wurde folgendermaßen vorgenommen: <br> Natural Language Processing: NLP, nlp, (n|N)atural (l|L)anguage (p|P)rocessing, (g|G)enerative ((m|M)odel|(n|N)etwork), Textual Embedding <br> Pre-Transformer Modelle: Word2Vec, GloVe, Bag-of-Words, n-grams, TF-IDF <br> Transformer-Modelle: BERT, GPT, LLaMA, RoBERTa, DeBERTa, XLM, DistilBERT, XLNet, ChatGPT, SetFit, ELECTRA, PaLM, Longformer, SBERT, SentenceTransformer, Transformers, (l|L)arge (l|L)anguage (m|M)odel, Gemini</span>"


ggplot(nlp_long, aes(x = jahr, y = value, color = variable, group = variable)) +   
  geom_vline(xintercept = 2017.9, linetype = "dashed", color = "#525252", size=.5, alpha = .5)+   
  geom_vline(xintercept = 2019, linetype = "dashed", color = "#525252", size=.5, alpha = .5)+  
  geom_vline(xintercept = 2020.6, linetype = "dashed", color = "#525252", size=.5, alpha = .5)+
  geom_line(size = 1, alpha = .7) + 
  #stat_smooth(se = FALSE)+
  #geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = c("#195365", "#6896A8", "#ADA58B"),
                     labels = c("pre Transformer-Modelle", "Transformer-Modelle")) +
  labs(
    title = "Kursangebote im Themengebiet Natural Language Processing (NLP)/<br>Transformer-Modelle – 2017 bis 2022",
    x = "Jahr",
    y = "Anzahl Kurse",
    caption = caption
  ) +
  theme_minimal() +  # Ein einfaches und sauberes Thema verwenden
  # scale_x_continuous(
  #   #breaks = 1:6, 
  #   #labels = c("10/2017", "10/2018", "10/2019", "10/2020", "10/2021", "10/2022")
  # ) +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(family = "Brandon"),
    plot.title = element_markdown(size = 16,
                                  color = '#525252',
                                  lineheight = 1.2),
    plot.caption = element_textbox_simple(
      size = 8,
      lineheight = 1.2,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    
    )) + 
    
  # Färbung Quadranten  
  annotate("rect", xmin = 2017.9, xmax = 2019, ymin = 0, ymax = 10, fill="#525252", alpha = .05) + 
  
  annotate("rect", xmin = 2017.2, xmax = 2017.85, ymin = 6.1, ymax = 9.8, fill = "#ffffff", colour = "#E73F0C", alpha = 1,
  size=.1)+
  # Beschriftung Quadrant

  annotate('text', x = 2017.8, y = 8, label = 'Zeitraum\nzwischen\nerster\nPublikation\nund erstem\nKurs zum\nThema\nTransformer-\nModelle', size = 3, family = "Brandon", hjust = 1, lineheight = 0.8) +

# Linien zu den Pfeilen:

  annotate('curve', x = 2017.85, y = 7.7, xend = 2018.45, yend = 5.5, curvature = -0.4, color = "#E73F0C", size=.2) +

  annotate('segment', x = 2018.45, y = 5.5, xend = 2018.45, yend = 4.5, color = "#E73F0C", size=.2) +

  #Pfeil rechts
  annotate('curve', x = 2018.45, y = 4.5, xend = 2018.9, yend = 2.5, curvature = 0.4, arrow = arrow(length = unit(0.12, "inches")), color = "#E73F0C", size=.2) +

   #Pfeil links
  annotate('curve', x = 2018.45, y = 4.5, xend = 2018, yend = 2.5, curvature = -0.4, arrow = arrow(length = unit(0.12, "inches")), color = "#E73F0C", size=.2) +





  annotate('text', x = 2020.7, y = 5, label = 'release OpenAI ChatGPT3', size = 2.5, family = "Brandon", angle = 90)+



  annotate('text', x = 2019.15, y = 5, label = 'erster Kurs zum\n Thema Transformer-Modelle', size = 2.5, family = "Brandon", angle = 90, lineheight = 0.8)+



  annotate('text', x = 2018.04, y = 5, label = 'erste Publikation zum\n Thema Transformer-Modelle', size = 2.5, family = "Brandon", angle = 90, lineheight = 0.8)
 
   
  # df_sprache_2022 <- df_sprache_2022 |>
#                    mutate(color = c("#195365", "#6896A8", "#ADA58B", "#91BEA0", "#B5BFC5",
#                                     "#E73F0C", "#9650EB", "#AFD700"))
