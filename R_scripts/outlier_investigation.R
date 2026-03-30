### Title: Exploring leaf variation
# Libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)

# checking for outliers in soil phosphate measurements 
# note this script continues from the modelling_script as thats where
## the data sets 'new_soil' and 'new_leaf' come from


# checking for outliers in soil phosphate concentrations

ggplot(new_soil, aes(x = cultivation, 
                     y = phosphate_mg_per_g,
                     color = site)) +   # color by site
  geom_jitter(width = 0.08, size = 2.5) +
  geom_text_repel(aes(label = rownames(new_leaf))) +
  theme_classic() +
  labs(color = "Site")   # optional: label the legend


library(RColorBrewer)

outlierplot <- ggplot(new_soil, aes(x = cultivation, 
                                    y = phosphate_mg_per_g,
                                    color = site)) +
  
  geom_jitter(width = 0.08, size = 2.8, alpha = 0.8) +
  
  geom_text_repel(aes(label = rownames(new_soil)),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.3,
                  point.padding = 0.2) +
  
  scale_color_brewer(
    palette = "Set1",
    labels = c("DD" = "Dundreggan", "SD" = "Glen Shieldaig")
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "right",
    legend.title = element_text(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(r = 12))
  ) +
  
  labs(
    x = "Ground preparation",
    y = "Soil phosphate concentration (mg g⁻¹ P)",
    color = "Site"
  )

outlierplot
# saving soil phosphate plot 

ggsave("outlier_phos_plot.tiff",
       plot = outlierplot,
       width = 7,
       height = 5,
       dpi = 600)







