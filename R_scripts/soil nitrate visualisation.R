
# setting limit of detection
LOD <- (3.96414409/1000)

LOD_soil <- clean_data_soils %>%
  mutate(detection_status = ifelse(nitrate_mg_per_g > LOD,
                                   "Above LOD",
                                   "Below LOD"))

LOD_soil <- LOD_soil %>%
  mutate(cultivation =
           ifelse(treatment == "Uncultivated",
                  "Uncultivated",
                  "Cultivated"))

library(ggplot2)

LOD_plot <- ggplot(LOD_soil,
                   aes(x = cultivation,
                       y = nitrate_mg_per_g,
                       colour = detection_status)) +
  
  geom_jitter(width = 0.08, height = 0, size = 2.5, alpha = 0.9) +
  
  geom_hline(yintercept = LOD,
             linetype = "dashed",
             linewidth = 0.6,
             colour = "red") +
  
  scale_colour_manual(
    name = "Detection status",
    values = c(
      "Below LOD" = "grey80",
      "Above LOD" = "black"
    )
  ) +
  
  labs(
    x = "Ground preparation",
    y = expression("Soil nitrate concentration (mg g"^{-1}~"N)")
  ) +
  
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 10))
  )




LOD_plot

# Save the figure 
ggsave("soil_nitrate_LOD_plot.tiff",
       plot = LOD_plot,
       width = 7,
       height = 6,
       dpi = 600)
