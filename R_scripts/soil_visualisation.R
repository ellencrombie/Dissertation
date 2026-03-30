## Libraries ----
library(dplyr)
library(ggplot2)
library(patchwork)

citation("dplyr")
citation("tidyverse")
citation("patchwork")

# Importing the soils dataset ----
final_data_soils <- read.csv("final_data_soils.csv")

# Cleaning Data ----
# Checking classes of columns in my data set
str(final_data_soils)

#changing classes of data
final_data_soils$site <- as.factor(final_data_soils$site)
final_data_soils$block_number <- as.factor(final_data_soils$block_number)
final_data_soils$species <- as.factor(final_data_soils$species)
final_data_soils$treatment <- as.factor(final_data_soils$treatment)
final_data_soils$phosphate_mg_per_g <- as.numeric(final_data_soils$phosphate_mg_per_g)

# Re checking classes of columns in my data set
str(final_data_soils)

# removing data from blanks from this data set
## note - need to ask advice on what to do about the 
# values of nitrate that are below the limit of detection
## temporarily i have removed these to check distribution

clean_data_soils <- final_data_soils[-c(33, 34,35,36), ]
str(clean_data_soils)

clean_data_soils
# converting N from ug'g to mg/g --> this has recently been added

clean_data_soils <- clean_data_soils %>%
  mutate(
    nitrate_ug_per_g = nitrate_ug_per_g / 1000,
    
  )
# changing the column header to reflect the new units
clean_data_soils <- clean_data_soils %>%
  rename(nitrate_mg_per_g = nitrate_ug_per_g)


clean_data_soils <- droplevels(clean_data_soils)
unique(clean_data_soils$treatment) # checking for spacing issues in treatment column of table
# i found spacing issue, this should fix it 

clean_data_soils$treatment <- trimws(as.character(clean_data_soils$treatment))
clean_data_soils$treatment <- factor(clean_data_soils$treatment)
#checking issue is fixed - it is, it now has three levels
unique(clean_data_soils$treatment)

# This script will not deliniate between mounded and screefed 

filtered_soil <- clean_data_soils 

filtered_soil <- filtered_soil %>%
  mutate(treatment = recode(treatment,
                              "Mounded" = "Cultivated",
                              "Screefed" = "Cultivated"))
# Check result
table(filtered_soil$treatment)
# getting the mean of the data to make a bar chart
filtered_summary <- filtered_soil %>%
  group_by(site, treatment, species) %>%
  summarise(
    mean_nitrate = mean(nitrate_mg_per_g, na.rm = TRUE),
    se_nitrate   = sd(nitrate_mg_per_g, na.rm = TRUE)/sqrt(n()),
    mean_phosphate = mean(phosphate_mg_per_g, na.rm = TRUE),
    se_phosphate   = sd(phosphate_mg_per_g, na.rm = TRUE)/sqrt(n()),
    mean_ammonium = mean(ammonium_mg_per_g, na.rm = TRUE),
    se_ammonium   = sd(ammonium_mg_per_g, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

# changing names in the table
filtered_summary$site <- factor(
  filtered_summary$site,
  levels = c("DD", "SD"),
  labels = c("Dundreggan", "Glen Shieldaig")
)


# plotting bar charts ----

# Nitrate Bar Chart
nplot_2 <- ggplot(filtered_summary, aes(x = treatment, y = mean_nitrate, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_nitrate - se_nitrate, ymax = mean_nitrate + se_nitrate),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Soil nitrate concentration (mg g⁻¹ N)", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

nplot_2

# ammonuium bar chart 
aplot_2 <- ggplot(filtered_summary, aes(x = treatment, y = mean_ammonium, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_ammonium - se_ammonium, ymax = mean_ammonium + se_ammonium),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Soil ammonium concentration (mg g⁻¹ N)", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

aplot_2

# Phosphate bar chart

pplot_2 <- ggplot(filtered_summary, aes(x = treatment, y = mean_phosphate, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_phosphate - se_phosphate, ymax = mean_phosphate + se_phosphate),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Soil phosphate concentration (mg g⁻¹ P)", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )
pplot_2



# presenting these plots together
# taking labels off bottom graphs


pplot_2 <- pplot_2 + theme(strip.text = element_blank(),
                       strip.background = element_blank())


aplot_2 <- aplot_2 + theme(axis.title.x = element_blank())

# collecting all graphs together 
combined_plot <- (aplot_2 / pplot_2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "right")

combined_plot

ggsave("soil_combined.tiff",
       plot = combined_plot,
       width = 7,
       height = 8,
       dpi = 600)
