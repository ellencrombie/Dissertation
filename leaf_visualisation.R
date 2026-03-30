# start this after running the code from the first leaf script (up to where i start making the histograms i think)

# This script will NOT deliniate between mounded and screefed 

# Wrangling leaf Data ----
filtered_leaf <- final_data_leaves 

filtered_leaf <- filtered_leaf %>%
  mutate(treatment = recode(treatment,
                            "Mounded" = "Cultivated",
                            "Screefed" = "Cultivated"))
# changing names in the table
filtered_leaf$site <- factor(
  filtered_leaf$site,
  levels = c("DD", "SD"),
  labels = c("Dundreggan", "Glen Shieldaig")
)

# getting the mean of the data to make a bar chart

filtered_sum_leaf <- filtered_leaf %>%
  group_by(site, treatment, species) %>%
  summarise(
    mean_nitrate  = mean(n_concentration_mg_per_g, na.rm = TRUE),
    se_nitrate    = sd(n_concentration_mg_per_g, na.rm = TRUE) / sqrt(n()),
    mean_phosphate = mean(p_concentration_mg_per_g, na.rm = TRUE),
    se_phosphate   = sd(p_concentration_mg_per_g, na.rm = TRUE) / sqrt(n())
  )

# Making bar charts ----

# N Bar Chart
nplot_leaf <- ggplot(filtered_sum_leaf, aes(x = treatment, y = mean_nitrate, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_nitrate - se_nitrate, ymax = mean_nitrate + se_nitrate),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Foliar nitrogen concentration (mg g⁻¹ N)", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

nplot_leaf


# P Bar chart
# still need to edit axis labels on this 
pplot_leaf <- ggplot(filtered_sum_leaf, aes(x = treatment, y = mean_phosphate, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_phosphate - se_phosphate, ymax = mean_phosphate + se_phosphate),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Foliar phosphorous concentration (mg g⁻¹ P)", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

pplot_leaf

# presenting these plots together
# taking labels off bottom graphs

pplot_leaf <- pplot_leaf + theme(strip.text = element_blank(),
                           strip.background = element_blank())

nplot_leaf <- nplot_leaf + theme(axis.title.x = element_blank())


# collecting all graphs together 
combined_plot_leaf <- (nplot_leaf / pplot_leaf) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "right")

combined_plot_leaf

ggsave("Figure_2.tiff",
       plot = combined_plot_leaf,
       width = 7,
       height = 8,
       dpi = 600)



# Introducing ratio data ----

# Importing leaf ratio data
ratio_leaves <- read.csv("ratio.csv")

# getting the mean of the data to make a bar chart for NP ratio

ratio_sum_leaf <- ratio_leaves %>%
  group_by(site, treatment, species) %>%
  summarise(
    mean_ratio  = mean(N.P, na.rm = TRUE),
    se_ratio    = sd(N.P, na.rm = TRUE) / sqrt(n())
  )

# Bar Charts for ratio data ----

# Bar chart with Screefed, Mounded and uncultivated as separate variables 
ratioplot_leaf <- ggplot(ratio_sum_leaf, aes(x = treatment, y = mean_ratio, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Foliar N:P ratio", x = "Ground preparation method") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

ratioplot_leaf

ggsave("ratio_separate.tiff",
       plot = ratioplot_leaf,
       width = 7,
       height = 5,
       dpi = 600)

# getting data for the summary, combined bar chart (ratios) ----
# changing treatment to just cultivated vs uncultivated
filtered_leaf <- ratio_leaves %>%
  mutate(treatment = recode(treatment,
                            "Mounded" = "Cultivated",
                            "Screefed" = "Cultivated"))

combined_ratio <- filtered_leaf %>%
  group_by(site, treatment, species) %>%
  summarise(
    mean_ratio  = mean(N.P, na.rm = TRUE),
    se_ratio    = sd(N.P, na.rm = TRUE) / sqrt(n())
  )


# bar charts with JUST cultivated vs Uncultivated 


ratioplot_leaf_1 <- ggplot(combined_ratio, aes(x = treatment, y = mean_ratio, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(name = "Tree species",
                    values = c("Birch" = "#E69F00", "Pine" = "#0072B2"),
                    labels = c("Downy birch", "Scots pine")) +
  labs(y = "Foliar N:P ratio", x = "Ground preparation") +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  # space to the right of y-axis label
    axis.title.x = element_text(margin = margin(t = 15))   # space above x-axis label
  )

ratioplot_leaf_1

# the plot above needs fixing

ggsave("ratio_combined.tiff",
       plot = ratioplot_leaf_1,
       width = 7,
       height = 5,
       dpi = 600)

