library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. THE DATA: A list of nail polish brands, their prices, and what's inside them
df <- data.frame(
  brand = c("Beetles", "Modelones", "Saviland", "Morovan", "Jodsone", 
            "MelodySusie", "DND", "Essie", "Kiara Sky", "Gelish", "Madam Glam",
            "OPI", "Sally Hansen", "Olive & June"),
  price = c(7.99, 8.50, 7.50, 6.99, 5.99, 8.99, 9.00, 13.00, 12.99, 14.50, 15.00, 20.00, 11.00, 14.00),
  segment = c("Budget/DIY", "Budget/DIY", "Budget/DIY", "Budget/DIY", "Budget/DIY",
              "Prosumer", "Professional", "Retail/Legacy", "Professional", "Professional", "Prosumer",
              "Retail/Legacy", "Retail/Drugstore", "Retail/Drugstore"),
  hema = c(12.5, 12.0, 15.0, 15.0, 18.0, 0, 15, 10, 10, 8, 5, 0, 0, 0),
  tpo = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  toxic_trio = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

# 2. THE SCORE: Calculating a "safety number" for each brand
# Higher numbers mean more chemicals that might cause skin allergies or itching
df <- df %>%
  mutate(toxicity_score = (hema * 0.5) + (tpo * 10) + (toxic_trio * 5),
         `Risk Tier` = case_when(
           toxicity_score > 15 ~ "Tier 3: High Risk",
           toxicity_score >= 5 ~ "Tier 2: Elevated Risk",
           TRUE ~ "Tier 1: Standard Safety"
         ))

# Grouping the brands so they show up in a nice order on the chart's legend
df$segment <- factor(df$segment, levels = c("Budget/DIY", "Professional", "Prosumer", "Retail/Drugstore", "Retail/Legacy"))

# 3. CLEANING IT UP: Making sure the brand names are easy to read
# We tell the computer to push some names up and some names down so they don't overlap
df <- df %>%
  mutate(
    h_nudge = ifelse(brand == "Kiara Sky", -0.9, 0), # Gently move Kiara Sky to the left
    v_nudge = case_when(
      brand %in% c("Jodsone", "Saviland", "Beetles", "Essie", "Gelish", "OPI") ~ 5, # Move these up
      brand %in% c("Morovan", "MelodySusie", "DND", "Modelones", "Kiara Sky", "Sally Hansen", "Olive & June", "Madam Glam") ~ -5, # Move these down
      TRUE ~ 0
    )
  )

# 4. THE GRAPH: Drawing the actual picture
final_plot <- ggplot(df, aes(x = price, y = toxicity_score)) +
  # Add a dashed line to show the general pattern: Higher price usually means lower risk
  geom_smooth(method = "lm", se = FALSE, color = "gray92", linewidth = 0.5, linetype = "dashed") +
  # Draw the colorful dots for each brand
  geom_point(aes(color = `Risk Tier`, shape = segment), size = 5.5, alpha = 0.8) +
  
  # Adding the brand names and drawing small lines to connect them to their dots
  geom_text_repel(
    aes(label = brand),
    family = "sans", fontface = "bold", size = 3.5,
    nudge_x = df$h_nudge,      
    nudge_y = df$v_nudge,      
    direction = "both", 
    box.padding = 0.5, 
    point.padding = 0.5,
    min.segment.length = 0, 
    max.overlaps = Inf,
    segment.color = "grey75"
  ) +
  
  # Labeling the bottom (Price) and the side (Toxicity)
  scale_x_continuous(breaks = seq(5, 20, by = 5)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  expand_limits(y = c(-8, 35)) +
  
  # Picking the colors: Green for safe, Orange for medium, and Dark Orange for higher risk
  scale_color_manual(values = c("Tier 2: Elevated Risk" = "#E69F00", "Tier 3: High Risk" = "#D55E00", "Tier 1: Standard Safety" = "#009E73")) +
  
  # Making the background white and the text clear
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey30", margin = margin(t = 15))
  ) +
  # Adding the titles and the safety rules explanation at the bottom
  labs(title = "Nail Salon Occupational Exposure Model",
       subtitle = "Composite Toxicity vs. Price (2026 Standards)",
       x = "Unit Price (USD)", y = "Composite Toxicity Index",
       shape = "Market Segment",
       caption = "Index = (HEMA % × 0.5) + (TPO Risk × 10) + (Toxic Trio × 5). Scoring based on 2026 safety rules.")

# 5. SAVE: Saving the finished graph to your computer
ggsave("Nail_Safety_Final_Readable.png", plot = final_plot, width = 14, height = 8, dpi = 300)

# Show the graph on your screen
print(final_plot)
