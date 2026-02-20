### Plot Selection for Thesis ###
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(viridis)

# Wide save:
# ggsave("plots/2026-02-00_PLOTNAME.png", width = 300, height = 175, units = "mm", dpi = 300, bg = "white")
# Tall save
# ggsave("plots/2026-02-01_PLOTNAME.png", width = 200, height = 250, units = "mm", dpi = 300, bg = "white")

# Setup -------------------------------------------------------------------

# Full results of all Tiles X all Bands
result_table <- read.csv("results/2025-10-20_main.csv")

# Subset: High Canopy:
# high_canopy <- filter(result_table, Location == "Brazil" | Location == "Malaysia" | Location == "USA East" | Location =="USA West")
# tol_muted_11 <- c("#332288",  "#44AA99","#DDCC77",  "#AA4499")

# Subset: Strong responders
# subset_table <- result_table %>%
  # filter(band %in% c("Blue", "NIR", "NIR2", "RedEdge"))

# Colour Scales:
band_map <- c( Blue = "02",  Green  = "03",  Red = "04",  RedEdge= "05",  
               NIR = "08", NIR2 = "8A",  SWIR1  = "11",  SWIR2  = "12")
band_names <- paste( band_map[unique(result_table$band)], collapse = "+")


cbf_colors <- c(  Blue     = "#0072B2", Green    = "#009E73", Red      = "#D55E00", RedEdge  = "#CC79A7",
                  NIR      = "#9E0142", NIR2     = "#5E4FA2", SWIR1    = "#E6AB02", SWIR2    = "#999999")

tol_muted_11 <- c("#332288",  "#6699CC",  "#88CCEE",  "#44AA99",  "#117733",  "#999933",  
                  "#DDCC77",  "#661100",  "#CC6677",  "#882255",  "#AA4499")

int_colors <- c(ALL = "#009E73", Blue = "#88CCEE", High = "#DDCC77", Low = "#CC79A7", RGB = "#882255" )

# For interactions:
# cbf_colors <- int_colors


# Spectral Line -----------------------------------------------------------

#### With Lables ####
label_data <- result_table %>%
  group_by(band, abs_increment) %>%
  summarise(avg_difference_percent = mean(avg_difference_percent),
            .groups = "drop") %>%
  group_by(band) %>%
  filter(abs_increment == max(abs_increment))


ggplot(result_table, aes(x = abs_increment, y = avg_difference_percent,
                         color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  geom_text_repel(data = label_data,
                  aes(label = band),
                  direction = "y",
                  hjust = 0,
                  nudge_x = 0.5,
                  segment.color = NA,
                  size = 4,
                  show.legend = FALSE) +
  scale_color_manual(values = cbf_colors,  breaks = c("Blue", "NIR2", "NIR", "Green","SWIR2", "Red", "SWIR1", "RedEdge")) +
  scale_fill_manual(values = cbf_colors, breaks = c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge")) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
    labs(x = "Manipulation Degree [%]", y = "Average Relative Difference [%]") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")



##### With legend ####
# result_table$band <- factor(result_table$band, levels = c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge"))
ggplot(result_table, aes(x = abs_increment, y = avg_difference_percent, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = cbf_colors, breaks=c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge")) +
  scale_fill_manual(values = cbf_colors, breaks=c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge")) +
  labs(x = "Manipulation Degree [%]", y = "Average Relative Difference [%]",
       color = "Band", fill = "Band") +
  theme_minimal(base_size = 14)



# Spectral Facett ---------------------------------------------------------

# Facetted by band line plot
ggline(
  result_table,
  x = "increment",
  y = "average_difference",
  color = "band",
  fill = "band",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  facet.by = "band",
  scales = "fixed",
  ncol = 2
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Difference [m]",
    color = "Band"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "none"  )


# Boxplot -----------------------------------------------------------------

# Absolute diff Facett by band + points
ggboxplot(subset_table, x = "increment",  y = "average_difference",
          fill = "band",  color = "black",
          palette = cbf_colors,  facet.by = "band",  scales = "fixed", alpha = 0.8, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  labs(x = "Manipulation [%]", y = "Average Difference [m]") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")

# Relative diff Facett by band + points
ggboxplot(result_table, x = "increment",  y = "avg_difference_percent",
          fill = "band",  color = "black",
          palette = cbf_colors,  facet.by = "band",  scales = "fixed", alpha = 0.8, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  scale_x_discrete(
    breaks = c("-20", "-10", "0", "10", "20")  # specify which factor levels to show
  ) +  labs(x = "Manipulation [%]", y = "Average Relative Difference [%]") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")



# Grouped Tiles ----------------------------------------------------------------

## Relative Difference Groups
label_data <- result_table %>%
  group_by(Location, abs_increment) %>%
  summarise(avg_difference_percent = mean(avg_difference_percent),
            .groups = "drop") %>%
  group_by(Location) %>%
  filter(abs_increment == max(abs_increment))


ggplot(result_table, aes(x = abs_increment, y = avg_difference_percent,  color = Location)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  geom_text_repel(data = label_data, aes(label = Location),
    direction = "y",        # repel vertically only
    hjust = 0,  nudge_x = 0.5,          # push labels to the right
    segment.color = NA,     # no connecting lines
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("#332288",  "#117733","#117733","#117733","#117733","#117733",
                                "#DDCC77", "#882255","#882255","#882255", "#882255"),
                                  breaks = c("Finland", "Australia", "Mongolia", "Poland",  "Germany" ,"Cameroon",
                                            "Switzerland", "USA East", "USA West", "Malaysia", "Brazil"))+
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))+
  labs(x = "Manipulation Degree [%]", y = "Average Relative Difference [%]" ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")

## Average difference group plot
label_data_2 <- result_table %>%
  group_by(Location, abs_increment) %>%
  summarise(average_difference = mean(average_difference),
            .groups = "drop") %>%
  group_by(Location) %>%
  filter(abs_increment == max(abs_increment))

ggplot(result_table, aes(x = abs_increment, y = average_difference,  color = Location)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  geom_text_repel(data = label_data_2, aes(label = Location),
                  direction = "y",        # repel vertically only
                  hjust = 0,  nudge_x = 0.5,          # push labels to the right
                  segment.color = NA,     # no connecting lines
                  size = 4,
                  show.legend = FALSE
  ) +
  scale_color_manual(values = c("#332288",  "#332288","#332288","#DDCC77","#DDCC77","#117733",
                                "#DDCC77", "#117733","#117733","#882255", "#882255"),
                     breaks = c("Finland", "Australia", "Mongolia", "Poland",  "Germany" ,"Cameroon",
                                "Switzerland", "USA East", "USA West", "Malaysia", "Brazil"))+
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))+
  labs(x = "Manipulation Degree [%]", y = "Average Difference [m]" ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")




# Location Facett ----------------------------------------------------------------

plot_data <- result_table %>%
  group_by(band, Location, abs_increment) %>%
  summarise(avg_difference_percent = mean(avg_difference_percent, na.rm = TRUE), .groups = "drop")
plot_data$manipulation <- plot_data$abs_increment#*100

# Facetted by band location plot, avg relative difference
ggline(
  plot_data,
  x = "manipulation",
  y = "avg_difference_percent",
  color = "Location",
  fill  = "Location",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = tol_muted_11,
  facet.by = "band",
  scales = "fixed",
  ncol = 2
) +
  geom_hline(
    yintercept = c(-100,-50,50,100),
    linetype = "dashed",
    color = "grey85",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]",
    color = "Location",
    fill  = "Location"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom"
  )


# Single band by location -------------------------------------------------


band_color <- "NIR"

# Example color-blind–friendly palette (viridis)
cb_palette <- viridis::viridis(length(unique(result_table$Location)), option = "D")

tol_muted_11_alternative <- c("#DDCC77", "#AA4499", "#44AA99", "#CC6677", "#661100",
                              "#117733", "#882255", "#332288", "#88CCEE", "#999933", "#6699CC" )

# Order Legend entries
  # average_difference
  # avg_difference_percent
legend_order <- result_table %>%
  filter(band == band_color, increment == 25) %>%
  arrange(desc(avg_difference_percent)) %>%   # top to bottom
  pull(Location)

plot_data <- result_table %>%
  filter(band == band_color) %>%
  mutate(Location = factor(Location, levels = legend_order))


ggplot( filter(plot_data),  # select band # alternatively ->  ,band == band_color
        aes(x = increment, y = average_difference,
        color = Location, group = Location) ) +
  
  geom_ribbon( # Add shaded SD ribbon
    aes(ymin = average_difference - std_dev,
        ymax = average_difference + std_dev,
        fill = Location),
    alpha = 0.2, color = NA ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Color-blind–friendly scales
  scale_color_manual(values = tol_muted_11_alternative) +
  scale_fill_manual(values = tol_muted_11_alternative) +
  
  labs(x = "Manipulation [%]",
    y = "Average Difference [m]",
    # y = "Average Relative Difference [%]",
    color = "Location", fill = "Location") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# Facett Location and Band ------------------------------------------

# Relative difference
ggplot(result_table, aes(x = abs(increment), y = avg_difference_percent, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  scale_fill_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  facet_wrap(~Location, ncol = 3) +
  labs(x = "Manipulation Degree [%]", y = "Mean Relative Difference [%]") +
  theme_minimal()


# Compact version with changed legend:
ggplot(result_table, aes(x = abs(increment), y = average_difference, color = band, fill = band)) +
  geom_point(alpha = 0.4, size = 3) +  # control point size
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = cbf_colors, 
                     breaks = c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  scale_fill_manual(values = cbf_colors, 
                    breaks = c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  facet_wrap(~Location, ncol = 3) +
  labs(x = "Manipulation Degree [%]", y = "Mean Difference [m]", color = "Band", fill = "Band") +
  theme_minimal() +
  theme(legend.position = c(0.86, 0.06),   # x = 0.95 (right), y = 0.05 (bottom)
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.direction = "horizontal",
        legend.box = "vertical",                     # allows title above
        legend.spacing.x = unit(0.5, "cm"),         # horizontal spacing between keys
        legend.key.size = unit(1, "cm"),            # size of legend keys
        legend.margin = margin(5, 5, 5, 5),         # padding inside legend box
        legend.box.just = "center"                   # center legend in the box
  ) +
  guides(
    color = guide_legend(
      title.position = "top",                  # title above
      nrow = 2,                                # number of rows
      byrow = TRUE,                            # fill rows first
      label.position = "bottom",               # labels below keys
      keywidth = unit(1.3, "cm"),               # width of keys
      keyheight = unit(0.5, "cm")             # height of keys
    ),
    fill = guide_legend(
      title.position = "top",
      nrow = 2,
      byrow = TRUE,
      label.position = "bottom",
      keywidth = unit(1, "cm"),
      keyheight = unit(0.5, "cm")
    )
  )

