library(terra)
library(tidyverse)
library(dplyr)
library(landscapemetrics)
library(ggplot2)
library(viridis)

f <- list.files("/home/emilio/canopy_height/results/originals/", full.names = T)

# Categories
orig <- list.files("/home/emilio/canopy_height/results/originals/", full.names = T)
path_to_files <- "/data/ESA99/export/2026-01-19/difference_rasters/"
categories <- c(15,30)
TILENAME <- "35VML"
# All tiles: "10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"

orig_ras <- rast(str_subset(orig, TILENAME))
plot(orig_ras)

cat_ras <- classify(orig_ras, rbind(c(-Inf, categories[1], 1), c(categories[1], categories[2], 2), c(categories[2], Inf, 3)))
plot(cat_ras)

diff <- list.files(path_to_files, pattern = TILENAME, full.names = T)
diff <- str_subset(diff, "original", negate = T)

diff_ras <- rast(diff[1])
plot(diff_ras)

# Calculate mean per category
mean_per_category <- zonal(diff_ras, cat_ras, fun = "mean", na.rm = TRUE)

# Calculate counts per category
cat_counts <- freq(cat_ras)
total_cells <- sum(cat_counts$count)
mean_per_category$proportion <- round(cat_counts$count / total_cells *100, 1)

mean_per_category$category <- c("Low", "Medium", "High")
mean_per_category


# Zone Stats --------------------------------------------------------------

# Initialize list to store results
results_list <- list()

for (zone_val in 1:3) {
  
  # Mask raster to current category
  zone_mask <- mask(diff_ras, cat_ras == zone_val, maskvalues = FALSE)
  
  # Values in this zone
  vals <- values(zone_mask, na.rm = TRUE)
  
  # Skip empty zones
  if(length(vals) == 0) next
  
  results_list[[zone_val]] <- data.frame(
    zone = zone_val,
    category = c("Low", "Medium", "High")[zone_val],
    mean = mean(vals),
    mean_positive = ifelse(any(vals > 0), mean(vals[vals > 0]), NA),
    mean_negative = ifelse(any(vals < 0), mean(vals[vals < 0]), NA),
    mean_abs = mean(abs(vals)),
    prop_positive = sum(vals > 0)/length(vals) * 100,
    prop_negative = sum(vals < 0)/length(vals) * 100,
    prop_zero = sum(vals == 0)/length(vals) * 100
  )
}

zone_stats <- bind_rows(results_list)
zone_stats



# Diff by category --------------------------------------------------------

# Define categories
categories <- c(15, 30)
category_labels <- c("Low", "Medium", "High")

# # Create categorical raster once
# cat_ras <- classify(orig_ras, 
#                     rbind(c(-Inf, categories[1], 1), 
#                           c(categories[1], categories[2], 2), 
#                           c(categories[2], Inf, 3)))

# Initialize list to store results
all_results <- list()

# Loop over all difference rasters
for (diff_file in diff) {
  
  # Read difference raster
  diff_ras <- rast(diff_file)
  
  # Calculate mean per category
  mean_per_category <- zonal(diff_ras, cat_ras, fun = "mean", na.rm = TRUE)
  
  # Calculate proportion of each category
  cat_counts <- freq(cat_ras)
  total_cells <- sum(cat_counts$count)
  mean_per_category$proportion <- round(cat_counts$count / total_cells * 100, 1)
  
  # Add category labels
  mean_per_category$category <- category_labels
  
  # Add raster name
  mean_per_category$raster_file <- basename(diff_file)
  
  # Append to list
  all_results[[diff_file]] <- mean_per_category
}

# Combine all results into one data frame
results_df <- bind_rows(all_results)

# Pivot all raster columns into a long format
results_long <- results_df %>%
  pivot_longer(
    cols = starts_with(TILENAME),  # all raster mean columns
    names_to = "raster_name",
    values_to = "mean"
  ) %>%
  drop_na(mean)  # remove rows where the mean is NA

# Optional: keep category, proportion, and raster_file columns
results_long <- results_long %>%
  select(raster_file, raster_name, category, mean, proportion)

results_long


ggplot(results_long, aes(x = category, y = mean)) +
     geom_boxplot(aes(fill = category)) +
     theme_minimal() +
     labs(title = "Distribution of mean values per category across all rasters") +
     scale_fill_viridis_d()



# UNCATEGORIZED: ----------------------------------------------------------


for (i in 1:11) {
  r <- rast(f[i])
   
  m <- global(r, fun = "mean", na.rm = TRUE)[[1]]
  max <- global(r, fun = "max", na.rm = TRUE)[[1]]
  sd <- global(r, fun = "sd", na.rm = TRUE)[[1]]
  shannon_df <- lsm_l_shdi(r)
  shannon_val <- shannon_df$value[1]
     
  cat(basename(f[i]), "mean height ", m, "SD ",sd,"max ", max,"Shannon ",shannon_val,"\n")
    
}


files <- f

result_list <- lapply(files, function(fpath) {
  r <- rast(fpath)
  r_cat <- classify(r, rbind(c(-Inf, 15, 1), c(15, 30, 2), c(30, Inf, 3)))
  tab <- freq(r_cat)
  tab <- tab[!is.na(tab$value), ]
  tab$percent <- (tab$count / sum(tab$count)) * 100
  tab$category <- c("low <15", "medium 15-29", "high >=30")[tab$value]
  data.frame(raster = basename(fpath), category = tab$category, percent = tab$percent)
})

all_rasters_percent <- do.call(rbind, result_list)
all_rasters_percent








result_list <- lapply(files, function(fpath) {
  r <- rast(fpath)
  
  # Categorize into three classes: 1 = <15, 2 = 15–29.999, 3 = >=30
  r_cat <- classify(r, rbind(
    c(-Inf, 15, 1),
    c(15, 30, 2),
    c(30, Inf, 3)
  ))
  
  # Count pixels per category
  tab <- freq(r_cat)
  tab <- tab[!is.na(tab$value), ]
  
  # Compute percentages
  tab$percent <- round((tab$count / sum(tab$count)) * 100, 1)
  
  # Add readable category names
  category_names <- c("low <15", "medium 15-29", "high >=30")
  tab$category <- category_names[tab$value]
  
  data.frame(raster = basename(fpath), category = tab$category, percent = tab$percent)
})

all_rasters_percent <- do.call(rbind, result_list)
all_rasters_percent

all_rasters_percent$location


tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany",
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia",
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")
all_rasters_percent$Location <- factor(substr(all_rasters_percent$raster, 1,5), levels = names(tile_label), labels = tile_label)

# substr(all_rasters_percent$raster, 1,5)

# Pivot wider
all_rasters_wide <- all_rasters_percent %>%
  select(category, percent, Location) %>%   # ensure we only keep needed columns
  pivot_wider(
    names_from = category,   # new columns come from 'category'
    values_from = percent    # fill them with 'percent'
  )%>%
  arrange(Location) 

# View result
all_rasters_wide


### Plot

all_rasters_long <- all_rasters_wide %>%
  pivot_longer(
    cols = starts_with(c("low", "medium", "high")),  # all category columns
    names_to = "Category",
    values_to = "Percent"
  )


# Optional: clean up category names
all_rasters_long$Category <- recode(all_rasters_long$Category,
                                    "low <15" = "Low",
                                    "medium 15-29" = "Medium",
                                    "high >=30" = "High")


all_rasters_long <- all_rasters_long %>%
  mutate(Location = factor(Location, levels = rev(sort(unique(Location)))))

all_rasters_long <- all_rasters_long %>%
  mutate(
    Category = factor(Category, levels = c("High", "Medium", "Low"))
  )

# Horizontal stacked bar plot
ggplot(all_rasters_long, aes(x = Location, y = Percent, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # horizontal bars
  # scale_fill_manual(values = c("Low" = "#4A7BB7", "Medium" = "#FEDA8B", "High" = "#F67E4B")) +
  scale_fill_manual(values = c("Low" = "#1B5E20", "Medium" = "#7FAE6C", "High" = "#CCDDAA")) +
  labs(
    x = "",
    y = "%",
    fill = "Tree height"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "top",
        legend.direction = "horizontal")

