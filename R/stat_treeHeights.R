
f <- list.files("/home/emilio/canopy_height/results/originals/", full.names = T)
library(landscapemetrics)

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
  scale_fill_manual(values = c("Low" = "#4A7BB7", "Medium" = "#FEDA8B", "High" = "#F67E4B")) +
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

