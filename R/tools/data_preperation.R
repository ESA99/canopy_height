### Data preperation of result tables GCHM ###
library(dplyr)


# Read data ---------------------------------------------------------------

A <-read.csv("results/2026-01-27_merged_Interactions_CLEAN.csv")
head(A)
B <-read.csv("results/2026-01-27_result_table_interactions_2T.csv")
head(B)

result_table <- B

# Check and add 0-Increment Row -------------------------------------------

# Checks for tile-band combinations without 0-Increment row and adds them if needed
add_zero_step_rows <- function(result_table) {
  
  # 1: Identify which combinations are missing a zero increment row
  missing_zero_rows <- result_table %>%
    distinct(tile, band) %>%  # Identify all tile-band combinations
    anti_join(
      result_table %>% filter(increment == 0), # check which combinations are missing
      by = c("tile", "band")
    )
  
  if (nrow(missing_zero_rows) == 0) {
    message("No columns missing")
  } else {
    
    # 2: Create the zero increment rows
    zero_increment_rows <- missing_zero_rows %>%
      mutate(
        increment = 0,
        decrease = "True",
        out_name = paste(tile, band, "original", sep = "_"),
        year = 2020
      )
    
    # 3: Add missing columns
    missing_cols <- setdiff(names(result_table), names(zero_increment_rows))
    zero_increment_rows[missing_cols] <- 0.0  # Add all missing columns as 0.0
    
    # 4: Combine with original table
    new_table <- bind_rows(result_table, zero_increment_rows)
    
    cat(nrow(missing_zero_rows), "rows added to table.\n")
    return(new_table)
  }
}
result_table <- add_zero_step_rows(result_table)

# Adjust and add columns --------------------------------------------------

### Turn Increment into negative and positive percent values ###
result_table <- result_table %>%
  mutate(increment = increment * 100) %>% 
  mutate(increment = ifelse(decrease == "True", -abs(increment), abs(increment)))
# Absolute increment column
result_table$abs_increment <- abs(result_table$increment)


### Add Location column as factor based on tiles ###
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany",
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia",
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")

result_table$Location <- factor(result_table$tile, levels = names(tile_label), labels = tile_label)


### Add original T/F column ###
result_table$original <- grepl("original", result_table$out_name)

#### Add tile_band column ###
# Add if desired
# result_table <- result_table %>%
#   mutate(tile_band = paste(tile, band, sep = "_"))

stop()

# Band name translation (NON-Interaction) ----------------------------------
# Convert Band names to factor
band_labels <- c("B02" = "Blue", "B03" = "Green",  "B04" = "Red",  "B08" = "NIR",
                 "B05" = "RedEdge", "B8A" = "NIR2",  "B11" = "SWIR1",  "B12" = "SWIR2")
result_table$band <- factor(result_table$band, levels = names(band_labels), labels = band_labels)


# Interactions ------------------------------------------------------------
# Use for interaction tables only

### Rename bands to interaction group names ###
result_table <- result_table %>%
  mutate(
    band = recode(
      band,
      "B02-B03-B04-B05-B08-B8A-B11-B12" = "ALL",
      "B04-B11-B12"                    = "Low",
      "B02-B05-B08-B8A"                = "High",
      "B02"                            = "Blue",
      "B02-B03-B04"                    = "RGB"
    )
  )


# Merge Tables ------------------------------------------------------------
# Rename a column to match if needed
# names(result_table)[names(result_table) == "avg_differece_percent"] <- "avg_difference_percent"

Table_A <- A
Table_B <- result_table

merged <- rbind(Table_A, Table_B)


# Export table ------------------------------------------------------------

# Band names for export names -> NON INTERACTION tables only
band_names <- paste(gsub("\\D", "", unique(merged$band)), collapse = "+")


write.csv(merged, "results/2026-01-29_merged_Interactions_11T.csv", row.names = F)

