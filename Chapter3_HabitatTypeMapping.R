# load library
library(sf)
library(RColorBrewer)
library(lubridate)
library(GISTools)
library(dplyr)

#---------------------------------------------------------------------------------
setwd("F:/HABITAT ANALYSIS/Used/vegetation transect data")
# available 
# transect id, quadrat id, habitat id, season, habtiat variables, mean temp, lat, lon
avail <- read.csv("vegetation transect data.csv", header = TRUE)
veg_gps <- read.csv("gpspoints veg transects.csv", header=TRUE)

avail <- avail |>
  mutate(date=dmy(date))


veg_gps <- veg_gps |>
  mutate(date = dmy(date))

avail <- left_join(avail, veg_gps, by=c("location", "transect", "quadrat"))

names(avail)

# Convert avail to an sf object with the same CRS as hab_sf_full
avail_sf <- st_as_sf(avail, coords = c("lon", "lat"), crs = 4326)  # Assuming avail uses WGS84 (EPSG:4326)


# shapefile 
setwd("F:/HABITAT ANALYSIS/Used/Shapefiles")

hab_sf <- st_read("AMTECH_Vegetation_-_Broad_Structural_Groups.shp")
hab_sf$Structural
hab_sf$habitat_type <- NA # create new column for assigning new habitat types - confirmed with Will Osborne


# Simplify structural groups into habitat types
hab_sf$habitat_type[1] <- "Natural temperate grassland"
hab_sf$habitat_type[2] <- "Exotic pasture"
hab_sf$habitat_type[3] <- "Natural temperate grassland"
hab_sf$habitat_type[4] <- "Exotic pasture"
hab_sf$habitat_type[5] <- "Exotic pasture"
hab_sf$habitat_type[6] <- "Exotic pasture"
hab_sf$habitat_type[7] <- "Exotic pasture"
hab_sf$habitat_type[8] <- "Natural temperate grassland"
hab_sf$habitat_type[9] <- "Natural temperate grassland"
hab_sf$habitat_type[10] <- "Exotic pasture"
hab_sf$habitat_type[11] <- "Exotic pasture"
hab_sf$habitat_type[12] <- "Exotic pasture"
hab_sf$habitat_type[13] <- "Exotic pasture"
hab_sf$habitat_type[14] <- "Exotic pasture"
hab_sf$habitat_type[15] <- "Exotic pasture"
hab_sf$habitat_type[16] <- "Natural temperate grassland"
hab_sf$habitat_type[17] <- "Natural temperate grassland"
hab_sf$habitat_type[18] <- "Natural temperate grassland"
hab_sf$habitat_type[19] <- "Natural temperate grassland"
hab_sf$habitat_type[20] <- "Exotic pasture"
hab_sf$habitat_type[21] <- "Exotic pasture"
hab_sf$habitat_type[22] <- "Exotic pasture"
hab_sf$habitat_type[23] <- "Exotic pasture"
hab_sf$habitat_type[24] <- "Native pasture"


# Fill in the gaps
# create a convex hull that wraps around the habitat polygons
hull <- st_convex_hull(st_union(hab_sf))

# spatial difference to find the blank areas
blank_areas <- st_difference(hull, st_union(hab_sf))

#assign a new habitat type to the blank areas
# create new df for blank areas with the "Unclassified" habitat type
blank_areas_sf <- st_sf(geometry = blank_areas, habitat_type = "Native pasture")

# add missing columns to blank_areas_sf, filling with NA values
blank_areas_sf$Structural <- NA  # Add each missing column one by one
blank_areas_sf$GlobalID<- NA  # Continue until all columns match

# combine the original habitat polygons with the blank areas
hab_sf_full <- rbind(hab_sf, blank_areas_sf)

habitat_types <- unique(hab_sf_full$habitat_type)
hab_types <- unique(hab_sf_full$Structural)

# re-plot the habitat polygons including the blank areas
# Assign colors to the habitat types, including the new "Unclassified" category
habitat_types_full <- unique(hab_sf_full$habitat_type)


# Assign colors again to include unclassified areas
if (length(habitat_types_full) > 12) {
  colors_full <- setNames(colorRampPalette(brewer.pal(12, "Set3"))(length(habitat_types_full)), habitat_types_full)
} else {
  colors_full <- setNames(brewer.pal(length(habitat_types_full), "Set3"), habitat_types_full)
}


# Transform to match hab_sf_full if necessary
avail_sf <- st_transform(avail_sf, st_crs(hab_sf_full))

png("habitat_map.png", width=6, height=6, units="in", res=300)
par(mar = c(1,1,1,1))
# Plot the full map with blank areas

colors_full <- c("Natural temperate grassland" = "#5d646f",
                 "Exotic pasture" = "#7678ed",
                 "Native pasture" = "#f7b801")


plot(st_geometry(hab_sf_full), col = colors_full[hab_sf_full$habitat_type],
     main = "")

# Add the avail locations in red
plot(st_geometry(avail_sf), col="black",bg="red", pch = 21, add = TRUE)


# Add a legend
legend("topright", legend = names(colors_full), fill = colors_full, 
       title = "Macrohabitat Type", border = "black", bty = "n", cex = 1)
legend("topright", legend = "Vegetation quadrats", col = "black", pt.bg="red", pch = 21,
       bty = "n", cex = 1, inset = c(0.056, 0.122), x.intersp=1.5)


dev.off()

# Calculate the area for each polygon (in square meters)
hab_sf_full$area_sqm <- st_area(hab_sf_full)

# Convert to hectares (optional)
hab_sf_full$area_ha <- hab_sf_full$area_sqm / 10000
hab_sf_full$area_sqm <- as.numeric(hab_sf_full$area_sqm)
hab_sf_full$area_ha <- as.numeric(hab_sf_full$area_ha)

total_site_area <- sum(hab_sf_full$area_ha)
hab_sf_full$total_site_area <- total_site_area
area_summary <- aggregate(area_ha ~ habitat_type, data = hab_sf_full, sum)

# Add the total habitat type area for each row in hab_sf_full
hab_sf_full <- merge(hab_sf_full, area_summary, by = "habitat_type", suffixes = c("", "_total"))
# Rename the column for clarity
names(hab_sf_full)[names(hab_sf_full) == "area_ha_total"] <- "total_habitat_type_area"

# Summarise the total area by habitat type
area_summary$area_ha <- as.numeric(area_summary$area_ha)


# View the summary
print(area_summary) # summary of ha available for each habitat type

# ------------------------------------------------------------------------------

# Add habitat types to gps points in available df

# check that both df's have same CRS 
avail_sf <- st_transform(avail_sf, st_crs(hab_sf_full))

#spatial join
avail_with_habitat <- st_join(avail_sf, hab_sf_full["habitat_type"], left=TRUE)

avail3 <- as.data.frame(avail_with_habitat)

names(avail3)[names(avail3) == "habitat_type.y"] <-"Habitat_type"

head(avail3)

#setwd("F:/HABITAT ANALYSIS/Used/vegetation transect data")
#write.csv(avail3, "avail3.csv")

#-------------------------------------------------------------------------------

# Add habitat types to gps points in used df

setwd("F:/HABITAT ANALYSIS/Used")
used <- read.csv("used3.csv")
head(used)

str(used)
# format date
used <- used |>
  mutate(date = dmy(date))|>
  glimpse()

#remove 2022
used <- used |>
  filter(!year(date) %in% c(2022))|>
  glimpse()

head(used)


#----------------------------------------------------------------------------------
colnames(used)
class(used)


# Clean and convert columns to numeric values
used <- used |>
  mutate(lon = as.numeric(gsub("c\\(", "", geometry)),  # Remove "c(" and convert to numeric
         lat = as.numeric(gsub("\\)", "", trimws(X))))          # Remove ")" and convert to numeric

# Check if conversion worked
head(used[, c("lon", "lat")])

used_sf <- st_as_sf(used, coords = c("lon", "lat"), crs = st_crs(hab_sf_full))

# Verify the geometry
st_geometry(used_sf)

# Ensure used_sf has the same CRS as hab_sf_full
used_sf <- st_transform(used_sf, st_crs(hab_sf_full))

# Perform spatial join to assign habitat types
used_sf <- st_join(used_sf, hab_sf_full, left = TRUE)

# Count how many used points fall within each habitat type
used_habitat_counts <- used_sf %>%
  st_drop_geometry() %>%
  group_by(habitat_type) %>%
  summarise(Used = n(), .groups = "drop") %>%
  mutate(Proportion_Used = Used / sum(Used))  # Convert to proportions

# Print results
print(used_habitat_counts)

# Calculate habitat availability
availability <- hab_sf_full %>%
  group_by(habitat_type) %>%
  summarise(Available_Area = sum(area_ha), .groups = "drop") %>%
  mutate(Proportion_Available = Available_Area / sum(Available_Area))

# Print results
print(availability)


# Merge used and available habitat data
habitat_comparison <- left_join(used_habitat_counts, availability, by = "habitat_type")

# Fill NA values with 0 (in case some habitat types were not used)
habitat_comparison[is.na(habitat_comparison)] <- 0

print(habitat_comparison)

# Perform Chi-Square Test
chisq_test <- chisq.test(habitat_comparison$Used, 
                         p = habitat_comparison$Proportion_Available)

print(chisq_test)

# Calculate Selection Ratio (Wi)
habitat_comparison <- habitat_comparison %>%
  mutate(Selection_Ratio = Proportion_Used / Proportion_Available)

print(habitat_comparison)


unique(used_habitat_counts$habitat_type)
unique(availability$habitat_type)

expected_counts <- habitat_comparison$Proportion_Available * sum(habitat_comparison$Used)

chisq_test <- chisq.test(habitat_comparison$Used, p = expected_counts / sum(expected_counts))

print(chisq_test)

habitat_comparison <- habitat_comparison %>%
  mutate(Proportion_Available = ifelse(Proportion_Available == 0, 1e-6, Proportion_Available),
         Selection_Ratio = Proportion_Used / Proportion_Available)

print(habitat_comparison)

habitat_comparison_clean <- habitat_comparison %>%
  select(-geometry)

print(habitat_comparison_clean)

colnames(used)[27] <- "habitat_type"
used$habitat_type[used$habitat_type == "Phalaris"] <- "Exotic pasture"

used$season <- case_when(
  month(used$date) %in% c(12,1,2) ~ "Summer 2023",
  month(used$date) %in% c(3,4,5) ~ "Autumn 2023",
  month(used$date) %in% c(6,7,8) ~ "Winter 2023",
  month(used$date) %in% c(9,10,11) ~ "Spring 2024"
)

# Perform seasonal habitat selection analysis
seasonal_results <- used %>%
  group_by(season, habitat_type) %>%
  summarise(Used = n(), .groups = "drop") %>%
  group_by(season) %>%
  #left_join(availability, by = "habitat_type") %>%
  mutate(Proportion_Used = Used / sum(Used)) %>%
  ungroup() %>%
  left_join(availability, by="habitat_type") %>%
  mutate(Selection_Ratio = Proportion_Used / Proportion_Available)

print(seasonal_results)

# test for seasonal differences in habitat selection
seasonal_table <- used %>%
  group_by(season, habitat_type) %>%
  summarise(Used = n(), .groups = "drop") %>%
  pivot_wider(names_from = "habitat_type", values_from = "Used", values_fill = 0)

# Perform Chi-Square Test for seasonal habitat selection
chisq_seasonal <- chisq.test(seasonal_table[,-1])  # Remove 'season' column before testing

print(chisq_seasonal)
# p = < 0.05

chisq.test(table(used$season, used$habitat_type))