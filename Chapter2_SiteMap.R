library(ozmaps)
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)


# Replace with your actual key
#register_stadiamaps(key = "b8debd2b-498f-4275-9786-ae7913ccc739")


# Load map data
aus_sf <- ozmap_data("states")
act_sf <- ozmap_data("abs_ste") |> filter(NAME == "Australian Capital Territory")

# Get ACT bounding box as red rectangle
act_box <- st_as_sfc(st_bbox(act_sf))  # gives rectangle geometry

# Define full-Australia view
bbox_aus <- c(left = 109, bottom = -46, right = 160, top = -8.5)

# Create the map
p1 <- ggplot() +
  geom_sf(data = aus_sf, fill = "white", color = "black") +
  geom_sf(data = act_box, fill = NA, color = "red", linewidth = 1.2) +
  #annotation_scale(location = "bl", bar_cols = c("black", "white"), width_hint = 0.25) +
  #annotation_north_arrow(location = "tr", which_north = "true") +
  coord_sf(
    xlim = c(bbox_aus["left"], bbox_aus["right"]),
    ylim = c(bbox_aus["bottom"], bbox_aus["top"]),
    expand=FALSE) +
  theme_void(base_size = 14)+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# ACT

# Matrix of study site coordinates (clockwise)
study_coords <- matrix(
  c( 149.155338, -35.328038,
     149.178409,-35.327877,
     149.178310,-35.346456,
     149.155535, -35.346536,
     149.155338, -35.328038), # repeat first point to close
  ncol = 2, byrow = TRUE
)

tnr_coords <- matrix(c( 148.890002,-35.457499,
                        148.916163,-35.457499,
                        148.916050,-35.478714,
                        148.890002,-35.478898,
                        148.890002,-35.457499),
                     ncol=2, byrow=TRUE)

# Create polygon geometry
study_polygon <- st_sfc(st_polygon(list(study_coords)), crs = 4326)
tnr_polygon <- st_sfc(st_polygon(list(tnr_coords)), crs = 4326)


study_bbox <- st_bbox(study_polygon)
tnr_bbox <- st_bbox(tnr_polygon)

buffer <- 0.05

zoom_bbox <- c(
  xmin = study_bbox["xmin"] - buffer,
  xmax = study_bbox["xmax"] + buffer,
  ymin = study_bbox["ymin"] - buffer,
  ymax = study_bbox["ymax"] + buffer)

zoom_tnr_bbox <- c(
  xmin = tnr_bbox["xmin"] - buffer,
  xmax = tnr_bbox["xmax"] + buffer,
  ymin = tnr_bbox["ymin"] - buffer,
  ymax = tnr_bbox["ymax"] + buffer)

cbr<- ggplot() +
  geom_sf(data = act_sf, fill = "white", color = "grey85") +
  geom_sf(data = study_polygon, fill = NA, color = "red", linewidth = 1.2) +
  geom_sf(data = tnr_polygon, fill = NA, color = "blue", linewidth = 1.2) +
  annotation_scale(location = "bl", bar_cols = c("black", "white"), width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  coord_sf(
    xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]),
    ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
    expand = FALSE
  ) +
  coord_sf(
    xlim = c(zoom_tnr_bbox["xmin"], zoom_tnr_bbox["xmax"]),
    ylim = c(zoom_tnr_bbox["ymin"], zoom_tnr_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_classic(base_size = 14)+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "grey85", color = NA)
  )+
labs(x = "Longitude", y = "Latitude")


final_plot <- ggdraw() +
  draw_plot(cbr) +  # main map
  draw_plot(p1, x = 0.587, y = 0.088, width = 0.2, height = 0.2)  # inset position

final_plot


ggsave("final_map.png", final_plot, dpi = 600, width = 7.74, height = 6.26, units = "in")
