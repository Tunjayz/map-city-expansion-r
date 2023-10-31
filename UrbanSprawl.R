# libraries we need
libs <- c(
  "tidyverse", "sf", "osmdata",
  "terra", "httr", "XML"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. FETCH ALL RASTER LINKS
#--------------------------
# website
url <-
  "https://glad.umd.edu/users/Potapov/GLCLUC2020/Built-up_change_2000_2020/"

get_raster_links <- function() {
  res <- httr::GET(url) # make http request
  parse <- XML::htmlParse(res) # parse data to html format
  links <- XML::xpathSApply( # scrape all the href tags
    parse,
    path = "//a", XML::xmlGetAttr, "href"
  )
  lnks <- links[-c(1:5)] # grab links
  for (l in lnks) { # make all links and store in a list
    rlinks <- paste0(url, lnks)
  }
  
  return(rlinks)
}

rlinks <- get_raster_links()

# 1. GET BUILT-UP DATA FOR NAIROBI lat ~ 01S, long ~ 36E
#-------------------------------------------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load_builtup_data <- function() {
  l <- rlinks[grepl("00N_030E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()


# 2. GET NAIROBI BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Nairobi, Kenya"
# define longlat projection

nairobi_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

terra::plot(builtup_data)
plot(nairobi_border, add = T)

# 3. CROP NAIROBI RASTER
# METHOD 1: POLYGON
#-------------------
crop_builtup_data_with_polygon <- function() {
  nairobi_vect <- terra::vect(nairobi_border)
  nairobi_raster <- terra::crop(builtup_data, nairobi_vect)
  nairobi_raster_cropped <- terra::mask(
    nairobi_raster, nairobi_vect
  )
  return(nairobi_raster_cropped)
}

nairobi_raster_cropped <- crop_builtup_data_with_polygon()
terra::plot(nairobi_raster_cropped)

# 3. CROP NAIROBI RASTER
# METHOD 2: BOUNDING BOX
#-----------------------
bbox <- sf::st_bbox(nairobi_border)
bbox_poly <- sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(
      bbox["xmin"], bbox["xmax"],
      bbox["xmax"], bbox["xmin"], bbox["xmin"]
    ),
    c(
      bbox["ymin"], bbox["ymin"],
      bbox["ymax"], bbox["ymax"], bbox["ymin"]
    )
  ))),
  crs = crsLONGLAT
)

crop_builtup_data_with_bbox <- function() {
  nairobi_vect <- terra::vect(bbox_poly)
  nairobi_raster <- terra::crop(builtup_data, nairobi_vect)
  nairobi_raster_cropped <- terra::mask(
    nairobi_raster, nairobi_vect
  )
  return(nairobi_raster_cropped)
}

nairobi_raster_cropped <- crop_builtup_data_with_bbox()
terra::plot(nairobi_raster_cropped)

# 3. MAKE BUFFER AROUND NAIROBI
# METHOD 3: BUFFER
#----------------------------
get_buffer <- function() {
  nairobi_cents <- sf::st_centroid(nairobi_border)
  nairobi_circle <- sf::st_buffer(
    nairobi_cents,
    dist = units::set_units(30, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(nairobi_circle)
}

nairobi_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = nairobi_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = nairobi_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  nairobi_vect <- terra::vect(nairobi_circle)
  nairobi_raster <- terra::crop(builtup_data, nairobi_vect)
  nairobi_raster_cropped <- terra::mask(
    nairobi_raster, nairobi_vect
  )
  return(nairobi_raster_cropped)
}

nairobi_raster_cropped <- crop_builtup_data()
terra::plot(nairobi_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  nairobi_df <- terra::as.data.frame(
    nairobi_raster_cropped,
    xy = T
  )
  
  return(nairobi_df)
}

nairobi_df <- raster_to_df()
head(nairobi_df)
names(nairobi_df)[3] <- "value"

# define categorical values
nairobi_df$cat <- round(nairobi_df$value, 0)
nairobi_df$cat <- factor(nairobi_df$cat,
                       labels = c("no built-up", "new", "existing")
)

# 5. GET NAIROBI ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(nairobi_border)
  roads <- bbox |>
    opq() |>
    add_osm_feature(
      key = "highway",
      value = road_tags
    ) |>
    osmdata::osmdata_sf()
  
  return(roads)
}

roads <- get_osm_roads()
nairobi_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = nairobi_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = nairobi_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP NAIROBI ROADS WITH BUFFER
#--------------------------------
nairobi_roads_cropped <- sf::st_intersection(
  nairobi_roads, nairobi_circle
)

ggplot() +
  geom_sf(
    data = nairobi_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = nairobi_roads_cropped, fill = "transparent",
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 7. MAP
#-------
colrs <- c(
  "grey20", "#FCDD0F", "#287DFC"
)

p <- ggplot() +
  geom_raster(
    data = nairobi_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = nairobi_roads_cropped,
    color = "grey20",
    size = .1,
    alpha = 1,
    fill = "transparent"
  ) +
  scale_fill_manual(
    name = "",
    values = colrs,
    drop = F
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(35, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "top"
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(.5, 1.05),
    legend.text = element_text(size = 12, color = "white"),
    legend.title = element_text(size = 14, color = "white"),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_line(color = "grey20", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 20, color = "grey80", hjust = .5, vjust = 2
    ),
    plot.caption = element_text(
      size = 9, color = "grey90", hjust = .5, vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = 0, b = 0, l = 0), "lines"
    ),
    plot.background = element_rect(fill = "grey20", color = NA),
    panel.background = element_rect(fill = "grey20", color = NA),
    legend.background = element_rect(fill = "grey20", color = NA),
    legend.key = element_rect(colour = "white"),
    panel.border = element_blank()
  ) +
  labs(
    x = "",
    y = NULL,
    title = "Nairobi",
    subtitle = "",
    caption = "©2023 EUGENE TULU  Data: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "Z:/Corporate/Extra/GIS/Urban Sprawl/nairobi_built_up.png",
  width = 6, height = 6, dpi = 600,
  device = "png", p
)