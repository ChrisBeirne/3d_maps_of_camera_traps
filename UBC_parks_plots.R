# Interactive/3d maps

# I could only get the highquality 3d render working with the development version
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(rayrender)
library(geoviz)
library(raster)
library(elevatr)
library(sf)

# For importing data
library(googledrive)
library(purrr)

# For importing data
library(googledrive)
library(purrr)

# Import your camera locations
# run this line, and enter "Yes". It will open up a browser window, and you'll have to allow access to your Google Drive account
x <- drive_find(n_max = 30)
1
# Create the file
dir.create("Data")

# Downloads all the files into a subfolder
drive_download("https://docs.google.com/spreadsheets/d/1lro9Q4iZ3fyk9FSkOmOpSmJMlqqqpbnDDYkfSgsqsZU"
    , type = "csv", path = paste("Data", "joffgari_aug2020", sep = "/"), 
    overwrite=T)

all.locs<- read.csv("Data/joffgari_aug2020.csv", header=T)
# Subset to the locations you want to plot
locs <- all.locs[substr(all.locs$Deployment.Location.ID,1,1)=="J",]

# Convert them to sf
tmp2 <- st_as_sf(locs,coords=c("Longitude", "Latitude"), crs=4326)

# Convert them to UTM (so that you can link xy distance to elevation (if you want realistic maps)st_transform
locs.m <- st_transform(tmp2,26910)
bounds <- st_as_sfc(st_bbox(locs.m))


# Get the elevation data for the defined limits
elevation <- get_elev_raster(locations = bounds, 
                             z=14) # The z value determines the zoom (higher numbers = more zoomed in)
# If you have issues with getting a DEM try a different provider -> src = c("aws", "gl3", "gl1", "alos", "srtm15plus"),
#?get_elev_raster

# Determine the elevation of the cameras (so you can plot them on your map)
locs.m$Elevation <- extract(elevation,locs.m)

# Check all is as it seems with a basic plot
#image(elevation, asp=1)
#points(st_coordinates(locs.m)[,1], st_coordinates(locs.m)[,2])
#points(st_coordinates(locs.m[locs.m$Deployment.Location.ID=="JOFF15",])[,1], st_coordinates(locs.m[locs.m$Deployment.Location.ID=="JOFF15",])[,2], pch=19)
# Create the matrix required for 3d plots
elmat = raster_to_matrix(elevation)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "imhof1") %>% # makes the green colour
  plot_map()

# Basic 3d map
#elmat %>%
#  sphere_shade(texture = "imhof1") %>% # makes the green colour
#  plot_3d(elmat, zscale = 3, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))


# FANCY 3d map with satellite imagery
################
sunangle <- 270

# Download the imagery and make the overlay
# You need a Mapbox API to make this work
mapbox_key <- "pk.eyJ1IjoiY3diZWlybmUiLCJhIjoiY2t1bmwyMmdhNDNqMDMybnowZXoxamE3NiJ9.gMl3Tx7e8_6TiwbPPNRtHw"
overlay_image <-
  slippy_overlay(
    elevation,
    image_source = "mapbox",
    image_type = "satellite",
    png_opacity = 0.6,
    api_key = mapbox_key
  )

# Draw the scene
scene <- elmat %>%
  sphere_shade(sunangle = sunangle, texture = "imhof1") %>%    #"bw"
  add_overlay(overlay_image)
# Remove previous plot
rgl::rgl.clear()
# Render it
rayshader::plot_3d(
  scene,
  elmat,
  zscale = 1, # How exagerated the elevation is (smaller = larger z scale)
  solid = TRUE , # Should your map have a solid base?
  #soliddepth = 100, # Where should th solid bit end
  solidcolor = "grey20",
  shadow = TRUE , # Should you have a shadow
  #shadowdepth = -50,
  background = "#F2E1D0", # Background colour
  shadowcolor = "#523E2B", # Shadowcolour,
  theta=215
)
#?plot_3d
# Low quality snapshot
#?render_snapshot

# add camera stations - clear previus additions first
render_points(clear_previous = TRUE)

# Add the points
render_points(extent = attr(elevation,"extent"),
              lat = st_coordinates(locs.m)[,2], long = st_coordinates(locs.m)[,1],
              altitude = (locs.m$Elevation)+10, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="red", size=6)


Sys.sleep(0.2)

render_highquality(filename="Plots/JOFF_render.png", # The file name you want!
                   # Light controls - if you supply multiple values you get multiple lightsources
                   lightdirection = c(-45,45), lightaltitude  = 100, clamp_value = 10,
                   samples = 400, 
                   # Camera locations
                   camera_lookat= c(0,-50,0), 
                   # The floor colour
                   ground_material= diffuse(color="#F2E1D0"),
                   # The size of the camera stations
                   point_radius=25, 
                   # The resultant plot size
                   width = 1200, height= 1200)








#######################################################
# GARI
locs <- all.locs[substr(all.locs$Deployment.Location.ID,1,1)=="G",]

# Convert them to sf
tmp2 <- st_as_sf(locs,coords=c("Longitude", "Latitude"), crs=4326)

# Convert them to UTM (so that you can link xy distance to elevation (if you want realistic maps)st_transform
locs.m <- st_transform(tmp2,26910)
bounds <- st_as_sfc(st_bbox(locs.m))


# Get the elevation data for the defined limits
elevation <- get_elev_raster(locations = bounds, 
                             z=14) # The z value determines the zoom (higher numbers = more zoomed in)
# If you have issues with getting a DEM try a different provider -> src = c("aws", "gl3", "gl1", "alos", "srtm15plus"),
#?get_elev_raster

# Determine the elevation of the cameras (so you can plot them on your map)
locs.m$Elevation <- extract(elevation,locs.m)

# Check all is as it seems with a basic plot
#image(elevation, asp=1)
#plot(st_geometry(locs.m), add=T)
plot(st_geometry(locs.m[locs.m$Deployment.Location.ID=="GARI05",]), add=T, pch=19)
# Create the matrix required for 3d plots
elmat = raster_to_matrix(elevation)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "imhof1") %>% # makes the green colour
  plot_map()

# Basic 3d map
#elmat %>%
#  sphere_shade(texture = "imhof1") %>% # makes the green colour
#  plot_3d(elmat, zscale = 3, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))


# FANCY 3d map with satellite imagery
################
sunangle <- 270

# Download the imagery and make the overlay
# You need a Mapbox API to make this work
mapbox_key <- "pk.eyJ1IjoiY3diZWlybmUiLCJhIjoiY2t1bmwyMmdhNDNqMDMybnowZXoxamE3NiJ9.gMl3Tx7e8_6TiwbPPNRtHw"
overlay_image <-
  slippy_overlay(
    elevation,
    image_source = "mapbox",
    image_type = "satellite",
    png_opacity = 0.6,
    api_key = mapbox_key
  )

# Draw the scene
scene <- elmat %>%
  sphere_shade(sunangle = sunangle, texture = "imhof1") %>%    #"bw"
  add_overlay(overlay_image)
# Remove previous plot
rgl::rgl.clear()
# Render it
rayshader::plot_3d(
  scene,
  elmat,
  zscale = 1, # How exagerated the elevation is (smaller = larger z scale)
  solid = TRUE , # Should your map have a solid base?
  #soliddepth = 100, # Where should th solid bit end
  solidcolor = "grey20",
  shadow = TRUE , # Should you have a shadow
  #shadowdepth = -50,
  background = "#F2E1D0", # Background colour
  shadowcolor = "#523E2B", # Shadowcolour,
  theta=215
)
#?plot_3d
# Low quality snapshot
#?render_snapshot

# add camera stations - clear previus additions first
render_points(clear_previous = TRUE)

# Add the points
render_points(extent = attr(elevation,"extent"),
              lat = st_coordinates(locs.m)[,2], long = st_coordinates(locs.m)[,1],
              altitude = (locs.m$Elevation)+10, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="red", size=6)


Sys.sleep(0.2)

render_highquality(filename="Plots/GARI_render.png", # The file name you want!
                   # Light controls - if you supply multiple values you get multiple lightsources
                   lightdirection = c(-45,45), lightaltitude  = 100, clamp_value = 10,
                   samples = 400, 
                   # Camera locations
                   camera_lookat= c(0,-50,0), 
                   # The floor colour
                   ground_material= diffuse(color="#F2E1D0"),
                   # The size of the camera stations
                   point_radius=25, 
                   # The resultant plot size
                   width = 1200, height= 1200)