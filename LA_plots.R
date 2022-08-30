# Interactive/3d maps

# I could only get the 3d render working with the development version
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

# Import your camera locations
# run this line, and enter "Yes". It will open up a browser window, and you'll have to allow access to your Google Drive account
x <- drive_find(n_max = 30)
1
# Create the file
dir.create("data/raw-data")

# Los Aigos data entry
# Data entry - target the data entry sheet
x <- drive_find(n_max = 30)
1
folder_url <- "https://drive.google.com/drive/u/0/folders/1nxFVm2Aib1m7TSNEkJjozP-x_GJtM1fo"
folder <- drive_get(as_id(folder_url))
dataentry_files <- drive_ls(folder, recursive=F)

tmp <- dataentry_files[dataentry_files$name %in% c("Entrada_data","Camera_locations_master"),]

# Downloads all the files into a subfolder
for (i in 1:nrow(tmp)) {
  drive_download(as_id(tmp$id[i]), type = "csv",
                 path = paste("Data/", tmp$name[i], sep = "/"), 
                 overwrite=T)
}


all.locs <- read.csv("Data/Camera_locations_master.csv", header=T, sep=",")

# Subset to the locations you want to plot
locs <- all.locs[all.locs$Group=="CM2",]

# Convert them to sf
tmp2 <- st_as_sf(locs,coords=c("Longitude", "Latitude"), crs=4326)

# Convert them to UTM (so that you can link xy distance to elevation (if you want realistic maps)st_transform
locs.m <- st_transform(tmp2, 32719)

bounds <- st_as_sfc(st_bbox(tmp2))


# Get the elevation data for the defined limits
elevation <- get_elev_raster(locations = bounds, 
                             z=14) # The z value determines the zoom (higher numbers = more zoomed in)
# If you have issues with getting a DEM try a different provider -> src = c("aws", "gl3", "gl1", "alos", "srtm15plus"),
#?get_elev_raster

# Determine the elevation of the cameras (so you can plot them on your map)
tmp2$Elevation <- extract(elevation,tmp2)

# Check all is as it seems with a basic plot
#image(elevation, asp=1)
#points(st_coordinates(locs.m)[,1], st_coordinates(locs.m)[,2])

# Create the matrix required for 3d plots
elmat = raster_to_matrix(elevation)

# We can another one of rayshader's built-in textures to make a somple map:
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
  soliddepth = 100, # Where should th solid bit end
  solidcolor = "grey20",
  shadow = TRUE , # Should you have a shadow
  shadowdepth = 70, # The height at which the show is cast - takes a bit of playing around to get right
  background = "#F2E1D0", # Background colour
  shadowcolor = "#523E2B" # Shadowcolour
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

render_highquality(filename="Plots/CM2_render.png", # The file name you want!
                   # Light controls - if you supply multiple values you get multiple lightsources
                   lightdirection = c(-45,45), lightaltitude  = 35, clamp_value = 10,
                   samples = 400, 
                   # Camera locations
                   camera_lookat= c(0,-50,0), 
                   # The floor colour
                   ground_material= diffuse(color="#F2E1D0"),
                   # The size of the camera stations
                   point_radius=25, 
                   # The resultant plot size
                   width = 1200, height= 1200)



####################################################################
# Repeat for CM1 ###################################################
####################################################################

# Subset to the locations you want to plot
locs <- all.locs[all.locs$Group=="CM1",]

# Convert them to sf
tmp2 <- st_as_sf(locs,coords=c("Longitude", "Latitude"), crs=4326)

# Convert them to UTM (so that you can link xy distance to elevation (if you want realistic maps)st_transform
locs.m <- st_transform(tmp2, 32719)
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

# Create the matrix required for 3d plots
elmat = raster_to_matrix(elevation)
# Sort out some naughty values
elmat[elmat<100] <- 100
# We can another one of rayshader's built-in textures to make a somple map:
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
  soliddepth = 100, # Where should th solid bit end
  solidcolor = "grey20",
  shadow = TRUE , # Should you have a shadow
  shadowdepth = 70, # The height at which the show is cast - takes a bit of playing around to get right
  background = "#F2E1D0", # Background colour
  shadowcolor = "#523E2B" # Shadowcolour
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

#Station
tmp <- data.frame(Latitude=-12.569167, Longitude=-70.100111)
tmp <- st_as_sf(tmp,coords=c("Longitude", "Latitude"), crs=4326)
tmp <- st_transform(tmp,32719)
tmp$Elevation <- extract(elevation,tmp)


# Add the research station
render_points(extent = attr(elevation,"extent"),
              lat = st_coordinates(tmp)[,2], long = st_coordinates(tmp)[,1],
              altitude = (tmp$Elevation)+30, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="orange", size=6)



Sys.sleep(0.2)

render_highquality(filename="Plots/CM1_render_2.png", # The file name you want!
                   # Light controls - if you supply multiple values you get multiple lightsources
                   lightdirection = c(-45,45), lightaltitude  = 35, clamp_value = 10,
                   samples = 400, 
                   # Camera locations
                   camera_lookat= c(0,-50,0), 
                   # The floor colour
                   ground_material= diffuse(color="#F2E1D0"),
                   # The size of the camera stations
                   point_radius=25, 
                   # The resultant plot size
                   width = 1200, height= 1200)




# Context map
library(leaflet)

all.locs
tmp3 <- st_as_sf(all.locs,coords=c("Longitude", "Latitude"), crs=4326)
tmp3<- tmp3[tmp3$Group!="",]
# Generate colours to display the catagory levels - R needs them as a factor

n.stat <- length(unique(tmp3$Location.Name))


tmp <- data.frame(Latitude=-12.569167, Longitude=-70.100111)
tmp <- st_as_sf(tmp,coords=c("Longitude", "Latitude"), crs=4326)


m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group="Base") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  # Add satellite data
  
  addCircleMarkers(lng= st_coordinates(tmp3)[,1], lat= st_coordinates(tmp3)[,2],
                   color= "#f90000",stroke=F,
                   popup=paste( tmp3$Location.ID), fillOpacity=0.8, radius=4) %>%
  
  addCircleMarkers(lng= st_coordinates(tmp)[,1], lat= st_coordinates(tmp)[,2],
                   color= "#ffd901",stroke=F,
                   popup=paste( tmp3$Location.ID), fillOpacity=0.8, radius=4) %>%
  
  
  addLegend("topleft", colors = c("#f90000","#ffd901") ,
            labels=c("Cameras", "Research station"),
            opacity = 1
  ) %>%
  addScaleBar(
    position = c("bottomleft"),
    options = scaleBarOptions()
  ) %>%

  # Layers control
  addLayersControl(
    baseGroups = c("Satellite", "Base"),
    options = layersControlOptions(collapsed = FALSE)
  )
m

