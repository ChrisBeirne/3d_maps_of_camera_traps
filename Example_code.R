# Interactive/3d maps

# I could only get the highquality 3d render working with the development version
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(rayrender)
library(geoviz)
library(raster)
library(elevatr)
library(sf)

# Import your camera locations
#locs <- read.csv("data/raw-data/Camera_locations_master.csv", header=T, sep=",")

# Or use my example from Peru
locs <- data.frame("Latitude"= c( -12.43517, -12.43508, -12.42785, -12.42765, -12.41975, -12.41984, -12.44023, -12.44020, -12.44794, -12.44779, -12.44048,
 -12.44035, -12.43266, -12.43276, -12.45678, -12.45661, -12.44928, -12.44934, -12.44110, -12.44122, -12.43374, -12.43386,
 -12.42553, -12.42546, -12.42097, -12.42085, -12.42869, -12.42867, -12.43651, -12.43641, -12.44480, -12.44477, -12.45214,
 -12.45204, -12.46594, -12.46583, -12.45781, -12.45808, -12.45051, -12.45049, -12.44224, -12.44234, -12.43493, -12.43471,
 -12.45352, -12.45349, -12.44577, -12.44555, -12.43834, -12.43832, -12.43039, -12.43026, -12.46185, -12.46173, -12.42417,
 -12.42406, -12.41666, -12.41650, -12.41214, -12.41204),
 "Longitude"=
  c(-70.24387, -70.24400, -70.23917, -70.23915, -70.23550, -70.23563, -70.24680, -70.24664, -70.24068, -70.24063, -70.23648,
  -70.23648, -70.23120, -70.23125, -70.22510, -70.22496, -70.22035, -70.22049, -70.21535, -70.21532, -70.21071, -70.21075,
  -70.20652, -70.20644, -70.21441, -70.21422, -70.21879, -70.21864, -70.22342, -70.22353, -70.22780, -70.22782, -70.23296,
  -70.23276, -70.20870, -70.20885, -70.20427, -70.20440, -70.19971, -70.19988, -70.19540, -70.19535, -70.19041, -70.19052,
  -70.21224, -70.21204, -70.20786, -70.20783, -70.20333, -70.20342, -70.19858, -70.19868, -70.21647, -70.21632, -70.22680,
  -70.22692, -70.22214, -70.22206, -70.23004, -70.23009))

# Convert them to sf
tmp2 <- st_as_sf(locs,coords=c("Longitude", "Latitude"), crs=4326)

# Convert them to UTM (so that you can link xy distance to elevation (if you want realistic maps)st_transform
locs.m <- st_transform(tmp2, 32719)

# Specify the plot limits
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
              lat = locs$Latitude, long = locs$Longitude,
              altitude = locs$Elevation, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="red", size=6)


Sys.sleep(0.2)

render_highquality(filename="Plots/EXAMPLE_render.png", # The file name you want!
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
tmp2 <- st_as_sf(locs, coords=c("Longitude", "Latitude"), crs=4326)

# Specify the plot limits
loc.df <- data.frame(x=c(min(locs$Longitude, na.rm=T), max(locs$Longitude, na.rm=T)),
                     y=c(min(locs$Latitude, na.rm=T), max(locs$Latitude, na.rm=T)))


# Get the elevation data for the defined limits
elevation <- get_elev_raster(locations = loc.df, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                             z=14) # The z value determines the zoom (higher numbers = more zoomed in)

# Determine the elevation of the cameras (so you can plot them on your map)
locs$Elevation <- extract(elevation,tmp2)

# Check all is as it seems with a basic plot
#image(elevation, asp=1)
#points(locs$Longitude, locs$Latitude)

# Create the matrix required for 3d plots
elmat = raster_to_matrix(elevation)
mean(elmat)
elmat[elmat<100] <- 100
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

# Render it
# Remove previous plot
rgl::rgl.clear()
rayshader::plot_3d(
  scene,
  elmat,
  zscale = 1, # How exagerated the elevation is (smaller = larger z scale)
  solid = TRUE , # Should your map have a solid base?
  soliddepth = 100, # Where should th solid bit end
  solidcolor = "grey20",
  shadow = TRUE , # Should you have a shadow
  shadowdepth = 70,
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
              lat = locs$Latitude, long = locs$Longitude,
              altitude = locs$Elevation, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="red", size=6)


Sys.sleep(0.2)

render_highquality(filename="CM1_render.png", # The file name you want!
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




#### REPEAT FOR JOFF?

joff.locs<- read.csv("data/raw-data/JOFF_coords.csv", header=T)
# Subset to the locations you want to plot
locs <- joff.locs[substr(joff.locs$Deployment.Location.ID,1,1)=="J",]
# Convert them to sf
tmp2 <- st_as_sf(locs, coords=c("Longitude", "Latitude"), crs=4326)

# Specify the plot limits
loc.df <- data.frame(x=c(min(locs$Longitude, na.rm=T), max(locs$Longitude, na.rm=T)),
                     y=c(min(locs$Latitude, na.rm=T), max(locs$Latitude, na.rm=T)))


# Get the elevation data for the defined limits
elevation <- get_elev_raster(locations = loc.df, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                             z=14) # The z value determines the zoom (higher numbers = more zoomed in)
#?get_elev_raster()
# Determine the elevation of the cameras (so you can plot them on your map)
locs$Elevation <- extract(elevation,tmp2)

# Check all is as it seems with a basic plot
#image(elevation, asp=1)
#points(locs$Longitude, locs$Latitude)

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
              lat = locs$Latitude, long = locs$Longitude,
              altitude = locs$Elevation, zscale=1,   # zscale must match that of the above (its scales the exageration of elevation) 
              color="red", size=6)


Sys.sleep(0.2)

render_highquality(filename="JOFF_render.png", # The file name you want!
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

