
library(lidR)

lidr <- here::here("/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/Download_2111980/england-laz-2017_4758656/tl")
laz_files <- list.files(lidr, "laz", full.names = TRUE)

las <- readLAS(laz_files[1], select = "xyzr", filter = "-drop_z_below 0")
las <- classify_ground(las, algorithm = csf())

plo

dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 

chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))


chm <- rasterize_canopy(las, res = .5, algorithm = p2r(.2, na.fill = tin()))
col <- terrain.colors(25)
plot(chm, col = col)
x <- plot(las, bg = "white", size = 4)

ttops <- locate_trees(las, lmf(ws = 5))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 16)

add_treetops3d(x, ttops)



LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0")
chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))
x <- plot(las, bg = "white", size = 4)

las

chm
ttops <- locate_trees(las, lmf(ws = 5))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

add_treetops3d(x, ttops)

ttops_3m <- locate_trees(las, lmf(ws = 3))
ttops_11m <- locate_trees(las, lmf(ws = 11))

par(mfrow=c(1,2))
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops_3m), add = TRUE, pch = 3)
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops_11m), add = TRUE, pch = 3)


# Point-to-raster 2 resolutions
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
chm_pitfree_05_1 <- rasterize_canopy(las, 0.5, pitfree(), pkg = "terra")
chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)


ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
ttops_chm_pitfree_05_1 <- locate_trees(chm_pitfree_05_1, lmf(5))
ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

par(mfrow=c(3,2))
col <- height.colors(50)
plot(chm_p2r_05, main = "CHM P2R 0.5", col = col); plot(sf::st_geometry(ttops_chm_p2r_05), add = T, pch =3)
plot(chm_p2r_1, main = "CHM P2R 1", col = col); plot(sf::st_geometry(ttops_chm_p2r_1), add = T, pch = 3)
plot(chm_p2r_05_smoothed, main = "CHM P2R 0.5 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_05_smoothed), add = T, pch =3)
plot(chm_p2r_1_smoothed, main = "CHM P2R 1 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_1_smoothed), add = T, pch =3)
plot(chm_pitfree_05_1, main = "CHM PITFREE 1", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_1), add = T, pch =3)
plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)


algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

tree110 <- filter_poi(las, treeID == 110)
plot(tree110, size = 8, bg = "white")

crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
crowns <- algo()
par(mfrow = c(1, 1))
plot(crowns, col = pastel.colors(200))

algo1 <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
algo2 <- li2012()
las <- segment_trees(las, algo1, attribute = "IDdalponte")
las <- segment_trees(las, algo2, attribute = "IDli")

x <- plot(las, bg = "white", size = 4, color = "IDdalponte", colorPalette = pastel.colors(200))
#> The argument 'coloPalette' is deprecated. Use 'pal' instead
plot(las, add = x + c(100,0), bg = "white", size = 4, color = "IDli", pal = pastel.colors(200))
#> The argument 'coloPalette' is deprecated. Use 'pal' instead
cloud_metrics(las, func = .stdmetrics)
pixel_metrics(las, func = .stdmetrics)
tree_metrics(las, func = .stdmetrics)
voxel_metrics(las, func = .stdmetrics)
