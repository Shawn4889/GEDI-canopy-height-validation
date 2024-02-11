#LIDAR 3D plots
library(lidR)
library(plot3D)
library(rayshader)
library(plotfunctions)

LASfile_low = "E:\\ALS_archive\\LiDAR_UTM\\Welverdiendt\\tile_31920_-2718680.las"
las_ori_low = readLAS(LASfile_low)
las_low = clip_rectangle(las_ori_low, 330005, 7282015, 330030, 7282040)
las_low = normalize_height(las_low, grid_terrain(las_low, 1, knnidw(k = 6L, p = 2)))

LASfile_med = "E:\\ALS_archive\\LiDAR_UTM\\Welverdiendt\\tile_20920_-2718680.las"
las_ori_med = readLAS(LASfile_med)
las_med = clip_rectangle(las_ori_med, 318670, 7281283, 318695, 7281308)
las_med = normalize_height(las_med, grid_terrain(las_med, 1, knnidw(k = 6L, p = 2)))

LASfile_high = "E:\\ALS_archive\\LiDAR_UTM\\Welverdiendt\\tile_22920_-2719680.las"
las_ori_high = readLAS(LASfile_high)
las_high = clip_rectangle(las_ori_high, 320666, 7280560, 320691, 7280585)
las_high = normalize_height(las_high, grid_terrain(las_high, 1, knnidw(k = 6L, p = 2)))

#plot(las_low, legend=TRUE,axis=TRUE)

png("C:\\Users\\Shawn\\Desktop\\Composite0128\\high.png", width = 30, height = 60, units = 'cm', res = 600)
par(mfrow=c(3,1),mgp=c(1,1,0),mar = c(2, 2, 2, 6),oma=c(2,2,2,2),cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3)

scatter3D(las_low@data$X,las_low@data$Y,las_low@data$Z,
          pch = 19,cex = 2,phi = 15,alpha=1,theta=45,
          zlim=c(0,15),
          main="Welverdiendt S28 (low AGBD = 6 Mg/ha)",
          col = ramp.col(c("black", "green", "yellow", "red")), colkey=FALSE,
          bty = "u",col.panel ="white",surface=TRUE,
          col.grid = "gray50", 
          xlab="UTM Easting (m)", 
          ylab="UTM Northing (m)", 
          zlab="Elevation (m)")

scatter3D(las_med@data$X,las_med@data$Y,las_med@data$Z,
          pch = 19,cex = 2,phi = 15,alpha=1,theta=45,
          zlim=c(0,15),
          main="Andover S40 (medium AGBD = 27.4 Mg/ha)",
          col = ramp.col(c("black", "green", "yellow", "red")), colkey=FALSE,
          bty = "u",col.panel ="white",surface=TRUE,
          col.grid = "gray50", 
          xlab="UTM Easting (m)", 
          ylab="UTM Northing (m)", 
          zlab="Elevation (m)")

mtext("Top Margin", side = 4, line = 0, col = "red", cex = 3)

gradientLegend(valRange = c(0, 15), cex = 4,
               color = ramp.col(c("black", "green", "yellow", "red")),
               n.seg = 5, pos.num = 2,
               pos = 0.5,side = 4,length = 1,
               inside = FALSE)

scatter3D(las_high@data$X,las_high@data$Y,las_high@data$Z,
          pch = 19,cex = 2,phi = 15,alpha=1,theta=45,
          zlim=c(0,15),
          main="Andover S41 (high AGBD = 50.7 Mg/ha)",
          col = ramp.col(c("black", "green", "yellow", "red")), colkey=FALSE,
          bty = "u",col.panel ="white",surface=TRUE,
          col.grid = "gray50", 
          xlab="UTM Easting (m)", 
          ylab="UTM Northing (m)", 
          zlab="Elevation (m)")



dev.off()





