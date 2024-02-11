#The development version:
#library(devtools)
#devtools::install_github("carlos-alberto-silva/rGEDI", dependencies = TRUE)
library(rGEDI)
library(lidR)
# Specifying the path to ALS data
las <- "C:/Users/lxiao/Desktop/20200329"
outdir <- "C:/Users/lxiao/Desktop/20200329"
lasfile_amazon <- file.path(las, "test.las")

# Reading and plot ALS file
las_amazon <- readLAS(lasfile_amazon,select = "*", filter = "-keep_scan_angle -90 90")
las_test <- rlas::read.las(lasfile_amazon)
#plot(las_amazon)


listCoord <- "E:/temp/Konrad/LiDAR_CHM/gediSimulator/tile_36000_-2473000.txt"

# Simulating GEDI full-waveform
output <- paste0(outdir,"/gediWF_amazon_simulations.h5")
wf_amazon<-gediWFSimulator(input=lasfile_amazon, output=output, 
                           listCoord = listCoord)

# Plotting ALS and GEDI simulated full-waveform
png("gediWf.png", width = 8, height = 6, units = 'in', res = 300)

par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 16,colkey = FALSE, main="",
          cex = 0.5,bty = "u",col.panel ="gray90",phi = 30,alpha=1,theta=45,
          col.grid = "gray50", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)")

plot(wf_amazon, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="", ylab="Elevation (m)", ylim=c(90,140))

grid()
dev.off()


#Extracting GEDI full-waveform derived metrics without adding noise to the full-waveform
wf_amazon_metrics<-gediWFMetrics(input=wf_amazon,
                                 outRoot=file.path(outdir, "Addo"))

metrics<-rbind(wf_amazon_metrics,wf_savanna_metrics)
rownames(metrics)<-c("Amazon","Savanna")
head(metrics[,1:8])
#Extracting GEDI full-waveform derived metrics after adding noise to the full-waveform
wf_amazon_metrics_noise<-gediWFMetrics(input=wf_amazon,
                                       outRoot=file.path(outdir, "Addo"),
                                       linkNoise= c(3.0103,0.95),
                                       maxDN= 4096,
                                       sWidth= 0.5,
                                       varScale= 3)
#Always close gedi objects, so HDF5 files won't be blocked!
close(wf_amazon)
