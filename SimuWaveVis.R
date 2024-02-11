# Reading and plot ALS file
library(rGEDI)
library(lidR)
library(plot3D)
library(data.table)
library(stringr)
library(Metrics)
library(lattice)
library(reshape2)
library(ggplot2)
windowsFonts(A = windowsFont("Times New Roman"))


#png("E:\\GEDI\\Result\\Figure\\waveform.png", width = 18, height = 18, units = 'in', res = 600)
layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6), byrow = TRUE,nrow = 3, ncol = 2)
layout(mat = layout.matrix,
       heights = c(2,2,2), # Heights of the two rows
       widths = c(2,2,2)) # Widths of the two columns

layout.show(6)



par(mgp=c(3,1,0),mar=c(4,5,4,4), family = "A",oma=c(2,2,2,2),cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5)

#lidar 3d vis ---forest
outdir <- "C:\\Users\\Shawn\\Desktop\\Duku_Subset\\las"
lasfile_amazon <- file.path(outdir, "tile_-69080_-3142680_66.las")
wave_amazon <- paste0(tools::file_path_sans_ext(lasfile_amazon),".h5")
las_amazon<-readLAS(lasfile_amazon,select = "i,c,r")
xcenter_amazon = mean(las_amazon@bbox[1,])
ycenter_amazon = mean(las_amazon@bbox[2,])

wf_amazon<-gediWFSimulator(input=lasfile_amazon,output=wave_amazon,coords = c(xcenter_amazon, ycenter_amazon))
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 19,main="Duku Forest Point Cloud",cex = 1,colkey = TRUE,  
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="", ylab="", zlab="Elevation (m)")




#lidar 3d vis ---savanna
outdir <- "C:\\Users\\Shawn\\Desktop\\Venetia_Subset\\las"
lasfile_savanna <- file.path(outdir, "tile_33000_-2470000.las")
wave_savanna <- paste0(tools::file_path_sans_ext(lasfile_savanna),".h5")
las_savanna<-readLAS(lasfile_savanna,select = "i,c,r")
xcenter_savanna = mean(las_savanna@bbox[1,])
ycenter_savanna = mean(las_savanna@bbox[2,])
wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,coords = c(xcenter_savanna, ycenter_savanna))
scatter3D(las_savanna@data$X,las_savanna@data$Y,las_savanna@data$Z,pch = 19,main="Venetia Savanna Point Cloud",cex = 1,colkey = TRUE, 
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="", ylab="", zlab="")

#simulated waveform---forest
plot(wf_amazon, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",cex = 1,
     xlab="Simulated Waveform Amplitude (%)", ylab="Elevation (m)",ylim=c(55,80))
grid()


#simulated waveform---waveform
plot(wf_savanna, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",cex = 1,
     xlab="Simulated Waveform Amplitude (%)", ylab="Elevation (m)", ylim=c(574,587))
grid()



plotWFMetrics_v2 = function(level1b, level2a, shot_number, rh=c(25, 50, 75), ...) {
  # Avoid NOTEs from checking
  elevation = NULL
  # Extracting GEDI full waveform for a giving shotnumber
  wf <- getLevel1BWF(level1b, shot_number=shot_number)
  
  level2AM<-getLevel2AM(level2a)
  shotid_mask = which(level2AM$shot_number == shot_number)
  
  level2_shot = level2AM[shotid_mask,]
  ground_z = level2_shot$elev_lowestmode
  rhs = paste0(as.character(c("rh0", as.character(paste0("rh",rh)), "rh100")))
  rh = level2_shot[, rhs, with=FALSE]
  rh_z = rh + ground_z
  
  top_z = level2AM[shotid_mask,]$elev_highestreturn
  
  range_energy = range(wf@dt$rxwaveform)
  range_abs_diff = abs(diff(range_energy))
  requireNamespace("data.table")
  
  range_z = range(rh_z)
  min_z = range_z[1]
  max_z = range_z[2]
  diff_z = max_z - min_z
  wf_between = wf@dt[elevation %between% range_z,,]
  energy_offset = min(range_energy)
  energy_no_offset = (wf_between$rxwaveform - energy_offset)
  cumsum_energy = cumsum(rev(energy_no_offset))
  
  range_cumsum = range(cumsum_energy)
  range_abs_diff_cumsum = abs(diff(range_cumsum))
  energy_cum_normalized = ((cumsum_energy)/(range_abs_diff_cumsum/range_abs_diff))+energy_offset
  max(wf@dt$rxwaveform)
  max(energy_cum_normalized)
  offset = diff_z*0.2
  ymin = min_z-offset
  ymax = max_z+offset
  wf_interest=wf@dt[wf@dt$elevation >= ymin & wf@dt$elevation <= ymax,]$rxwaveform
  qts=quantile(wf_interest, c(0.05, 1), type=1)
  
  z_masked = rev(wf_between$elevation)
  
  ticks = seq(min_z, max_z, length=4)
  closest_to_ground = which.min(abs(ticks-ground_z))
  ticks[closest_to_ground] = ground_z
  ticks_label = format(ticks-ground_z, digits = 2)
  
  rh_closest_en = list()
  for (i in 1:length(rh_z)) {
    absdiff_rh = abs(z_masked-rh_z[[i]])
    rh_closest_en[[names(rh_z)[[i]]]] = which(absdiff_rh==min(abs(absdiff_rh)))
  }
  
  # Make marks for RH based in a point
  mark = function(x, y, ...) {
    arrows(x, y, x, min_z, length=.1, code = 3)
  }
  
  # Find mid y for rh labels
  ymidpoint = function(x) {
    x-(x-min_z)/2
  }
  
  plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=1, col="forestgreen",
       xlab="On-orbit Waveform Amplitude", ylab="Elevation (m)", ylim=c(ymin, ymax), xlim=qts+c(0, 0.1*abs(diff(qts))), ...)
  par(new=TRUE)
  plot(energy_cum_normalized, z_masked, lwd=2, axes=F, bty="n", type="l", xlab = "", ylab = "", ylim=c(ymin, ymax), xlim=qts)
  axis(side=4, at = ticks, labels=ticks_label)
  mtext("Height (m)", side=4, line=2, outer = T)
  
  for (i in 2:(length(rh_z)-1)) {
    mark(energy_cum_normalized[rh_closest_en[[i]]], rh_z[[i]])
    text(energy_cum_normalized[rh_closest_en[[i]]], ymidpoint(rh_z[[i]]), toupper(names(rh_z)[[i]]), cex = 2,pos = 4)
  }
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)]], "RH100", cex = 2,pos=3)
  abline(rh_z[[length(rh_z)]], 0, lty="dashed")
  text(qts[2]-diff(qts)/2, rh_z[[1]], "RH0", cex = 2,pos=1)
  abline(rh_z[[1]], 0, lty="dashed")
  
  text(qts[1], ground_z, "GZ (m)", cex = 2,adj=c(0, -1))
  abline(ground_z, 0, lty="dashed", col="brown")
}



#vis real GEDI waveform---forest
real_1b_duku <- "C:\\Users\\Shawn\\Desktop\\Duku_Subset\\Duku_O07668.h5"
reral_2a_duku <- "E:\\GEDI\\GEDI_archive\\GEDI01B\\Duku7668\\GEDI02_A_2020111002733_O07668_T02426_02_001_01.h5"
gedilevel1b_duku<-readLevel1B(level1Bpath = real_1b_duku)
gedilevel2a_duku<-readLevel2A(level2Apath = reral_2a_duku)
shot_number_duku="76680311100049684"
plotWFMetrics(gedilevel1b_duku, gedilevel2a_duku, shot_number_duku, rh=c(25, 50, 75, 90))

level2AM<-getLevel2AM(gedilevel2a_duku)
level2AM[level2AM$shot_number == shot_number_duku,]

#vis real GEDI waveform----savanna
real_1b_venetia <- "C:\\Users\\Shawn\\Desktop\\Venetia_Subset\\Venetia_O07433.h5"
reral_2a_venetia <- "E:\\GEDI\\GEDI_archive\\GEDI01B\\Duku7668\\GEDI02_A_2020095203140_O07433_T04317_02_001_01.h5"
gedilevel1b_venetia<-readLevel1B(level1Bpath = real_1b_venetia)
gedilevel2a_venetia<-readLevel2A(level2Apath = reral_2a_venetia)
shot_number_venetia="74331116500372178"
#wf <- getLevel1BWF(gedilevel1b, shot_number)
plotWFMetrics_v2(gedilevel1b_venetia, gedilevel2a_venetia, shot_number_venetia, rh=c(50, 75, 90, 98))


#dev.off()


#-------------------------------------------------------------------------------------------------
#random error
outdir <- "C:\\Users\\Shawn\\Desktop\\Test"
lasfile_savanna <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM\\Welverdiendt\\tile_18920_-2719680.las"
wave_savanna <- "C:\\Users\\Shawn\\Desktop\\Test\\Test.h5"
las_savanna<-readLAS(lasfile_savanna,select = "i,c,r")
xcenter_savanna = 317268.76 
ycenter_savanna = 7280784.47
RandomNum <- round(runif(100, -2, 2),2)
RH98 <- 0
wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,
                            coords = c(xcenter_savanna, ycenter_savanna),
                                       res = 0.01)
wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                  outRoot=file.path(outdir,"Test"),
                                  ground = TRUE,
                                  rhRes=1
                                  )

for (i in 1:100) {
  Random <- RandomNum[i]
  RandomNum_x <- xcenter_savanna + Random
  RandomNum_y <- ycenter_savanna + Random
  
  wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,coords = c(RandomNum_x, RandomNum_y))
  wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                    outRoot=file.path(outdir,"Test"),
                                    rhRes=1,
                                    ground = TRUE)
  
  close(wf_savanna)
  
  RH98[i] <- wf_savanna_metrics[,213]
}

as.data.frame(table(RH98))







#-------------------------------------------------------------------------------------------------
#random error -2 -1 1 2
dir <- "C:\\Users\\Shawn\\Desktop\\All_on_test.csv"
outdir <- "C:\\Users\\Shawn\\Desktop\\Test"
lasdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM"
wave_savanna <- "C:\\Users\\Shawn\\Desktop\\Test\\Test.h5"
Data = read.csv(dir,header=T)

Data <- Data[Data$site == "Welverdiendt",]

RH98 <- 0
RH98_R_2 <- 0
RH98_orb<- 0
diff_2 <- 0
percent_2 <- 0

list <- seq(0, 2, by=0.2)
for (m in list){
  print(m)
  for (i in 1:nrow(Data)) {
    tryCatch({
      print(i)
      lasfile_savanna <- gsub(" ", "", paste(lasdir,"\\",Data["site"][i,],"\\",Data["LAS_NM"][i,],".las"))
      xcenter_savanna = Data["longitude"][i,]
      ycenter_savanna = Data["latitude"][i,]
      
      #ori
      wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,
                                  coords = c(xcenter_savanna, ycenter_savanna),
                                  res = 0.01)
      wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                        outRoot=file.path(outdir,"Test"),
                                        rhRes=1,
                                        ground = TRUE)
      RH98[i] <- wf_savanna_metrics[,213]
      
      
      RandomNum_x <- xcenter_savanna - m
      RandomNum_y <- ycenter_savanna - m
      wf_savanna_R_2<-gediWFSimulator(input=lasfile_savanna,
                                      output=wave_savanna,
                                      coords = c(RandomNum_x, RandomNum_y),
                                      res = 0.01)
      wf_savanna_metrics_R_2<-gediWFMetrics(input=wf_savanna_R_2,
                                            outRoot=file.path(outdir,"Test"),
                                            rhRes=1,
                                            ground = TRUE)
      RH98_R_2[i] <- wf_savanna_metrics_R_2[,213]
      RH98_orb[i] <- Data["RHS_98"][i,]
      diff_2[i] <- RH98_R_2[i]-RH98[i]
      percent_2[i] <- diff_2[i]/(RH98_orb[i]-RH98[i])*100
      
      
    }, error=function(e){})
    
  }
  write.table(as.data.frame(cbind(RH98_orb,RH98,
                                  RH98_R_2,diff_2,percent_2)),
              file=file.path(outdir,paste0(m,"_result.csv")), sep=",",row.names=F)
  close(wf_savanna)
  
}




dir <- "C:\\Users\\Shawn\\Desktop\\Test\\percent_merge.csv"
Data = read.csv(dir,header=T)
colMeans(Data)
plot(colMeans(Data))

dir <- "C:\\Users\\Shawn\\Desktop\\Test\\diff_merge.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$RH98 >= 2.34 & Data$RH98_orb >= 2.34,]
list_RMSE <- 0
list_RMSE[1] <- sqrt(mean(Data$RE_diff_0^2))
list_RMSE[2] <- sqrt(mean(Data$RE_diff_0.2^2))
list_RMSE[3] <- sqrt(mean(Data$RE_diff_0.4^2))
list_RMSE[4] <- sqrt(mean(Data$RE_diff_0.6^2))
list_RMSE[5] <- sqrt(mean(Data$RE_diff_0.8^2))
list_RMSE[6] <- sqrt(mean(Data$RE_diff_1^2))
list_RMSE[7] <- sqrt(mean(Data$RE_diff_1.2^2))
list_RMSE[8] <- sqrt(mean(Data$RE_diff_1.4^2))
list_RMSE[9] <- sqrt(mean(Data$RE_diff_1.6^2))
list_RMSE[10] <- sqrt(mean(Data$RE_diff_1.8^2))
list_RMSE[11] <- sqrt(mean(Data$RE_diff_2^2))

sqrt(mean((Data$RH98_orb - Data$RH98)^2))


dir <- "C:\\Users\\Shawn\\Desktop\\Test\\diff_merge.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$RH98 >= 2.34 & Data$RH98_orb >= 2.34,]
quantile(Data$RE_diff_2, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_1.8, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_1.6, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_1.4, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_1.2, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_1, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_0.8, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_0.6, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_0.4, probs = seq(.1, .9, by = .1))
quantile(Data$RE_diff_0.2, probs = seq(.1, .9, by = .1))




dir <- "C:\\Users\\Shawn\\Desktop\\Test\\percent_merge.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$RH98 >= 2.34 & Data$RH98_orb >= 2.34,]
quantile(Data$RE_percent_2, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_1.8, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_1.6, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_1.4, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_1.2, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_1, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_0.8, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_0.6, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_0.4, probs = seq(.1, .9, by = .1))
quantile(Data$RE_percent_0.2, probs = seq(.1, .9, by = .1))




dir <- "C:\\Users\\Shawn\\Desktop\\RLIP.csv"
Data = read.csv(dir,header=T)
Trans <- melt(Data, id.vars="RD")
names(Trans)[2] <- "RLIP"

x <- Trans$RD
y <- Trans$RLIP

# Heatmap 
ggplot(Trans, aes(RD, RLIP, fill= value)) + 
  geom_tile(color = "black")+
  theme_bw()+
  scale_fill_gradientn(
    breaks = seq(-100,100,20),
    colors = hcl.colors(20, "RdYlGn"),
    name = "RLIP (%)")+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.key.height=unit(3,"cm"))+
  labs(x=expression("Relocate Distance"), 
       y=expression("Random Location Influence Percentage (%)"))

out = "C:\\Users\\Shawn\\Desktop\\RLIP.jpg"
ggsave(out,height=12, width=12, dpi=600)






#-------------------------------------------------------------------------------------------------
#random error --100 footprints in ALL
outdir <- "C:\\Users\\Shawn\\Desktop\\Test_3"
wave_savanna <- "C:\\Users\\Shawn\\Desktop\\Test_3\\Test.h5"
dir <- "C:\\Users\\Shawn\\Desktop\\All_on_test.csv"

Data = read.csv(dir,header=T)

Data <- Data[sample(nrow(Data), 100), ]
write.table(as.data.frame(Data),
            file=file.path(outdir,paste0("All_100.csv")), sep=",",row.names=F)
#RNG
RandomNum_1 <- round(runif(100, -2, 2),2)
RandomNum_2 <- round(runif(100, -2, 2),2)
write.table(as.data.frame(cbind(RandomNum_1,RandomNum_2)),
            file=file.path(outdir,paste0("100_RNG_1.csv")), sep=",",row.names=F)

for (m in 1:100) {
  lasfile_savanna <- file.path("E:\\GEDI\\ALS_archive\\LiDAR_UTM",Data$site[m],paste0(Data$LAS_NM[m],".las"))
  las_savanna<-readLAS(lasfile_savanna,select = "i,c,r")
  xcenter_savanna = Data$longitude[m]
  ycenter_savanna = Data$latitude[m]

  RH98 <- 0
  wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,
                              coords = c(xcenter_savanna, ycenter_savanna),
                              res = 0.01)
  wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                    outRoot=file.path(outdir,"Test"),
                                    rhRes=1,
                                    ground = TRUE)
  
  RH98[1] <- wf_savanna_metrics[,213]
  
  for (i in 2:100) {
    
    Random_1 <- RandomNum_1[i]
    Random_2 <- RandomNum_2[i]
    RandomNum_x <- xcenter_savanna + Random_1
    RandomNum_y <- ycenter_savanna + Random_2
    
    wf_savanna<-gediWFSimulator(input=lasfile_savanna,output=wave_savanna,
                                coords = c(RandomNum_x, RandomNum_y),
                                res = 0.01)
    wf_savanna_metrics<-gediWFMetrics(input=wf_savanna,
                                      outRoot=file.path(outdir,"Test"),
                                      rhRes=1,
                                      ground = TRUE)
    
    RH98[i] <- wf_savanna_metrics[,213]
  }
  write.table(as.data.frame(RH98),
              file=file.path(outdir,paste0(m,"_ind.csv")), sep=",",row.names=F)
}
close(wf_savanna)




#-------------------------------------------------------------------------------------------------
png("E:\\GEDI\\Result\\Figure\\waveform2.png", width = 12, height = 12, units = 'in', res = 600)


plotWFMetrics_v2 = function(level1b, level2a, shot_number, rh=c(25, 50, 75), ...) {
  # Avoid NOTEs from checking
  elevation = NULL
  # Extracting GEDI full waveform for a giving shotnumber
  wf <- getLevel1BWF(level1b, shot_number=shot_number)
  
  level2AM<-getLevel2AM(level2a)
  shotid_mask = which(level2AM$shot_number == shot_number)
  
  level2_shot = level2AM[shotid_mask,]
  ground_z = level2_shot$elev_lowestmode
  rhs = paste0(as.character(c("rh0", as.character(paste0("rh",rh)), "rh100")))
  rh = level2_shot[, rhs, with=FALSE]
  rh_z = rh + ground_z
  
  top_z = level2AM[shotid_mask,]$elev_highestreturn
  
  range_energy = range(wf@dt$rxwaveform)
  range_abs_diff = abs(diff(range_energy))
  requireNamespace("data.table")
  
  range_z = range(rh_z)
  min_z = range_z[1]
  max_z = range_z[2]
  diff_z = max_z - min_z
  wf_between = wf@dt[elevation %between% range_z,,]
  energy_offset = min(range_energy)
  energy_no_offset = (wf_between$rxwaveform - energy_offset)
  cumsum_energy = cumsum(rev(energy_no_offset))
  
  range_cumsum = range(cumsum_energy)
  range_abs_diff_cumsum = abs(diff(range_cumsum))
  energy_cum_normalized = ((cumsum_energy)/(range_abs_diff_cumsum/range_abs_diff))+energy_offset
  max(wf@dt$rxwaveform)
  max(energy_cum_normalized)
  offset = diff_z*0.2
  ymin = min_z-offset
  ymax = max_z+offset
  wf_interest=wf@dt[wf@dt$elevation >= ymin & wf@dt$elevation <= ymax,]$rxwaveform
  qts=quantile(wf_interest, c(0.05, 1), type=1)
  
  z_masked = rev(wf_between$elevation)
  
  ticks = seq(min_z, max_z, length=4)
  closest_to_ground = which.min(abs(ticks-ground_z))
  ticks[closest_to_ground] = ground_z
  ticks_label = format(ticks-ground_z, digits = 2)
  
  rh_closest_en = list()
  for (i in 1:length(rh_z)) {
    absdiff_rh = abs(z_masked-rh_z[[i]])
    rh_closest_en[[names(rh_z)[[i]]]] = which(absdiff_rh==min(abs(absdiff_rh)))
  }
  
  # Make marks for RH based in a point
  mark = function(x, y, ...) {
    arrows(x, y, x, min_z, length=.1, code = 3)
  }
  
  # Find mid y for rh labels
  ymidpoint = function(x) {
    x-(x-min_z)/2
  }
  
  plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=1, col="forestgreen",
       cex=2,cex.lab=2,cex.axis = 2,
       xlab="On-orbit Waveform Amplitude", ylab="Elevation (m)", 
       ylim=c(ymin, ymax), xlim=qts+c(0, 0.1*abs(diff(qts))), ...)
  
  
  text(qts[1], ground_z, "GZ (m)", cex = 2,adj=c(0, -1))
  abline(ground_z, 0, lty="solid", col="brown")
  #rh98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]-0.5, "Simulated GEDI RH98: 11.27 m", cex = 2,pos=3, col="blue")
  abline(ground_z+11.27, 0 ,lwd = 2, lty="solid", col="blue")
  #p98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]+4.5, "ALS P98: 12.03 m", cex = 2,pos=3, col="red")
  abline(ground_z+12.03, 0, lwd=2, lty="solid", col="red")
}

layout.matrix <- matrix(c(1, 2, 3, 4), byrow = TRUE,nrow = 2, ncol = 2)
layout(mat = layout.matrix,
       heights = c(2,2), # Heights of the two rows
       widths = c(2,2)) # Widths of the two columns
layout.show(4)
par(mgp=c(3,1,0),mar=c(4,5,4,5), family = "A",oma=c(2,2,2,2),cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5)

#lidar 3d vis ---forest
outdir <- "E:\\GEDI\\Result\\backup\\las\\Welverdiendt"
lasfile_amazon <- file.path(outdir, "Shot_66400000400344621.las")
las_amazon<-readLAS(lasfile_amazon,select = "i,c,r")
xcenter_amazon = mean(las_amazon@bbox[1,])
ycenter_amazon = mean(las_amazon@bbox[2,])
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 19,cex = 1,colkey = TRUE,  
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="", ylab="", zlab="Elevation (m)")


#vis real GEDI waveform---forest
real_1b <- "C:\\Users\\Shawn\\Desktop\\height_percentile\\GEDI01_B_2020044164536_O06640_04_T01318_02_005_01_V002.h5"
real_2a <- "E:\\GEDI\\GEDI_archive\\GEDI02A\\SA\\GEDI02_A_2020044164536_O06640_04_T01318_02_003_01_V002.h5"
level1b <- readLevel1B(level1Bpath = real_1b)
level2a <- readLevel2A(level2Apath = real_2a)
shot_number="66400000400344621"
#plotWFMetrics_v2(level1b, level2a, shot_number, rh=c(50, 75, 90, 98))

#RHs
wf_amazon<-gediWFSimulator(input=lasfile_amazon,output="C:\\Users\\Shawn\\Desktop\\height_percentile\\sim.h5",
                            coords = c(xcenter_amazon, ycenter_amazon),
                            res = 0.01)
wf_amazon_metrics<-gediWFMetrics(input=wf_amazon,
                                  outRoot="C:\\Users\\Shawn\\Desktop\\height_percentile",
                                  ground = TRUE,
                                  rhRes=1)
wf_amazon_metrics <- t(wf_amazon_metrics[115:215])

#Percentile
csv_file <- "C:\\Users\\Shawn\\Desktop\\height_percentile\\Shot_1.csv"
Data = read.csv(csv_file,header=T)
merge = cbind(Data,wf_amazon_metrics)
plot(merge$wf_amazon_metrics,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
     col="blue",xlab="Percentile (%)",ylab="Height (m)", pch = 19)
lines(merge$wf_amazon_metrics,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
     col="blue",xlab="Percentile (%)",ylab="Height (m)", pch = 19)

ytick<-seq(-4, 14, by=2)
axis(side=2, at=ytick, labels = TRUE,cex.lab=2,cex=1,cex.axis = 2)
par(new=TRUE)
points(merge$X0,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
      col="red",axes=T,xlab="",ylab="", pch = 19)
lines(merge$X0,cex.lab=2,cex=1,cex.axis = 2,lty=2,lwd=2,
     col="red",axes=T, xlab="",ylab="", pch = 19)

abline(h=0, col="Black")
legend(0, 14, legend=c("Simulated GEDI RH", "ALS Percentile Height"),
       col=c("blue", "red"), pch=19, cex=2,box.lwd = 0,lty=2,lwd=2,
       bty="n",x.intersp=0.3,y.intersp=1)



#lidar 3d vis ---savanna
plotWFMetrics_v2 = function(level1b, level2a, shot_number, rh=c(25, 50, 75), ...) {
  # Avoid NOTEs from checking
  elevation = NULL
  # Extracting GEDI full waveform for a giving shotnumber
  wf <- getLevel1BWF(level1b, shot_number=shot_number)
  
  level2AM<-getLevel2AM(level2a)
  shotid_mask = which(level2AM$shot_number == shot_number)
  
  level2_shot = level2AM[shotid_mask,]
  ground_z = level2_shot$elev_lowestmode
  rhs = paste0(as.character(c("rh0", as.character(paste0("rh",rh)), "rh100")))
  rh = level2_shot[, rhs, with=FALSE]
  rh_z = rh + ground_z
  
  top_z = level2AM[shotid_mask,]$elev_highestreturn
  
  range_energy = range(wf@dt$rxwaveform)
  range_abs_diff = abs(diff(range_energy))
  requireNamespace("data.table")
  
  range_z = range(rh_z)
  min_z = range_z[1]
  max_z = range_z[2]
  diff_z = max_z - min_z
  wf_between = wf@dt[elevation %between% range_z,,]
  energy_offset = min(range_energy)
  energy_no_offset = (wf_between$rxwaveform - energy_offset)
  cumsum_energy = cumsum(rev(energy_no_offset))
  
  range_cumsum = range(cumsum_energy)
  range_abs_diff_cumsum = abs(diff(range_cumsum))
  energy_cum_normalized = ((cumsum_energy)/(range_abs_diff_cumsum/range_abs_diff))+energy_offset
  max(wf@dt$rxwaveform)
  max(energy_cum_normalized)
  offset = diff_z*0.2
  ymin = min_z-offset
  ymax = max_z+offset
  wf_interest=wf@dt[wf@dt$elevation >= ymin & wf@dt$elevation <= ymax,]$rxwaveform
  qts=quantile(wf_interest, c(0.05, 1), type=1)
  
  z_masked = rev(wf_between$elevation)
  
  ticks = seq(min_z, max_z, length=4)
  closest_to_ground = which.min(abs(ticks-ground_z))
  ticks[closest_to_ground] = ground_z
  ticks_label = format(ticks-ground_z, digits = 2)
  
  rh_closest_en = list()
  for (i in 1:length(rh_z)) {
    absdiff_rh = abs(z_masked-rh_z[[i]])
    rh_closest_en[[names(rh_z)[[i]]]] = which(absdiff_rh==min(abs(absdiff_rh)))
  }
  
  # Make marks for RH based in a point
  mark = function(x, y, ...) {
    arrows(x, y, x, min_z, length=.1, code = 3)
  }
  
  # Find mid y for rh labels
  ymidpoint = function(x) {
    x-(x-min_z)/2
  }
  
  plot(wf, relative=FALSE, polygon=TRUE, type="l", lwd=1, col="forestgreen",
       cex=2,cex.lab=2,cex.axis = 2,
       xlab="On-orbit Waveform Amplitude", ylab="Elevation (m)", 
       ylim=c(ymin, ymax), xlim=qts+c(0, 0.1*abs(diff(qts))), ...)
  
  
  text(qts[1], ground_z, "GZ (m)", cex = 2,adj=c(0, -1))
  abline(ground_z, 0, lty="solid", col="brown")
  #rh98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]+2, "Simulated GEDI RH98: 4.57 m", cex = 2,pos=3, col="blue")
  abline(ground_z+4.57, 0 ,lwd = 2, lty="solid", col="blue")
  #p98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]+4, "ALS P98: 4.68 m", cex = 2,pos=3, col="red")
  abline(ground_z+4.68, 0, lwd=2, lty="solid", col="red")
}


outdir <- "E:\\GEDI\\Result\\backup\\las\\Welverdiendt"
lasfile_amazon <- file.path(outdir, "Shot_74331100400377802.las")
las_amazon<-readLAS(lasfile_amazon,select = "i,c,r")
xcenter_amazon = mean(las_amazon@bbox[1,])
ycenter_amazon = mean(las_amazon@bbox[2,])
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 19,cex = 1,colkey = TRUE,  
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="", ylab="", zlab="Elevation (m)")


#vis real GEDI waveform---savanna
real_1b <- "E:\\GEDI\\GEDI_archive\\GEDI02A\\SA\\GEDI01_B_2020095203140_O07433_04_T04317_02_005_01_V002.h5"
real_2a <- "E:\\GEDI\\GEDI_archive\\GEDI02A\\SA\\GEDI02_A_2020095203140_O07433_04_T04317_02_003_01_V002.h5"
level1b <- readLevel1B(level1Bpath = real_1b)
level2a <- readLevel2A(level2Apath = real_2a)
shot_number="74331100400377802"
#plotWFMetrics_v2(level1b, level2a, shot_number, rh=c(50, 75, 90, 98))


#RHs
wf_amazon<-gediWFSimulator(input=lasfile_amazon,output="C:\\Users\\Shawn\\Desktop\\height_percentile\\sim2.h5",
                           coords = c(xcenter_amazon, ycenter_amazon),
                           res = 0.01)
wf_amazon_metrics<-gediWFMetrics(input=wf_amazon,
                                 outRoot="C:\\Users\\Shawn\\Desktop\\height_percentile",
                                 ground = TRUE,
                                 rhRes=1)
wf_amazon_metrics <- t(wf_amazon_metrics[115:215])


#Percentile
csv_file <- "C:\\Users\\Shawn\\Desktop\\height_percentile\\Shot_2.csv"
Data = read.csv(csv_file,header=T)
merge = cbind(Data,wf_amazon_metrics)
plot(merge$wf_amazon_metrics,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
     col="blue",xlab="Percentile (%)",ylab="Height (m)", pch = 19)
lines(merge$wf_amazon_metrics,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
      col="blue",xlab="Percentile (%)",ylab="Height (m)", pch = 19)

ytick<-seq(-4, 14, by=2)
axis(side=2, at=ytick, labels = TRUE,cex.lab=2,cex=1,cex.axis = 2)
par(new=TRUE)
points(merge$X0,cex.lab=2,cex=1,cex.axis = 2,yaxt ="n",
       col="red",axes=T,xlab="",ylab="", pch = 19)
lines(merge$X0,cex.lab=2,cex=1,cex.axis = 2,lty=2,lwd=2,
      col="red",axes=T, xlab="",ylab="", pch = 19)

abline(h=0, col="Black")
legend(0, 7, legend=c("Simulated GEDI RH", "ALS Percentile Height"),
       col=c("blue", "red"), pch=19, cex=2,box.lwd = 0,lty=2,lwd=2,
       bty="n",x.intersp=0.3,y.intersp=1)

dev.off()


