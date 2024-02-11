#Composite visualization
# Reading and plot ALS file
library(rGEDI)
library(lidR)
library(plot3D)
library(data.table)
windowsFonts(A = windowsFont("Times New Roman"))
# ------------------------------------------------------------------------------------------------ #
png("E:\\GEDI\\Result\\Figure\\3D.png", width = 12, height = 12, units = 'in', res = 600)
par(mgp=c(3,1,0),mar=c(4,5,4,4), family = "A",oma=c(2,2,2,2),cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3)
#lidar 3d vis ---forest
outdir <- "C:\\Users\\Shawn\\Desktop\\Venetia_Composite"
lasfile_amazon <- file.path(outdir, "t27.las")
wave_amazon <- paste0(tools::file_path_sans_ext(lasfile_amazon),".h5")
las_amazon<-readLAS(lasfile_amazon,select = "i,c,r")
xcenter_amazon = mean(las_amazon@bbox[1,])
ycenter_amazon = mean(las_amazon@bbox[2,])

wf_amazon<-gediWFSimulator(input=lasfile_amazon,output=wave_amazon,coords = c(xcenter_amazon, ycenter_amazon))
scatter3D(las_amazon@data$X,las_amazon@data$Y,las_amazon@data$Z,pch = 19,main="Venetia (Savanna) Point Cloud",cex = 1,colkey = TRUE,  
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)")
dev.off()

# ------------------------------------------------------------------------------------------------ #
png("E:\\GEDI\\Result\\Figure\\GEDI_simulated.png", width = 18, height = 12, units = 'in', res = 600)
par(mgp=c(3,1,0),mar=c(4,5,4,4), family = "A",oma=c(2,2,2,2),cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3)
#simulated waveform---forest
plot(wf_amazon, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",cex = 1,
     xlab="Simulated Waveform Amplitude (%)", ylab="Elevation (m)",ylim=c(565,590))
grid()
dev.off()

# ------------------------------------------------------------------------------------------------ #
png("E:\\GEDI\\Result\\Figure\\GEDI_RH.png", width = 18, height = 12, units = 'in', res = 600)
par(mgp=c(3,1,0),mar=c(4,5,4,4), family = "A",oma=c(2,2,2,2),cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3)
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
       cex.lab=3, cex.axis=3, mgp=c(3,1,0),
       xlab="On-orbit Waveform Amplitude", ylab="Elevation (m)", 
       ylim=c(ymin, ymax), xlim=qts+c(0, 0.1*abs(diff(qts))), ...)
  par(new=TRUE)
  plot(energy_cum_normalized, z_masked, lwd=2, axes=F, bty="n", type="l", 
       xlab = "", ylab = "", ylim=c(ymin, ymax), xlim=qts)
  axis(side=4, at = ticks, labels=ticks_label)
  mtext("Height (m)", side=4, line=3, cex=3)
  
  for (i in 2:(length(rh_z)-1)) {
    mark(energy_cum_normalized[rh_closest_en[[i]]], rh_z[[i]])
    text(energy_cum_normalized[rh_closest_en[[i]]], ymidpoint(rh_z[[i]]), toupper(names(rh_z)[[i]]), cex = 3,pos = 1)
  }
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)]], "RH100", cex = 3,pos=3)
  abline(rh_z[[length(rh_z)]], 0, lty="dashed")
  text(qts[2]-diff(qts)/2, rh_z[[1]], "RH0", cex = 3,pos=1)
  abline(rh_z[[1]], 0, lty="dashed")
  
  text(qts[1], ground_z, "GZ (m)", cex = 3,adj=c(0, -1))
  abline(ground_z, 0, lty="dashed", col="brown")
}
#vis real GEDI waveform----savanna
real_1b <- "E:\\GEDI\\GEDI_archive\\GEDI01B\\SA\\GEDI01_B_2020095203140_O07433_04_T04317_02_005_01_V002.h5"
reral_2a <- "E:\\GEDI\\GEDI_archive\\GEDI02A\\SA\\GEDI02_A_2020095203140_O07433_04_T04317_02_003_01_V002.h5"
level1b<-readLevel1B(level1Bpath = real_1b)
level2a<-readLevel2A(level2Apath = reral_2a)
shot_number="74331100400372152"
#wf <- getLevel1BWF(gedilevel1b, shot_number)
plotWFMetrics_v2(level1b, level2a, shot_number, rh=c(50, 75, 90))
dev.off()



# ------------------------------------------------------------------------------------------------ #
#figure scatterplot
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- Data[Data$GEDI_sim_rh_98 <15,]
Data <- na.omit(Data)
r2 = round(cor(Data$RHS_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RHS_98)
MD <- mean(Data$RHS_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RHS_98)
SD <- sd(Data$RHS_98 - Data$GEDI_sim_rh_98)
txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "Bias: ", round(MD,3),"m",
             "\n" , "Sample size: ", nrow(Data))


grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RHS_98)



ggplot(Data, aes(x=GEDI_sim_rh_98, y=RHS_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  annotation_custom(grob) +
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(2,"cm"))

out = "E:\\GEDI\\Result\\Figure\\figure scatterplot leaf on.jpg"
ggsave(out,height=12, width=12, dpi=600)




# ------------------------------------------------------------------------------------------------ #
#figure bias
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)

Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RHS_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RHS_98 - Data$GEDI_sim_rh_98)
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b1 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                     7,8,9,10,11,12,13,14,15),
                                      dig.lab=1), mean)
RB1 <- a1/b1*100

ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_point(aes(x = 2.675, y = a1[1]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 3.5, y = a1[2]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 4.5, y = a1[3]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 5.5, y = a1[4]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 6.5, y = a1[5]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 7.5, y = a1[6]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 8.5, y = a1[7]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 9.5, y = a1[8]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 10.5, y = a1[9]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 11.5, y = a1[10]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 12.5, y = a1[11]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 13.5, y = a1[12]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 14.5, y = a1[13]), color = "blue", size = 2,shape=8) +
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"))+
  theme(plot.title = element_text(hjust = 0.5))

out = "E:\\GEDI\\Result\\Figure\\figure bias leaf on.jpg"
ggsave(out,height=6, width=12, dpi=600)



