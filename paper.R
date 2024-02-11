#Author: Xiaoxuan Li
library(corrplot)
library(tidyverse)
library(psych)
library(xlsx)
library(car)
library(MASS)
library(rsq)
library(ggpmisc)
library(ggrepel)
library(dplyr)
library(rlist)
library(data.table)
library(lemon)
library(lmtest)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(mltools)
library(tidyr)
library(stats)
library(lidR)
library(tools) 
library(raster)
library(ggpointdensity)
library(viridis)
library(grid)
library(readxl)
library(ehaGoF)
library(Metrics)
library(rGEDI)
windowsFonts(A = windowsFont("Times New Roman"))
# ------------------------------------------------------------------------------------------------ #
#table for paper -- testcase
Locdir <- "E:\\GEDI\\Result\\Result"
pattern = 'testcase'
files <- list.files(path = Locdir, full.names = TRUE, pattern = paste0(pattern,"_sa.csv"))
listt <- list("Studysite","Orbit","Sample","Power","Coverage",
              "r2","Power_R2","Coverage_R2",
              "Mean", "RMSE","%RMSE",
              "MD","RB","MAE","Solar")
df <- data.frame(listt)
colnames(df) <- listt
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  Data1 <- Data[Data$Type == 1,]
  list1 <- list()
  Data = read.csv(i,header=T)
  Data <- Data %>% separate(Beam, c("Beam", "Type"))
  if(length(Data[,1])==0) next
  Data1 <- Data[Data$Type == 1,]
  Data2 <- Data[Data$Type == 0,]
  Data <- na.omit(Data)
  
  
  if (nrow(Data1) > 1){
    r2_p <- cor(Data1$RH1_98, Data1$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_p = 0
  }
  if (nrow(Data2) > 1){
    r2_c <- cor(Data2$RH1_98, Data2$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_c = 0
  }
  
  Mean <- mean(Data$RH1_98)       
  RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
  rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
  MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
  RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
  MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
  Sub = strsplit(substring(strsplit(i, "Result")[[1]][3], first = 2),"_")
  Studysite = Sub[[1]][1]
  Orbit = substring(Sub[[1]][2],first = 3)
  print(paste0("Studysite ",Studysite))
  print(paste0("Orbit: ",Orbit))
  print(paste0("Sample: ",nrow(Data)))
  print(paste0("Power: ",nrow(Data1)))
  print(paste0("Coverage: ",nrow(Data2)))
  print(paste0("r2: ",round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)))
  print(paste0("r2_p: ",r2_p))
  print(paste0("r2_c: ",r2_c))
  print(paste0("Mean ",round(Mean,3)))
  print(paste0("RMSE: ",round(RMSE,3)))
  print(paste0("RB: ",round(RB,3))) 
  print(paste0("%RMSE: ",round(rRMSE,3)))
  print(paste0("Mean bias: ",round(MD,3)))
  print(paste0("MAE: ",round(MAE,3)))
  print(paste0("Solar: ",mean(Data$solar_elevation)))
  list1[[1]] <- Studysite
  list1[[2]] <- Orbit
  list1[[3]] <- nrow(Data)
  list1[[4]] <- nrow(Data1)
  list1[[5]] <- nrow(Data2)
  list1[[6]] <- round(cor(Data$RH1_98, Data$GEDI_sim_rh_98)^2,3)
  list1[[7]] <- r2_p
  list1[[8]] <- r2_c
  list1[[9]] <- round(Mean,3)
  list1[[10]] <- round(RMSE,3)
  list1[[11]] <- round(rRMSE,3)
  list1[[12]] <- round(MD,3)
  list1[[13]] <- round(RB,3)
  list1[[14]] <- round(MAE,3)
  list1[[15]] <- mean(Data$solar_elevation)
  listt <- mapply(c, listt, list1, SIMPLIFY=FALSE)
  df1 <- data.frame(list1)
  colnames(df1) <- listt <- list("Studysite","Orbit","Sample","Power","Coverage",
                                 "r2","Power_R2","Coverage_R2",
                                 "Mean", "RMSE","%RMSE",
                                 "MD","RB","MAE","Solar")
  df <- rbind(df,df1)
}

write.table(df,file=file.path(Locdir,paste0(pattern,"_table98.csv")), quote=F,sep=",",row.names=F)


#table for paper -- all.csv
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
data = read.csv(filedir,header=T)
data <- na.omit(data)
data <- data[data$GEDI_sim_rh_98 >=2.35,]
data <- data %>% separate(Beam, c("Beam", "Type"))

listt <- list("Studysite","Orbit","Sample","Power","Coverage",
              "r2","Power_R2","Coverage_R2",
              "Mean", "RMSE","%RMSE",
              "MD","RB","MAE","Solar")
df <- data.frame(listt)
colnames(df) <- listt

orbits <- unique(data[c("site", "orbit")])

for (i in 1: nrow(orbits)){
  print(i)
  print(orbits[[1]][i])
  print(orbits[[2]][i])
  Data = data[ which(data$site ==orbits[[1]][i] & data$orbit == orbits[[2]][i]), ]
  list1 <- list()

  if(length(Data[,1])==0) next
  Data1 <- Data[Data$Type == 1,]
  Data2 <- Data[Data$Type == 0,]
  Data <- na.omit(Data)
  
  
  if (nrow(Data1) > 1){
    r2_p <- cor(Data1$RH1_98, Data1$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_p = 0
  }
  if (nrow(Data2) > 1){
    r2_c <- cor(Data2$RH1_98, Data2$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_c = 0
  }
  
  Mean <- mean(Data$RH1_98)       
  RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
  rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
  MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
  RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
  MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)

  Studysite = orbits[[1]][i]
  Orbit = orbits[[2]][i]
  print(paste0("Studysite ",Studysite))
  print(paste0("Orbit: ",Orbit))
  print(paste0("Sample: ",nrow(Data)))
  print(paste0("Power: ",nrow(Data1)))
  print(paste0("Coverage: ",nrow(Data2)))
  print(paste0("r2: ",round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)))
  print(paste0("r2_p: ",r2_p))
  print(paste0("r2_c: ",r2_c))
  print(paste0("Mean ",round(Mean,3)))
  print(paste0("RMSE: ",round(RMSE,3)))
  print(paste0("RB: ",round(RB,3))) 
  print(paste0("%RMSE: ",round(rRMSE,3)))
  print(paste0("Mean bias: ",round(MD,3)))
  print(paste0("MAE: ",round(MAE,3)))
  print(paste0("Solar: ",mean(Data$solar_elevation)))
  list1[[1]] <- Studysite
  list1[[2]] <- Orbit
  list1[[3]] <- nrow(Data)
  list1[[4]] <- nrow(Data1)
  list1[[5]] <- nrow(Data2)
  list1[[6]] <- round(cor(Data$RH1_98, Data$GEDI_sim_rh_98)^2,3)
  list1[[7]] <- r2_p
  list1[[8]] <- r2_c
  list1[[9]] <- round(Mean,3)
  list1[[10]] <- round(RMSE,3)
  list1[[11]] <- round(rRMSE,3)
  list1[[12]] <- round(MD,3)
  list1[[13]] <- round(RB,3)
  list1[[14]] <- round(MAE,3)
  list1[[15]] <- mean(Data$solar_elevation)
  listt <- mapply(c, listt, list1, SIMPLIFY=FALSE)
  df1 <- data.frame(list1)
  colnames(df1) <- listt <- list("Studysite","Orbit","Sample","Power","Coverage",
                                 "r2","Power_R2","Coverage_R2",
                                 "Mean", "RMSE","%RMSE",
                                 "MD","RB","MAE","Solar")
  df <- rbind(df,df1)
}
Locdir <- "E:\\GEDI\\Result\\Result"
write.table(df,file=file.path(Locdir,"_1010.csv"), quote=F,sep=",",row.names=F)
# ------------------------------------------------------------------------------------------------ #
#figure algorithm
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)

Title <- "SG S"
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1), mean)
RB <- a/b*100

x <- seq(2,14,1)
l <- cbind(x,RB)
df <- data.frame(l)
pS <- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  ggtitle(Title) +
  theme_bw()+
  coord_cartesian(ylim = c(-40, 50))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4,p5,p6,pS,ncol=3,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\figure algorithm diff.jpg"

ggsave(out,height=12, width=18, dpi=600)

# ------------------------------------------------------------------------------------------------ #
#back
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
Data$ground <- Data$elev_lowestmode_a5 - Data$elev_lowestmode_a3 
Data$top <- Data$elev_lowestmode_a5 + Data$RH5_98 - (Data$elev_lowestmode_a3 + Data$RH3_98)
Data$index <- 1:nrow(Data)

MD <- mean(Data$ground)
STD <- sd(Data$ground)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p1<-ggplot(Data, aes(x=index, y=ground)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Ground (SG 5-3)") +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



MD <- mean(Data$top)
STD <- sd(Data$top)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p2<- ggplot(Data, aes(x=index, y=top)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Canopy top (SG 5-3)") +
  theme(plot.title = element_text(hjust = 0.5))


#front
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
Data$ground <- Data$elev_lowestmode_a4 - Data$elev_lowestmode_a1 
Data$top <- Data$elev_lowestmode_a4 + Data$RH4_98 - (Data$elev_lowestmode_a1 + Data$RH1_98)
Data$index <- 1:nrow(Data)

MD <- mean(Data$ground)
STD <- sd(Data$ground)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p3<-ggplot(Data, aes(x=index, y=ground)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Ground (SG 4-1)") +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



MD <- mean(Data$top)
STD <- sd(Data$top)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p4<- ggplot(Data, aes(x=index, y=top)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Canopy top (SG 4-1)") +
  theme(plot.title = element_text(hjust = 0.5))


#gaussian
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
Data$ground <- Data$elev_lowestmode_a3 - Data$elev_lowestmode_a1 
Data$top <- Data$elev_lowestmode_a3 + Data$RH3_98 - (Data$elev_lowestmode_a1 + Data$RH1_98)
Data$index <- 1:nrow(Data)

MD <- mean(Data$ground)
STD <- sd(Data$ground)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p5<-ggplot(Data, aes(x=index, y=ground)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Ground (SG 3-1)") +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



MD <- mean(Data$top)
STD <- sd(Data$top)
txt <- paste("\n" , "Mean difference: ", round(MD,3),"m",
             "\n" , "Standard Deviation: ", round(STD,3))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p6<- ggplot(Data, aes(x=index, y=top)) +
  geom_point()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="Index", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))+
  ggtitle("Canopy top (SG 3-1)") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p3,p5,p2,p4,p6,nrow = 2,ncol = 3)

out = "E:\\GEDI\\Result\\Figure\\figure sg setting.jpg"

ggsave(out,height=12, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure factor
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98

Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$sensitivity_a1,breaks = c(0.9,0.91,0.92,0.93,0.94,0.95,
                                                    0.96,0.97,0.98),dig.lab=1)
Data$Beam[Data$Beam == "BEAM0000_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0010_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0001_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0011_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM1011_1"]="Power"
Data$Beam[Data$Beam == "BEAM0101_1"]="Power"
Data$Beam[Data$Beam == "BEAM0110_1"]="Power"
Data$Beam[Data$Beam == "BEAM1000_1"]="Power"
Data$Day[Data$orbit == "O02229"]="Day"
Data$Day[Data$orbit == "O06494"]="Day"
Data$Day[Data$orbit == "O07348"]="Day"
Data$Day[Data$orbit == "O06823"]="Day"
Data$Day[Data$orbit == "O06701"]="Day"
Data$Day[Data$orbit == "O02122"]="Day"
Data$Day[Data$orbit == "O05243"]="Day"
Data$Day[Data$orbit == "O06372"]="Day"
Data$Day[Data$orbit == "O09675"]="Day"
Data$Day[Data$orbit == "O08760"]="Day"
Data$Day[Data$orbit == "O02816"]="Day"
Data$Day[Data$orbit == "O09102"]="Day"
Data$Day[Data$orbit == "O03610"]="Day"
Data$Day[Data$orbit == "O09407"]="Day"
Data$Day[Data$orbit == "O02541"]="Day"
Data$Day[Data$orbit == "O03335"]="Day"
Data$Day[Data$orbit == "O02587"]="Day"
Data$Day[Data$orbit == "O04215"]="Day"
Data$Day[Data$orbit == "O06189"]="Day"
Data$Day[Data$orbit == "O09163"]="Day"
Data$Day[is.na(Data$Day)]="Night"
Data <- na.omit(Data)
Data$grp <- paste(Data[,"Beam"],Data[,"Day"])

Data <- Data[sample(nrow(Data), 100), ]

r1 <- compare_means(
  sensitivity_a1~grp, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r1[c("group1", "group2", "p.signif")]


#figure factor2
give.n <- function(x){
  return(c(x = x, y = 1, label = length(x)))
}
p1<- ggplot(Data, aes(x=grp, y=sensitivity_a1*100, fill=grp)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x="Beam types", 
       y="Sensitivity (%)")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25),axis.text=element_text(size=25))

out = "E:\\GEDI\\Result\\Figure\\figure factor2.jpg"
ggsave(out,height=12, width=18, dpi=600)



filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
#Data <- Data[Data$site != "Addo",]
#Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- Data[Data$status == "Leaf-on",]
Data <- na.omit(Data)
Data <- Data[Data$CC > 0,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98

Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$CC,breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                        0.6,0.7,0.8,0.9,1),dig.lab=1)



p2 <- ggplot(Data, aes(x=CC, y=sensitivity_a1, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  coord_cartesian(ylim = c(0.85, 1))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(minor_breaks = seq(-5, 5, 0.5))+
  theme_bw()+
  labs(x="Canopy cover",y="Sensitivity (%)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



ggplot(Data, aes(x=CC, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-6, 4))+
  scale_y_continuous(minor_breaks = seq(-6, 4, 1),breaks = seq(-6, 4, 2))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1))+
  theme_bw()+
  labs(x="Canopy cover",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25),axis.text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

out = "E:\\GEDI\\Result\\Figure\\figure factor CC.jpg"
ggsave(out,height=12, width=18, dpi=600)
# ------------------------------------------------------------------------------------------------ #
#figure green_phenology
#diff~green 
filedir <- "E:\\GEDI\\Result\\Result\\1010.csv"
Data = read.csv(filedir,header=T)

Data$Colour = 0
Data$Colour[Data$status == "Leaf-on"]="#90ee90"
Data$Colour[Data$status == "Leaf-off"]="red"
Data$Colour[Data$status == "Transition"]="blue"
Data$SVI = Data$NDVI_diff/Data$SD_NDVI
reg1 <- lm(Data$MD ~ Data$SVI, data = Data)
coeff <- round(reg1$coefficients , 2)


p1 <- ggplot(Data, aes(x=SVI, y=RB, color=status))+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank(),legend.position=c(0.8,0.2))+
  scale_color_manual(labels = c("Leaf-off","Leaf-on", "Transition"), 
                     values = c("red","#90ee90", "blue"))+
  theme(text=element_text(size=25))+ 
  theme(text=element_text(family="A"))+
  annotate("text",x=-6,y=0.8,hjust = 0,size = 8,family= "A",
           label= paste(expression(R^2),": ",round(summary(reg1)$adj.r.squared,3)), parse=TRUE) + 
  geom_point(size = 3)+
  geom_smooth(
    method="lm", 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+ 
  labs(x="Relative Greenness", y="%Bias (%)")

#diff~phenology boxplot
give.n <- function(x){
  return(c(x = x, y = 0, label = length(x)))
}
p2 <- ggplot(Data, aes(x=status, y=RB, fill=status)) + 
  theme_bw()+
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2)+
  theme(text=element_text(size=25)) +
  stat_summary(fun.data = give.n, geom = "text",size=7)+ 
  labs(x="Phenology")+
  theme(axis.title.y=element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank(),legend.position=c(0.8,0.2))+
  scale_fill_manual(values=c("red","#90ee90", "blue"))

ggarrange(p1,p2,ncol=2,nrow=1) 

out = "E:\\GEDI\\Result\\Figure\\figure phenology_greenness.jpg"
ggsave(out,height=12, width=12, dpi=600)



# ------------------------------------------------------------------------------------------------ #
#figure p98
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_p98.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RH1_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)


p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
         ),parse=TRUE) + 
  annotate("text",x=3,y=11.4,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " %Bias: ", round(RB,3),"%",
             "\n" , " Sample size: ", nrow(Data))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  geom_abline(intercept = MD, slope = 1, color="red", 
              linetype="solid", size=1.5)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))



Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)


p3 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=11.4,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " %Bias: ", round(RB,3),"%",
             "\n" , " Sample size: ", nrow(Data))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  geom_abline(intercept = MD, slope = 1, color="red", 
              linetype="solid", size=1.5)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))

ggarrange(p2,p3,ncol=2,nrow=1) 

out = "E:\\GEDI\\Result\\Figure\\figure p98.jpg"
ggsave(out,height=12, width=18, dpi=600)



# ------------------------------------------------------------------------------------------------ #

#zg/toploc test 
filedir <- "E:\\GEDI\\Result\\Result\\test_zg.xlsx"
Data = read_excel(filedir)
Data <- Data[Data$status != "Transition",]
Data <- Data[Data$status != "Evergreen",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- Data[Data$ground >=-50,]
Data <- Data[Data$top >=-50,]

Data1 <- Data[Data$status == "Leaf-on",]
Data2 <- Data[Data$status == "Leaf-off",]

filedir <- "E:\\GEDI\\Result\\Result\\test_zg.xlsx"
Data3 = read_excel(filedir)
Data3 <- Data3[Data3$status != "Transition",]
Data3 <- Data3[Data3$status != "Evergreen",]
Data3 <- Data3[Data3$GEDI_sim_rh_98 <=2.35,]
Data3 <- Data3[Data3$ground >=-50,]
Data3 <- Data3[Data3$top >=-50,]

myvars <- c("ground","top")
Data <- Data[myvars]
Melt <- melt(Data)

MD_1 <- mean(Data$top)
STD_1 <- sd(Data$top)
MD_2 <- mean(Data$ground)
STD_2 <- sd(Data$ground)

Data1<-Data1[sample(nrow(Data1), 100), ]
Data2<-Data2[sample(nrow(Data2), 100), ]

t.test(Data1$ground,Data2$ground)
t.test(Data1$top,Data2$top)



txt <- paste("\n" , "Canopy top mean: ", round(MD_1,3),"m",
             "\n" , "Canopy top STD: ", round(STD_1,3),
             "\n" , "Ground mean: ", round(MD_2,3),"m",
             "\n" , "Ground STD: ", round(STD_2,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.3, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))


p1<- ggplot(Melt, aes(x=variable, y=value)) + 
  geom_boxplot()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25),axis.text=element_text(size=25))+
  ggtitle("GEDI Leaf-on & Leaf-off (>2.35m)") +
  theme(plot.title = element_text(hjust = 0.5))


Data1 <- Data1[myvars]
Melt <- melt(Data1)

MD_1 <- mean(Data1$top)
STD_1 <- sd(Data1$top)
MD_2 <- mean(Data1$ground)
STD_2 <- sd(Data1$ground)

txt <- paste("\n" , "Canopy top mean: ", round(MD_1,3),"m",
             "\n" , "Canopy top STD: ", round(STD_1,3),
             "\n" , "Ground mean: ", round(MD_2,3),"m",
             "\n" , "Ground STD: ", round(STD_2,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.3, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p2<- ggplot(Melt, aes(x=variable, y=value)) + 
  geom_boxplot()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25),axis.text=element_text(size=25))+
  ggtitle("GEDI Leaf-on (>2.35m)") +
  theme(plot.title = element_text(hjust = 0.5))


Data2 <- Data2[myvars]
Melt <- melt(Data2)

MD_1 <- mean(Data2$top)
STD_1 <- sd(Data2$top)
MD_2 <- mean(Data2$ground)
STD_2 <- sd(Data2$ground)

txt <- paste("\n" , "Canopy top mean: ", round(MD_1,3),"m",
             "\n" , "Canopy top STD: ", round(STD_1,3),
             "\n" , "Ground mean: ", round(MD_2,3),"m",
             "\n" , "Ground STD: ", round(STD_2,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.3, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p3<- ggplot(Melt, aes(x=variable, y=value)) + 
  geom_boxplot()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25),axis.text=element_text(size=25))+
  ggtitle("GEDI Leaf-off (>2.35m)") +
  theme(plot.title = element_text(hjust = 0.5))


Data3 <- Data3[myvars]
Melt <- melt(Data3)

MD_1 <- mean(Data3$top)
STD_1 <- sd(Data3$top)
MD_2 <- mean(Data3$ground)
STD_2 <- sd(Data3$ground)

txt <- paste("\n" , "Canopy top mean: ", round(MD_1,3),"m",
             "\n" , "Canopy top STD: ", round(STD_1,3),
             "\n" , "Ground mean: ", round(MD_2,3),"m",
             "\n" , "Ground STD: ", round(STD_2,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.3, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

p4<- ggplot(Melt, aes(x=variable, y=value)) +
  geom_boxplot()+
  theme_bw()+
  annotation_custom(grob) +
  theme(legend.title = element_blank())+
  labs(x="", 
       y="Difference (m)")+
  coord_cartesian(ylim = c(-10, 6))+
  scale_y_continuous(minor_breaks = seq(-10, 6, 2),breaks = seq(-10, 6, 2))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25),axis.text=element_text(size=25))+
  ggtitle("GEDI Leaf-on & Leaf-off (<2.35m)") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p4,p2,p3)

out = "E:\\GEDI\\Result\\Figure\\figure GZ.jpg"

ggsave(out,height=12, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure bias
filedir <- "E:\\GEDI\\Result_2022\\Result\\All_rh_biom.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-on" | Data$status == "Evergreen",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]

Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
b1 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                    7,8,9,10,11,12,13,14,15),
                                     dig.lab=1), mean)
RB1 <- a1/b1*100

p1<- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
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
  theme(axis.title.x=element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x1 <- seq(2,14,1)
l1 <- cbind(x1,RB1)
df1 <- data.frame(l1)

p3 <- ggplot(df1, aes(x=x1+0.5, y=RB1)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-on+evergreen)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                    7,8,9,10,11,12,13,14,15),
                     dig.lab=1)

a2 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
b2 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                    7,8,9,10,11,12,13,14,15),
                                     dig.lab=1), mean)
RB2 <- a2/b2*100


p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_point(aes(x = 2.675, y = a2[1]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 3.5, y = a2[2]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 4.5, y = a2[3]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 5.5, y = a2[4]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 6.5, y = a2[5]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 7.5, y = a2[6]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 8.5, y = a2[7]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 9.5, y = a2[8]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 10.5, y = a2[9]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 11.5, y = a2[10]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 12.5, y = a2[11]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 13.5, y = a2[12]), color = "blue", size = 2,shape=8) +
  geom_point(aes(x = 14.5, y = a2[13]), color = "blue", size = 2,shape=8) +
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = seq(0, 15, 1),breaks = seq(0, 15, 1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-on+evergreen)"), 
       y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x2 <- seq(2,14,1)
l2 <- cbind(x2,RB2)
df2 <- data.frame((l2))

p4 <- ggplot(df2, aes(x=x2+0.5, y=RB2)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  theme(axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-off)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2) 

out = "E:\\GEDI\\Result\\Figure\\figure bias.jpg"
ggsave(out,height=12, width=18, dpi=600)



#height distribution
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
#Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data$group_RH <- cut(Data$GEDI_sim_rh_98,
                     breaks = c(0,2.35,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                     dig.lab=3)
#Data$group_RH <- gsub(',', '-', Data$group_RH)
ggplot(Data, aes(x=group_RH)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(aes(y = stat(count) / sum(count)),
                 color =  "white",fill = "grey",stat="count",alpha = 0.8) +
  xlab(expression("GEDI-RH98"["Sim"]~"(m)")) + ylab("Frequency (%)")+
  scale_x_discrete(labels=c("0-2.35","2.35-3","3-4","4-5","5-6","6-7","7-8",
                            "8-9","9-10","10-11","11-12","12-13","13-14","14-15"))+
  scale_y_continuous(labels = scales::percent)+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25),axis.text=element_text(size=25))


out = "E:\\GEDI\\Result\\Figure\\figure height histogram.jpg"
ggsave(out,height=12, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#Addo supplementary
filedir <- "E:\\GEDI\\Result_2022\\Result\\All_rh_biom.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site == "Addo",]

Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Bias: ", round(MD,3),"m",
             "\n" , "%Bias: ", round(RB,3),"%",
             "\n" , "Sample size: ", nrow(Data))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.77, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)

ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=9.1,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=8,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " %Bias: ", round(RB,3),"%",
             "\n" , " Sample size: ", nrow(Data))) + 
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,10),minor_breaks = seq(0,10,2))+
  scale_y_continuous(limits = c(min2,10),minor_breaks = seq(0,10,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))

out = "E:\\GEDI\\Result\\Figure\\figure p98 addo.jpg"
ggsave(out,height=12, width=12, dpi=600)

#Duku supplementary
filedir <- "E:\\GEDI\\Result_2022\\Result\\All_rh_biom.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site == "Duku",]

Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Bias: ", round(MD,3),"m",
             "\n" , "%Bias: ", round(RB,3),"%",
             "\n" , "Sample size: ", nrow(Data))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.77, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)

ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=17.5,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=15,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " %Bias: ", round(RB,3),"%",
             "\n" , " Sample size: ", nrow(Data))) + 
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,20),minor_breaks = seq(0,20,2))+
  scale_y_continuous(limits = c(min2,20),minor_breaks = seq(0,20,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))

out = "E:\\GEDI\\Result\\Figure\\figure p98 duku.jpg"
ggsave(out,height=12, width=12, dpi=600)
# ------------------------------------------------------------------------------------------------ #

#evergreen leaf on
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)


Data <- Data[Data$GEDI_sim_rh_98 <= 4,]
Data <- Data[Data$GEDI_sim_rh_98 >= 2.35,]

100*mean(Data$RH1_98 - Data$GEDI_sim_rh_98)/mean(Data$GEDI_sim_rh_98)

#transition
filedir <- "E:\\GEDI\\Result\\Result\\All_transition.csv"
Data = read.csv(filedir,header=T)

#leaf off
filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

#all
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)


Data <- Data[Data$GEDI_sim_rh_98 <= 15,]

Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
bias <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*bias/mean(Data$GEDI_sim_rh_98)
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)

print(r2)
print(bias)
print(RB)
print(RMSE)
print(rRMSE)





#canopy cover


#sensitivity ~ bias
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
out = "E:\\GEDI\\Result\\Figure\\figure Sensitivity CC.jpg"

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data = read.csv(filedir,header=T)
out = "E:\\GEDI\\Result\\Figure\\figure CC on.jpg"

filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)
out = "E:\\GEDI\\Result\\Figure\\figure CC off.jpg"

Data <- na.omit(Data)
Data <- Data[Data$CC > 0,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98

Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$CC,breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                                    0.6,0.7,0.8,0.9,1),dig.lab=1)



ggplot(Data, aes(x=CC, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  coord_cartesian(ylim = c(-4, 3))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(minor_breaks = seq(-5, 5, 0.5))+
  theme_bw()+
  labs(x="Canopy cover",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$sensitivity_a1,breaks = 10,dig.lab=1)

ggplot(Data, aes(x=sensitivity_a1, y=diff,group=group_RH)) + 
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(Data, aes(x=CC, y=sensitivity_a1, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  coord_cartesian(ylim = c(0.85, 1))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(minor_breaks = seq(-5, 5, 0.5))+
  theme_bw()+
  labs(x="Canopy cover",y="Sensitivity")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


p3<- ggplot(Data, aes(x=CC, y=GEDI_sim_rh_98, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  coord_cartesian(ylim = c(0, 15))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(minor_breaks = seq(0, 15, 1))+
  theme_bw()+
  labs(x="Canopy cover",y=expression("GEDI-RH98"["Sim"]~"(m)"))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,ncol=2,nrow=1) 
ggsave(out,height=12, width=18, dpi=600)


round(table(cut(Data$CC,breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))/13056,2)

round(table(cut(Data$CC,breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))/9379,2)

tapply(Data$diff, cut(Data$CC,breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                         0.6,0.7,0.8,0.9,1),
                      dig.lab=1), median)




#algorithm 10
out = "E:\\GEDI\\Result\\Figure\\figure 7 algor on.jpg"
filedir <- "E:\\GEDI\\Result\\Result\\All_on_sa.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen_sa.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  
Data <- na.omit(Data)

out = "E:\\GEDI\\Result\\Figure\\figure 7 algor off.jpg"
filedir <- "E:\\GEDI\\Result\\Result\\All_off_sa.csv"
Data = read.csv(filedir,header=T)


out = "E:\\GEDI\\Result\\Figure\\figure 7 algor.jpg"
filedir <- "E:\\GEDI\\Result\\Result\\All_sa.csv"
Data = read.csv(filedir,header=T)



Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                    7,8,9,10,11,12,
                                                    13,14,15)
                     ,dig.lab=1)

a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                                    7,8,9,10,11,12,13,14,15),
                                     dig.lab=1), mean)
RB <- a/b*100
x <- seq(1,14,1)
l <- cbind(x,RB)
df <- data.frame(l)

p10 <- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 70))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%Bias (%) (S)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

x <- c(1,2,3,4,5,6,7)
y_r2 <- c(round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH2_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH4_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH5_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH6_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
)
l <- cbind(x,y_r2)
df <- data.frame(l)

pr <-ggplot(df, aes(x=x, y=y_r2)) + 
  geom_bar(stat='identity')+
  scale_x_continuous(minor_breaks = seq(1, 7, 1),breaks= x, labels=c("1","2","3","4","5","6","S"))+
  scale_y_continuous(limits = c(0,0.8))+
  geom_text(aes(label=y_r2,family="A",size = 20), 
            position=position_dodge(width=0.9), 
            vjust=-1)+
  theme_bw()+
  labs(x="Setting group number", 
       y=expression("R"^2))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4,p5,p6,p10,ncol=4,nrow=2) 
ggsave(out,height=12, width=18, dpi=600)




#bias~rh98

out = "E:\\GEDI\\Result\\Figure\\figure cc bias.jpg"
filedir <- "E:\\GEDI\\Result\\Result\\All_CC.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)


Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data <- Data[Data$CC > 0,]


Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$CC,breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                        0.6,0.7,0.8,0.9,1),dig.lab=1)




ggplot(Data, aes(x=CC, y=GEDI_sim_rh_98, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single"))+
  coord_cartesian(ylim = c(0, 15))+
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1),breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(minor_breaks = seq(0, 15, 1))+
  theme_bw()+
  labs(x="Canopy cover",y=expression("GEDI-RH98"["Sim"]~"(m)"))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(out,height=12, width=18, dpi=600)






filedir <- "E:\\GEDI\\Result\\Result_20210831\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 <= 2.35,]
plot(Data$GEDI_sim_rh_90,Data$RH1_90)




filedir <- "E:\\GEDI\\Result\\Result_20210831\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >= 8,]
Data <- Data[Data$GEDI_sim_rh_98 <= 12,]





filedir <- "E:\\GEDI\\Result\\Result_20210831\\All_mode.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data1 <- Data[Data$GEDI_sim_rh_98 >= 2.35,]
Data2 <- Data[Data$GEDI_sim_rh_98 <= 7,]
nrow(subset(Data1,modes == 1))/nrow(Data1)
nrow(subset(Data2,modes == 1))/nrow(Data2)






filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)

Data1 <- Data[Data$selected_algorithm == 1,]
Data2 <- Data[Data$selected_algorithm == 2,]
Data5 <- Data[Data$selected_algorithm == 5,]
Data10 <- Data[Data$selected_algorithm == 10,]







filedir <- "E:\\GEDI\\Result\\Result_20210831\\All.csv"
df = read.csv(filedir,header=T)

Data <- na.omit(df)
Data <- Data[Data$GEDI_sim_rh_98 <= 2.35,]
Data <- Data[Data$GEDI_sim_rh_98 >= 2.35,]
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)


print(r2)
print(MD)
print(RB)
print(RMSE)
print(rRMSE)

Data <- na.omit(df)
Data <- Data[Data$GEDI_sim_rh_98_y <= 15,]
r2 = round(cor(Data$RH1_98_y, Data$GEDI_sim_rh_98_y,method = "pearson")^2,3)
MD <- mean(Data$RH1_98_y - Data$GEDI_sim_rh_98_y)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98_y)
RMSE <- sqrt(mean((Data$RH1_98_y - Data$GEDI_sim_rh_98_y)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98_y - Data$GEDI_sim_rh_98_y)^2))/mean(Data$GEDI_sim_rh_98_y)
print("On-orbit")
print(r2)
print(MD)
print(RB)
print(RMSE)
print(rRMSE)


r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
print("On-orbit")
print(r2)
print(MD)
print(RB)
print(RMSE)
print(rRMSE)


#figure p98
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)

Data <- na.omit(Data)
round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))

Data1 <- Data[Data$selected_algorithm == 1,]
Data2 <- Data[Data$selected_algorithm == 2,]
Data5 <- Data[Data$selected_algorithm == 5,]
Data10 <- Data[Data$selected_algorithm == 10,]






filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data <- na.omit(Data)
round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))

#3d
library(plot3D)
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data <- na.omit(Data)
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
scatter3D(Data$GEDI_sim_rh_98,Data$diff,Data$CC,pch = 19,main="Height~Bias~CC",cex = 1,colkey = TRUE,  
          axis.ticks=TRUE,axis.scales=TRUE,
          bty = "u",col.panel ="white",phi = 0,alpha=1,theta=45,surface=TRUE,
          col.grid = "gray50", xlab="Height (m)", ylab="Bias (m)", zlab="CC")


#addo p98

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data = read.csv(filedir,header=T)

#SG3
Data$RH1_98 <- Data$RH3_98

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]

Data <- Data[Data$site == "Addo",]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Bias: ", round(MD,3),"m",
             "\n" , "%Bias: ", round(RB,3),"%",
             "\n" , "Sample size: ", nrow(Data))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.77, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

max1 <- max(Data$GEDI_sim_rh_98)
max2 <- max(Data$RH1_98)



ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(1.5,10),minor_breaks = seq(0,10,0.2))+
  scale_y_continuous(limits = c(1.5,10),minor_breaks = seq(0,10,0.2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  annotation_custom(grob) +
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  geom_abline(intercept = MD, slope = 1, color="red", 
              linetype="solid", size=1.5)+
  geom_abline(intercept = MD + SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD + 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


#histogram of CHMs
i <- "E:\\GEDI\\ALS_archive\\LiDAR_CHM_Mosaic\\Addo.tif"
chm = raster(i)
chm2 = chm[chm < 2.35] 

Data<- as.data.frame(chm)
Data <- na.omit(Data)
Data$group_RH <- cut(Data$Addo,breaks = seq(0,20,1),dig.lab=1)
ggplot(Data, aes(x=group_RH)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(aes(y = stat(count) / sum(count)),
                 color =  "white",fill = "blue",stat="count",alpha = 0.2) +
  ggtitle(basename(i)) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(expression("CHM (m)")) + ylab("Frequency (%)")+
  coord_cartesian(xlim = c(0, 20))+
  scale_y_continuous(labels = scales::percent)+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))

#histogram of point cloud < 2.35
#bulk thin process--CC method for 2018 data -- test -- outdated
Locdir <- "E:\\GEDI\\Result\\ALS\\PointCloud"
files <- list.files(path = Locdir, full.names = TRUE,pattern=".las")
for (i in files){
  print(i)
  las = readLAS(i,select = "i,c,r")
  las = normalize_height(las, tin(),na.rm = TRUE)
  out <- file.path(Locdir,paste0(strsplit(basename(i),'.las')[[1]],"_nlz.las"))
  writeLAS(las, out, index = FALSE)
}



library(lattice)
library(gridExtra)
library(grid)
i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Addo.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p1<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Agincourt.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p2<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\DNyala.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p3<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Duku.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p4<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Ireagh.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p5<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Justicia.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p6<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Venetia.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p7<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Welverdiendt.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p8<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)

i <- "E:\\GEDI\\Result\\ALS\\PointCloud\\Merge\\Limpopo.txt"
print(basename(i))
a<-read.table(i, header = FALSE, sep = "", dec = ".")
p9<- histogram(a$V1,xlim=c(-0.5, 2.5),main=basename(i),freq=FALSE)


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol=3)




#algirithm test -- backthreshold
#figure algorithm
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)

a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                           dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                                     dig.lab=1), mean)
RB <- a/b*100

x <- seq(2,14,1)
l <- cbind(x,RB)
df <- data.frame(l)
p3<- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 60))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG3, 6 sigma)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data$diff2 <- Data$RH2_98 - Data$GEDI_sim_rh_98
a2 <- tapply(Data$diff2, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                             dig.lab=1), mean)
RB2 <- a2/b*100
x <- seq(2,14,1)
l <- cbind(x,RB2)
df <- data.frame(l)

p2<- ggplot(df, aes(x=x+0.5, y=RB2)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 60))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG2, 3 sigma)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data$diff5 <- Data$RH5_98 - Data$GEDI_sim_rh_98
a5 <- tapply(Data$diff5, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                             dig.lab=1), mean)
RB5 <- a5/b*100
x <- seq(2,14,1)
l <- cbind(x,RB5)
df <- data.frame(l)

p5<- ggplot(df, aes(x=x+0.5, y=RB5)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 60))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG5, 2 sigma)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(p3,p2,p5)




ggplot(df, aes(x=x+0.5, y=RB_diff)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-60, 20))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG4-SG1)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >= 8,]
Data <- Data[Data$GEDI_sim_rh_98 <= 12,]



#algorithm test -- smooth width zcross
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)

a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                           dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                                     dig.lab=1), mean)
RB <- a/b*100

x <- seq(2,14,1)
l <- cbind(x,RB)
df <- data.frame(l)
p3<- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG3)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data$diff1 <- Data$RH1_98 - Data$GEDI_sim_rh_98
a1 <- tapply(Data$diff1, cut(Data$GEDI_sim_rh_98,breaks = breakbin,
                             dig.lab=1), mean)
RB1 <- a1/b*100
x <- seq(2,14,1)
l <- cbind(x,RB1)
df <- data.frame(l)

p1<- ggplot(df, aes(x=x+0.5, y=RB1)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SG1)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p3)


#figure algorithm
filedir <- "E:\\GEDI\\Result\\Result_20210831\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]

Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1), mean)
RB <- a/b*100

x <- seq(2,14,1)
l <- cbind(x,RB)
df <- data.frame(l)
pS <- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 50))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%bias (SGS)(%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = breakbin,
                     breaks = breakbin)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4,p5,p6,pS,ncol=3,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\figure algorithm diff.jpg"

ggsave(out,height=12, width=18, dpi=600)




#error test -- ZG (ground)

filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]

#test back 3
ggplot(Data) + 
  geom_jitter(aes(elev_lowestmode_a3,elev_lowestmode_a2), colour="red") + 
  geom_smooth(aes(elev_lowestmode_a3,elev_lowestmode_a2), method=lm, se=FALSE)+
  
  geom_jitter(aes(elev_lowestmode_a3,elev_lowestmode_a5), colour="orange") + 
  geom_smooth(aes(elev_lowestmode_a3,elev_lowestmode_a5), method=lm, se=FALSE)+
  
  geom_jitter(aes(elev_lowestmode_a3,elev_lowestmode_a6), colour="yellow") + 
  geom_smooth(aes(elev_lowestmode_a3,elev_lowestmode_a6), method=lm, se=FALSE)
  
  
  
#test front 4

#test gaussian 1


#error test -- RH98+ZG (top)
#figure algorithm
#figure algorithm
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site != "Addo",]
Data <- Data[Data$site != "Duku",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
Data1 <- Data[Data$GEDI_sim_rh_98 <=3,]
a3_1 <- Data$elev_lowestmode_a3 - Data$elev_lowestmode_a1
h3_1 <- Data$elev_lowestmode_a3 - Data$elev_lowestmode_a1 + Data$RH3_98 - Data$RH1_98

plot(a3_1)
plot(h3_1)




filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)

Data <- Data[Data$status == "Leaf-on",]

Data <- Data[Data$status == "Leaf-off",]

Data <- Data[Data$site == "Welverdiendt" | Data$site == "Justicia" | Data$site == "Ireagh"| Data$site == "Agincourt",]

Data <- Data[Data$site == "Venetia" | Data$site == "Limpopo1" | Data$site == "Limpopo2"| Data$site == "Limpopo3"| Data$site == "DNyala",]

round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
100*MD/mean(Data$GEDI_sim_rh_98)
sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)


myvars <- c("ground", "orbit", "shot_number","Beam","RH1_98","RH3_98")
newdata <- Data[myvars]
write.xlsx(newdata, "C:/Users/Shawn/Desktop/newdata.xlsx")












#histogram of GEDI L4A
filedir1 <- "E:\\GEDI\\Result\\Result\\Welverdiendt_c.csv"
Data = read_csv(filedir1)
Data1 <- Data[Data$status == "Leaf-on" & Data$site == "Welverdiendt",]
Data2 <- Data[Data$status == "Leaf-off" & Data$site == "Welverdiendt",]

RH_on_mean <- mean(Data1$RH1_98)
RH_off_mean <- mean(Data2$RH1_98)
Biom_on_mean <- mean(Data1$AGBD)
Biom_off_mean <- mean(Data2$AGBD)

p1<- ggplot(Data1, aes(RH1_98)) +
  theme_bw()+
  annotate("text",x=7,y=700,hjust = 0,size = 7,family= "A",
           label= paste0("Blue: Leaf-on \nRed: Leaf-off \nLeaf-on count: 2730\nLeaf-off count: 3641")) + 
  coord_cartesian(xlim = c(2, 13),ylim = c(0, 1200))+
  scale_x_continuous(minor_breaks = seq(2, 13, 1),breaks = seq(2, 13, 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 15,alpha = 0.2)+
  geom_histogram(data = Data2, fill = "red", bins = 15,alpha = 0.2) + 
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20)) 


p2<- ggplot(Data1, aes(AGBD)) +
  theme_bw()+
  annotate("text",x=15,y=1000,hjust = 0,size = 7,family= "A",
           label= paste0("Blue: Leaf-on \nRed: Leaf-off \nLeaf-on count: 2730\nLeaf-off count: 3641")) + 
  coord_cartesian(xlim = c(0, 50))+
  scale_x_continuous(minor_breaks = seq(0, 50, 10),breaks = seq(0, 50, 10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2)+
  geom_histogram(data = Data2, fill = "red",bins = 30, alpha = 0.2) + 
  xlab("Biomass (Mg/ha)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20)) 

ggarrange(p1,p2,ncol=2,nrow=1) 


Data <- Data1
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98

breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b1 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                     7,8,9,10,11,12,13,14,15),
                                      dig.lab=1), mean)
RB1 <- a1/b1*100

p1<- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(x = "RH98 (m) (Leaf-on)",
    y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data <- Data2
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                    7,8,9,10,11,12,13,14,15),
                     dig.lab=1)

a2 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b2 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                     7,8,9,10,11,12,13,14,15),
                                      dig.lab=1), mean)
RB2 <- a2/b2*100


p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = seq(0, 15, 1),breaks = seq(0, 15, 1))+
  theme_bw()+
  labs(x = "RH98 (m) (Leaf-off)",
       y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,ncol=2,nrow=1) 






#histogram of GEDI L4A
filedir1 <- "E:\\Biomass\\CSIR\\result\\biom_combo.csv"
Data = read_csv(filedir1)
Data = Data[Data$ql4 == 1,]
Data1 <- Data[Data$status == "Leaf-on" & Data$site == "Welverdiendt",]
Data2 <- Data[Data$status == "Leaf-off" & Data$site == "Welverdiendt",]

Data <- Data1
Data$diff <- Data$Diff/0.049
Data$Combo <- Data$Combo/0.049
#Data <- Data[Data$Combo <6,]

Data$group_RH <- cut(Data$Combo,breaks = seq(0,140, 10),
                     dig.lab = 5)
a2 <- tapply(Data$diff, cut(Data$Combo,breaks = seq(0,140, 10),
                            dig.lab = 5), mean)
b2 <- tapply(Data$Combo, cut(Data$Combo,breaks = seq(0,140, 10),
                                      dig.lab=1), mean)
RB2 <- a2/b2*100
write.table(a2,file="C:\\Users\\Shawn\\Desktop\\a2_on.csv",sep=" ")

Data <- na.omit(Data)

p1<- ggplot(Data, aes(x=group_RH, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.3,position = position_dodge(width = 0.75)) +
  geom_boxplot(width = 0.5)+
  coord_cartesian(ylim = c(-30, 100))+
  #scale_y_continuous(minor_breaks = seq(0,6, 1),breaks = seq(0,6, 1))+
  theme_bw()+
  labs(x = "Biomass (Mg/ha) (Leaf-on)",
       y="Bias (Mg/ha)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


 
Data <- Data2
Data$diff <- Data$Diff/0.049
Data$Combo <- Data$Combo/0.049
#Data <- Data[Data$Combo <6,]

Data$group_RH <- cut(Data$Combo,breaks = seq(0,140, 10),
                     dig.lab = 5)

a2 <- tapply(Data$diff, cut(Data$Combo,breaks = seq(0,140, 10),
                            dig.lab = 5), mean)
b2 <- tapply(Data$Combo, cut(Data$Combo,breaks = seq(0,140, 10),
                             dig.lab=1), mean)
RB2 <- a2/b2*100
write.table(a2,file="C:\\Users\\Shawn\\Desktop\\a2_off.csv",sep=" ")

Data <- na.omit(Data)

p2<- ggplot(Data, aes(x=group_RH, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.3,position = position_dodge(width = 0.75)) +
  geom_boxplot(width = 0.5)+
  coord_cartesian(ylim = c(-30, 100))+
  #scale_y_continuous(minor_breaks = seq(0,6, 1),breaks = seq(0,6, 1))+
  theme_bw()+
  labs(x = "Biomass (Mg/ha) (Leaf-off)",
       y="Bias (Mg/ha)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,ncol=2,nrow=1) 


out = "C:\\Users\\Shawn\\Desktop\\Biom_bias.jpg"
ggsave(out,height=12, width=30, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#p98 ALS P98 vs. simulated RH98
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$GEDI_sim_rh_98 >0,]
Data <- Data[Data$p98 >0,]
Data <- na.omit(Data)
#Data <- Data[Data$status == "Leaf-off",]
r2 = round(cor(Data$p98,Data$GEDI_sim_rh_98, method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$p98-Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$p98-Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$GEDI_sim_rh_98-Data$p98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)

ggplot(Data, aes(x=p98, y=GEDI_sim_rh_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  coord_cartesian(xlim = c(0, 15),ylim = c(0, 15))+
  annotate("text",x=3,y=14.5,hjust = 0,size = 10,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=12.1,hjust = 0,size = 10,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(MD,3),"m",
             "\n" , " %Bias: ", round(RB,3),"%"
           )) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=35)) + 
  scale_x_continuous(limits = c(0,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(0,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(y=expression("GEDI-RH98"["Sim"]~"(m)"), 
       x=expression("ALS P98 (m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


out = "E:\\GEDI\\Result\\backup\\Result\\figure scatterplot p98 vs. GEDI simulated 98.jpg"
ggsave(out,height=12, width=12, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#scatterplot on-orbit vs simulated GEDI RH98
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_RH1.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RH1_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)
p1 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=11.3,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(MD,3),"m",
             "\n" , " %Bias: ", round(RB,3),"%"
           )) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-on)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_RH1.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$GEDI_sim_rh_98 >=2.35,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH1_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH1_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RH1_98)
MD <- mean(Data$RH1_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RH1_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH1_98)
p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH1_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=11.3,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(MD,3),"m",
             "\n" , " %Bias: ", round(RB,3),"%"
           )) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-off)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


ggarrange(p1,p2,ncol=2,nrow=1) 
out = "E:\\GEDI\\Result\\backup\\Result\\figure scatterplot on-orbit vs. sim GEDI RH98.jpg"
ggsave(out,height=12, width=24, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#scatterplot on-orbit vs. p98
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-on",]
Data$GEDI_sim_rh_98 <- Data$p98
Data <- Data[Data$GEDI_sim_rh_98 >=0,]
Data <- na.omit(Data)
r2 = round(cor(Data$RHS_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RHS_98)
MD <- mean(Data$RHS_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
SD <- sd(Data$RHS_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RHS_98)


p1 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RHS_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=11.3,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(MD,3),"m",
             "\n" , " %Bias: ", round(RB,3),"%"
           )) +  
  coord_cartesian(xlim=c(2, 15),ylim = c(2, 15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(2,15),breaks = seq(2,15,2))+
  scale_y_continuous(limits = c(2,15),breaks = seq(2,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(x=expression("ALS P98 (m) (Leaf-on)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-off",]
Data$GEDI_sim_rh_98 <- Data$p98
Data <- Data[Data$GEDI_sim_rh_98 >=0,]
Data <- na.omit(Data)
r2 = round(cor(Data$RHS_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RHS_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
bias(Data$GEDI_sim_rh_98, Data$RHS_98)
MD <- mean(Data$RHS_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH1_98)
SD <- sd(Data$RHS_98 - Data$GEDI_sim_rh_98)
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RHS_98)


p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RHS_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotate("text",x=3,y=13,hjust = 0,size = 8,family= "A",
           label= paste(expression(" "~R^2),": ",r2 
           ),parse=TRUE) + 
  annotate("text",x=3,y=11.3,hjust = 0,size = 8,family= "A",
           label= paste(
             "  RMSE: ", round(RMSE,3),"m",             
             "\n" , " %RMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(MD,3),"m",
             "\n" , " %Bias: ", round(RB,3),"%"
             )) + 
  coord_cartesian(xlim=c(2, 15),ylim = c(2, 15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  scale_x_continuous(limits = c(2,15),breaks = seq(2,15,2))+
  scale_y_continuous(limits = c(2,15),breaks = seq(2,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  labs(x=expression("ALS P98 (m) (Leaf-off)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


ggarrange(p1,p2,ncol=2,nrow=1) 
out = "E:\\GEDI\\Result\\backup\\Result\\figure scatterplot on-orbit GEDI vs. P98.jpg"
ggsave(out,height=12, width=24, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure bias boxplot on-orbit GEDI RH98 vs. p98.jpg
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_P98_PC_combined_RH1.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$RH1_98 >2.34,]
Data <- Data[Data$p98 >2.34,]
Data <- na.omit(Data)
Data <- Data[Data$p98 <=15,]
Data$diff <- Data$RH1_98 - Data$p98
Data$abs_diff <- abs(Data$RH1_98 - Data$p98)
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Data$group_RH <- cut(Data$p98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$p98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b1 <- tapply(Data$p98, cut(Data$p98,
                           breaks = c(2,3,4,5,6,
                                      7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
RB1 <- a1/b1*100

p1<- ggplot(Data, aes(x=p98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(y="Bias (m)")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x1 <- seq(2,14,1)
l1 <- cbind(x1,RB1)
df1 <- data.frame(l1)

p3 <- ggplot(df1, aes(x=x1+0.5, y=RB1)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  labs(x=expression("CHM P98 (m) (Leaf-on)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$RH1_98 >2.34,]
Data <- Data[Data$p98 >2.34,]
Data <- na.omit(Data)

Data <- Data[Data$p98 <=15,]
Data$diff <- Data$RH1_98 - Data$p98
Data$abs_diff <- abs(Data$RH1_98 - Data$p98)
Data$group_RH <- cut(Data$p98,
                     breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                     dig.lab=1)

a2 <- tapply(Data$diff, cut(Data$p98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b2 <- tapply(Data$p98, cut(Data$p98,
                           breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
RB2 <- a2/b2*100


p2 <- ggplot(Data, aes(x=p98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = seq(0, 15, 1),breaks = seq(0, 15, 1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  labs(x=expression("ALS P98 (m) (Leaf-off)"), 
       y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x2 <- seq(2,14,1)
l2 <- cbind(x2,RB2)
df2 <- data.frame((l2))

p4 <- ggplot(df2, aes(x=x2+0.5, y=RB2)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  theme(axis.title.y=element_blank())+
  labs(x=expression("ALS P98 (Leaf-off)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2) 

out = "E:\\GEDI\\Result\\backup\\Result\\figure bias boxplot on-orbit GEDI RH98 vs. p98.jpg"
ggsave(out,height=12, width=24, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure bias boxplot on-orbit vs. sim GEDI RH98.jpg
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_P98_PC_combined_RH1.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$RH1_98 >2.34,]
Data <- Data[Data$p98 >2.34,]
Data <- na.omit(Data)

Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
breakbin = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b1 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                     7,8,9,10,11,12,13,14,15),
                                      dig.lab=1), mean)
RB1 <- a1/b1*100

p1<- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(y="Bias (m)")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

x1 <- seq(2,14,1)
l1 <- cbind(x1,RB1)
df1 <- data.frame(l1)

p3 <- ggplot(df1, aes(x=x1+0.5, y=RB1)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-on)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_P98_PC_combined_RH1.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$ql2a == 1,]
Data <- Data[Data$ql2b == 1,]
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$RH1_98 >2.34,]
Data <- Data[Data$p98 >2.34,]
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 <=15,]
Data$diff <- Data$RH1_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH1_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                    7,8,9,10,11,12,13,14,15),
                     dig.lab=1)

a2 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
b2 <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(2,3,4,5,6,
                                                                     7,8,9,10,11,12,13,14,15),
                                      dig.lab=1), mean)
RB2 <- a2/b2*100


p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(minor_breaks = seq(0, 15, 1),breaks = seq(0, 15, 1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-off)"), 
       y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x2 <- seq(2,14,1)
l2 <- cbind(x2,RB2)
df2 <- data.frame((l2))

p4 <- ggplot(df2, aes(x=x2+0.5, y=RB2)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 20))+
  theme(axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-off)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(minor_breaks = seq(1, 15, 1),breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2) 
out = "E:\\GEDI\\Result\\backup\\Result\\figure bias boxplot on-orbit vs. sim GEDI RH98.jpg"
ggsave(out,height=12, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure bias -- star only
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >= 2,]
Data <- Data[Data$GEDI_sim_rh_98 <= 11,]

Data1 <- read.csv(filedir,header=T)
Data1 <- Data1[Data1$GEDI_sim_rh_98 >= 2 & Data1$GEDI_sim_rh_98 <= 11,]

Data2 <- Data1[Data1$GEDI_sim_rh_98 >= 10 & Data1$GEDI_sim_rh_98 <= 11,]
nrow(Data2)/nrow(Data1)

Data <- Data[Data$p98 >0,]
Data$diff <- Data$RHS_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a1 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11),
                            dig.lab=1), mean)
Data$diff <- Data$RHS_98 - Data$p98
Data$group_RH <- cut(Data$p98,breaks = c(2,3,4,5,6,
                                           7,8,9,10,11),
                     dig.lab=1)
a2 <- tapply(Data$diff, cut(Data$p98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11),
                            dig.lab=1), mean)


Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$GEDI_sim_rh_98 >= 2,]
Data <- Data[Data$GEDI_sim_rh_98 <= 11,]
Data <- Data[Data$p98 >0,]
Data$diff <- Data$RHS_98 - Data$GEDI_sim_rh_98
breakbin = c(2,3,4,5,6,7,8,9,10,11)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)

a3 <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11),
                            dig.lab=1), mean)

Data$diff <- Data$RHS_98 - Data$p98
Data$group_RH <- cut(Data$p98,breaks = c(2,3,4,5,6,
                                           7,8,9,10,11),
                     dig.lab=1)
a4 <- tapply(Data$diff, cut(Data$p98,
                            breaks = c(2,3,4,5,6,
                                       7,8,9,10,11),
                            dig.lab=1), mean)

df1 <- data.frame(a1)
df1$Legend <- "green"
df2 <- data.frame(a2)
df2$Legend <- "blue"
df3 <- data.frame(a3)
df3$Legend <- "orange"
df4 <- data.frame(a4)
df4$Legend <- "red"
df1$var_name = rownames(df1)
df2$var_name = rownames(df2)
df3$var_name = rownames(df3)
df4$var_name = rownames(df4)
colnames(df2) <- colnames(df1) 
colnames(df3) <- colnames(df1) 
colnames(df4) <- colnames(df1) 
df <- rbind(df1,df2,df3,df4)
df$var_name <- factor(df1$var_name, levels =df1$var_name)
df$Legend<-factor(df$Legend, levels=c("green","blue", "orange","red"))

ggplot(df, aes(x=var_name, y=a1, group=var_name,color = Legend)) + 
  geom_point(data = df, aes(colour = Legend), size = 5,shape=16)+
  geom_abline(intercept = 0, slope = 0, color="black", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-7, 1))+
  theme_bw()+
  labs(x=expression("GEDI-RH98"["Sim"]~"or ALS P98 (m)"), 
       y=expression("Bias of GEDI-RH98"["Orb"]~"vs. GEDI-RH98"["Sim"]~"or ALS P98 (m)"))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("green","blue", "orange","red"),
                     labels = c("Leaf-on, Simulated GEDI-RH98",
                                "Leaf-on, ALS P98",
                                "Leaf-off, Simulated GEDI-RH98",
                                "Leaf-off, ALS P98"))+
  theme(legend.position=c(.2,.3))+
  theme(text=element_text(size=35))

out = "E:\\GEDI\\Result\\backup\\Result\\figure bias.jpg"
ggsave(out,height=16, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#figure bias boxplot p98 vs. sim GEDI RH98.jpg
filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$GEDI_sim_rh_98 >= 0,]
Data <- Data[Data$GEDI_sim_rh_98 <= 11,]
Data <- Data[Data$p98 >0,]
Data$diff <- Data$GEDI_sim_rh_98 - Data$p98

breakbin = c(2,3,4,5,6,7,8,9,10,11)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=group_RH, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 0.7)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-5, 5))+
  scale_y_continuous(minor_breaks = seq(-5, 5, 1),breaks = seq(-5, 5, 2))+
  theme_bw()+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-on)"), 
       y="Difference between simulated GEDI RH98 and ALS percentile 98 (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=35))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

filedir <- "E:\\GEDI\\Result\\backup\\Result\\All_false.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$status == "Leaf-off",]
Data <- Data[Data$GEDI_sim_rh_98 > 0,]
Data <- Data[Data$GEDI_sim_rh_98 < 11,]
Data <- Data[Data$p98 >0,]
Data$diff <- Data$GEDI_sim_rh_98 - Data$p98 
breakbin = c(2,3,4,5,6,7,8,9,10,11)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=group_RH, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 0.7)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-5, 5))+
  scale_y_continuous(minor_breaks = seq(-5, 5, 1),breaks = seq(-5, 5, 2))+
  theme_bw()+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (Leaf-off)"), 
       y="Difference between simulated GEDI RH98 and ALS percentile 98 (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=35))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.y=element_blank())


ggarrange(p1,p2) 

out = "E:\\GEDI\\Result\\backup\\Result\\figure bias boxplot p98 vs. sim GEDI RH98.jpg"
ggsave(out,height=16, width=24, dpi=600)





#composite plot --Welverdiendt	O06640	2020044164536_O06640_04_T01318_02_0	Shot_66400500400336092
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
  par(new=TRUE)
  plot(energy_cum_normalized, z_masked, lwd=2, axes=F, 
       cex.lab=2,cex=2,cex.axis = 2,bty="n", type="l", xlab = "", ylab = "", 
       ylim=c(ymin, ymax), xlim=qts)
  axis(side=4, cex.lab=2,at = ticks, cex.axis = 2,labels=ticks_label)
  mtext("Height (m)", cex=2,cex.lab=2,cex.axis = 2,side=4, line=3)
  for (i in 2:(length(rh_z)-1)) {
    mark(energy_cum_normalized[rh_closest_en[[i]]], rh_z[[i]])
    text(energy_cum_normalized[rh_closest_en[[i]]], ymidpoint(rh_z[[i]]), 
         toupper(names(rh_z)[[i]]), cex = 2,pos = 3)
  }
  
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)]], "On-orbit GEDI RH100: 9.2m", cex = 2,pos=3)
  abline(rh_z[[length(rh_z)]], 0, lty="solid")
  
  text(qts[2]-diff(qts)/2, rh_z[[1]], "RH0", cex = 2,pos=1)
  abline(rh_z[[1]], 0, lty="solid")
  
  text(qts[1], ground_z, "GZ (m)", cex = 2,adj=c(0, -1))
  abline(ground_z, 0, lty="solid", col="brown")
  #rh98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]+2.5, "On-orbit GEDI RH98: 6.88m", cex = 2,pos=3, col="blue")
  abline(ground_z+6.88, 0 ,lwd = 2, lty="solid", col="blue")
  #p98
  text(qts[2]-diff(qts)/2, rh_z[[length(rh_z)-2]]+4.5, "ALS P98: 7.36m", cex = 2,pos=3, col="red")
  abline(ground_z+7.36, 0, lwd=2, lty="solid", col="red")
}

#vis real GEDI waveform----savanna
library(gridBase)
real_1b <- "E:\\GEDI\\GEDI_archive\\GEDI01B\\SA\\GEDI01_B_2019271231724_O04504_04_T01318_02_005_01_V002.h5"
real_2a <- "E:\\GEDI\\GEDI_archive\\GEDI02A\\SA\\GEDI02_A_2019271231724_O04504_04_T01318_02_003_01_V002.h5"
level1b <- readLevel1B(level1Bpath = real_1b)
level2a <- readLevel2A(level2Apath = real_2a)
shot_number="45041100400335415"

#png("E:\\GEDI\\Result\\backup\\Result\\composite.jpg", width = 16, height = 12, units = 'in', res = 600)
par(mar= c(4,7,2,7))
plotWFMetrics_v2(level1b, level2a, shot_number, rh=c(50, 75, 90, 98))
#dev.off()


