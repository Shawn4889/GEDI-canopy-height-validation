#Author: Xiaoxuan Li
#03062023
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
library(rgdal)
library(caret)
library(randomForest)
library(DAAG)
library(matrixStats)
windowsFonts(A = windowsFont("Times New Roman"))


#ALS + CSIR
filedir <- "E:\\Biomass\\CSIR\\result\\CSIR_25m\\biom_CSIR_25m_combo_1_h_cc.csv"
Data1 = read.csv(filedir)
Data1 <- na.omit(Data1)
Data1<-Data1[!(Data1$plot=="Ven_S63_SP_5_np13"),]
Data1<-Data1[!(Data1$plot=="Ven_S63_SP_5_np15"),]
HMB_data_S <- Data1[Data1$CSIR >0 & Data1$CSIR < 60,]




#GEDI + ALS
dir <- "E:\\Biomass\\CSIR\\result\\All_02272023.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$status == "Leaf-on",]
Data <- Data[Data$ql4 ==1,]
Data <- Data[Data$ql2a ==1,]
Data <- Data[Data$ql2b ==1,]
Data$Combo <-  4*Data$MEAN^1.19
Data$RH1_98 <- sqrt(Data$RH1_98)
Data <- na.omit(Data)
HMB_data <- Data[Data$Combo >0 & Data$Combo < 60,]



pop_U = sample(nrow(HMB_data), 5000)
pop_Sa = sample(pop_U, 500)
pop_S = sample(nrow(HMB_data_S), 100)
#CSIR biomass + ALS predictors
y_S = data.frame(HMB_data_S[pop_S, "CSIR"])
X_S = data.frame(HMB_data_S[pop_S, c("MEAN")])
#ALS predictors + GEDI predictors
X_Sa = data.frame(HMB_data[pop_Sa, c("MEAN")])
Z_Sa = HMB_data[pop_Sa, c("RH1_98", "GEDI_Cover")]
#GEDI parameters
Z_U = HMB_data[pop_U, c("RH1_98", "GEDI_Cover")]
Omega_S = diag(1, nrow(X_S))
Sigma_Sa = diag(1, nrow(Z_Sa))
ghmb_model = ghmb(
  y_S, X_S, X_Sa, Z_Sa, Z_U, Omega_S, Sigma_Sa)

ghmb_model

getSpec(ghmb_model)
