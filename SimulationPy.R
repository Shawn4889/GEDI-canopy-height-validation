#Xiaoxuan Li
#The development version:
#devtools::install_github("carlos-alberto-silva/rGEDI", dependencies = TRUE)
#library(devtools)
library(rGEDI)
library(lidR)
library(plot3D)
library(stringr)
x <- commandArgs(trailingOnly = TRUE)
x <- "SA"
#Specify dir and files
if (x != "Amazon"){
    lasdir_root <- "E:\\temp\\Konrad\\LiDAR_CHM\\LiDAR"
    outdir <- "E:/temp/Konrad/LiDAR_CHM/gediSimulator"
}else{
    lasdir_root <- "E:\\temp\\Konrad\\LiDAR_CHM_Amazon\\LiDAR"
    outdir <- "E:/temp/Konrad/LiDAR_CHM_Amazon/gediSimulator"
}
print(outdir)
lasdir_sec <- c("Agincourt", "Duku", "Welverdient", "Venetia", "Ireagh")
txtfiles <- list.files(path = outdir, pattern = glob2rx('*.txt'))
for (j in txtfiles){
    for (n in lasdir_sec){
        lasdir <- file.path(lasdir_root, n)
        lasfiles <- list.files(path = lasdir, pattern = glob2rx('*.las'))
        for (i in lasfiles){
            #match txt and las filenames
            substr_i <- sub('\\_subset.las$', '', i)
            substr_j <- sub('\\.txt$', '', j)
            if (substr_i == substr_j){
                print(substr_i)
                print(substr_j)
                
                #list of coords
                listCoord <- file.path(outdir, j)
                # Specifying the path to ALS data
                lasfile_SA <- file.path(lasdir, i)
                try({
                    # Simulating GEDI full-waveform
                    # Run gediSimulator-gediRat
                    output_Rat <- file.path(outdir, paste0(substr_j,"_",n,".h5"))
                    if (!file.exists(output_Rat)){
                        wf_SA<-gediWFSimulator(input=lasfile_SA, output=output_Rat, 
                                               listCoord = listCoord)
                        # Run gediSimulator-gediMetric
                        out_root <- file.path(outdir, paste0(substr_j,"_",n))
                        wf_SA_metrics<-gediWFMetrics(input=wf_SA,
                                                     outRoot=out_root)
                        #file.remove(output_Rat)
                        #file.remove(listCoord)
                        close(wf_SA)
                    }
                    
                })
                
            }
        
            
        }
        
    }
}


