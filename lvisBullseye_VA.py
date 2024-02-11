# Author: Xiaoxuan Li
import os
import arcpy
from arcpy.sa import *
import shutil
import subprocess
import pandas as pd
import numpy as np
from whitebox_tools import WhiteboxTools
#import Basic_GEDI


def csv_to_shp():
    print(Out_XY_dir3)
    if os.path.isfile(Out_XY_dir3):
        print("GEDI shapefiles already exist")
    else:
        arcpy.management.XYTableToPoint(Result_coordinate_dir, Out_XY_dir, "long", "lat")
        arcpy.FeatureClassToShapefile_conversion(Out_XY_dir, SHP_dir)
        # set SR to buffered GEDI shots
        arcpy.Project_management(Out_XY_dir2, Out_XY_dir3, BoundingBox_dir)


def copy_txt():
    list = os.listdir(LiDAR_dir+Location)
    # output las file list
    VM_dir = r'/home/shawn/Desktop/ALS_List/LiDAR/'
    txt = LvisBullsEye_dir + Location + "_" + track + "_las.txt"
    with open(txt, 'w') as output:
        for i in list:
            if ".las" in i:
                output.write(VM_dir + Location + "/" + str(i) + '\n')
    filesize = os.path.getsize(txt)
    if filesize == 0:
        os.remove(txt)


def lvisbullseye_txt():
    # output Lvisbullseye syntax list
    VM_dir = r'/home/shawn/Desktop/'
    LvisBullsEye_folder = os.listdir(LvisBullsEye_dir)
    with open(LvisBullsEye_dir + "run_lvisbullseye.txt", 'w') as output:
        with open(LvisBullsEye_dir + "run_gedimetrics.txt", 'w') as output1:
            for i in LvisBullsEye_folder:
                if "las" in i and os.path.getsize(LvisBullsEye_dir + i) != 0:
                    print("Code determined: 32617 for case: " + i)
                    #study site + track
                    fullname = str(i).split("_")[0] + "_" + str(i)[-35:-8]
                    # run gediSimulator: LvisBullseye
                    if "_O2766_" in i:
                        version = "_02_003_02.h5"
                    else:
                        version = "_02_003_01.h5"
                    output.write("singularity exec --bind /home/shawn/Desktop/ "
                                 "gediSingularity lvisBullseye -listAls " + VM_dir + "ALS_List/LvisBullseye/"
                                 + str(i)
                                 + " -gedi /home/shawn/Desktop/L1B/GEDI01_B_" + str(i)[-35:-8]
                                 + version + " -readHDFged -aEPSG 32617"
                                 + " -solveCofG -fixFsig -fSigma 5.5 -decimate 4 "
                                   "-minSense 0.9 -minDense 3 -geoError 20 10 -writeWaves " + VM_dir
                                 + fullname + "_corr.h5" + '\n')
                    #Copy correlation table from orginal folder to desktop
                    output.write("sudo cp /home/shawn/Desktop/GEDI/GEDISimulator/teast.correl " + VM_dir
                                 + fullname + "_teast.correl" + '\n')
                    #run gediSimulator: gediMetric
                    output1.write("singularity exec --bind /home/shawn/Desktop/ "
                                 "gediSingularity gediMetric -input /home/shawn/Desktop/ALS_List/LvisBullseye/"
                                 + fullname + "_corr.h5 " +
                                 "-readHDFgedi ground -linkNoise 3 0.95 -varScale 2.5 -minWidth 3 "
                                 "-sWidth 0.7 -outRoot " + VM_dir + "ALS_List/LvisBullseye/" + fullname + " test1" + '\n')


def join():
    #gedi metrics attributes
    columns = ['shot_numbe', 'GEDI_sim_Cover','GEDI_sim_rhGauss_50','GEDI_sim_rhGauss_75','GEDI_sim_rhGauss_90',
               'GEDI_sim_rhGauss_95', 'GEDI_sim_rhGauss_100', 'GEDI_sim_rhMax_50', 'GEDI_sim_rhMax_75',
               'GEDI_sim_rhMax_90', 'GEDI_sim_rhMax_95', 'GEDI_sim_rhMax_100', 'GEDI_sim_rhInfl_50',
               'GEDI_sim_rhInfl_75', 'GEDI_sim_rhInfl_90', 'GEDI_sim_rhInfl_95', 'GEDI_sim_rhInfl_100',
               'fSigma','longitude','latitude','GEDI_sim_FHD']
    metric_folder = os.listdir(LvisBullsEye_dir)
    for Location in studysite:
        Location = Location.split("_")[0]
        print("gediMetric running...Case: " + Location)
        df_track = pd.read_csv(GEDI_folder + Location + "_GEDI.csv", sep=",")
        for i in metric_folder:
            #Quality check
            if Location in i and "metric" in i and int(os.path.getsize(LvisBullsEye_dir + i)) > 10000:
                print("Joining gediMetric results with real GEDI...Processing test case: " + i)
                df_initial = pd.read_csv(LvisBullsEye_dir + str(i),
                                         usecols=[0, 10, 23, 28, 31, 32, 33, 44, 49, 52, 53, 54, 65, 70, 73, 74, 75,
                                                  103, 106, 107, 116],
                                         header=0, sep=" ")
                df_initial.columns = columns
                df_initial["shot_number"] = "Shot_" + df_initial["shot_numbe"].str.split(".", expand=True)[2]
                df_merge = pd.merge(df_initial, df_track, left_on='shot_number', right_on='shot_number',
                                    how='left').dropna()
                df_merge.drop_duplicates(keep='first', inplace=True)
                df_merge.to_csv(LvisBullsEye_dir + str(i).split(".")[0] + "_merged.csv")
            else:
                print("Fail to process gedimetric file, skip to the next one...")


def output_shp():
    merge_files = os.listdir(LvisBullsEye_dir)
    for file in merge_files:
        if 'merge' in file:
            if "Addo" in file or "Venetia" in file or "Limpopo" in file or "DNyala" in file:
                Proj = Proj_dir + "UTM35S.prj"
            elif "Agulhas" in file:
                Proj = Proj_dir + "UTM34S.prj"
            else:
                Proj = Proj_dir + "UTM36S.prj"
            out_shp = GDB_dir + str(file).split(".")[0]
            out_buffer = SHP_dir + str(file).split(".")[0] + "_buffer.shp"
            print("Converting csv to buffered shp: " + file)
            try:
                arcpy.management.XYTableToPoint(LvisBullsEye_dir + file, out_shp,
                                                "longitude", "latitude", '',Proj)
                arcpy.Buffer_analysis(out_shp, out_buffer, "11 Meters")
            except:
                print("Failed to generate shapefile from test case: " + file + ", skip to the next test case")


def las_subset():
    lidar_filelist = os.listdir(LvisBullsEye_dir)
    for file in lidar_filelist:
        if '.metric.txt' in file:
            if not os.path.isdir(LiDAR_dir + str(file).split(".")[0]):
                os.makedirs(LiDAR_dir + str(file).split(".")[0])
            laslist_file = LvisBullsEye_dir + file.split(".")[0] + "_las.txt"
            df = pd.read_csv(laslist_file,header=None)
            for i in range(len(df)):
                part = df.iloc[i,0].split("/")[6]
                las = r"G:/LiDAR/LiDAR_UTM/" + file.split("_")[0] + "/" + part
                output_las = r'E:/temp/Konrad/LiDAR_CHM/LiDAR/' + file.split(".")[0] + "/" + part.split(".")[0] + "_subset.las"
                polygon = SHP_dir + file.split(".")[0] + "_merged_buffer.shp"
                wbt.clip_lidar_to_polygon(i=las, polygons=polygon, output=output_las)


def rasterization():
    List_location = os.listdir(LiDAR_dir)
    for Location in List_location:
        # Delete empty folders
        print(Location)
        if len(os.listdir(LiDAR_dir + Location)) == 0:
            shutil.rmtree(LiDAR_dir + Location)
        else:
            if not os.path.isdir(CHM_dir + Location):
                os.makedirs(CHM_dir + Location)
            if not os.path.isdir(Canopy_dir + Location):
                os.makedirs(Canopy_dir + Location)
            if not os.path.isdir(Ground_dir + Location):
                os.makedirs(Ground_dir + Location)
            # Subset LAS to CHMs
            commands = [r'E:/R/R-3.6.2/bin/Rscript.exe', r'C:/Users/lxiao/Desktop/R/CHM.R']
            args = [Location]
            subprocess.call(commands + args)


def append():
    List_location = os.listdir(ALS_dir)
    for sub in List_location:
        print(sub)
        List_location_sub = os.listdir(ALS_dir + sub)
        for Location in List_location_sub:
            print(Location)
            if len(os.listdir(ALS_dir + sub + "/" + Location)) == 0:
                shutil.rmtree(ALS_dir + sub + "/" + Location)
            else:
                chm_merge = ALS_dir + sub + "/" + Location + "/" + sub + "_Merged.tif"
                chm_list = []
                chm_folder = os.listdir(ALS_dir + sub + "/" + Location)
                for chm in chm_folder:
                    fullname = ALS_dir + sub + "/" + Location + "/" + chm
                    if 'Merged' in chm:
                        os.remove(fullname)
                    else:
                        chm_list.append(fullname)
                shutil.copy2(chm_list[0], chm_merge)
                arcpy.Append_management(chm_list[1:], chm_merge, "TEST")


def zonalstat():
    List_location = os.listdir(ALS_dir)
    for sub in List_location:
        List_location_sub = os.listdir(ALS_dir + sub)
        for Location in List_location_sub:
            print(Location)
            if len(os.listdir(ALS_dir + sub + "/" + Location)) == 0:
                shutil.rmtree(ALS_dir + sub + "/" + Location)
            else:
                # CHMs projections
                print("Zonal statistics: " + Location)
                chm_merge = ALS_dir + sub + "/" + Location + "/" + sub + "_Merged.tif"
                shp_dir = SHP_dir + Location + "_merged_buffer.shp"
                table = GDB_dir + Location + "_" + sub + "_ZonalStatistics"
                xls = Merge_dir + Location + "_" + sub + "_stat.xls"
                csv = Merge_dir + Location + "_" + sub + "_stat.csv"
                ZonalStatisticsAsTable(shp_dir, "shot_numbe", chm_merge, table)
                arcpy.TableToExcel_conversion(table, xls)
                data_xls = pd.read_excel(xls)
                data_xls.to_csv(csv)
                os.remove(xls)


def merge():
    lvis_files = os.listdir(Canopy_dir)
    for file in lvis_files:
        print("Merging GEDI and CHMs: " + file)
        result = Merge_dir + file + "_result.csv"
        shapefile = GDB_dir + file + "_Result"
        df_gedi = pd.read_csv(LvisBullsEye_dir + file + "_merged.csv")
        df_chm = pd.read_csv(Merge_dir + file + "_CHM_stat.csv")
        df_chm_sel = df_chm[["shot_numbe", "COUNT", "MAX", "MIN", "MEAN"]]
        df_chm_sel.columns = ["shot_numbe", "COUNT", "CHM_MAX", "CHM_MIN", "CHM_MEAN"]
        df_canopy = pd.read_csv(Merge_dir + file + "_Canopy_stat.csv")
        df_canopy_sel = df_canopy[["shot_numbe", "MAX"]]
        df_canopy_sel.columns = ["shot_numbe", "Canopy_MAX"]
        df_ground = pd.read_csv(Merge_dir + file + "_Ground_stat.csv")
        df_ground_sel = df_ground[["shot_numbe", "MIN"]]
        df_ground_sel.columns = ["shot_numbe", "Ground_MIN"]
        df_merge1 = pd.merge(df_gedi, df_chm_sel, left_on='shot_numbe', right_on='shot_numbe', how='left').dropna()
        df_merge2 = pd.merge(df_merge1, df_canopy_sel, left_on='shot_numbe', right_on='shot_numbe', how='left').dropna()
        df_merge3 = pd.merge(df_merge2, df_ground_sel, left_on='shot_numbe', right_on='shot_numbe', how='left').dropna()
        df_merge3.drop_duplicates(keep='first', inplace=True)
        df_merge3.to_csv(result)
        arcpy.management.XYTableToPoint(result, shapefile, "long", "lat")


LiDAR_dir = r'E:\temp\Konrad\LiDAR_CHM\LiDAR/'
ALS_dir = r"E:\temp\Konrad\LiDAR_CHM\ALS/"
CHM_dir = ALS_dir + 'CHM/'
Canopy_dir = ALS_dir + 'Canopy/'
Ground_dir = ALS_dir + 'Ground/'
GDB_dir = "E:/temp/Konrad/LiDAR.gdb/"
SHP_dir = r"E:/temp/Konrad/LiDAR_CHM/SHP/"
Merge_dir = r"E:\temp\Konrad\LiDAR_CHM\Merge/"
LvisBullsEye_dir = r"E:/temp/Konrad/LiDAR_CHM/LvisBullseye/"
Proj_dir = r'E:\temp\Konrad\Projection/'
BB_dir = r"E:/temp/Konrad/Boundingbox/"

if not os.path.isdir(SHP_dir):
    os.makedirs(SHP_dir)
if not os.path.isdir(LvisBullsEye_dir):
    os.makedirs(LvisBullsEye_dir)
if not os.path.isdir(Merge_dir):
    os.makedirs(Merge_dir)
if not os.path.isdir(LiDAR_dir):
    os.makedirs(LiDAR_dir)
if not os.path.isdir(ALS_dir):
    os.makedirs(ALS_dir)
    os.makedirs(CHM_dir)
    os.makedirs(Canopy_dir)
    os.makedirs(Ground_dir)

wbt = WhiteboxTools()
arcpy.env.overwriteOutput = True

GEDI_folder = r"E:/temp/Konrad/LiDAR_CHM/Basic_GEDI/"
studysite = os.listdir(GEDI_folder)
for Location in studysite:
    Location = Location.split("_")[0]
    print("Site: " + Location)
    BoundingBox_dir = BB_dir + Location + ".shp"
    try:
        arcpy.Delete_management('Boundingbox_lyr')
    except:
        print("Making feature layer: " + Location)
    arcpy.MakeFeatureLayer_management(BoundingBox_dir, 'Boundingbox_lyr')
    Out_XY_dir = GDB_dir + Location
    Out_XY_dir2 = SHP_dir + Location + ".shp"
    Out_XY_dir3 = SHP_dir + Location + "_Project.shp"
    Result_coordinate_dir = GEDI_folder + Location + "_GEDI.csv"
    df_track = pd.read_csv(Result_coordinate_dir, sep=",")
    df_site = df_track["track"]
    df_site.drop_duplicates(keep='first', inplace=True)
    list_site = df_site.values.tolist()
    print(list_site)
    print("Converting GEDI csv file to shapefile: " + Location)

    #convert GEDI shots in excel to shapefiles by study site
    csv_to_shp()

    layer = arcpy.MakeFeatureLayer_management(Out_XY_dir3, 'XY')
    '''
    list = os.listdir(LiDAR_dir + Location)
    print(list)
    for las in list:
        if ".las" in las:
            part1 = las.split("_")[4]
            part2 = las.split("_")[5]
            os.rename(LiDAR_dir + Location + "/" + las, LiDAR_dir + Location + "/" + part1 + "_" + part2 + ".las")
    '''
    for track in list_site:
        print("Track: " + track)
        copy_txt()

print("Output LvisBullsEye syntax txt")
lvisbullseye_txt()


#section two: lvisbullseye should be run on Linux before running the following codes
print('Performing join')
#join()
print('Outputing buffered GEDI shot shapefiles')
#output_shp()
print("Subsetting las files")
#las_subset()
print("Generating CHMs")
#rasterization()
print("Appending CHMs")
#append()
print("raster_to_point")
#zonalstat()
print("Result output")
#merge()
