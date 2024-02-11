import pandas as pd
import os
import subprocess
import arcpy
#import Parquet_SA
#import LiDAR_CHM


print("gediSimulator.py running")
def add_field():
    for fc in fcs:
        sj = GDB_dir + fc
        print(fc)
        beam = str(fc).split("_")[0][-1]
        print(beam)
        arcpy.AddField_management(sj, "BEAM", "SHORT")
        arcpy.CalculateField_management(sj, "BEAM", '{}'.format(beam))


def project():
    for fc in fcs:
        sj = GDB_dir + fc
        sj_pro = GDB_dir + fc + "_P"
        print(fc)
        location = str(fc).split("_")[0]
        print(location)
        SR_dir = r"E:\temp\Konrad\LiDAR_CHM\Boundingbox/" + location + ".shp"
        arcpy.Project_management(sj, sj_pro, SR_dir)
        arcpy.CalculateGeometryAttributes_management(
            sj_pro, [["lon", "POINT_X"], ["lat", "POINT_Y"]])
        arcpy.Delete_management(sj)


def merge():
        arcpy.Merge_management(arcpy.ListFeatureClasses(), merge_output)


def spatial_join():
    SJ_table = simulator_folder + "GEDI_CHM.xls"
    if arcpy.Exists(SJ_output):
        print("Spatial join already completed")
    else:
        arcpy.MakeFeatureLayer_management(BoundingBox_dir, 'Boundingbox_lyr')
        arcpy.SpatialJoin_analysis(merge_output, 'Boundingbox_lyr', SJ_output,"", "", "", "WITHIN")
    arcpy.TableToExcel_conversion(SJ_output, SJ_table)


def txt():
    for i in list_lasname:
        output = simulator_folder + str(i) + ".txt"
        print(i)
        df_sub = df.loc[df['LAS_NM'] == str(i)]
        df_text = df_sub[["lon", "lat", "shot_numbe"]]
        df_text.to_csv(output, header=False, index=None, sep='	')


def gedi_simulator():
    print("running gediSimulator in R")
    commands = [r'E:/R/R-3.6.2/bin/Rscript.exe', r'C:/Users/lxiao/Desktop/R/SimulationPy.R']
    args = ["SA"]
    subprocess.call(commands + args)


def join():
    initial = []
    df_initial = pd.DataFrame(initial)
    #gedi metrics attributes
    columns = ['shot_numbe', 'GEDI_sim_rhGauss_50','GEDI_sim_rhGauss_75','GEDI_sim_rhGauss_90',
               'GEDI_sim_rhGauss_95', 'GEDI_sim_rhGauss_100', 'GEDI_sim_rhMax_50', 'GEDI_sim_rhMax_75',
               'GEDI_sim_rhMax_90', 'GEDI_sim_rhMax_95', 'GEDI_sim_rhMax_100', 'GEDI_sim_rhInfl_50',
               'GEDI_sim_rhInfl_75', 'GEDI_sim_rhInfl_90', 'GEDI_sim_rhInfl_95', 'GEDI_sim_rhInfl_100',
               'GEDI_sim_guass_cover',  'GEDI_sim_max_cover', 'GEDI_sim_infl_cover','GEDI_sim_FHD']

    tile_folder = os.listdir(simulator_folder)
    for i in tile_folder:
        if "metric" in i:
            df_loop = pd.read_csv(simulator_folder + i,
                                  usecols=[0,23,28,31,32,33,44,49,52,53,54,65,70,73,74,75,98,99,100,116],
                                  header=0,sep=" ")
            df_initial = pd.concat([df_initial, df_loop])
    df_initial.columns = columns
    df_merge = pd.merge(df_initial, df, left_on='shot_numbe', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_initial.to_csv(simulator_folder + "gediSimulator.csv", index=None)
    df_merge.to_csv(csv, index=None)


def output():
    df_csv = pd.read_csv(csv)
    df_locname = df_csv['Site']
    list_locname = set(df_locname.values.tolist())
    for i in list_locname:
        #output csv
        final_output = simulator_folder + "Loc_" + i + ".csv"
        df_loc = df_csv.loc[df_csv["Site"] == i]
        df_loc.to_csv(final_output, index=None)
        #output shapefile
        SR_dir = r"E:\temp\Konrad\LiDAR_CHM\Boundingbox/" + i + ".shp"
        final_SHP = GDB_dir + "final_shp" + i
        final_SHP_buffer = GDB_dir + "final_" + i
        arcpy.management.XYTableToPoint(final_output, final_SHP, "lon", "lat","",SR_dir)
        arcpy.Buffer_analysis(final_SHP, final_SHP_buffer, "25 meters")
        arcpy.Delete_management(merge_output)
        arcpy.Delete_management(SJ_output)
        arcpy.Delete_management(GDB_dir + i + "_Merge_P")
        arcpy.Delete_management(final_SHP)


#folder directories
simulator_folder = r"E:\temp\Konrad\LiDAR_CHM\gediSimulator/"
xls = simulator_folder + "GEDI_CHM.xls"
csv = simulator_folder + "merged.csv"
GDB_dir = "E:/temp/Konrad/LiDAR.gdb/"

merge_output = GDB_dir + 'Shp_merged'
SJ_output = GDB_dir + "Merge_SJ"
BoundingBox_dir = r"E:/temp/Konrad/LiDAR_CHM/Boundingbox/BoundingBox.shp"
#dir test
if not os.path.isdir(simulator_folder):
    os.makedirs(simulator_folder)
#set up initial GDB workspace
arcpy.env.workspace = GDB_dir
fcs = arcpy.ListFeatureClasses()


#GDB processing
#add field, GDB
print("addfield")
'add_field()'
#project and merge, GDB
print("project")
project()
#merge, GDB
print("merge")
merge()
#spatial_join, GDB
print("spatial_join")
spatial_join()

#dataframe set up
df = pd.read_excel(xls)
df['LAS_NM'] = df['LAS_NM'].str.strip()
df_lasname = df['LAS_NM']
list_lasname = set(df_lasname.values.tolist())

#create gedi shot txt files
print("txt")
txt()
#run gediSimulator in R
print("gedi_simulator")
gedi_simulator()

#merge gedisimulated metrics txt files
print("join")
join()

#Output csvs for each study sites
print("output")
output()
