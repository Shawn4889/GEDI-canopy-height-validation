# Author: Xiaoxuan Li
import os
import h5py
import arcpy
from arcpy.sa import *
import shutil
import subprocess
import pandas as pd
import numpy as np
import laspy
from whitebox_tools import WhiteboxTools
wbt = WhiteboxTools()
arcpy.env.overwriteOutput = True

studysite1 = ['Addo','Agincourt','Agulhas','DNyala','Duku','Welverdiendt',
             'Venetia','Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']
studysite = ['Agincourt','DNyala','Welverdiendt',
             'Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']

LvisBullsEye_dir = r"E:\GEDI\Result\Collocate/"
GEDI_folder = r"E:\GEDI\Result\Basic_GEDI/"
Result_dir = r"E:\GEDI\Result\LvisBullsEye/"
Out_dir = r"E:\GEDI\Result\Result/"
Proj_dir = r'E:\GEDI\Boundingbox/'
GDB_dir = r"D:\temp\ArcGIS_project\GEDI/GEDI.gdb/"
SHP_dir = r"E:\GEDI\Result\SHP/"


def join():
    #gedi metrics attributes
    columns = ['shot_numbe','als_ground','als_top',
               'GEDI_sim_rh_98','GEDI_sim_rh_100',
               'longitude','latitude']
    metric_folder = os.listdir(LvisBullsEye_dir)
    for Location in studysite:
            print("gediMetric running...Case: " + Location)
            for i in metric_folder:
                #Quality check
                if Location in i and "metric" in i and int(os.path.getsize(LvisBullsEye_dir + i)) > 10000:
                    print("Joining gediMetric results with real GEDI...Processing test case: " + i)
                    df_initial = pd.read_csv(LvisBullsEye_dir + str(i),
                                             usecols=[0, 1, 2,
                                                      414, 416,
                                                      426, 427],
                                             header=0, sep=" ")
                    df_initial.columns = columns
                    df_initial["shot_number"] = "Shot_" + df_initial["shot_numbe"].str.split(".", expand=True)[2]
                    df_initial.to_csv(Result_dir + str(i).split(".")[0] + "_joined.csv")


def merge_csv1():
    list_testcase = []
    csv_folder = os.listdir(Result_dir)
    for i in csv_folder:
        if "join" in i:
            testcase = i.split("_")[0] + "_" + i.split("_")[1]
            list_testcase.append(testcase)

    for i in list(set(list_testcase)):
        out_csv = Result_dir + i + "_testcase.csv"
        df_list = []
        df = pd.DataFrame(df_list)
        print("Merging test case..." + i)
        for j in csv_folder:
            if i in j and "_testcase" not in j:
                df_item = pd.read_csv(Result_dir + j)
                df = pd.concat([df, df_item])
        df.to_csv(out_csv, index=None)


def merge_csv2():
    list_testcase = []
    csv_folder = os.listdir(Result_dir)
    for i in csv_folder:
        testcase = i.split("_")[0]
        list_testcase.append(testcase)
    for i in list(set(list_testcase)):
        out_csv = Result_dir + i + "_c.csv"
        df_list = []
        df = pd.DataFrame(df_list)
        print("Merging test case..." + i)
        for j in csv_folder:
            if i in j and "_testcase" in j:
                df_item = pd.read_csv(Result_dir + j)
                df_item["site"] = j.split("_")[0]
                df_item["orbit"] = j.split("_")[1]
                df = pd.concat([df, df_item])
        if not df.empty:
            df.to_csv(out_csv, index=None)


def merge_csv3():
    list_testcase = []
    csv_folder = os.listdir(Result_dir)
    df = pd.DataFrame(list_testcase)
    out_csv = Out_dir + "All_sim.csv"
    for i in csv_folder:
        if "_c.csv" in i:
            list_testcase.append(i)
    print("Merging all test case...")
    for j in list_testcase:
        print("Merging..." + j)
        in_csv = Result_dir + j
        df_item = pd.read_csv(in_csv)
        df = pd.concat([df, df_item])
    df.to_csv(out_csv, index=None)


def merge_csv4():
    df_basic = pd.read_csv(Out_dir + "All_basic.csv", sep=",")
    df_sim = pd.read_csv(Out_dir + "All_sim.csv", sep=",")
    df_merge = pd.merge(df_basic, df_sim, left_on='shot_number', right_on='shot_number',
                        how='left').dropna()
    out_csv = Out_dir + "All_b_s.csv"
    df_merge.to_csv(out_csv, index=None)


def add_phenology():
    csv = r"E:\GEDI\Result\Result/All_b_s.csv"
    out = r"E:\GEDI\Result\Result/All_b_s_p.csv"
    df = pd.read_csv(csv,header=0)
    df['status'] = 0
    df.loc[(df['track'].str[4:7].astype(int) >= 1) &
           (df['track'].str[4:7].astype(int) <= 120), 'status'] = 'Leaf-on'
    df.loc[(df['track'].str[4:7].astype(int) >= 305) &
           (df['track'].str[4:7].astype(int) <= 366), 'status'] = 'Leaf-on'
    df.loc[(df['track'].str[4:7].astype(int) >= 152) &
           (df['track'].str[4:7].astype(int) <= 273), 'status'] = 'Leaf-off'
    df.loc[(df['track'].str[4:7].astype(int) >= 121) &
           (df['track'].str[4:7].astype(int) <= 151), 'status'] = 'Transition'
    df.loc[(df['track'].str[4:7].astype(int) >= 274) &
           (df['track'].str[4:7].astype(int) <= 304), 'status'] = 'Transition'
    df.loc[(df['site'] == "Duku") | (df['site'] == "Addo"), 'status'] = 'Evergreen'

    print("All database output...")
    df.to_csv(out, index=None)
    df.to_csv(out, index=None)


def csv_shp():
    files = os.listdir(Result_dir)
    for file in files:
        if "All" not in file and "_c.csv" in file:
            print(file)
            if "Addo" in file or "Venetia" in file or "DNyala" in file:
                Proj = Proj_dir + "UTM35S.prj"
            elif "Agulhas" in file:
                Proj = Proj_dir + "UTM34S.prj"
            else:
                Proj = Proj_dir + "UTM36S.prj"
            out_shp = GDB_dir + str(file).split(".")[0]
            out_buffer = SHP_dir + str(file).split(".")[0] + "_buffer.shp"
            print("Converting csv to buffered shp: " + out_buffer)

            arcpy.management.XYTableToPoint(Result_dir + file, out_shp,
                                            "longitude", "latitude", '', Proj)
            arcpy.Buffer_analysis(out_shp, out_buffer, "12.5 Meters")


def add_cc():
    shp_dir = r"E:\GEDI\Result\SHP/"
    chm_dir = r"E:\ALS_archive\LiDAR_CHM_CC15/"
    out_dir = r"E:\GEDI\Result\Canopy/"
    arcpy.env.workspace = chm_dir
    files = arcpy.ListRasters()
    for i in files:
        if "Agulhas" not in i:
            print("Process..." + os.path.splitext(i)[0])
            inshp = shp_dir + os.path.splitext(i)[0] + "_c_buffer.shp"
            inras = chm_dir + i
            table = GDB_dir + os.path.splitext(i)[0][4:] + "_CC_ZonalStatistics"
            xls = out_dir + "CC_" + os.path.splitext(i)[0] + ".xls"
            csv = out_dir + "CC_" + os.path.splitext(i)[0] + ".csv"
            ZonalStatisticsAsTable(inshp, "shot_numbe", inras, table,"DATA","SUM")
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
    #merge all csv
    dir = r"E:\GEDI\Result\Canopy/"
    initial_list = []
    df_initial = pd.DataFrame(initial_list)
    files = os.listdir(dir)
    for file in files:
        if "CC" in file:
            print("Merging..." + file)
            df = pd.read_csv(dir + file, header=0)
            df = df[["shot_numbe", "SUM", "COUNT"]]
            df_initial = pd.concat([df_initial, df],sort=False)
    df_initial["CC15"] = df_initial["SUM"] / df_initial["COUNT"]
    out = dir + "All_C.csv"
    print("Output merged file..." + out)
    df_initial.to_csv(out, index=None)
    #output CC
    csv1 = r"E:\GEDI\Result\Canopy/All_C.csv"
    csv2 = r"E:\GEDI\Result\Result\All_b_s_p_cc05_m_biom.csv"
    output = r"E:\GEDI\Result\Result/All_b_s_p_cc15_m_biom.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv2, df_csv1, left_on='shot_numbe', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)



def add_mean():
    shp_dir = r"E:\GEDI\Result\SHP/"
    chm_dir = r"E:\ALS_archive\LiDAR_CHM_Mosaic/"
    out_dir = r"E:\GEDI\Result\Canopy/"
    arcpy.env.workspace = chm_dir
    files = arcpy.ListRasters()
    for i in files:
        if "Agulhas" not in i:
            print("Process..." + os.path.splitext(i)[0])
            inshp = shp_dir + os.path.splitext(i)[0] + "_c_buffer.shp"
            inras = chm_dir + i
            table = GDB_dir + os.path.splitext(i)[0][4:] + "_m_ZonalStatistics"
            xls = out_dir + "m_" + os.path.splitext(i)[0] + ".xls"
            csv = out_dir + "m_" + os.path.splitext(i)[0] + ".csv"
            ZonalStatisticsAsTable(inshp, "shot_numbe", inras, table,"DATA","MEAN")
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
    #merge all csv
    dir = r"E:\GEDI\Result\Canopy/"
    initial_list = []
    df_initial = pd.DataFrame(initial_list)
    files = os.listdir(dir)
    for file in files:
        if "m_" in file:
            print("Merging..." + file)
            df = pd.read_csv(dir + file, header=0)
            df = df[["shot_numbe", "MEAN"]]
            df_initial = pd.concat([df_initial, df],sort=False)
    out = dir + "m_All.csv"
    print("Output merged file..." + out)
    df_initial.to_csv(out, index=None)
    #output CC
    csv1 = r"E:\GEDI\Result\Canopy/m_All.csv"
    csv2 = r"E:\GEDI\Result\Result\All_b_s_p_cc.csv"
    output = r"E:\GEDI\Result\Result/All_b_s_p_cc_m.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv2, df_csv1, left_on='shot_numbe', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)


def add_biomass():
    df_basic = pd.read_csv(Out_dir + "All_b_s_p_cc_m.csv", sep=",")
    df_biomass = pd.read_csv(Out_dir + "All_biomass.csv", sep=",")
    df_merge = pd.merge(df_basic, df_biomass, left_on='shot_number', right_on='shot_number',
                        how='left').dropna()
    out_csv = Out_dir + "All_b_s_p_cc_m_biom.csv"
    df_merge.to_csv(out_csv, index=None)


def add_slope():
    shp_dir = r"E:\GEDI\Result\SHP/"
    chm_dir = r"E:\ALS_archive\LiDAR_CHM_slope/"
    out_dir = r"E:\GEDI\Result\Canopy/"
    arcpy.env.workspace = chm_dir
    files = arcpy.ListRasters()
    for i in files:
        if "Agulhas" not in i:
            print("Process..." + os.path.splitext(i)[0])
            inshp = shp_dir + os.path.splitext(i)[0] + "_c_buffer.shp"
            inras = chm_dir + i
            table = GDB_dir + os.path.splitext(i)[0][4:] + "_slope_ZonalStatistics"
            xls = out_dir + "slope_" + os.path.splitext(i)[0] + ".xls"
            csv = out_dir + "slope_" + os.path.splitext(i)[0] + ".csv"
            ZonalStatisticsAsTable(inshp, "shot_numbe", inras, table,"DATA","MEAN")
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
    #merge all csv
    dir = r"E:\GEDI\Result\Canopy/"
    initial_list = []
    df_initial = pd.DataFrame(initial_list)
    files = os.listdir(dir)
    for file in files:
        if "slope_" in file:
            print("Merging..." + file)
            df = pd.read_csv(dir + file, header=0)
            df = df[["shot_numbe", "MEAN"]]
            df_initial = pd.concat([df_initial, df],sort=False)
    out = dir + "slope_All.csv"
    print("Output merged file..." + out)
    df_initial.to_csv(out, index=None)
    #output CC
    csv1 = r"E:\GEDI\Result\Canopy/slope_All.csv"
    csv2 = r"E:\GEDI\Result\Result\All_b_s_p_cc_m_biom.csv"
    output = r"E:\GEDI\Result\Result/All_b_s_p_cc_m_biom_slope.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv2, df_csv1, left_on='shot_numbe', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)
