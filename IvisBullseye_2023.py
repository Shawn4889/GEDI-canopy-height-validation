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


def initialization():
    for Location in studysite:
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
        # convert GEDI shots in excel to shapefiles by study site
        print(Out_XY_dir3)
        if os.path.isfile(Out_XY_dir3):
            print("GEDI shapefiles already exist")
        else:
            arcpy.management.XYTableToPoint(Result_coordinate_dir, Out_XY_dir, "long", "lat")
            arcpy.FeatureClassToShapefile_conversion(Out_XY_dir, SHP_dir)
            # set SR to buffered GEDI shots
            arcpy.Project_management(Out_XY_dir2, Out_XY_dir3, BoundingBox_dir)

        arcpy.MakeFeatureLayer_management(Out_XY_dir3, 'XY')
        for track in list_site:
            print("Track: " + track)
            print("Orbit: " + track.split("_")[1])
            L1B = L1B_dir + "GEDI01_B_" + track + "05_02_V002.h5"
            if not os.path.isfile(L1B):
                L1B = L1B_dir + "GEDI01_B_" + track + "05_01_V002.h5"
            try:
                f_L1B = h5py.File(L1B, 'r')
                beam = list(f_L1B.keys())
                for b in beam:
                    print(b)
                    if b in beam0:
                        Beam = b + "_0"
                    else:
                        Beam = b + "_1"
                    list_a = []
                    # Select by attribute: track
                    query = """ "track" = '%s'""" % track + " AND " + """ "BEAM" = '%s'""" % Beam
                    arcpy.SelectLayerByAttribute_management('XY', 'NEW_SELECTION', query)
                    arcpy.SelectLayerByLocation_management('Boundingbox_lyr', 'INTERSECT', 'XY')
                    field = ['LAS_NM']
                    with arcpy.da.SearchCursor('Boundingbox_lyr', field) as cursor:
                        for row in cursor:
                            list_a.append(str(row[0].replace(" ", "")) + ".las")
                    matchcount = int(arcpy.GetCount_management('Boundingbox_lyr')[0])
                    if matchcount == 0:
                        print('no features matched spatial and attribute criteria')
                    else:
                        print('{0} las tiles that matched criteria written to {0}'.format(matchcount))
                    # output las file list_a
                    VM_dir = r'/home/xiaoxuan/Desktop/ALS/'
                    txt = LvisBullsEye_dir + Location + "_" + track.split("_")[1] + "_" + b + "_las.txt"
                    with open(txt, 'w') as output:
                        for i in list_a:
                            if str(i).split(".")[1] == "las":
                                # record las dir and names into a text file
                                output.write(VM_dir + Location + "/" + str(i) + '\n')
                    filesize = os.path.getsize(txt)
                    if filesize == 0:
                        os.remove(txt)
            except:
                print("No h5 file.."+L1B)


def lvisbullseye_txt():
    VM_dir = r'/home/xiaoxuan/Desktop/'
    LvisBullsEye_folder = os.listdir(LvisBullsEye_dir)
    with open(LvisBullsEye_dir + "run_lvisbullseye.txt", 'w') as output:
        with open(LvisBullsEye_dir + "run_gedimetrics.txt", 'w') as output1:
            for i in LvisBullsEye_folder:
                if "las" in i and os.path.getsize(LvisBullsEye_dir + i) != 0:
                    print("Processing..." + i)
                    b = i.split("_")[2]
                    if b == "BEAM0000":
                        b_i = "10000000"
                    elif b == "BEAM0001":
                        b_i = "01000000"
                    elif b == "BEAM0010":
                        b_i = "00100000"
                    elif b == "BEAM0011":
                        b_i = "00010000"
                    elif b == "BEAM0101":
                        b_i = "00001000"
                    elif b == "BEAM0110":
                        b_i = "00000100"
                    elif b == "BEAM1000":
                        b_i = "00000010"
                    elif b == "BEAM1011":
                        b_i = "00000001"
                    print("Beam determined: " + str(b) + " for case: " + i)
                    # output Lvisbullseye syntax list
                    if "Venetia" in i or "Addo" in i or "DNyala" in i :
                        code = 32735
                    elif "Agulhas" in i:
                        code = 32734
                    else:
                        code = 32736
                    print("Code determined: " + str(code) + " for case: " + i)
                    #study site + track
                    fullname = i.split(".")[0][:-4]
                    gedi_archive = r"E:\GEDI\GEDI_archive\GEDI01B\SA"
                    gedi_archive_folder = os.listdir(gedi_archive)
                    for j in gedi_archive_folder:
                        if i.split("_")[1] in j:
                            gedi_dir = "/home/xiaoxuan/Desktop/GEDI/" + j
                    # run gediSimulator: LvisBullseye
                    output.write("singularity exec --bind /home/xiaoxuan/Desktop/ "
                                 "gediSingularity collocateWaves -listAls "
                                 + VM_dir + "Result/" + str(i)
                                 + " -gedi " + gedi_dir
                                 + " -readHDFgedi -aEPSG " + str(code)
                                 + " -solveCofG -fixFsig -fSigma 6.25"
                                 + " -readPulse " + VM_dir + "Result/" + b + ".txt"
                                 + " -decimate 6 -beamList " + b_i
                                 + " -minSense 0.9 -minDense 3 -geoError 10 5 -writeWaves " + VM_dir
                                 + "H5/" + fullname + "_corr.h5" + '\n')
                    #Copy correlation table from orginal folder to desktop
                    output.write("sudo cp /home/xiaoxuan/gediSimulator/teast.correl " + VM_dir + "H5/" + fullname + "_cor.txt" + '\n')
                    #run gediSimulator: gediMetric. Be sure to use correct h5 name to run gedimetric
                    output1.write("singularity exec --bind /home/xiaoxuan/Desktop/ "
                                 "gediSingularity gediMetric -input /home/xiaoxuan/Desktop/Result/"
                                 + fullname + "_corr.h5 " +
                                 "-readHDFgedi -ground -linkNoise 0 0.95 -varScale 2.5 -minWidth 3 "
                                 "-rhres 1 -sWidth 0.7 -outRoot " + VM_dir + "Result/" + fullname + " test1" + '\n')


def tx_ascii():
    LvisBullsEye_folder = os.listdir(LvisBullsEye_dir)
    for i in LvisBullsEye_folder:
        if "run" not in i:
            print("ascii..."+i)
            site = i.split("_")[0]
            orbit = i.split("_")[1]
            beam = i.split("_")[2]
            gedi_ = r"E:\GEDI\GEDI_archive\GEDI01B\SA/"
            ascii_dir = gedi_ + site + "/" + site + "_" + orbit + "_" + beam + ".filt"
            gedi01b_dir = gedi_ + site + "/" + site + "_" + orbit + ".h5"
            f_L1B = h5py.File(gedi01b_dir, 'r')
            df_waveform = pd.DataFrame(np.array(f_L1B[beam + '/txwaveform'][:]), columns=['waveform'])
            x = 0
            with open(ascii_dir, 'w') as output:
                for c in range(df_waveform.shape[0]):
                    output.write(str(round(x,2)) + " " + str(df_waveform.at[c,'waveform']) + '\n')
                    x = x + 0.15
                    if x > 19.05:
                        x = 0


def rename():
    LvisBullsEye_folder = os.listdir(LvisBullsEye_dir)
    for i in LvisBullsEye_folder:
        if ".h5_" in i:
            os.rename(LvisBullsEye_dir + i, LvisBullsEye_dir + i.split(".")[0] + ".h5")
        if "txt_" in i:
            os.rename(LvisBullsEye_dir + i, LvisBullsEye_dir + i.split(".")[0] + "_cor" + ".txt")


def join():
    #gedi metrics attributes
    columns = ['shot_numbe','als_ground','als_top',
               'GEDI_sim_rh_98','GEDI_sim_rh_100',
               'longitude','latitude']
    metric_folder = os.listdir(LvisBullsEye_dir)
    for Location in studysite:
        if "All" not in Location:
            Location = Location.split("_")[0]
            print("gediMetric running...Case: " + Location)
            df_track = pd.read_csv(GEDI_folder + Location + "_GEDI.csv", sep=",")
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
                    df_merge = pd.merge(df_track, df_initial, left_on='shot_number', right_on='shot_number',
                                        how='left').dropna()
                    df_merge.drop_duplicates(keep='first', inplace=True)
                    df_merge.to_csv(Result_dir + str(i).split(".")[0] + "_joined.csv")


def collocation_info():
    df_initial_list = []
    df_initial = pd.DataFrame(df_initial_list)
    output = Result_dir + "Correlation.csv"
    lidar_filelist = os.listdir(LvisBullsEye_dir)
    for file in lidar_filelist:
        if "cor_cor.txt" in file:
            join_file = Result_dir + \
                        file.split("_")[0] + "_" + \
                        file.split("_")[1] + "_" + \
                        file.split("_")[2] + "_" + \
                        "joined.csv"
            if os.path.isfile(join_file):
                print("File in process..." + file)
                site = file.split("_")[0]
                orbit = file.split("_")[1]
                beam = file.split("_")[2]
                df = pd.read_csv(LvisBullsEye_dir + str(file), skiprows = 1, header=None, sep=" ")
                df['site'] = site
                df['orbit'] = orbit
                df['beam'] = beam
                df.columns = ["dx", "dy", "dz", "fsigma", "correlation",
                              "sample","deltaCofG","site","orbit","beam"]
                if df["correlation"][0] < 0.85 or df["dz"][0] > 50 or df["dz"][0] < -50:
                    print("Correlation not qualified, remove..." + str(file))
                    os.remove(join_file)
                else:
                    print("correlation: " + str(df["correlation"][0]))
                    print("dz: " + str(df["dz"][0] < 50))
                    df_initial = pd.concat([df_initial, df])
    df_initial.to_csv(output)


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
    out_csv = Result_dir + "All_record.csv"
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


def output_shp1():
    merge_files = os.listdir(Result_dir)
    for file in merge_files:
        if "_c" in file and "All" not in file:
            if "Addo" in file or "Venetia" in file or "DNyala" in file:
                Proj = Proj_dir + "UTM35S.prj"
            elif "Agulhas" in file:
                Proj = Proj_dir + "UTM34S.prj"
            else:
                Proj = Proj_dir + "UTM36S.prj"
            out_shp = GDB_dir + str(file).split(".")[0]
            out_buffer = SHP_dir + str(file).split(".")[0] + "_buffer.shp"
            print("Converting csv to buffered shp: " + out_buffer)
            try:
                arcpy.management.XYTableToPoint(Result_dir + file, out_shp,
                                                "longitude", "latitude", '', Proj)
                arcpy.Buffer_analysis(out_shp, out_buffer, "12.5 Meters")
            except:
                print("Failed to generate shapefile from test case: " + file + ", skip to the next test case")


def add_cc_ind():
    shp_dir = r"E:\GEDI\Result\SHP/"
    chm_dir = r"E:\ALS_archive\LiDAR_CHM_CC15/"
    out_dir = r"E:\GEDI\Result\ALS\Canopy/"

    arcpy.env.workspace = shp_dir
    files = arcpy.ListFeatureClasses()
    for i in files:
        if "_c_buffer" in i:
            print("Process..." + os.path.splitext(i)[0])
            inshp = shp_dir + i
            inras = chm_dir + os.path.splitext(i)[0].split("_")[0] + ".tif"
            print(inras)
            table = GDB_dir + os.path.splitext(i)[0][4:] + "_CC_ZonalStatistics"
            xls = out_dir + "CC_" + os.path.splitext(i)[0] + ".xls"
            csv = out_dir + "CC_" + os.path.splitext(i)[0] + ".csv"
            ZonalStatisticsAsTable(inshp, "shot_numbe", inras, table,"DATA","SUM")
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
    #merge all csv
    dir = r"E:\GEDI\Result\ALS\Canopy/"
    initial_list = []
    df_initial = pd.DataFrame(initial_list)
    files = os.listdir(dir)
    for file in files:
        print("Merging..." + file)
        df = pd.read_csv(dir + file, header=0)
        df_initial = pd.concat([df_initial, df],sort=False)
    df_initial["CC_15"] = df_initial["SUM"] / df_initial["COUNT"]
    out = dir + "CC_All.csv"
    print("Output merged file..." + out)
    df_initial.to_csv(out, index=None)
    #output CC
    csv1 = r"E:\GEDI\Result\ALS\Canopy/CC_All.csv"
    csv2 = r"E:\GEDI\Result\Result\\All_record_phenology.csv"
    output = r"E:\GEDI\Result\Result/All_PC.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv2, df_csv1, left_on='shot_number', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)


def add_mean():
    shp_dir = r"E:\GEDI\Result\SHP/"
    chm_dir = r"E:\ALS_archive\LiDAR_CHM_Mosaic/"
    out_dir = r"E:\GEDI\Result\ALS\Canopy/"
    arcpy.env.workspace = shp_dir
    files = arcpy.ListFeatureClasses()
    for i in files:
        if "_c_buffer" in i:
            print("Process..." + os.path.splitext(i)[0])
            inshp = shp_dir + i
            inras = chm_dir + os.path.splitext(i)[0].split("_")[0] + ".tif"
            print(inras)
            table = GDB_dir + os.path.splitext(i)[0][4:] + "_m_ZonalStatistics"
            xls = out_dir + "m_" + os.path.splitext(i)[0] + ".xls"
            csv = out_dir + "m_" + os.path.splitext(i)[0] + ".csv"
            ZonalStatisticsAsTable(inshp, "shot_numbe", inras, table,"DATA","MEAN")
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
    #merge all csv
    dir = r"E:\GEDI\Result\ALS\Canopy/"
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
    csv1 = r"E:\GEDI\Result\ALS\Canopy/m_All.csv"
    csv2 = r"E:\GEDI\Result\Result\\All_PC.csv"
    output = r"E:\GEDI\Result\Result/All_PCM.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv2, df_csv1, left_on='shot_number', right_on='shot_numbe', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)


def add_phenology():
    csv = r"E:\GEDI\Result\Result/All_record.csv"
    out = r"E:\GEDI\Result\Result/All_record_phenology.csv"
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



def output_biom_csv():
    dir = r"E:\GEDI\Result\Result/"
    csv = dir + "All_rh_biom.csv"
    df = pd.read_csv(csv)
    studysite = ['Agincourt', 'DNyala', 'Welverdiendt',
                 'Venetia', 'Limpopo1', 'Limpopo2', 'Limpopo3', 'Justicia', 'Ireagh']
    for s in studysite:
        print(s)
        out = dir + s + "_rh_biom.csv"
        df_s = df.loc[df['site'] == s]
        df_s.to_csv(out)


def output_shp2():
    merge_files = os.listdir(Result_dir)
    for file in merge_files:
        if "_c.csv" in file and "All" not in file:
            if "Addo" in file or "Venetia" in file or "DNyala" in file:
                Proj = Proj_dir + "UTM35S.prj"
            elif "Agulhas" in file:
                Proj = Proj_dir + "UTM34S.prj"
            else:
                Proj = Proj_dir + "UTM36S.prj"
            out_shp = GDB_dir + str(file).split(".")[0]
            out_buffer = SHP_dir + str(file).split(".")[0] + "_buffer.shp"
            print("Converting csv to buffered shp: " + out_buffer)
            try:
                arcpy.management.XYTableToPoint(Result_dir + file, out_shp,
                                                "longitude", "latitude", '', Proj)
                arcpy.Buffer_analysis(out_shp, out_buffer, "12.5 Meters")
            except:
                print("Failed to generate shapefile from test case: " + file + ", skip to the next test case")


def slope():
    dtm_dir = r"E:\GEDI\ALS_archive\LiDAR_DTM/"
    folders = os.listdir(dtm_dir)
    for folder in folders:
        print("Site..." + folder)
        slope_folder_dir = slope_dir + folder + "/"
        if not os.path.isdir(slope_folder_dir):
            os.makedirs(slope_folder_dir)
        arcpy.env.workspace = dtm_dir + folder
        rasters = arcpy.ListRasters("*", "tif")
        for inraster in rasters:
            print("Processing..." + inraster)
            out = slope_folder_dir + inraster
            outSlope = Slope(inraster, "DEGREE", 1)
            outSlope.save(out)


def slope_merge():
    folders = os.listdir(slope_dir)
    for folder in folders:
        print("Processing..." + folder)
        slope_folder_dir = slope_dir + folder + "/"
        arcpy.env.workspace = slope_folder_dir
        rasters = arcpy.ListRasters("*", "tif")
        outraster = folder + ".tif"
        arcpy.MosaicToNewRaster_management(rasters, slope_folder_dir, outraster, "", "32_BIT_FLOAT", 1, 1, "", "FIRST")


def slope_zonal():
    folders = os.listdir(slope_dir)
    for Location in folders:
        print("Processing..." + Location)
        chm_merge = slope_dir + Location + "/" + Location + ".tif"
        shp_dir = SHP_dir + "Merge.shp"
        shp_check = SHP_dir + Location + "_buffer.shp"
        table = GDB_dir + Location + "_Slope_ZonalStatistics"
        xls = Slp_dir + Location + "_stat.xls"
        csv = Slp_dir + Location + "_stat.csv"
        if os.path.isfile(shp_check):
            ZonalStatisticsAsTable(shp_dir, "shot_numbe", chm_merge, table)
            arcpy.TableToExcel_conversion(table, xls)
            data_xls = pd.read_excel(xls)
            data_xls.to_csv(csv)
            os.remove(xls)
        else:
            print("No GEDI footprint for site: " + Location)


def ndvi():
    GEDI_L2A_dir = r"E:\GEDI\GEDI_archive\GEDI02A\SA/"
    table = r"E:\GEDI\Result\Result\table.csv"
    df = pd.read_csv(table, header=0)
    rows = df.shape[0]
    print(rows)
    for row in range(rows):
        site = df.iloc[row]["Studysite"]
        ts_orbit = df.iloc[row]["Orbit"]
        ts = site + "_" + ts_orbit
        GEDIs = os.listdir(GEDI_L2A_dir)
        for GEDI in GEDIs:
            GEDI_Orbit = GEDI.split("_")[3]
            if ts_orbit == GEDI_Orbit:
                GEDI_time = GEDI[9:16]
                GEDI_year = str(GEDI_time)[0:4]
                GEDI_days = str(GEDI_time)[4:]
                GEDI_date = datetime.datetime(int(GEDI_year), 1, 1) + datetime.timedelta(int(GEDI_days) - 1)
                GEDI_month = GEDI_date.month
        if site == "Addo":
            NDVI_tile = "h20v12"
        elif site == "Agulhas":
            NDVI_tile = "h19v12"
        else:
            NDVI_tile = "h20v11"
        NDVI_list = os.listdir(NDVI_dir)
        for NDVI in NDVI_list:
            if NDVI_tile in NDVI:
                NDVI_time = NDVI[1:8]
                NDVI_year = str(NDVI_time)[0:4]
                NDVI_days = str(NDVI_time)[4:]
                NDVI_date = datetime.datetime(int(NDVI_year), 1, 1) + datetime.timedelta(int(NDVI_days) - 1)
                NDVI_month = NDVI_date.month
                if GEDI_year == NDVI_year and GEDI_month == NDVI_month:
                    print("GEDI orbit determined: " + ts_orbit)
                    print("NDVI tile determined: " + NDVI_tile)
                    print("GEDI date determined: " + str(GEDI_time))
                    print("NDVI date determined: " + str(NDVI_time))
                    print("Matching year/month determined: " + str(NDVI_year) + " "+ str(NDVI_month))
                    print("Zonal stats: " + ts + " and " + NDVI)
                    shp_zs = BB_dir + site.split("_")[0] + ".shp"
                    print("shp in progress: " + shp_zs)
                    ndvi_zs = NDVI_dir + NDVI
                    #GEDI/NDVI date matching
                    table = GDB_dir + ts.split(".")[0] + "_NDVI_zonalstat"
                    xls = NDVI_result_dir + ts.split(".")[0] + "_NDVI.xls"
                    csv = NDVI_result_dir + ts.split(".")[0] + "_NDVI.csv"
                    ZonalStatisticsAsTable(shp_zs, "FID", ndvi_zs, table)
                    arcpy.TableToExcel_conversion(table, xls)
                    data_xls = pd.read_excel(xls)
                    data_xls.to_csv(csv)
                    os.remove(xls)
                    #GEDI/20 year mean NDVI matching
                    print("Match GEDI with 20 year mean NDVI...test case: " + ts)
                    ndvim_zs = NDVI_mean_dir + NDVI_tile + ".tif"
                    table = GDB_dir + ts.split(".")[0] + "_NDVI_zonalstat"
                    xls = NDVI_result_dir + ts.split(".")[0] + "_NDVI_mean.xls"
                    csv = NDVI_result_dir + ts.split(".")[0] + "_NDVI_mean.csv"
                    ZonalStatisticsAsTable(shp_zs, "FID", ndvim_zs, table)
                    arcpy.TableToExcel_conversion(table, xls)
                    data_xls = pd.read_excel(xls)
                    data_xls.to_csv(csv)
                    os.remove(xls)


def ndvi_ext():
    ts_list = os.listdir(NDVI_result_dir)
    for ts in ts_list:
        #ts_site = ts.split("_")[0]
        #ts_orbit = ts[-9:-3]
        if "mean" not in ts:
            df = pd.read_csv(NDVI_result_dir + ts, header=0)
            mean_ndvi = df["MEAN"].mean()
            print("Mean: " + ts + " " + str(round(mean_ndvi/100000000,3)))
        else:
            df = pd.read_csv(NDVI_result_dir + ts, header=0)
            mean_20ndvi = df["MEAN"].mean()
            print("20 year mean: " + str(round(mean_20ndvi,3)))
            std_20ndvi = df["MEAN"].std()
            print("20 year std: " + str(round(std_20ndvi, 3)))


def las_subset():
    shp_dir = r"E:\GEDI\Result\SHP/"
    out_dir = r'E:\GEDI\Result\ALS\PointCloud/'
    arcpy.env.workspace = shp_dir
    fcList = arcpy.ListFeatureClasses()
    for fc in fcList:
        if "buffer_235.shp" in fc:
            #site = fc.split("_")[0]
            site = "Addo"
            dir = r"E:\GEDI\ALS_archive\LiDAR_UTM/" + site
            print(fc)
            print(site)
            try:
                os.makedirs(out_dir + site)
            except:
                print("folder already exist..." + out_dir + site)
            lidars = os.listdir(dir)
            for lidar in lidars:
                if "nlz.las" in lidar:
                    las = dir + "/" + lidar
                    output_las = out_dir + site + "/" + lidar
                    wbt.clip_lidar_to_polygon(i=las, polygons=shp_dir + fc, output=output_las)


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
        df_gedi = pd._csv(LvisBullsEye_dir + file + "_merged.csv")
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


def P98_lid_clip():
    studysite = ['Agincourt', 'DNyala', 'Welverdiendt','Venetia',
                 'Limpopo1', 'Limpopo2', 'Limpopo3', 'Justicia', 'Ireagh']
    for site in studysite:
        if site == "Venetia" or site == "DNyala":
            dir_shp = r'E:\GEDI\Result\ALS\P98\ALL_35_SJB.shp'
        else:
            dir_shp = r'E:\GEDI\Result\ALS\P98\ALL_36_SJB.shp'
        arcpy.MakeFeatureLayer_management(dir_shp, 'SHP')
        with arcpy.da.SearchCursor('SHP', ['LAS_NM','site','shot_numbe']) as cursor:
            for row in cursor:
                if row[1] == site:
                    print(row)
                    dir_lidar = r"E:\ALS_archive\LiDAR_UTM/" + row[1] + "/" + row[0].replace(" ", "") + ".las"
                    output_las = r"E:\GEDI\Result\ALS\P98/" + row[1] + "/" + row[2] + ".las"
                    query = """ "shot_numbe" = '{0}'""".format(row[2])
                    arcpy.SelectLayerByAttribute_management("SHP", 'NEW_SELECTION', query)
                    dir_shp_temp = r'E:\GEDI\Result\ALS\P98/'
                    arcpy.FeatureClassToShapefile_conversion("SHP", dir_shp_temp)
                    wbt.clip_lidar_to_polygon(i=dir_lidar, polygons=dir_shp_temp + "SHP.shp", output=output_las)


def las_processing():
    studysite = ['Agincourt', 'DNyala', 'Welverdiendt','Venetia',
                 'Limpopo1', 'Limpopo2', 'Limpopo3', 'Justicia', 'Ireagh']
    list_shot = []
    list_P98 = []
    for site in studysite:
        dir = r"E:\GEDI\Result\ALS\P98/" + site + "/"
        lastool_ground = r"D:\lastool\LAStools\bin\lasground.exe "
        lastool_height = r"D:\lastool\LAStools\bin\lasheight.exe "
        lastool_las = r"D:\lastool\LAStools\bin\las2las.exe "
        lastool_thin = r"D:\lastool\LAStools\bin\lasthin.exe "
        output_ground = r"E:\GEDI\Result\ALS\P98_result\ground.las"
        output_height = r"E:\GEDI\Result\ALS\P98_result\normalized.las"
        output_las = r"E:\GEDI\Result\ALS\P98_result\filtered.las"
        files = os.listdir(dir)
        for file in files:
            print(site + " " + file)
            file_dir = dir + file
            output_thin = r"E:\GEDI\Result\ALS\P98_result/" + site + "/" + file
            subprocess.call(lastool_ground + " -i " + file_dir + " -step 1 -o " + output_ground)
            subprocess.call(lastool_height + " -i " + output_ground + " -replace_z -o " + output_height)
            subprocess.call(lastool_las + " -i " + output_height + " -drop_class 2 -drop_z_below 0.01 -o " + output_las)
            subprocess.call(lastool_thin + " -i " + output_las + " -step 1000 -percentile 98 -ignore_class 2 -o " + output_thin)
            try:
                las = laspy.read(output_thin)
                list_P98.append(max(las.Z / 1000))
                list_shot.append(file.split(".")[0])
            except:
                print("Error, skipped")

    df_shot = pd.DataFrame(list_shot)
    df_p98 = pd.DataFrame(list_P98)
    df = pd.concat([df_shot, df_p98], axis=1)
    df.columns = ["shot", "p98"]
    output_csv = r"E:\GEDI\Result\Result\All_P98.csv"
    df.to_csv(output_csv, index=None)


def combine():
    dir1 = r"E:\GEDI\Result\Result\All_02272023.csv"
    dir2 = r"E:\GEDI\Result\Result\All_P98.csv"
    out = r"E:\GEDI\Result\Result\All_02272023_2.csv"
    df1 = pd.read_csv(dir1)
    df2 = pd.read_csv(dir2)
    df_merge = pd.merge(df1, df2, left_on='shot_number', right_on='shot',
                        how='left').dropna()

    df_merge.to_csv(out, index=False)


def merge_csv_final():
    Input_dir = r"E:\GEDI\Result_Misha\Basic_GEDI/"
    Result_dir = r"E:\GEDI\Result_Misha\Result/"
    out_csv = Result_dir + "All.csv"
    list_testcase = []
    df = pd.DataFrame(list_testcase)
    csv_folder = os.listdir(Input_dir)

    for i in csv_folder:
        print("Merging..." + i)
        in_csv = Input_dir + i
        df_item = pd.read_csv(in_csv)
        df = pd.concat([df, df_item])
    df.to_csv(out_csv, index=None)


def join_csv_final():
    csv1 = r"E:\GEDI\Result\Result/All_02272023_2.csv"
    csv2 = r"E:\GEDI\Result_Misha\Result/All.csv"
    output = r"E:\GEDI\Result_Misha\Result/All_Misha.csv"
    df_csv1 = pd.read_csv(csv1)
    df_csv2 = pd.read_csv(csv2)
    df_merge = pd.merge(df_csv1, df_csv2, left_on='shot_number', right_on='shot_number', how='left')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(output, index=None)


studysite = ['Agincourt', 'DNyala', 'Welverdiendt', 'Venetia',
             'Limpopo1', 'Limpopo2', 'Limpopo3', 'Justicia', 'Ireagh']

beam0 = ["BEAM0000", "BEAM0001", "BEAM0010", "BEAM0011"]
beam1 = ["BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"]

dir = r"E:\GEDI\Result/"
LiDAR_dir = dir + r'LiDAR/'
ALS_dir = dir + r"ALS/"
CHM_dir = ALS_dir + 'CHM/'
Canopy_dir = ALS_dir + 'Canopy/'
Ground_dir = ALS_dir + 'Ground/'
GDB_dir = r"D:\temp\ArcGIS_project\GEDI_I2/GEDI_I2.gdb/"
SHP_dir = dir + r"SHP/"
NDVI_result_dir = dir + r"NDVI/"
NDVI_dir = r"E:\GEDI\NDVI\reprojection/"
NDVI_mean_dir = r"E:\GEDI\NDVI\mosaic/"
Merge_dir = dir + r"Merge/"
LvisBullsEye_dir = dir + r"Collocate/"
Proj_dir = r'E:\GEDI\Boundingbox/'
BB_dir = r"E:\GEDI\Boundingbox/"
L1B_dir = r"E:\GEDI\GEDI_archive\GEDI01B\SA/"
GEDI_folder = dir + r"Basic_GEDI/"
Result_dir = dir + r"Result/"
Slp_dir = dir + r"Slope/"
slope_dir = r"E:\GEDI\ALS_archive\LiDAR_Slope/"
if not os.path.isdir(SHP_dir):
    os.makedirs(SHP_dir)
if not os.path.isdir(LvisBullsEye_dir):
    os.makedirs(LvisBullsEye_dir)
if not os.path.isdir(Merge_dir):
    os.makedirs(Merge_dir)
if not os.path.isdir(NDVI_dir):
    os.makedirs(NDVI_dir)
if not os.path.isdir(LiDAR_dir):
    os.makedirs(LiDAR_dir)
if not os.path.isdir(ALS_dir):
    os.makedirs(ALS_dir)
    os.makedirs(CHM_dir)
    os.makedirs(Canopy_dir)
    os.makedirs(Ground_dir)
if not os.path.isdir(Slp_dir):
    os.makedirs(Slp_dir)
if not os.path.isdir(NDVI_result_dir):
    os.makedirs(NDVI_result_dir)
if not os.path.isdir(Result_dir):
    os.makedirs(Result_dir)


#step 1
print('Initializing...')
#initialization()
print('Output gediSimulator txts...')
#lvisbullseye_txt()

#step 2
#before gediMetric
print('Rename gediSimulator results...')
#rename()

#step 3
print('Performing join by GEDI tracks')
#join()
print('Extract collocation info and remove unqualified beams')
#collocation_info()
#step 4 merge
print('Performing merge by test case')
#merge_csv1()
print('Performing merge by study site, be sure to remove outliers')
#merge_csv2()
print("Performing merge by all sites")

#check quality before you run the final merge func
print('Output combined csv')
#merge_csv3()
print('Output buffered GEDI shot shapefiles')
#output_shp2()

print("Add CHM Canopy cover factor")
#add_cc()
print("Add MEAN CHM Canopy")
#add_mean()

print("Add phenology and split the original data to leaf-on/off")
#add_phenology()
print("Output biom+rh csv")
#output_biom_csv()
print('Outputing buffered GEDI shot shapefiles')
#output_shp2()

#other factors, optional
#slope()
print('Merging slope rasters')
#slope_merge()
print('Slope zonal stats')
#slope_zonal()
print('Add NDVI factor')
#ndvi()
#ndvi_ext()

#CHM generation, optional
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

print("las clip to GEDI footprint")
#P98_lid_clip()
print("las_processing")
#las_processing()
print("combine")
#combine()
print("Merge all basic files")
#merge_csv_final()
print("Merge all basic files")
#join_csv_final()


las = r"E:\Biomass\CSIR\Result_0622\Wel.las"
shp = r"E:\Biomass\CSIR\Result_0622\Welverdiendt_sel.shp"
output_las = r"E:\Biomass\CSIR\Result_0622\Wel_sel.las"
wbt.clip_lidar_to_polygon(i=las, polygons=shp, output=output_las)
