#Author: Xiaoxuan Li
import pandas as pd
import numpy as np
import shutil
import os
import h5py

print("Basic_GEDI_old.py running, please make sure that you select the correct study sites")

studysite = ['Agincourt','DNyala','Welverdiendt','Venetia','Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']


beam0 = ["BEAM0000", "BEAM0001", "BEAM0010", "BEAM0011"]
beam1 = ["BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"]
GEDI_loc = r"E:\GEDI\GEDI_archive"
GEDI_L2A_dir = GEDI_loc + r"/GEDI02A/SA_2023/"
GEDI_L2B_dir = GEDI_loc + r"/GEDI02B/SA_2023/"
CSV_dir = GEDI_loc + r"/SA.csv"
Result_dir = r"E:\GEDI\Result\Basic_GEDI_PFT/"

try:
    os.makedirs(Result_dir)
except:
    print("Folder already exists")

for Location in studysite:
    print(Location)
    # result directory
    coordinate_output = Result_dir + Location + "_GEDI_pft_2023.csv"
    # coord df
    coordinate_initial = []
    df_coordinate_initial = pd.DataFrame(coordinate_initial)
    profile_initial = []
    df_profile_initial = pd.DataFrame(profile_initial)
    #Study site bounds
    df_geo = pd.read_csv(CSV_dir, sep=",")
    lat_max = df_geo.loc[df_geo['Location'] == Location, 'ul_lat'].iloc[0]
    lon_min = df_geo.loc[df_geo['Location'] == Location, 'ul_lon'].iloc[0]
    lat_min = df_geo.loc[df_geo['Location'] == Location, 'lr_lat'].iloc[0]
    lon_max = df_geo.loc[df_geo['Location'] == Location, 'lr_lon'].iloc[0]
    HDF_folder = os.listdir(GEDI_L2A_dir)
    #for i in range(6):
    for files in HDF_folder:
        print(files)
        # read HDF file
        GEDI_L2A = GEDI_L2A_dir + files
        GEDI_L2B_1 = GEDI_L2B_dir + 'GEDI02_B_' + os.path.splitext(files)[0][9:-15] + "_02_003_01_V002.h5"
        f_L2A = h5py.File(GEDI_L2A, 'r')
        if os.path.exists(GEDI_L2B_1):
            f_L2B = h5py.File(GEDI_L2B_1, 'r')
        else:
            GEDI_L2B_2 = GEDI_L2B_dir + 'GEDI02_B_' + os.path.splitext(files)[0][9:-15] + "_02_003_02_V002.h5"
            f_L2B = h5py.File(GEDI_L2B_2, 'r')
        # beam list
        beam = list(f_L2A.keys())
        beam.remove("METADATA")
        for b in beam:
            print(b)
            # beam sel
            if b in beam0:
                Beam = b + "_0"
            else:
                Beam = b + "_1"
            print(Location + "_" + Beam)
            df_lat = pd.DataFrame(np.array(f_L2A[b + '/lat_lowestmode'][:]), columns=['lat'])
            df_long = pd.DataFrame(np.array(f_L2A[b + '/lon_lowestmode'][:]), columns=['long'])
            df_quality_l2A = pd.DataFrame(np.array(f_L2B[b + '/l2a_quality_flag'][:]), columns=['ql2a'])
            df_quality_l2B = pd.DataFrame(np.array(f_L2B[b + '/l2b_quality_flag'][:]), columns=['ql2b'])
            df_Coor = pd.concat([df_lat, df_long,df_quality_l2A,df_quality_l2B], axis=1)
            df_interaction = df_Coor[(df_Coor["lat"] > lat_min) &
                                     (df_Coor["lat"] < lat_max) &
                                     (df_Coor["long"] > lon_min) &
                                     (df_Coor["long"] < lon_max) &
                                     (df_Coor["ql2a"] == 1) &
                                     (df_Coor["ql2b"] == 1)]
            index_result = np.asarray(df_interaction.index.values)
            print(index_result)
            if not df_interaction.empty:
                # Geolocation & Quality data
                df = pd.DataFrame(np.array(f_L2B[b + '/shot_number'][:]), columns=['shot_number'])
                df = df.loc[index_result, :]
                df_coordinate_initial = pd.concat([df_coordinate_initial,df])

                rh_initial = []
                df_rh_initial = pd.DataFrame(rh_initial)

                #pft metrics
                df_profile = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/pft_class'][:]), columns=['pft'])
                df_profile = df_profile.loc[index_result,:]
                df_out = pd.concat([df_rh_initial, df_profile], axis=1)
                df_profile_initial = pd.concat([df_profile_initial, df_out])
            else:
                print("no interaction between the GEDI track and bounding box")
    if not df_coordinate_initial.empty:
        df_coordinate_initial = pd.concat([df_coordinate_initial, df_profile_initial], axis=1)
        df_coordinate_initial['shot_number'] = 'Shot_' + df_coordinate_initial['shot_number'].astype(str)
        df_coordinate_initial.to_csv(coordinate_output, index=None)
    else:
        print("no GEDI shots locate within the area: " + Location)
    if len(os.listdir(Result_dir)) == 0:
        shutil.rmtree(Result_dir)

