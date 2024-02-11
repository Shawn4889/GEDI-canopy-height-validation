#Author: Xiaoxuan Li
import pandas as pd
import numpy as np
import shutil
import os
import h5py

#SA setup
studysite = ['Agincourt','DNyala','Welverdiendt','Venetia',
             'Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']


beam0 = ["BEAM0000", "BEAM0001", "BEAM0010", "BEAM0011"]
beam1 = ["BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"]
GEDI_loc = r"E:\GEDI\GEDI_archive"
GEDI_L2A_dir = GEDI_loc + r"/GEDI02A/SA_2023/"
GEDI_L2B_dir = GEDI_loc + r"/GEDI02B/SA_2023/"
CSV_dir = GEDI_loc + r"/SA.csv"
Result_dir = r"E:\GEDI\Result_Misha\Basic_GEDI/"

for Location in studysite:
    if "Venetia" in Location:
        print(Location)
        # result directory
        coordinate_output = Result_dir + Location + "_GEDI_2023.csv"
        # coord df
        coordinate_initial = []
        df_coordinate_initial = pd.DataFrame(coordinate_initial)
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
                                         (df_Coor["long"] < lon_max)]
                index_result = np.asarray(df_interaction.index.values)
                print(index_result)
                if not df_interaction.empty:
                    # Geolocation & Quality data
                    df_shot = pd.DataFrame(np.array(f_L2B[b + '/shot_number'][:]), columns=['shot_number'])
                    df_sensitivity = pd.DataFrame(np.array(f_L2A[b + '/sensitivity'][:]),columns=['sensitivity'])
                    df_sf = pd.DataFrame(np.array(f_L2A[b + '/sensitivity'][:]),columns=['surface_flag'])
                    df_dif = pd.DataFrame(np.array(f_L2A[b + '/degrade_flag'][:]),
                                                           columns=['degrade_flag'])
                    df_pft_class = pd.DataFrame(np.array(f_L2A[b + '/land_cover_data/pft_class'][:]),
                                                           columns=['pft_class'])
                    df_region_class = pd.DataFrame(np.array(f_L2A[b + '/land_cover_data/region_class'][:]),
                                                           columns=['region_class'])
                    df_lwp = pd.DataFrame(np.array(f_L2A[b + '/land_cover_data/landsat_water_persistence'][:]),
                                                   columns=['landsat_water_persistence'])
                    df_up = pd.DataFrame(np.array(f_L2A[b + '/land_cover_data/urban_proportion'][:]),
                                          columns=['urban_proportion'])
                    df_profile = pd.concat([df_shot,df_Coor,df_sensitivity,df_sf,df_dif,
                                    df_pft_class,df_region_class,df_lwp,df_up], axis=1)
                    df_profile = df_profile.loc[index_result, :]
                    df_coordinate_initial = pd.concat([df_coordinate_initial,df_profile])
                else:
                    print("no interaction between the GEDI track and bounding box")
        if not df_coordinate_initial.empty:
            print("Output..." + coordinate_output)
            df_coordinate_initial['shot_number'] = 'Shot_' + df_coordinate_initial['shot_number'].astype(str)
            df_coordinate_initial.to_csv(coordinate_output, index=None)
        else:
            print("no GEDI shots locate within the area: " + Location)
