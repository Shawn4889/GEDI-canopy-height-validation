#Author: Xiaoxuan Li
import pandas as pd
import numpy as np
import shutil
import os
import h5py
import matplotlib.pyplot as plt
print("Basic_GEDI_old.py running, please make sure that you select the correct study sites")


#SA setup
studysite = ['Addo','Agincourt','Agulhas','DNyala','Duku','Welverdiendt',
             'Venetia','Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']
#studysite = ['SA']
beam0 = ["BEAM0000", "BEAM0001", "BEAM0010", "BEAM0011"]
beam1 = ["BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"]
GEDI_loc = r"E:\GEDI\GEDI_archive"
GEDI_L2A_dir = GEDI_loc + r"/GEDI02A/SA/"
GEDI_L2B_dir = GEDI_loc + r"/GEDI02B/SA/"
CSV_dir = GEDI_loc + r"/SA.csv"
Result_dir = r"E:\GEDI\Result\Basic_GEDI/"

'''
#VA setup
studysite = ['BF','SCBI','CTF']
GEDI_loc = r'G:\GEDI\ori\VA'
Result_dir = r"E:/temp/Konrad/LiDAR_CHM/Basic_GEDI/"
'''

try:
    os.makedirs(Result_dir)
except:
    print("Folder already exists")

for Location in studysite:
    print(Location)
    # result directory
    coordinate_output = Result_dir + Location + "_GEDI.csv"
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
        GEDI_L2B = GEDI_L2B_dir + 'GEDI02_B_' + os.path.splitext(files)[0][9:-15] + "_02_003_01_V002.h5"
        f_L2A = h5py.File(GEDI_L2A, 'r')
        f_L2B = h5py.File(GEDI_L2B, 'r')
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
            '''
            df_interaction = df_Coor[(df_Coor["lat"] > lat_min) &
                                     (df_Coor["lat"] < lat_max) &
                                     (df_Coor["long"] > lon_min) &
                                     (df_Coor["long"] < lon_max) &
                                     (df_Coor["ql2a"] == 1) &
                                     (df_Coor["ql2b"] == 1)]
            '''
            df_interaction = df_Coor[(df_Coor["lat"] > lat_min) &
                                     (df_Coor["lat"] < lat_max) &
                                     (df_Coor["long"] > lon_min) &
                                     (df_Coor["long"] < lon_max)]
            index_result = np.asarray(df_interaction.index.values)
            print(index_result)
            if not df_interaction.empty:
                # selected algor
                df_sa = pd.DataFrame(np.array(f_L2A[b + '/selected_algorithm'][:]), columns=['selected_algorithm'])
                # Degrade flag
                df_degrade = pd.DataFrame(np.array(f_L2A[b + '/degrade_flag'][:]), columns=['degrade_flag'])
                # Solar quality
                df_solar = pd.DataFrame(np.array(f_L2A[b + '/solar_elevation'][:]), columns=['solar_elevation'])
                #quality flag comps
                df_assess_quality_flag = pd.DataFrame(np.array(f_L2A[b + '/rx_assess/quality_flag'][:]),
                                                      columns=['assess_quality_flag'])
                df_surface_flag = pd.DataFrame(np.array(f_L2A[b + '/surface_flag'][:]), columns=['surface_flag'])
                df_geo_stale_return_flag = pd.DataFrame(np.array(f_L2A[b + '/geolocation/stale_return_flag'][:]),
                                                      columns=['stale_return_flag'])
                df_assess_rx_maxamp = pd.DataFrame(np.array(f_L2A[b + '/rx_assess/rx_maxamp'][:]),
                                                      columns=['rx_maxamp'])
                df_assess_sd_corrected = pd.DataFrame(np.array(f_L2A[b + '/rx_assess/sd_corrected'][:]),
                                                   columns=['sd_corrected'])

                # Geolocation & Quality data
                df_shot = pd.DataFrame(np.array(f_L2B[b + '/shot_number'][:]), columns=['shot_number'])
                df_ql2a = pd.DataFrame(np.array(f_L2B[b + '/l2a_quality_flag'][:]), columns=['l2a_quality_flag'])
                df_ql2b = pd.DataFrame(np.array(f_L2B[b + '/l2b_quality_flag'][:]), columns=['l2b_quality_flag'])
                # Merge all variables into one dataframe
                df = pd.concat([df_shot, df_ql2a, df_ql2b, df_Coor, df_solar, df_sa, df_degrade,
                                df_assess_quality_flag,df_surface_flag,df_geo_stale_return_flag,
                                df_assess_rx_maxamp,df_assess_sd_corrected], axis=1)
                df["track"] = os.path.splitext(files)[0][9:-10]
                df = df.loc[index_result, :]
                df_coordinate_initial = pd.concat([df_coordinate_initial, df])
                #relative height df
                # coord df
                rh_initial = []
                df_rh_initial = pd.DataFrame(rh_initial)
                for i in range(6):
                    # Relative height by methods
                    df_lowestmode = pd.DataFrame(np.array(f_L2A[b + '/geolocation/elev_lowestmode_a' + str(i + 1)][:]),
                                                 columns=['elev_lowestmode_a' + str(i + 1)])
                    df_highestreturn = pd.DataFrame(np.array(f_L2A[b + '/geolocation/elev_highestreturn_a' + str(i + 1)][:]),
                                                    columns=['elev_highestreturn_a' + str(i + 1)])
                    #quality flag comps:
                    df_sensitivity = pd.DataFrame(np.array(f_L2A[b + '/geolocation/sensitivity_a' + str(i + 1)][:]),
                                                  columns=['sensitivity_a' + str(i + 1)])

                    df_rx_algrunflag_a = pd.DataFrame(np.array(f_L2A[b + '/rx_processing_a' + str(i + 1) +"/rx_algrunflag"][:]),
                                                  columns=['algrunflag_a' + str(i + 1)])
                    df_rx_zcross_a = pd.DataFrame(np.array(f_L2A[b + '/rx_processing_a' + str(i + 1) +"/zcross"][:]),
                                                  columns=['zcross_a' + str(i + 1)])
                    df_rx_toploc_a = pd.DataFrame(np.array(f_L2A[b + '/rx_processing_a' + str(i + 1) +"/toploc"][:]),
                                                  columns=['toploc_a' + str(i + 1)])

                    #RH
                    df_rh = pd.DataFrame(np.array(f_L2A[b + '/geolocation/rh_a' + str(i + 1)][:]),
                                         columns=["{:02d}".format(x) for x in range(101)])
                    df_rh_a = df_rh.loc[index_result, ['49', '74', '89', '94', '97', '99']]
                    df_rh_a = df_rh_a / 100
                    df_rh_a[df_rh_a < 0] = 0
                    head = "RH" + str(i+1)
                    df_rh_a.columns = [head + '_50', head + '_75', head + '_90',
                                       head + '_95', head + '_98', head + '_100']
                    #relative height descriptive stats
                    #print(df_rh_a.describe(include='all'))
                    df_rh_initial = pd.concat([df_rh_initial, df_rh_a, df_lowestmode, df_highestreturn,df_sensitivity,
                                               df_rx_algrunflag_a, df_rx_zcross_a, df_rx_toploc_a], axis=1)
                    df_rh_initial = df_rh_initial.loc[index_result,:]
                #add canopy cover, PAI, PAVD, FHD
                df_cover = pd.DataFrame(np.array(f_L2B[b + '/cover'][:]))
                df_pai = pd.DataFrame(np.array(f_L2B[b + '/pai'][:]))
                df_fhd = pd.DataFrame(np.array(f_L2B[b + '/fhd_normal'][:]))
                df_pavd = pd.DataFrame(np.array(f_L2B[b + '/pavd_z'][:]))
                df_pavd_sel = df_pavd.iloc[:, 0]
                df_profile = pd.concat([df_cover, df_pai, df_fhd, df_pavd_sel], axis=1)
                df_profile = df_profile.loc[index_result,:]
                df_profile.columns = ["GEDI_Cover", "GEDI_PAI", "GEDI_FHD", "GEDI_PAVD"]
                df_out = pd.concat([df_rh_initial, df_profile], axis=1)
                df_out["Beam"] = Beam
                df_profile_initial = pd.concat([df_profile_initial, df_out])
            else:
                print("no interaction between the GEDI track and bounding box")
    if not df_coordinate_initial.empty:
        df_coordinate_initial = pd.concat([df_coordinate_initial, df_profile_initial], axis=1)
        df_coordinate_initial['shot_number'] = 'Shot_' + df_coordinate_initial['shot_number'].astype(str)
        #customize quality flags
        for a in range(6):
            print(a)
            col = 'new_quality_flag' + str(a + 1)
            #for i in df_coordinate_initial.index:
            df_coordinate_initial.loc[(df_coordinate_initial['assess_quality_flag'] == 1)
                                      & (df_coordinate_initial['surface_flag'] == 1)
                                      & (df_coordinate_initial['stale_return_flag'] == 0)
                                      & (df_coordinate_initial['rx_maxamp'] > 8 *
                                         df_coordinate_initial['sd_corrected'])
                                      & (df_coordinate_initial['sensitivity_a' + str(a + 1)] <= 1)
                                      & (df_coordinate_initial['algrunflag_a' + str(a + 1)] == 1)
                                      & (df_coordinate_initial['zcross_a' + str(a + 1)] > 0)
                                      & (df_coordinate_initial['toploc_a' + str(a + 1)] > 0)
                                      , col] = 1
            df_coordinate_initial.loc[(df_coordinate_initial[col] != 1), col] = 0



        df_coordinate_initial['RH10_98'] = df_coordinate_initial['RH1_98']

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 10, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 10, 'RH5_98'] - (
                    df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 10, 'elev_lowestmode_a1'] -
                    df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 10, 'elev_lowestmode_a5'])

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 6, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 6, 'RH6_98']

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 5, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 5, 'RH5_98']

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 4, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 4, 'RH4_98']

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 3, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 3, 'RH3_98']

        df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 2, 'RH10_98'] = \
            df_coordinate_initial.loc[df_coordinate_initial['selected_algorithm'] == 2, 'RH2_98']

        df_coordinate_initial.to_csv(coordinate_output, index=None)
    else:
        print("no GEDI shots locate within the area: " + Location)
    if len(os.listdir(Result_dir)) == 0:
        shutil.rmtree(Result_dir)
