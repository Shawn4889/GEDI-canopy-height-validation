#Author: Xiaoxuan Li
import pandas as pd
import numpy as np
import shutil
import os
import h5py
import matplotlib.pyplot as plt
print("Basic_GEDI_old.py running, please make sure that you select the correct study sites")


#SA setup

studysite1 = ['Addo','Agincourt','Agulhas','DNyala','Duku','Welverdiendt',
             'Venetia','Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']
studysite = ['Agincourt','DNyala','Welverdiendt',
             'Limpopo1','Limpopo2','Limpopo3','Justicia','Ireagh']


beam0 = ["BEAM0000", "BEAM0001", "BEAM0010", "BEAM0011"]
beam1 = ["BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"]
GEDI_loc = r"E:\GEDI\GEDI_archive"
GEDI_L2A_dir = GEDI_loc + r"/GEDI02A/SA_2023/"
GEDI_L2B_dir = GEDI_loc + r"/GEDI02B/SA_2023/"
CSV_dir = GEDI_loc + r"/SA.csv"
Result_dir = r"E:\GEDI\Result\Basic_GEDI/"

try:
    os.makedirs(Result_dir)
except:
    print("Folder already exists")

for Location in studysite:
    print(Location)
    # result directory
    coordinate_output = Result_dir + Location + "_GEDI_v2.csv"
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
                df_shot = pd.DataFrame(np.array(f_L2B[b + '/shot_number'][:]), columns=['shot_number'])
                df_lowestmode = pd.DataFrame(np.array(f_L2A[b + '/elev_lowestmode'][:]),
                                             columns=['elev_lowestmode'])
                df_highestreturn = pd.DataFrame(np.array(f_L2A[b + '/elev_highestreturn'][:]),
                                                columns=['elev_highestreturn'])
                df_sensitivity = pd.DataFrame(np.array(f_L2A[b + '/sensitivity'][:]),columns=['sensitivity'])
                df_shot["track"] = os.path.splitext(files)[0][9:-10]
                df = pd.concat([df_shot,df_Coor,df_lowestmode,df_sensitivity], axis=1)
                df = df.loc[index_result, :]
                df_coordinate_initial = pd.concat([df_coordinate_initial,df])

                rh_initial = []
                df_rh_initial = pd.DataFrame(rh_initial)
                #add canopy cover, PAI, PAVD, FHD, cover
                df_cc_ls = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/landsat_treecover'][:]), columns=['landsat_treecover'])
                df_cc_md = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/modis_treecover'][:]), columns=['modis_treecover'])
                df_cc_nv = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/modis_nonvegetated'][:]), columns=['modis_nonvegetated'])

                df_omega = pd.DataFrame(np.array(f_L2B[b + '/omega'][:]), columns=['omega'])
                df_pgap = pd.DataFrame(np.array(f_L2B[b + '/pgap_theta'][:]), columns=['pgap_theta'])
                df_pgap_z = pd.DataFrame(np.array(f_L2B[b + '/pgap_theta_z'][:]), columns=['pgap_theta_z'])
                df_fhd = pd.DataFrame(np.array(f_L2B[b + '/fhd_normal'][:]), columns=['fhd_normal'])

                df_leaf_on_doy = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/leaf_on_doy'][:]), columns=['leaf_on_doy'])
                df_leaf_on_cycle = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/leaf_on_cycle'][:]), columns=['leaf_on_cycle'])
                df_leaf_off_doy = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/leaf_off_doy'][:]), columns=['leaf_off_doy'])
                df_leaf_off_flag = pd.DataFrame(np.array(f_L2B[b + '/land_cover_data/leaf_off_flag'][:]), columns=['leaf_off_flag'])

                df_pavd = pd.DataFrame(np.array(f_L2B[b + '/pavd_z'][:]))
                df_pai = pd.DataFrame(np.array(f_L2B[b + '/pai_z'][:]))
                df_cc = pd.DataFrame(np.array(f_L2B[b + '/cover_z'][:]))
                pavd_sel_ini = []
                pai_sel_ini = []
                cc_sel_ini = []
                df_pavd_sel_ini = pd.DataFrame(pavd_sel_ini)
                df_pai_sel_ini = pd.DataFrame(pai_sel_ini)
                df_cc_sel_ini = pd.DataFrame(cc_sel_ini)
                for j in range(4):
                    df_pavd_sel = df_pavd.iloc[:, j]
                    df_pavd_sel_ini = pd.concat([df_pavd_sel_ini, df_pavd_sel], axis=1)
                    df_pai_sel = df_pai.iloc[:, j]
                    df_pai_sel_ini = pd.concat([df_pai_sel_ini, df_pai_sel], axis=1)
                    df_cc_sel = df_cc.iloc[:, j]
                    df_cc_sel_ini = pd.concat([df_cc_sel_ini, df_cc_sel], axis=1)
                df_pavd_sel_ini.columns = ["GEDI_PAVD_1", "GEDI_PAVD_2", "GEDI_PAVD_3", "GEDI_PAVD_4"]
                df_pai_sel_ini.columns = ["GEDI_PAI_1", "GEDI_PAI_2", "GEDI_PAI_3", "GEDI_PAI_4"]
                df_cc_sel_ini.columns = ["GEDI_cc_1", "GEDI_cc_2", "GEDI_cc_3", "GEDI_cc_4"]
                #add canopy cover, PAI, PAVD, FHD
                df_cover = pd.DataFrame(np.array(f_L2B[b + '/cover'][:]))
                df_pai = pd.DataFrame(np.array(f_L2B[b + '/pai'][:]))
                df_fhd = pd.DataFrame(np.array(f_L2B[b + '/fhd_normal'][:]))
                df_pavd = pd.DataFrame(np.array(f_L2B[b + '/pavd_z'][:]))
                df_pavd_sel = df_pavd.iloc[:, 0]
                df_profile = pd.concat([df_cover, df_pai, df_fhd, df_pavd_sel], axis=1)
                df_profile = df_profile.loc[index_result,:]
                df_profile.columns = ["GEDI_Cover", "GEDI_PAI", "GEDI_FHD", "GEDI_PAVD"]
                #RH metrics
                df_rh = pd.DataFrame(np.array(f_L2A[b + '/rh'][:]),
                                     columns=["{:02d}".format(x) for x in range(101)])
                df_rh_a = df_rh.loc[index_result, ['10', '30', '50', '70', '90', '95', '98', '100']]
                head = "RH"
                df_rh_a.columns = [head + '_10', head + '_30',
                                   head + '_50', head + '_70', head + '_90',
                                   head + '_95', head + '_98', head + '_100']

                df_profile = pd.concat([df_cc_ls,df_cc_md,df_cc_nv,df_omega,df_pgap,df_pgap_z,df_fhd,
                                        df_leaf_on_doy,df_leaf_on_cycle,df_leaf_off_doy,df_leaf_off_flag,
                                        df_pavd_sel_ini, df_pai_sel_ini,df_cc_sel_ini,df_profile,df_rh_a], axis=1)
                df_profile = df_profile.loc[index_result,:]
                df_out = pd.concat([df_rh_initial, df_profile], axis=1)
                df_out["Beam"] = Beam
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

