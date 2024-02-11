# PLOT A WAVEFORM WITH RH VALUES

# print "Usage: python py_waveplot.py <path_to_L1B_HDF5_file> <path_to_L2_txt_file> "

# Updated on 9 March 2018
# Added ability to autodetect waveform size

import sys
import h5py
import numpy as np
import matplotlib.pyplot as plt

mypath = sys.argv[1]
mytxtpath = sys.argv[2]

lvis = h5py.File(mypath, 'r')
# get wave index from shotnumber and test if exists
all_shotnums = np.array(lvis['SHOTNUMBER'])
wave_idx = ''
Zmax = ''
wfrange = ''
wfsize = ''


def checkIdxVal():
    global wave_idx
    global Zmax
    global wfrange
    global wfsize
    # Detecting waveform size
    for member in lvis:
        if member.startswith('Z'):
            wfsize = member.strip('Z')
    Zmax = ''.join(['Z', wfsize])
    wfrange = int(wfsize) + 1

    try:
        wave_idx = np.where(all_shotnums == myshotnum)[0][0]
        plotWaveform()
    except IndexError:
        print('Shot number entered does not exist in this dataset.')


def plotWaveform():
    global wave_idx
    global Zmax
    global wfrange
    global wfsize
    # extract the single waveform and elevation attributes Z0 and Z1023;
    waveform = lvis['RXWAVE'][wave_idx]
    Z0 = lvis['Z0'][wave_idx]
    Z1023 = lvis[Zmax][wave_idx]

    # find the elevation difference from Z0 to Z1023 and divide into 1023 equal intervals
    zstretch = np.add(Z1023, np.multiply(range(wfrange, 0, -1), ((Z0 - Z1023) / int(wfsize))))

    # search the L2 text file and extract the RH25, RH50.....RH100 values for the waveform you are querying
    with open(mytxtpath) as f:
        for line in f:
            try:
                myshot = line.split()[1]
                if int(myshot) == int(myshotnum):
                    ZG = float(line.split()[5])
                    ZT = float(line.split()[8])
                    RH25 = float(line.split()[12]) + ZG
                    RH50 = float(line.split()[17]) + ZG
                    RH75 = float(line.split()[22]) + ZG
                    RH10 = float(line.split()[9]) + ZG
                    break
            except ValueError:
                pass

    # set the z range limits for plotting the waveform to crop the noise
    zmin = RH10 - (ZT - RH10) / 6  # sets zmin at 15% below the waveform range defined in RH10-ZT
    zmax = ZT + (ZT - RH10) / 18  # sets zmax at 5% above the waveform range defined in RH10-ZT

    # crop the waveform and elevation arrays to the z range limits
    x = zstretch >= zmin  # this returns boolean True/False based on the comparison condition statement
    y = zstretch <= zmax
    z = (x == y)  # this creates a combined boolean result from the previous two condition statements
    waveform_crop = []
    zstretch_crop = []
    for i in range(0, len(waveform)):
        if z[i] == True:
            waveform_crop.append(waveform[i])
            zstretch_crop.append(zstretch[i])

            # plot the waveform as matplotlib figure
    fig = plt.figure(figsize=(15, 6))
    figplot = fig.add_subplot(121)
    figplot.plot(waveform, zstretch)
    plt.ylabel('elevation(m)')
    plt.xlabel('amplitude')

    figplot = fig.add_subplot(122)
    figplot.plot(waveform_crop, zstretch_crop)
    figplot.hlines(ZT, min(waveform), max(waveform), lw=1, linestyle='-', color='b', label='ZT')
    figplot.hlines(RH75, min(waveform), max(waveform), lw=1, linestyle='-', color='g', label='RH75')
    figplot.hlines(RH50, min(waveform), max(waveform), lw=1, linestyle='-', color='y', label='RH50')
    figplot.hlines(RH25, min(waveform), max(waveform), lw=1, linestyle='-', color='orange', label='RH25')
    figplot.hlines(ZG, min(waveform), max(waveform), lw=1, linestyle='-', color='r', label='ZG')
    plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3, ncol=5, mode="expand", borderaxespad=0.)
    plt.ylabel('elevation(m)')
    plt.xlabel('amplitude')
    plt.show(block=False)


while 1:
    available = ''.join(
        ['\nShot numbers available in this dataset: ', str(all_shotnums.min()), ' - ', str(all_shotnums.max())])
    print(available)

    try:
        myshotnum = int(input("Please enter shot number: "))
        checkIdxVal()
    except:
        print('Must be an integer.')
