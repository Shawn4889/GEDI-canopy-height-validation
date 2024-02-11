"""
---------------------------------------------------------------------------------------------------
 How to Access the LP DAAC Data Pool with Python
 The following Python code example demonstrates how to configure a connection to download data from
 an Earthdata Login enabled server, specifically the LP DAAC's Data Pool.
---------------------------------------------------------------------------------------------------
 Author: Cole Krehbiel
 Last Updated: 11/20/2018
 Contributor: Xiaoxuan Li
 Description: this code is used for downloading GEDI products, while the download speed is not that fast. 
 Discussion use only, I would recommend using R code uploaded. 
---------------------------------------------------------------------------------------------------
"""
# Load necessary packages into Python
from subprocess import Popen
from getpass import getpass
from netrc import netrc
import argparse
import time
import os
import requests

# ----------------------------------USER-DEFINED VARIABLES--------------------------------------- #
# Set up command line arguments


saveDir = 'C:\\Users\\lxiao\\Desktop' # Set local directory to download to
files = 'C:\\Users\\lxiao\\Desktop\\GEDI.txt'       # Define file(s) to download from the LP DAAC Data Pool
username = 
password = 
prompts = [username,password]

# ---------------------------------SET UP WORKSPACE---------------------------------------------- #
# Create a list of files to download based on input type of files above

fileList = open(files, 'r').readlines()  # If input is textfile w file URLs
saveDir = saveDir.strip("'").strip('"') + os.sep
urs = 'urs.earthdata.nasa.gov'    # Address to call for authentication

# --------------------------------AUTHENTICATION CONFIGURATION----------------------------------- #
# Determine if netrc file exists, and if so, if it includes NASA Earthdata Login Credentials
try:
    netrcDir = os.path.expanduser("~/.netrc")
    netrc(netrcDir).authenticators(urs)[0]

# Below, create a netrc file and prompt user for NASA Earthdata Login Username and Password
except FileNotFoundError:
    homeDir = os.path.expanduser("~")
    Popen('touch {0}.netrc | chmod og-rw {0}.netrc | echo machine {1} >> {0}.netrc'.format(homeDir + os.sep, urs), shell=True)
    Popen('echo login {} >> {}.netrc'.format(getpass(prompt=prompts[0]), homeDir + os.sep), shell=True)
    Popen('echo password {} >> {}.netrc'.format(getpass(prompt=prompts[1]), homeDir + os.sep), shell=True)

# Determine OS and edit netrc file if it exists but is not set up for NASA Earthdata Login
except TypeError:
    homeDir = os.path.expanduser("~")
    Popen('echo machine {1} >> {0}.netrc'.format(homeDir + os.sep, urs), shell=True)
    Popen('echo login {} >> {}.netrc'.format(getpass(prompt=prompts[0]), homeDir + os.sep), shell=True)
    Popen('echo password {} >> {}.netrc'.format(getpass(prompt=prompts[1]), homeDir + os.sep), shell=True)

# Delay for up to 1 minute to allow user to submit username and password before continuing
tries = 0
while tries < 30:
    try:
        netrc(netrcDir).authenticators(urs)[2]
    except:
        time.sleep(2.0)
    tries += 1

# -----------------------------------------DOWNLOAD FILE(S)-------------------------------------- #
# Loop through and download all files to the directory specified above, and keeping same filenames
for f in fileList:
    if not os.path.exists(saveDir):
        os.makedirs(saveDir)
    saveName = os.path.join(saveDir, f.split('/')[-1].strip())

    # Create and submit request and download file
    with requests.get(f.strip(), stream=True, auth=(netrc(netrcDir).authenticators(urs)[0], netrc(netrcDir).authenticators(urs)[2])) as response:
        if response.status_code != 200:
            print("{} not downloaded. Verify that your username and password are correct in {}".format(f.split('/')[-1].strip(), netrcDir))
        else:
            response.raw.decode_content = True
            content = response.raw
            with open(saveName, 'wb') as d:
                while True:
                    chunk = content.read(16 * 1024)
                    if not chunk:
                        break
                    d.write(chunk)
            print('Downloaded file: {}'.format(saveName))
