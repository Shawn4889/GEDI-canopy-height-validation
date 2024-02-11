Author: Xiaoxuan Li
Description: like GEDI_Download.py, this code can also be used to download GEDI products, 
while the download speed is not as fast as the R code provided, either. Discussion use only.  
import requests
import subprocess
import os


filename = "E:/Shared_Ubuntu/GEDI01_B_2019162004722_O02798_T00266_02_003_01.h5"
url_file = 'https://e4ftl01.cr.usgs.gov/GEDI/GEDI01_B.001/2019.06.11/GEDI01_B_2019162004722_O02798_T00266_02_003_01.h5'
username = 
password = 
space = r'E:\Shared_Ubuntu'



class SessionWithHeaderRedirection(requests.Session):
    AUTH_HOST = 'e4ftl01.cr.usgs.gov'
    def __init__(self, username, password):
        super().__init__()
        self.auth = (username, password)
    # Overrides from the library to keep headers when redirected to or from the NASA auth host.
    def rebuild_auth(self, prepared_request, response):
        headers = prepared_request.headers
        url = prepared_request.url
        if 'Authorization' in headers:
            original_parsed = requests.utils.urlparse(response.request.url)
            redirect_parsed = requests.utils.urlparse(url)
            if (original_parsed.hostname != redirect_parsed.hostname) and \
\
                    redirect_parsed.hostname != self.AUTH_HOST and \
\
                    original_parsed.hostname != self.AUTH_HOST:
                del headers['Authorization']
        return
session = SessionWithHeaderRedirection(username, password)
# submit the request using the session
response = session.get(url_file, stream=True)
# raise an exception in case of http errors
response.raise_for_status()
# save the file
with open(filename, 'wb') as fd:
   for chunk in response.iter_content():
       fd.write(chunk)
print("H5 file has been downloaded!")

