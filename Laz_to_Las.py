
from whitebox_tools import WhiteboxTools

wbt = WhiteboxTools()
wbt.work_dir = "/path/to/data/" # Sets the Whitebox working directory

wbt.d_inf_flow_accumulation("DEM.dep", "output.dep", log=True)
