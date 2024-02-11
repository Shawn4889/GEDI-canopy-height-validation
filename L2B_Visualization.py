# Example script: PLOT LVIS Level2 parameters vs Longitude for a selected sample in QGIS
# http://lvis.gsfc.nasa.gov

# canvas = qgis.utils.iface.mapCanvas()
# registry = QgsMapLayerRegistry.instance()
# layers = registry.mapLayers()
# vectorLyr =  QgsVectorLayer('c:/temp/my_subset.shp','Layer 1', 'ogr)
# vectorLyr.isValid()
# layer = iface.addVectorLayer("c:/temp/my_subset.shp", "layer 1", "ogr")


import matplotlib.pyplot
import pylab

layer = qgis.utils.iface.activeLayer()
selection = layer.selectedFeatures()
print(len(selection))
# initialize arrays
LON0 = []
ZG = []
RH25 = []
RH50 = []
RH75 = []
ZT = []
for feature in selection:
    attrs = feature.attributes()
    LON0.append(attrs[3])
    ZG.append(attrs[5])
    RH25.append(attrs[12] + attrs[5])
    RH50.append(attrs[17] + attrs[5])
    RH75.append(attrs[22] + attrs[5])
    ZT.append(attrs[8])

matplotlib.pyplot.scatter(LON0, ZG, color='r')
matplotlib.pyplot.scatter(LON0, RH25, color='orange')
matplotlib.pyplot.scatter(LON0, RH50, color='y')
matplotlib.pyplot.scatter(LON0, RH75, color='g')
matplotlib.pyplot.scatter(LON0, ZT, color='b')
matplotlib.pyplot.show()
