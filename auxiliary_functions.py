# uncompyle6 version 3.1.3
# Python bytecode 2.7 (62211)
# Decompiled from: Python 3.6.4 |Anaconda, Inc.| (default, Jan 16 2018, 10:22:32) [MSC v.1900 64 bit (AMD64)]
# Embedded file name: C:/Users/EPERLAB1706/.qgis2/python/plugins\QGIS2OpenDSS\auxiliary_functions.py
# Compiled at: 2017-06-27 11:07:18
from PyQt5.QtGui import * #Para desplegar mensajes, util para debugin
from PyQt5.QtCore import *
from qgis.gui import QgsMessageBar
from qgis.core import *

def getAttributeIndex(self, aLayer, attrName):
    if len(attrName) > 10 and aLayer.storageType() == 'ESRI Shapefile':
        self.iface.messageBar().pushCritical('Error', 'For ESRI Shapefiles, the maximum length of any attribute name is 10. Please choose a shorter attribute name.')
        return -3
    AttrIdx = aLayer.dataProvider().fieldNameIndex(attrName)
    if AttrIdx == -1:
        caps = aLayer.dataProvider().capabilities()
        if caps & QgsVectorDataProvider.AddAttributes:
            res = aLayer.dataProvider().addAttributes([QgsField(attrName, QVariant.String)])
            AttrIdx = aLayer.dataProvider().fieldNameIndex(attrName)
            aLayer.updateFields()
            if AttrIdx == -1:
                self.iface.messageBar().pushCritical('Error', 'Failed to create attribute!')
                return -1
        else:
            self.iface.messageBar().pushCritical('Error', 'Failed to add attribute!')
            return -1
    return AttrIdx


def newShape(shapeName, attributes, TYPE, path):
    strAttributes = ''
    print( "shapeName:", shapeName )
    for att in attributes:
        strAttributes = strAttributes + '&field=' + att + ':string(12)'

    try:
        layer = QgsProject.instance().mapLayersByName(shapeName)[0]
        allFeatIds = layer.allFeatureIds()
        layer.startEditing()
        layer.deleteFeatures(allFeatIds)
        layer.commitChanges()
    except IndexError:
        if TYPE == 'POINT':
            TYPE = 'Point?crs=epsg:5367'
        else:
            TYPE = 'LineString?crs=epsg:5367'
        layer = QgsVectorLayer(TYPE + strAttributes, shapeName, 'memory')
        layer.updateFields()
        pr = layer.dataProvider()
        fet = QgsFeature()
        layer.commitChanges()
        shp = str( path + '/' + shapeName + '.shp' )
        QgsVectorFileWriter.writeAsVectorFormat(layer, shp, "epsg:5367", QgsCoordinateReferenceSystem(), "ESRI Shapefile")
        layer = QgsVectorLayer(shp, shapeName, 'ogr')
        QgsProject.instance().addMapLayer(layer)

    return layer
