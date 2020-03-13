# -*- coding: utf-8 -*-
"""
/***************************************************************************
 QGIS2OpenDSS
                                 A QGIS plugin
 This plugin reads geographic information of electric distribution circuits and exports its configuration to OpenDss
                             -------------------
        begin                : 2015-11-22
        copyright            : (C) 2015 by EPERLab / Universidad de Costa Rica
        email                : eperlab.ucr.ac.cr@ucr.ac.cr
        git sha              : $Format:%H$
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
 This script initializes the plugin, making it known to QGIS.
"""


# noinspection PyPep8Naming
def classFactory(iface):  # pylint: disable=invalid-name
    """Load QGIS2OpenDSS class from file QGIS2OpenDSS.

    :param iface: A QGIS interface instance.
    :type iface: QgsInterface
    """
    #
    from .qgis2opendss import QGIS2OpenDSS
    return QGIS2OpenDSS(iface)
