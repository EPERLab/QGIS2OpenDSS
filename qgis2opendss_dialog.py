# -*- coding: utf-8 -*-
"""
/***************************************************************************
 QGIS2OpenDSSDialog
                                 A QGIS plugin
 This plugin reads geographic information of electric distribution circuits and exports its configuration to OpenDss
                             -------------------
        begin                : 2015-11-22
        git sha              : $Format:%H$
        copyright            : (C) 2015 by EperLab / Universidad de Costa Rica
        email                : eperlab.eie@ucr.ac.cr
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""

import os
dialog='qgis2opendss_dialog_base.ui'

from PyQt5 import QtCore, QtGui, QtWidgets
from qgis.PyQt import uic

FORM_CLASS, _ = uic.loadUiType(os.path.join(
    os.path.dirname(__file__), dialog))


class QGIS2OpenDSSDialog(QtWidgets.QDialog, FORM_CLASS):
    def __init__(self, parent=None):
        """Constructor."""
        super(QGIS2OpenDSSDialog, self).__init__(parent)
        # Set up the user interface from Designer.
        # After setupUI you can access any designer object by doing
        # self.<objectname>, and you can use autoconnect slots - see
        # http://qt-project.org/doc/qt-4.8/designer-using-a-ui-file.html
        # #widgets-and-dialogs-with-auto-connect
        self.setupUi(self)
