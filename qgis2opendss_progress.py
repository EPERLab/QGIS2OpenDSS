# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'qgis2opendss_progress.ui'
#
# Created: Mon Mar 21 17:23:48 2016
#      by: PyQt4 UI code generator 4.10.2
#
# WARNING! All changes made in this file will be lost!

import os

from qgis.PyQt import QtGui, uic
from PyQt5.QtWidgets import QApplication, QDialog, QWidget, QPushButton, QMessageBox
import sys

FORM_CLASS, _ = uic.loadUiType(os.path.join(
os.path.dirname(__file__), 'qgis2opendss_progress.ui'))

class Ui_Progress(QDialog, FORM_CLASS):

   def __init__(self, parent=None):
      """Constructor."""
      super(Ui_Progress, self).__init__(parent)
      self.setupUi(self)
