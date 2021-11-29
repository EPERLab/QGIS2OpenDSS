    # -*- coding: utf-8 -*-
"""
/***************************************************************************
 QGIS2OpenDSS
                                 A QGIS plugin
 This plugin reads geographic information of electric distribution circuits and exports command lines for OpenDSS
                              -------------------
        begin                : 2015-11-22
        git sha              : $Format:%H$
        copyright            : (C)2015 by EPERLAB / Universidad de Costa Rica
        email                : gvalverde@eie.ucr.ac.cr, abdenago.guzman@ucr.ac.cr, aarguello@eie.ucr.ac.cr
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option)any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
from __future__ import print_function
from __future__ import absolute_import
from builtins import str
from builtins import range
from builtins import object
import glob
import os
import shutil
import time
import timeit
import inspect
from math import sqrt
import math
import subprocess

import networkx as nx  # Para trabajar con teoria de grafos
import numpy as np
import os.path

from PyQt5.QtCore import *
from PyQt5 import QtCore 

from PyQt5.QtGui import QDesktopServices
from PyQt5 import QtGui #Paquetes requeridos para crear ventanas de diálogo e interfaz gráfica.
from PyQt5 import QtWidgets
from PyQt5.QtWidgets import QApplication, QWidget, QInputDialog, QLineEdit, QFileDialog, QMessageBox
import traceback

from qgis.core import *  # Paquetes requeridos para crer el registro de eventos.
from qgis.gui import *  # Paquete requerido para desplegar mensajes en la ventana principal de QGIS.

from tempfile import mkstemp
from shutil import move, copymode
from os import fdopen, remove
from qgis.core import NULL
from . import auxiliary_functions
from .LlamarOpenDSS import LlamarOpenDSS
# from dateTime import *
from . import lineOperations as lineOperations
from . import phaseOperations  # Realiza diferentes tareas con las fases de los elementos electricos.
#from . import trafoOperations  # Operaciones con transformadores
from . import trafoOperations as trafoOperations  # Operaciones con transformadores para consultoría con Perú
from . import resources
from . EVsFunctions import CreacionPerfilesEV, AnalizarEncuestas #funciones necesarias para VEs
from . optimizacion_buses import excel_to_dict #funciones necesarias para buses
# Initialize Qt resources from file resources.py
# Import the code for the dialog
from .qgis2opendss_dialog import QGIS2OpenDSSDialog
from .qgis2opendss_progress import Ui_Progress

import sys
from decimal import Decimal
import pandas as pd

class QGIS2OpenDSS(object):
    """QGIS Plugin Implementation."""

    def __init__(self, iface):
        """Constructor.

        :param iface: An interface instance that will be passed to this class
            which provides the hook by which you can manipulate the QGIS
            application at run Time.
        :type iface: QgsInterface
        """
        # Save reference to the QGIS interface
        self.iface = iface
        # initialize plugin directory
        self.plugin_dir = os.path.dirname(__file__)
        # initialize locale
        locale = QSettings().value('locale/userLocale')[0:2]

        if locale != (u'es'):  # es
            locale = (u'en') # en

        locale_path = os.path.join(self.plugin_dir, 'i18n', 'QGIS2OpenDSS_{}.qm'.format(locale)) # translation file

        if os.path.exists(locale_path):
            self.translator = QTranslator()
            self.translator.load(locale_path)

            if qVersion()> '4.3.3':
                QCoreApplication.installTranslator(self.translator)

        # Variable con los nombres de las capas
        
        # Create the dialog (after translation) and keep reference
        self.dlg = QGIS2OpenDSSDialog()
        self.progress = Ui_Progress()

        # Declare instance attributes
        self.actions = []
        self.menu = self.tr(u'&QGIS2OpenDSS')
        # TODO: We are going to let the user set this up in a future iteration
        self.toolbar = self.iface.addToolBar(u'QGIS2OpenDSS')
        self.toolbar.setObjectName(u'QGIS2OpenDSS')

        # Llama al metodo para seleccionar la carpeta de destino
        # self.dlg.lineEdit_nameCircuit.clear()
        # self.dlg.lineEdit_dirOutput.clear()
        self.dlg.pushButton.clicked.connect(self.select_output_folder)

        # Llama al método para seleccionar el archivo de perfiles de carga
        # self.dlg.lineEdit_AC.clear()
        self.dlg.pushButton_AC.clicked.connect(self.select_load_profile)
        self.dlg.button_box.helpRequested.connect(self.show_help)
        #self.dlg.button_box.accepted.connect(self.dlg.close)
        self.dlg.button_box.rejected.connect(self.dlg.close)
        
        self.mensaje_log_gral = ""
        self.msg_trafos = ""
        self.nombres_capas = ""
        #Planteles de buses
        """
        self.dlg.pushButton_bus.clicked.connect(self.select_csv_buses)
        self.dlg.pushButton_carg.clicked.connect(self.select_csv_cargadores)
        self.dlg.comboBox_plantelbuses.activated.connect(self.activate_csvs_buses)
        
        self.dlg.lineEdit_csvbus.setEnabled(False)
        self.dlg.lineEdit_dircsvcarg.setEnabled(False)
        self.dlg.pushButton_bus.setEnabled(False)
        self.dlg.pushButton_carg.setEnabled(False)
        """
    def configurate_combobox(self, layer_list):
        #Limpia los combobox
        self.dlg.comboBox_LMT1.clear()
        self.dlg.comboBox_LMT2.clear()
        self.dlg.comboBox_LMT3.clear()
        self.dlg.comboBox_LBT1.clear()
        self.dlg.comboBox_LBT2.clear()
        self.dlg.comboBox_LBT3.clear()
        self.dlg.comboBox_TR1.clear()
        self.dlg.comboBox_TR2.clear()
        self.dlg.comboBox_TR3.clear()
        self.dlg.comboBox_ACO1.clear()
        self.dlg.comboBox_ACO2.clear()
        self.dlg.comboBox_ACO3.clear()
        self.dlg.comboBox_CA1.clear()
        self.dlg.comboBox_CA2.clear()
        self.dlg.comboBox_CA3.clear()
        self.dlg.comboBox_CI1.clear()
        self.dlg.comboBox_CI2.clear()
        self.dlg.comboBox_CI3.clear()
        self.dlg.comboBox_SE.clear()
        self.dlg.comboBox_CAMT1.clear()
        self.dlg.comboBox_CAMT2.clear()
        self.dlg.comboBox_CAMT3.clear()
        self.dlg.comboBox_GD_lv.clear()
        self.dlg.comboBox_GD_ls.clear()
        self.dlg.comboBox_EV.clear()
                
        self.dlg.comboBox_LMT1.addItems(layer_list)
        self.dlg.comboBox_LMT2.addItems(layer_list)
        self.dlg.comboBox_LMT3.addItems(layer_list)
        self.dlg.comboBox_LBT1.addItems(layer_list)
        self.dlg.comboBox_LBT2.addItems(layer_list)
        self.dlg.comboBox_LBT3.addItems(layer_list)
        self.dlg.comboBox_TR1.addItems(layer_list)
        self.dlg.comboBox_TR2.addItems(layer_list)
        self.dlg.comboBox_TR3.addItems(layer_list)
        self.dlg.comboBox_ACO1.addItems(layer_list)
        self.dlg.comboBox_ACO2.addItems(layer_list)
        self.dlg.comboBox_ACO3.addItems(layer_list)
        self.dlg.comboBox_CA1.addItems(layer_list)
        self.dlg.comboBox_CA2.addItems(layer_list)
        self.dlg.comboBox_CA3.addItems(layer_list)
        self.dlg.comboBox_CI1.addItems(layer_list)
        self.dlg.comboBox_CI2.addItems(layer_list)
        self.dlg.comboBox_CI3.addItems(layer_list)
        self.dlg.comboBox_SE.addItems(layer_list)
        self.dlg.comboBox_CAMT1.addItems(layer_list)
        self.dlg.comboBox_CAMT2.addItems(layer_list)
        self.dlg.comboBox_CAMT3.addItems(layer_list)
        self.dlg.comboBox_GD_lv.addItems(layer_list)
        self.dlg.comboBox_GD_ls.addItems(layer_list)
        self.dlg.comboBox_EV.addItems(layer_list)
        
        # Plantel de buses
        self.dlg.comboBox_plantelbuses.clear()
        self.dlg.comboBox_plantelbuses.addItems(layer_list)
        
        # Protecciones y seccionadores
        self.dlg.comboBox_secc.clear()
        self.dlg.comboBox_secc.addItems(layer_list)
        self.dlg.comboBox_recon.clear()
        self.dlg.comboBox_recon.addItems(layer_list)
        self.dlg.comboBox_fusib.clear()
        self.dlg.comboBox_fusib.addItems(layer_list)
        
        # Reguladores
        self.dlg.comboBox_reg.clear()
        self.dlg.comboBox_reg.addItems(layer_list)
        
        # Capacitores
        self.dlg.comboBox_cap.clear()
        self.dlg.comboBox_cap.addItems(layer_list)

    
    """
    Función que habilita las líneas de csvs de buses cuando se pone el nombre de una capa en el
    espacio del nombre de la capa de planteles de buses
    """
    def activate_csvs_buses(self):
        #Se define el nombre de la capa de buses como una variable de clase
        self.name_layer_buses = self.dlg.comboBox_plantelbuses.currentText()
        
        #Caso en que no haya ninguna capa seleccionada
        if self.name_layer_buses == "":
            self.dlg.lineEdit_csvbus.setEnabled(False)
            self.dlg.lineEdit_dircsvcarg.setEnabled(False)
            self.dlg.pushButton_bus.setEnabled(False)
            self.dlg.pushButton_carg.setEnabled(False)
            
            self.dlg.lineEdit_csvbus.clear()
            self.dlg.lineEdit_dircsvcarg.clear()
        
        #Caso en que haya alguna capa seleccionada
        else:
            self.dlg.lineEdit_csvbus.setEnabled(True)
            self.dlg.lineEdit_dircsvcarg.setEnabled(True)
            self.dlg.pushButton_bus.setEnabled(True)
            self.dlg.pushButton_carg.setEnabled(True)
        
            
        
    # noinspection PyMethodMayBeStatic
    def tr(self, message):
        # noinspection PyTypeChecker,PyArgumentList,PyCallByClass
        return QCoreApplication.translate('dialog', 'QGIS2OpenDSS', message)

    def add_action(
            self,
            icon_path,
            text,
            callback,
            enabled_flag=True,
            add_to_menu=True,
            add_to_toolbar=True,
            status_tip=None,
            whats_this=None,
            parent=None):
        """Add a toolbar icon to the toolbar.
        :param icon_path: Path to the icon for this action. Can be a resource
            path (e.g. ':/plugins/foo/bar.png')or a normal file system path.
        :type icon_path: str
        :param text: Text that should be shown in menu items for this action.
        :type text: str
        :param callback: Function to be called when the action is triggered.
        :type callback: function
        :param enabled_flag: A flag indicating if the action should be enabled
            by default. Defaults to True.
        :type enabled_flag: bool
        :param add_to_menu: Flag indicating whether the action should also
            be added to the menu. Defaults to True.
        :type add_to_menu: bool
        :param add_to_toolbar: Flag indicating whether the action should also
            be added to the toolbar. Defaults to True.
        :type add_to_toolbar: bool
        :param status_tip: Optional text to show in a popup when mouse pointer
            hovers over the action.
        :type status_tip: str
        :param parent: Parent widget for the new action. Defaults None.
        :type parent: QWidget
        :param whats_this: Optional text to show in the status bar when the
            mouse pointer hovers over the action.
        :returns: The action that was created. Note that the action is also
            added to self.actions list.
        :rtype: QAction
        """

        icon = QtGui.QIcon(icon_path)
        action = QtWidgets.QAction(icon, text, parent)
        action.triggered.connect(callback)
        action.setEnabled(enabled_flag)

        if status_tip is not None:
            action.setStatusTip(status_tip)

        if whats_this is not None:
            action.setWhatsThis(whats_this)

        if add_to_toolbar:
            self.toolbar.addAction(action)

        if add_to_menu:
            self.iface.addPluginToMenu(
                self.menu,
                action)

        self.actions.append(action)

        return action

    def initGui(self):
        """Create the menu entries and toolbar icons inside the QGIS GUI."""
        icon_path = ':/plugins/QGIS2OpenDSS/icon.png'
        self.add_action(
            icon_path,
            text=self.tr(u'Export circuit to OpenDSS'),
            callback=self.run,
            parent=self.iface.mainWindow())

    def unload(self):
        """Removes the plugin menu item and icon from QGIS GUI."""
        for action in self.actions:
            self.iface.removePluginMenu(
                self.tr(u'&QGIS2OpenDSS'),
                action)
            self.iface.removeToolBarIcon(action)
        # remove the toolbar
        del self.toolbar
        
    def select_output_folder(self):
        """Método para seleccionar la carpeta de destino"""
        foldername = QFileDialog.getExistingDirectory(self.dlg, "Seleccione carpeta de destino","",)
        self.dlg.lineEdit_dirOutput.setText(foldername)
    
    #Función para cargar el csv de buses
    def select_csv_buses(self):
        csv_buses, _ = QFileDialog.getOpenFileName(self.dlg, "Seleccione el csv con la información de los parámetros de buses eléctricos", "", "*.csv")
        self.csv_buses = csv_buses #variable de la clase
        self.dlg.lineEdit_csvbus.setText(csv_buses)
    
    #Función para cargar el csv de los cargadores
    def select_csv_cargadores(self):
        csv_cargadores, _ = QFileDialog.getOpenFileName(self.dlg, "Seleccione el csv con la información de los cargadores de los buses eléctricos", "", "*.csv")
        self.csv_cargadores = csv_cargadores #variable de la clase
        self.dlg.lineEdit_dircsvcarg.setText(csv_cargadores)

    def select_load_profile(self):
        """Método para seleccionar el archivo de asignación de perfil de consumo"""
        # filename=QFileDialog.getOpenFileName(self.dlg,"Seleccione el archivo.txt para designar curva de carga conforme al consumo mensual promedio","",)
        foldername = QFileDialog.getExistingDirectory(self.dlg, "Seleccione la carpeta con las curvas de carga", "",)
        self.dlg.lineEdit_AC.setText(foldername)

    def show_help(self):
        """Display application help to the user."""

        help_file = 'file:///%s/help/Manual_QGIS2OPENDSS.pdf' % self.plugin_dir
        # For testing path:
        # QMessageBox.information(None, 'Help File', help_file)
        # noinspection PyCallByClass,PyTypeChecker
        QDesktopServices.openUrl(QUrl(help_file))

    def mkdir_p(self, mypath):
        '''Creates a directory. equivalent to using mkdir -p on the command line'''
        from errno import EEXIST
        from os import makedirs, path
        try:
            makedirs(mypath)
        except OSError as exc:  # Python >2.5
            if exc.errno == EEXIST and path.isdir(mypath):
                pass
            else:
                raise

    def CoordLineProcess(self, ObjetoLinea, tolerancia):  # Procesa las coodernadas de las líneas para agregar al grafo.
        #line = ObjetoLinea.geometry().asPolyline() # Lee la geometria de la linea
        
        geom = ObjetoLinea.geometry()
        line = self.MultiStringToMatrix(geom)
        
        n = len(line) # Cantidad de vértices de la línea
        X1 = int(float(line[0][0] / tolerancia))
        Y1 = int(float(line[0][1] / tolerancia))
        X2 = int(float(line[n - 1][0] / tolerancia))
        Y2 = int(float(line[n - 1][1] / tolerancia))
        P1 = (X1, Y1)
        P2 = (X2, Y2)
        return P1, P2

    def CoordPointProcees(self, ObjetoLinea, tolerancia):
        point = ObjetoLinea.geometry().asPoint() # Lee la geometria de la linea
        X1 = int(float(point[0] / tolerancia))
        Y1 = int(float(point[1] / tolerancia))
        P1 = (X1, Y1)
        return P1

    def ReaderDataLMT(self, layer, Grafo, datosLMT, toler, subterranea,
                      indexDSS):  # Lee los datos de capa de línea y las agrega al grafo
        try:
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            idx_bus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            idx_length = auxiliary_functions.getAttributeIndex(self, layer, "length")
            
            lineasMT = layer.getFeatures() # Recibe las caracteristicas de la capa de lineas de media tension.
            layer.startEditing() # Activa modo edición
            
            for lineaMT in lineasMT:
                geom = lineaMT.geometry()
                line = self.MultiStringToMatrix(geom)
                if line == 0:
                    return 0, 0
                n = len(line) # Cantidad de vértices de la línea
                fase = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseCodeODSS')
                faseOrig = lineaMT['PHASEDESIG']
                cantFases = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseNumber')
                nom_volt = lineaMT['NOMVOLT']
                datos_tension = lineOperations.renameVoltage(nom_volt)
                opervoltLN = datos_tension['LVCode']['LN']
                opervoltLL = datos_tension['LVCode']['LL']
                nodo1, nodo2 = self.CoordLineProcess(lineaMT, toler)
                LineLength = lineaMT.geometry().length()
                id_ = lineaMT.id()
                layer.changeAttributeValue(id_, idx_length, LineLength)
                layer.changeAttributeValue(id_, idx_bus1, NULL)
                layer.changeAttributeValue(id_, idx_bus2, NULL)
                layer.changeAttributeValue(id_, indexDSS, NULL)
                try:
                    group = lineaMT['MV_GROUP']
                except Exception:
                    group = 'N/A'
                
                if subterranea:  # Determina si la línea es aérea o subterránea
                    air_ugnd = 'ugnd'
                    datosLinea = {"PHASEDESIG": faseOrig, "INDEXDSS": indexDSS, 'ID': lineaMT.id(), "LAYER": layer,
                                  "nodo1": nodo1, "nodo2": nodo2, "X1": line[0][0], "Y1": line[0][1], "X2": line[n - 1][0],
                                  "Y2": line[n - 1][1], 'NEUMAT': lineaMT['NEUTMAT'], 'NEUSIZ': lineaMT['NEUTSIZ'],
                                  'PHAMAT': lineaMT['PHASEMAT'], 'PHASIZ': lineaMT['PHASESIZ'],
                                  'NOMVOLT': lineaMT['INSULVOLT'], 'PHASE': fase, 'SHLEN': LineLength, 'AIR_UGND': air_ugnd,
                                  'INSUL': lineaMT['INSULMAT'], 'NPHAS': cantFases, 'VOLTOPRLL': opervoltLL,
                                  'VOLTOPRLN': opervoltLN, "SHIELD": lineaMT["SHIELDING"],
                                  'MV_GROUP': group, 'idx_bus1': idx_bus1, 'idx_bus2': idx_bus2}
                else:
                    air_ugnd = 'air'
                    datosLinea = {"PHASEDESIG": faseOrig, "INDEXDSS": indexDSS, 'ID': lineaMT.id(), "LAYER": layer,
                                  "nodo1": nodo1, "nodo2": nodo2, "X1": line[0][0], "Y1": line[0][1], "X2": line[n - 1][0],
                                  "Y2": line[n - 1][1], 'NEUMAT': lineaMT['NEUTMAT'], 'NEUSIZ': lineaMT['NEUTSIZ'],
                                  'PHAMAT': lineaMT['PHASEMAT'], 'PHASIZ': lineaMT['PHASESIZ'], 'CCONF': lineaMT['LINEGEO'],
                                  'PHASE': fase, 'SHLEN': LineLength, 'AIR_UGND': air_ugnd, 'NPHAS': cantFases,
                                  'VOLTOPRLL': opervoltLL, 'VOLTOPRLN': opervoltLN,
                                  'MV_GROUP': group, 'idx_bus1': idx_bus1, 'idx_bus2': idx_bus2}
    
                if Grafo.get_edge_data(nodo1, nodo2)== None:  # se asegura que la línea no existe
                    Grafo.add_edge(nodo1, nodo2)
                    Grafo[nodo1][nodo2].update(datosLinea) # Agrega la línea al grafo con todos los datos
                else:  # Si la línea existe es porque están en paralelo
                    newLength = float(datosLinea["SHLEN"])/ 2
                    datosLinea["SHLEN"] = newLength
                    paralelNode = "Paralel" + str(nodo1)
                    datosLinea["nodo2"] = paralelNode
                    Grafo.add_edge(nodo1, paralelNode)
                    Grafo[nodo1][paralelNode].update(datosLinea) # Agrega la línea al grafo con todos los datos
    
                    datosLinea["nodo2"] = nodo2
                    datosLinea["nodo1"] = paralelNode
                    Grafo.add_edge(paralelNode, nodo2)
                    Grafo[paralelNode][nodo2].update(datosLinea) # Agrega la línea al grafo con todos los datos
    
            return Grafo, datosLMT
        except KeyError as e:
            cause = e.args[0]
            aviso = "Favor verifique que las capas de líneas MT tengan el atributo "
            aviso += cause + "\nPara más detalles:\n"
            aviso += self.print_error()
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura líneas MT", aviso)
            return 0, 0
        except Exception:
            aviso = "Hubo un error en la lectura de líneas de MT"
            aviso += "\nPara más detalles: \n"
            aviso += self.print_error()
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura líneas MT", aviso)
            return 0, 0
        finally:
            layer.commitChanges()

    def ReaderDataTrafos(self, layer, toler, datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                         Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal):
        try:
            
            list_sec_ne = []
            list_prim_ne = []
            
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexLvGroup = auxiliary_functions.getAttributeIndex(self, layer, 'LV_GROUP')
            
            trafos1 = layer.getFeatures() # Recibe las caracteristicas de la capa de transformadores.
            layer.startEditing() # Activa modo edición
                
            for trafo1 in trafos1:  # Separa los transformadores en tres listas: Monofasicos, Bifasicos y Trifasicos
                
                nodo = self.CoordPointProcees(trafo1, toler)
                point = trafo1.geometry().asPoint() # Lee la geometria de la linea
                fase = phaseOperations.renamePhase(trafo1['PHASEDESIG']).get('phaseCodeODSS') # define código de OpenDSS
                numfase = phaseOperations.renamePhase(trafo1['PHASEDESIG']).get(
                    'phaseNumberTraf') # define código de OpenDSS
                MVCode = trafo1['PRIMVOLT']
                LVCode = trafo1['SECVOLT']
                tap = str(format(float(trafo1['TAPSETTING']), '.4f'))
                voltages = trafoOperations.renameVoltage(int(MVCode), int(LVCode))
                if voltages["LVCode"]["LL"] == 0:
                    loadvolt = "UNKNOWN"
                    loadvoltLN = "UNKNOWN"
                    list_sec_ne.append(LVCode)
                else:
                    loadvolt = str(voltages["LVCode"]["LL"])
                    loadvoltLN = str(voltages["LVCode"]["LN"])
                if voltages["MVCode"]["LL"] == 0:
                    voltoprLL = "UNKNOWN"
                    voltoprLN = "UNKNOWN"
                    list_prim_ne.append(MVCode)
                else:
                    voltoprLL = str(voltages["MVCode"]["LL"])
                    voltoprLN = str(voltages["MVCode"]["LN"])
                try:
                    group_lv = trafo1['LV_GROUP']
                except KeyError:
                    group_lv = 'N/A'
                try:
                    group_mv = trafo1['MV_GROUP']
                except KeyError:
                    group_mv = 'N/A'
                    
                #Caso en que el trafo sea de media a media tensión
                try:
                    mv_mv = trafo1['MV/MV']
                    mv_mv = str(mv_mv).lower()
                    
                    if (mv_mv == 'yes' or mv_mv == 'si' or mv_mv == 'sí'
                        or mv_mv == '1' or mv_mv == 's' or mv_mv == 'y'
                        or mv_mv == 't' or mv_mv == 'true' ):
                            mv_mv = True
                    else:
                        mv_mv = False
                except Exception:
                    mv_mv = False
                if fase == '.1.2.3':  # Divide los transformadores trifasicos en transformadores simples y de multiples unidades monofasicas
                    # Revisa si es un banco de tres transformadores con placa diferente o una sola unidad
                    if (trafo1['SECCONN'] == '4D'):
                        datosMulti = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "INDEXDSS": indexDSS,
                                      'ID': trafo1.id(), "TAPS": tap, "LAYER": layer, "nodo": nodo, 'X1': point[0],
                                      'Y1': point[1], 'PHASE': fase, 'KVA_FA': trafo1['KVAPHASEA'],
                                      'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                                      'KVM': trafo1['PRIMVOLT'], 'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'],
                                      'CONBA': trafo1['SECCONN'], 'LOADCONNS': '.1.2.3', 'LOADCONF': 'delta',
                                      'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv,
                                      'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN,
                                      'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                                      'INDEX_LV_GROUP': indexLvGroup}
                        datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
    
                        datosT3F_Multi.append(datosMulti)
                        if (nodo in Graph_T3F_multi.nodes())and (
                                Graph_T3F_multi.nodes[nodo]['PHASE'] == datosMulti['PHASE']):
                            Graph_T3F_multi.nodes[nodo]['KVA_FA'] = float(datosMulti['KVA_FA'])+ float(
                                Graph_T3F_multi.nodes[nodo]['KVA_FA'])
                            Graph_T3F_multi.nodes[nodo]['KVA_FB'] = float(datosMulti['KVA_FB'])+ float(
                                Graph_T3F_multi.nodes[nodo]['KVA_FB'])
                            Graph_T3F_multi.nodes[nodo]['KVA_FC'] = float(datosMulti['KVA_FC'])+ float(
                                Graph_T3F_multi.nodes[nodo]['KVA_FC'])
                            aviso = 'Se aumentó la capacidad de un transformador trifásico de 3 unidades '
                            aviso += 'debido a su cercanía con otro banco de transformadores en: ('
                            aviso += str(Graph_T3F_multi.nodes[nodo]['X1']) + ', '
                            aviso += str(Graph_T3F_multi.nodes[nodo]['Y1']) + ')\n'
                            self.mensaje_log_gral += aviso
                            
                        else:
                            Graph_T3F_multi.add_node(nodo)
                            Graph_T3F_multi.nodes[nodo].update(datosMulti) # Agrega el trafo al grafo con todos los datos
                            
                            grafoBTTotal.add_node(nodo)
                            grafoBTTotal.nodes[nodo].update(datosTotalGraph)
                            
                    if (trafo1['SECCONN'] == 'Y') and (trafo1['PRIMCONN'] == 'Y' or trafo1['PRIMCONN'] == 'D'):
    
                        if float(trafo1['KVAPHASEA'])== float(trafo1['KVAPHASEB'])== float(trafo1['KVAPHASEC']):
                            datosSingleY = {'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'],
                                            'KVA_FC': trafo1['KVAPHASEC'], "NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode,
                                            "INDEXDSS": indexDSS, 'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, "TAPS": tap,
                                            'X1': point[0], 'Y1': point[1], 'PHASE': fase,
                                            'KVA': int(float(trafo1['RATEDKVA'])), 'KVM': trafo1['PRIMVOLT'],
                                            'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'], 'CONBA': trafo1['SECCONN'],
                                            'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye', 'LOADVOLT': loadvolt,
                                            'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                                            "VOLTMTLN": voltoprLN, 'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                                            'INDEX_LV_GROUP': indexLvGroup}
                            datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt,
                                               'LOADVOLTLN': loadvoltLN}
                            datosT3F_Single.append(datosSingleY)
    
                            if (nodo in Graph_T3F_single.nodes()) and (
                                    Graph_T3F_single.nodes[nodo]['PHASE'] == datosSingleY['PHASE']):
                                Graph_T3F_single.nodes[nodo]['KVA'] = float(datosSingleY['KVA'])+ float(
                                    Graph_T3F_single.nodes[nodo]['KVA'])
                                aviso = 'Se aumentó la capacidad de un transformador trifásico debido a '
                                aviso += 'su cercanía con otro transformador en: ('
                                aviso += str(Graph_T3F_single.nodes[nodo]['X1']) + ', '
                                aviso += str(Graph_T3F_single.nodes[nodo]['Y1']) + ')\n'
                                self.mensaje_log_gral += aviso
                            else:
                                Graph_T3F_single.add_node(nodo)
                                Graph_T3F_single.nodes[nodo].update(datosSingleY) # Agrega el trafo al grafo con todos los datos
                                
                                grafoBTTotal.add_node(nodo)
                                grafoBTTotal.nodes[nodo].update(datosTotalGraph)
    
                        else:
                            datosMulti = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "INDEXDSS": indexDSS,
                                      'ID': trafo1.id(), "TAPS": tap, "LAYER": layer, "nodo": nodo, 'X1': point[0],
                                      'Y1': point[1], 'PHASE': fase, 'KVA_FA': trafo1['KVAPHASEA'],
                                      'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                                      'KVM': trafo1['PRIMVOLT'], 'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'],
                                      'CONBA': trafo1['SECCONN'], 'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye',
                                      'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv,
                                      'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN,
                                      'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                                      'INDEX_LV_GROUP': indexLvGroup}
    
                            datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt,
                                               'LOADVOLTLN': loadvoltLN}
                            datosT3F_Multi.append(datosMulti)
    
                            if (nodo in Graph_T3F_multi.nodes()) and (
                                    Graph_T3F_multi.nodes[nodo]['PHASE'] == datosMulti['PHASE']):
                                Graph_T3F_multi.nodes[nodo]['KVA_FA'] = float(datosMulti['KVA_FA'])+ float(
                                    Graph_T3F_multi.nodes[nodo]['KVA_FA'])
                                Graph_T3F_multi.nodes[nodo]['KVA_FB'] = float(datosMulti['KVA_FB'])+ float(
                                    Graph_T3F_multi.nodes[nodo]['KVA_FB'])
                                Graph_T3F_multi.nodes[nodo]['KVA_FC'] = float(datosMulti['KVA_FC'])+ float(
                                    Graph_T3F_multi.nodes[nodo]['KVA_FC'])
                                aviso = 'Se aumentó la capacidad de un transformador trifásico de 3 unidades '
                                aviso += 'debido a su cercanía con otro banco de transformadores en: ('
                                aviso += str(Graph_T3F_multi.nodes[nodo]['X1']) + ', '
                                aviso += str(Graph_T3F_multi.nodes[nodo]['Y1']) + ')\n'
                                self.mensaje_log_gral += aviso
                            else:
                                Graph_T3F_multi.add_node(nodo)
                                Graph_T3F_multi.nodes[nodo].update(datosMulti) # Agrega el trafo al grafo con todos los datos
                                
                                grafoBTTotal.add_node(nodo)
                                grafoBTTotal.nodes[nodo].update(datosTotalGraph)
    
                    if (trafo1['SECCONN'] == 'D') and (trafo1['PRIMCONN'] == 'Y' or trafo1['PRIMCONN'] == 'D'):
                        datosSingleD = {'KVA_FA': int(float(trafo1['KVAPHASEA'])),
                                        'KVA_FB': int(float(trafo1['KVAPHASEB'])),
                                        'KVA_FC': int(float(trafo1['KVAPHASEC'])), "NPHAS": numfase, "MVCODE": MVCode,
                                        "LVCODE": LVCode, "TAPS": tap, "INDEXDSS": indexDSS, 'ID': trafo1.id(),
                                        "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1], 'PHASE': fase,
                                        'KVA': int(float(trafo1['RATEDKVA'])), 'KVM': trafo1['PRIMVOLT'],
                                        'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'], 'CONBA': trafo1['SECCONN'],
                                        'LOADCONNS': '.1.2.3', 'LOADCONF': 'delta', 'LOADVOLT': loadvolt,
                                        'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                                        "VOLTMTLN": voltoprLN, 'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                                        'INDEX_LV_GROUP': indexLvGroup}
                        datosT3F_Single.append(datosSingleD)
                        datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                        if (nodo in Graph_T3F_single.nodes()) and (
                                Graph_T3F_single.nodes[nodo]['PHASE'] == datosSingleD['PHASE']):
                            Graph_T3F_single.nodes[nodo]['KVA'] = float(datosSingleD['KVA'])+ float(
                                Graph_T3F_single.nodes[nodo]['KVA'])
                            aviso = 'Se aumentó la capacidad de un transformador trifásico debido ' 
                            aviso += 'a su cercanía con otro transformador en: ('
                            aviso += str(Graph_T3F_single.nodes[nodo]['X1']) + ', '
                            aviso += str(Graph_T3F_single.nodes[nodo]['Y1']) + ')\n'
                            self.mensaje_log_gral += aviso
                        else:
                            Graph_T3F_single.add_node(nodo)
                            Graph_T3F_single.nodes[nodo].update(datosSingleD) # Agrega el trafo al grafo con todos los datos
                            
                            grafoBTTotal.add_node(nodo)
                            grafoBTTotal.nodes[nodo].update(datosTotalGraph)
                elif fase == '.2.3' or fase == '.1.3' or fase == '.1.2':
                    datos2F = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "TAPS": tap, "INDEXDSS": indexDSS,
                               'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                               'PHASE': fase, 'KVA': int(float(trafo1['RATEDKVA'])), 'KVM': trafo1['PRIMVOLT'],
                               'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'], 'CONBA': trafo1['SECCONN'],
                               'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                               'LOADCONNS': '.1.2.3', 'LOADCONF': 'delta', 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN,
                               'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN, 
                               'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                               'INDEX_LV_GROUP': indexLvGroup}
                    datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                    datosT2F.append(datos2F)
                    if (nodo in Graph_T2F.nodes()) and (Graph_T2F.nodes[nodo]['PHASE'] == datos2F['PHASE']):
                        Graph_T2F.nodes[nodo]['KVA'] = float(datos2F['KVA'])+ float(Graph_T2F.nodes[nodo]['KVA'])
                        aviso = 'Se aumentó la capacidad de un transformador bifásico debido '
                        aviso += 'a su cercanía con otro transformador en: ('
                        aviso += str(Graph_T2F.nodes[nodo]['X1']) + ', '
                        aviso += str(Graph_T2F.nodes[nodo]['Y1'])+ ')\n'
                        self.mensaje_log_gral += aviso
                    else:
                        Graph_T2F.add_node(nodo)
                        Graph_T2F.nodes[nodo].update(datos2F) # Agrega el trafo al grafo con todos los datos
                        
                        grafoBTTotal.add_node(nodo)
                        grafoBTTotal.nodes[nodo].update(datosTotalGraph)
                elif fase == '.3' or fase == '.2' or fase == '.1':
                    datos1F = {'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                               "NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "TAPS": tap, "INDEXDSS": indexDSS,
                               'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                               'PHASE': fase, 'KVA': trafo1['RATEDKVA'], 'KVM': trafo1['PRIMVOLT'],
                               'KVL': trafo1['SECVOLT'], 'LOADCONF': 'delta', 'LOADCONNS': '.1.2', 'LOADVOLT': loadvolt,
                               'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                               "VOLTMTLN": voltoprLN, 'MV_MV': mv_mv, "INDEXBUS1": indexBus1,
                               'INDEX_LV_GROUP': indexLvGroup}
                    datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                    datosT1F.append(datos1F)
                    if (nodo in Graph_T1F.nodes()) and (Graph_T1F.nodes[nodo]['PHASE'] == datos1F['PHASE']):
                        Graph_T1F.nodes[nodo]['KVA'] = float(datos1F['KVA'])+ float(Graph_T1F.nodes[nodo]['KVA'])
                        aviso = 'Se aumentó la capacidad de un transformador monofásico debido '
                        aviso += 'a su cercanía con otro transformador en: ('
                        aviso += str(Graph_T1F.nodes[nodo]['X1']) + ', '
                        aviso += str(Graph_T1F.nodes[nodo]['Y1']) + ')\n'
                        self.mensaje_log_gral += aviso
                    else:
                        Graph_T1F.add_node(nodo)
                        Graph_T1F.nodes[nodo].update(datos1F) # Agrega el trafo al grafo con todos los datos
                        
                        grafoBTTotal.add_node(nodo)
                        grafoBTTotal.nodes[nodo].update(datosTotalGraph)
            if list_sec_ne:
                msg = 'No se encontraron los siguientes códigos '
                msg += 'de tensión en el secundario. Su modelo de '
                msg += 'transformador no será correcto:\n'
                msg += str(set(list_sec_ne))
                self.mensaje_log_gral += msg
                aviso = QCoreApplication.translate('dialog', msg) 
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
            if list_prim_ne:
                msg = 'No se encontraron los siguientes códigos '
                msg += 'de tensión en el primario. Su modelo de '
                msg += 'transformador no será correcto:\n'
                msg += str(set(list_prim_ne))
                self.mensaje_log_gral += msg
                aviso = QCoreApplication.translate('dialog', msg) 
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                
            return datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal
        except KeyError as e:
            cause = e.args[0]
            self.print_error()
            aviso = 'Favor verifique que la capa de transformadores '
            aviso += "tenga el siguiente atributo: "
            aviso += cause
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura transformadores", aviso)
            return 0, 0, 0, 0, 0, 0, 0, 0, 0
        except Exception:
            aviso = "Hubo un error en la lectura de transformadores"
            aviso += "\nPara más detalles: \n"
            aviso += self.print_error()
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura transformadores", aviso)
            return 0, 0, 0, 0, 0, 0, 0, 0, 0

    def ReaderDataLBT(self, layer, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS):
        try:
            
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            idx_bus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            idx_length = auxiliary_functions.getAttributeIndex(self, layer, "length")
            
            lineas = layer.getFeatures() # Caracteristicas de la capa
            layer.startEditing() # Activa modo edición
            
            for linea in lineas:
                geom = linea.geometry()
                line = self.MultiStringToMatrix(geom)
                
                if line == 0:
                    return 0, 0, 0

                LineLength = linea.geometry().length()
                id_ = linea.id()
                layer.changeAttributeValue(id_, idx_length, LineLength)

                n = len(line) # Cantidad de vértices de la línea
                LVCode = linea['NOMVOLT']
                nodo1, nodo2 = self.CoordLineProcess(linea, toler)
                conns = lineOperations.renameVoltage(linea['NOMVOLT']).get('conns') # phaseCodeOpenDSS
                cantFases = lineOperations.renameVoltage(linea['NOMVOLT']).get('cantFases') # 1 or 3 phases
                config = lineOperations.renameVoltage(linea['NOMVOLT']).get('config') # wye or delta
    
                try:
                    group = linea['LV_GROUP']
                except KeyError:
                    group = 'N/A'
                if subterranea:  # Determina si la línea es aérea o subterránea
                    air_ugnd = 'ugnd'
                    datosLinea = {"LVCODE": LVCode, "INDEXDSS": indexDSS, "LAYER": layer, "ID": linea.id(), "nodo1": nodo1,
                                  "nodo2": nodo2, 'NEUMAT': linea['NEUTMAT'], 'NEUSIZ': linea['NEUTSIZ'],
                                  'PHAMAT': linea['PHASEMAT'], 'PHASIZ': linea['PHASESIZ'], 'X1': line[0][0],
                                  'Y1': line[0][1], 'X2': line[n - 1][0], 'Y2': line[n - 1][1], 'SHLEN': LineLength,
                                  'AIR_UGND': air_ugnd, 'NPHAS': cantFases, 'CONNS': conns, 'CONF': config,
                                  'INSUL': linea['INSULMAT'], 'idx_bus1': idx_bus1, 'idx_bus2': idx_bus2,
                                  'GRUPO': group}  # , 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                    datosTotalGraph = {"type": "LBT", 'X1': line[0][0], 'Y1': line[0][1], 'X2': line[n - 1][0],
                                       'Y2': line[n - 1][1]}  # ,'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                else:
                    air_ugnd = 'air'
                    datosLinea = {"LVCODE": LVCode, "INDEXDSS": indexDSS, "LAYER": layer, "ID": linea.id(), "nodo1": nodo1,
                                  "nodo2": nodo2, 'NEUMAT': linea['NEUTMAT'], 'NEUSIZ': linea['NEUTSIZ'],
                                  'PHAMAT': linea['PHASEMAT'], 'PHASIZ': linea['PHASESIZ'], 'X1': line[0][0],
                                  'Y1': line[0][1], 'X2': line[n - 1][0], 'Y2': line[n - 1][1], 'SHLEN': LineLength,
                                  'AIR_UGND': air_ugnd, 'NPHAS': cantFases, 'CONNS': conns, 'CONF': config, 'GRUPO': group,
                                  'TIPO': linea['TYPE'], 'idx_bus1': idx_bus1, 'idx_bus2': idx_bus2}  # , 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                    datosTotalGraph = {"type": "LBT", 'X1': line[0][0], 'Y1': line[0][1], 'X2': line[n - 1][0],
                                       'Y2': line[n - 1][1]}  # , 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                datosLBT.append(datosLinea) ### Código viejo
    
                if grafoBT.get_edge_data(nodo1, nodo2)== None:  # se asegura que la línea no existe
                    grafoBT.add_edge(nodo1, nodo2)
                    grafoBT[nodo1][nodo2].update(datosLinea) # Agrega la línea al grafo con todos los datos
                    grafoBTTotal.add_edge(nodo1, nodo2)
                    grafoBTTotal[nodo1][nodo2].update(datosTotalGraph) # Agrega la línea al grafo con todos los datos
                    
                else:  # Si la línea existe es porque están en paralelo
                    newLength = float(datosLinea["SHLEN"])/ 2
                    datosLinea["SHLEN"] = newLength
                    paralelNode = "Paralel" + str(nodo1)
                    datosLinea["nodo2"] = paralelNode
    
                    grafoBT.add_edge(nodo1, paralelNode)
                    grafoBT[nodo1][paralelNode].update(datosLinea) # Agrega la línea al grafo con todos los datos
                    
                    grafoBTTotal.add_edge(nodo1, paralelNode)
                    grafoBTTotal[nodo1][paralelNode].update(datosTotalGraph) # Agrega la línea al grafo con todos los datos
    
                    datosLinea["nodo2"] = nodo2
                    datosLinea["nodo1"] = paralelNode
    
                    grafoBT.add_edge(paralelNode, nodo2)
                    grafoBT[paralelNode][nodo2].update(datosLinea) # Agrega la línea al grafo con todos los datos
                    
                    grafoBTTotal.add_edge(paralelNode, nodo2)
                    grafoBTTotal[paralelNode][nodo2].update(datosTotalGraph) # Agrega la línea al grafo con todos los datos
            return datosLBT, grafoBT, grafoBTTotal
        except KeyError as e:
            cause = e.args[0]
            self.print_error()
            aviso = "Favor verifique que las capas de líneas de BT tengan el atributo "
            aviso += cause
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura líneas BT", aviso)
            return 0, 0, 0
        except Exception:
            aviso = "Hubo un error en la lectura de líneas de BT"
            aviso += "\nPara más detalles: \n"
            aviso += self.print_error()
            title = "QGIS2OpenDSS Error lectura líneas BT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
        finally:
            layer.commitChanges()
            

    def ReaderDataAcom(self, layer, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT):
        try:
            
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            idx_bus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            idx_length = auxiliary_functions.getAttributeIndex(self, layer, "length")
            
            lineasACO = layer.getFeatures() # Recibe las caracteristicas de la capa de acometidas.
            layer.startEditing() # Activa modo edición
            
            for lineaACO in lineasACO:
                #line = lineaACO.geometry().asPolyline() # Lee la geometria de la linea
                
                geom = lineaACO.geometry()
                line = self.MultiStringToMatrix(geom)
                
                if line == 0:
                    return 0, 0, 0
                
                LineLength = lineaACO.geometry().length()
                n = len(line) # Cantidad de vértices de la línea
                nodo1, nodo2 = self.CoordLineProcess(lineaACO, toler)
                
                id_ = lineaACO.id()
                layer.changeAttributeValue(id_, idx_length, LineLength)
                
                
                LVCode = lineaACO['NOMVOLT']
                conns = lineOperations.renameVoltage(LVCode).get('conns') # phaseCodeOpenDSS
                cantFases = lineOperations.renameVoltage(LVCode).get('cantFases') # 1 or 3 phases
                config = lineOperations.renameVoltage(LVCode).get('config') # wye or delta
                try:
                    group = lineaACO['LV_GROUP']
                except KeyError:
                    group = 'N/A'
                datos = {"LVCODE": LVCode, "INDEXDSS": indexDSS, "LAYER": layer, "ID": lineaACO.id(), "nodo1": nodo1,
                         "nodo2": nodo2, 'PHAMAT': lineaACO['PHASEMAT'], 'PHASIZ': lineaACO['PHASESIZ'], 'X1': line[0][0],
                         'Y1': line[0][1], 'X2': line[n - 1][0], 'Y2': line[n - 1][1], 'SHLEN': LineLength,
                         'NPHAS': cantFases, 'CONNS': conns, 'CONF': config, 'GRUPO': group, 
                         'TIPO': lineaACO["TYPE"], 'idx_bus1': idx_bus1, 'idx_bus2': idx_bus2}  # 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN,
                datosTotalGraph = {"type": "ACO", 'X1': line[0][0], 'Y1': line[0][1], 'X2': line[n - 1][0],
                                   'Y2': line[n - 1][1]}  # 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN,
                datosACO.append(datos)
    
                if grafoBT.get_edge_data(nodo1,
                                         nodo2)!= None:  # Se asegura que la línea no se ha creado en el grafo de LBT
                    # print "Linea acometida ya existia en grafoTOTAL"
                    pass
                else:
                    if grafoACO.get_edge_data(nodo1, nodo2)== None:  # se asegura que la línea no existe
                        
                        grafoACO.add_edge(nodo1, nodo2)
                        grafoACO[nodo1][nodo2].update(datos) # Agrega la línea al grafo con todos los datos
                        
                        grafoBTTotal.add_edge(nodo1, nodo2)
                        grafoBTTotal[nodo1][nodo2].update(datosTotalGraph)
                        
                    else:  # Si la línea existe es porque están en paralelo
                        newLength = float(datos["SHLEN"])/ 2
                        datos["SHLEN"] = newLength
                        paralelNode = "Paralel" + str(nodo1)
                        datos["nodo2"] = paralelNode
                        
                        grafoACO.add_edge(nodo1, paralelNode)
                        grafoACO[nodo1][paralelNode].update(datos) # Agrega la línea al grafo con todos los datos
                        
                        grafoBTTotal.add_edge(nodo1, paralelNode)
                        grafoBTTotal[nodo1][paralelNode].update(datosTotalGraph)
    
                        datos["nodo2"] = nodo2
                        datos["nodo1"] = paralelNode
                        
                        grafoACO.add_edge(paralelNode, nodo2)
                        grafoACO[paralelNode][nodo2].update(datos) # Agrega la línea al grafo con todos los datos
                        
                        grafoBTTotal.add_edge(paralelNode, nodo2)
                        grafoBTTotal[paralelNode][nodo2].update(datosTotalGraph)
                        
            return datosACO, grafoACO, grafoBTTotal
        except KeyError as e:
            cause = e.args[0]
            aviso = "Favor verifique que las capas de acometidas tengan el atributo "
            aviso += cause
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura acometidas", aviso)
            return 0, 0, 0, 0
        except Exception:
            aviso = "Ocurrió un error en la lectura de las capas de acometidas."
            aviso += "Para más detalles:\n"
            aviso += self.print_error()
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura acometidas", aviso)
            return 0, 0, 0
        finally:
            layer.commitChanges()
            
        
    
    def ReaderDataGD(self, toler, layer, grafoGD, indexDSS, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F,
                     grafoCAR, circuitName, busBTid, busBT_List, busMT_List):
        try:
            GDs = layer.getFeatures() # Recibe las caracteristicas de la capa de cargas.
            mensaje_gd = ""
            for GD in GDs:
                point = GD.geometry().asPoint() # Lee la geometria de la linea
                nodo = self.CoordPointProcees(GD, toler)
                nodoInTraf = False
                if (nodo in grafoCAR.nodes()):
                    bus = grafoCAR.nodes[nodo]["BUS"]
                    if grafoCAR.nodes[nodo]["TRAFNPHAS"] != "NULL":
                        VOLTAGELL = grafoCAR.nodes[nodo]["TRAFVOLTLL"]
                        VOLTAGELN = grafoCAR.nodes[nodo]["TRAFVOLTLN"]
                        NPHAS = grafoCAR.nodes[nodo]["TRAFNPHAS"]
                        conf = grafoCAR.nodes[nodo]["CONF"]
                    else:
                        VOLTAGELL = "0.24"
                        VOLTAGELN = "0.12"
                        NPHAS = "1"
                        conf = "wye"
                elif (nodo in Graph_T3F_multi.nodes()):
                    nodoInTraf is True
                    bus = Graph_T3F_multi.nodes[nodo]["BUSBT"]
                    VOLTAGELL = Graph_T3F_multi.nodes[nodo]["LOADVOLT"]
                    VOLTAGELN = Graph_T3F_multi.nodes[nodo]["LOADVOLTLN"]
                    NPHAS = Graph_T3F_multi.nodes[nodo]["NPHAS"]
                    conf = Graph_T3F_multi.nodes[nodo]["LOADCONF"]
    
                elif (nodo in Graph_T3F_single.nodes()):
                    nodoInTraf is True
                    bus = Graph_T3F_single.nodes[nodo]["BUSBT"]
                    VOLTAGELL = Graph_T3F_single.nodes[nodo]["LOADVOLT"]
                    VOLTAGELN = Graph_T3F_single.nodes[nodo]["LOADVOLTLN"]
                    NPHAS = Graph_T3F_single.nodes[nodo]["NPHAS"]
                    conf = Graph_T3F_single.nodes[nodo]["LOADCONF"]
    
                elif (nodo in Graph_T2F.nodes()):
                    nodoInTraf is True
                    bus = Graph_T2F.nodes[nodo]["BUSBT"]
                    VOLTAGELL = Graph_T2F.nodes[nodo]["LOADVOLT"]
                    VOLTAGELN = Graph_T2F.nodes[nodo]["LOADVOLTLN"]
                    NPHAS = Graph_T2F.nodes[nodo]["NPHAS"]
                    conf = Graph_T2F.nodes[nodo]["LOADCONF"]
    
                elif (nodo in Graph_T1F.nodes()):
                    nodoInTraf is True
                    bus = Graph_T1F.nodes[nodo]["BUSBT"]
                    VOLTAGELL = Graph_T1F.nodes[nodo]["LOADVOLT"]
                    VOLTAGELN = Graph_T1F.nodes[nodo]["LOADVOLTLN"]
                    NPHAS = Graph_T1F.nodes[nodo]["NPHAS"]
                    conf = Graph_T1F.nodes[nodo]["LOADCONF"]
                elif (not nodoInTraf)and (nodo in busMT_List):
                    bus = busMT_List[nodo]["bus"]
                    VOLTAGELL = busMT_List[nodo]["VOLTAGELL"]
                    VOLTAGELN = busMT_List[nodo]["VOLTAGELN"]
                    NPHAS = busMT_List[nodo]["NPHAS"]
                    conf = "wye"
                else:
                    bus = 'BUSLV' + circuitName + str(busBTid)
                    VOLTAGELL = "0.24"
                    VOLTAGELN = "0.12"
                    NPHAS = "1"
                    conf = "wye"
                    busBT_List[nodo] = {'bus': bus, 'X': point[0], 'Y': point[1], "GRAFO": grafoGD, "VOLTAGELN": VOLTAGELN}
                    busBTid += 1
                    aviso = QCoreApplication.translate('dialog', 'Hay un generador desconectado en: (')+ str(
                        point[0])+ ',' + str(point[1])+ ')'
                    mensaje_gd += aviso +"\n"
                    #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Generador'), aviso)
    
                datos = {"CONF": conf, "NPHAS": NPHAS, "VOLTAGELN": VOLTAGELN, "VOLTAGELL": VOLTAGELL, "BUS": bus,
                         "INDEXDSS": indexDSS, 'ID': GD.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                         'KVA': GD['KVA'], "CURVE1": GD["CURVE1"], "CURVE2": GD["CURVE2"], "TECH": GD["TECH"]}
                
                grafoGD.add_node(nodo)
                grafoGD.nodes[nodo].update(datos)
                
            name_file_gd = "error_GD" + circuitName + ".log"
            dir_archivo_GD = self.dir_logs + "/" + name_file_gd
            if mensaje_gd != "":
                with open(dir_archivo_GD, 'w')as archivo_errgd:
                    archivo_errgd.write(mensaje_gd)
                aviso = "Existen uno o más generadores desconectados. Favor revise el archivo " + name_file_gd
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Generador'), aviso)
            
            else: #Si no hay mensajes de error se elimina el archivo para que no exista un archivo de errores falso
                try:
                    os.remove(dir_archivo_GD)
                except Exception:
                    pass
                
            return grafoGD, busBTid, busBT_List
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que su capa de GD tenga el atributo "
            aviso += cause + "."
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura GD", aviso)
            return 0, 0, 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de las capas de generación distribuida."
            aviso += "Para más detalles:\n" + msg
            QMessageBox.critical(None, "QGIS2OpenDSS Error lectura GD", aviso)
            return 0, 0, 0
    
    """
    **************************************************************************
    **************************** GetNominalVoltBT ****************************
    **************************************************************************
    
    Función que retorna la configuración y tensión nominal de un Código, basado en los códigos descritos en el manual de QGIS2OpenDSS.
    Se utiliza la librer[ia presente en renameVoltage.
    
    Parámetros de entrada:
    *Code (int): Código a ser analizado
    *service (str): Fases a las que se encuentra conectada la carga. Permite seleccionar entre Vll o Vln en conexiones estrella.
    
    Valores retornados:
    *nomvolt (str): valor de tensión nominal relacionado al código de entrada.
    *Conf (str): configuración de la conexión ("wye" para estrella o fase partida y "delta" para configuración delta).
    """
    
    def GetNominalVoltBT(self, Code, service):
        voltageCodes = lineOperations.renameVoltage(Code)
        conf = voltageCodes['config']
        
        #Se elige tensión LL si service es 12 o 123
        if service == "12" or service == "123" :
            nomvolt = str(voltageCodes['LVCode']['LL'])
        #Si no, se elige la tensión LN
        else:
            nomvolt = str(voltageCodes['LVCode']['LN'])
        return conf, nomvolt
    
    """
    **************************************************************************
    **************************** GetNominalVoltBT ****************************
    **************************************************************************
    
    Función que retorna la configuración y tensión nominal de un Código, basado en los códigos descritos en el manual de QGIS2OpenDSS.
    Se utiliza la librer[ia presente en renameVoltage.
    
    Parámetros de entrada:
    *Code (int): Código a ser analizado
    *cant_fases (str): Cantidad de fases de la carga asociada
    
    Valores retornados:
    *Tension (str): valor de tensión nominal relacionado al código de entrada.
    *conec (str): configuración de la conexión ("wye" para estrella o fase partida y "delta" para configuración delta).
    """
    
    def GetNominalVoltMT(self, Code, cant_fases):
        voltageCodes = lineOperations.renameVoltage(Code)
        conec = voltageCodes['config']
        
        #Se elige tensión LL si es bifásico o trifásico
        if cant_fases == "2" or cant_fases == "3":
            nomvolt = str(voltageCodes['LVCode']['LL'])
        #Si no, se elige la tensión LN
        else:
            nomvolt = str(voltageCodes['LVCode']['LN'])
        return conec, nomvolt


    """
    Función replace_lines_mt_trafo_mt
    Función que reescribe el archivo de líneas según la lista de buses
    que esté asociada a un trafo mt_mt
    Se busca el bus2, debido a que vamos a buscar la línea que va
    desde el trafo. No se puede hacer con bus1 debido a la forma en
    que la sentencia del trafo fue definida.
    
    *Parámetros de entrada:
    *file_path (str): path del directorio donde está el archivo
    *vector (list): lista de buses2 (tal y como aparecen en las líneas,
    es decir, sin el '_tx al final')
    
    *Parámetros de salida:
    1 si culmina de forma exitosa. 0 si hay errores
    """
    
    
    def replace_lines_mt_trafo_mt(self, file_path, vector):
        try:
            # Create temp file
            fh, abs_path = mkstemp()
            with fdopen(fh, 'w') as new_file:
                with open(file_path) as old_file:
                    for line in old_file:
                        # Se recorre el vector de buses en trafo mv_mv
                        for i in range(0, len(vector)):
                            pattern = 'bus1=' + vector[i] + "."
                            subs = 'bus1=' + vector[i] + '_tx.'
                            if line.find(pattern) != -1:
                                line = line.replace(pattern, subs)
                                del vector[i]
                                break
                        new_file.write(line)
    
            # Copy the file permissions from old file to the new file
            copymode(file_path, abs_path)
            # Remove original file
            remove(file_path)
            # Move new fi1le
            move(abs_path, file_path)
            return 1
        except Exception:
            msg = "Hubo un error al corregir las líneas MT "
            msg += "para incluir los buses de trafos mv-mv"
            print(msg)
            self.print_error()
            return 0

    
    # ********************************************************
    # ********************************************************
    """
    Esta función se encarga de determinar si el SERVICE asociado a 
    cierta carga tiene un tipo que cable que acepta esta conexion
    
    Retorna un 0 en caso de haber errores, un 1 en caso contrario
    """
    # ********************************************************
    # ********************************************************
    def CableAnalysis(self, carga, point, grafoBT, grafoACO, conns, toler,
                      tipo_analisis = "BT"):
        
        # Se determina a qué línea está conectada la carga para averi-
        # guar si el código de cable acepta el tipo de SERVICE indicado
        index_carga = carga['DSSName']
        x1 = point[0]
        y1 = point[1]
        x1_ = int(x1/toler)
        y1_ = int(y1/toler)
        nodo1 = (x1_, y1_)
        
        #Se configura para mostrar los mensajes adecuados
        if tipo_analisis == "BT":
            extra_message = "la carga de BT "
            message = "La carga de BT "
            ext = " ubicada "
        else:
           extra_message = "el VE "
           message = "El VE "
           ext = "ubicado "
        
        # Analiza los nodos vecinos
        try:
            # por la topología de la red sólo tendrá un nodo vecino
            
            #Revisa primero en capa BT
            nodo2 = nx.neighbors(grafoBT, nodo1)
        
        except: #Si no funciona, hacer el intento con ACOMETIDA
        
            try:
                nodo2 = nx.neighbors(grafoACO, nodo1)
                
            except Exception: #Ya si no funciona, entonces mandar al archivo de error
                
                if index_carga != None:
                    aviso = message + str(index_carga)
                    aviso += " no posee nodos vecinos. Verifique que "
                    aviso += "se encuentre conectada."
                else:
                    aviso = message + ext + "en (" + str(x1)+ ","
                    aviso += str(y1)+ ")no posee nodos vecinos. "
                    aviso += "Verifique que se encuentre conectada."
                aviso = str(aviso + "\n")
                self.mensaje_log_gral += aviso
                return 0
            
        # Recupera datos del grafo de BT (línea asociada a la carga)
        # las cargas (nodos)sólo tendrán un arista (un vecino)
        for nodo_ in nodo2:
            nodo2 = nodo_
        
        try:
            line_type = grafoBT.get_edge_data(nodo1, nodo2)['AIR_UGND']
        except:
            line_type = "air" #Acometidas
        
        # Lìnea aérea
        if line_type.lower()!= "air":
            return 1
        
        try:
            cable_type = grafoBT.get_edge_data(nodo1, nodo2)['TIPO']
        except:
            cable_type = grafoACO.get_edge_data(nodo1, nodo2)['TIPO']
            
        #Verifica que el tipo de cable y la conexión tengan sentido
        error_carga = 0
        if ((cable_type == "DPX" and conns != ".1") 
            or (cable_type == "DPX" and conns != ".2")):
            error_carga = 1
        elif cable_type == "TPX" and conns != ".1.2":
            error_carga = 1
        elif cable_type == "QPX" and conns != ".1.2.3":
            error_carga = 1
        #"BARE" puede tener cualquier valor válido
        #RHH puede tener cualquier valor válido
        
        #Para mostrar un mensaje al usuario
        if error_carga == 1:
            if index_carga == None:
                aviso = "Verifique que  " + extra_message + ext
                aviso += " en (" + str(x1)+ "," + str(y1)
                aviso += ") tenga el SERVICE correcto, ya que el "
                aviso += "tipo de línea " + cable_type + " no acepta "
                aviso += "estar conectada a la fase " + conns + "\n"
            else:
                aviso = "Verifique que " + extra_message
                aviso += str(index_carga) + " tenga el SERVICE "
                aviso += "correcto, ya que el tipo de línea "
                aviso += cable_type + " no acepta estar conectada "
                aviso += "a la fase " + conns + "\n"                
            self.mensaje_log_gral += aviso
            return 0
        return 1
    
    
    
    """
    # ################################################################
    # ################################################################
    Función que se encarga de seleccionar un día donde estén la mayor
    cantidad de columnas completas para ser utilizadas en un load_shape
    Inicialmente los IDs de los medidores están en los nombres de
    las columnas, y la columna 'Fecha' es donde se encuentra tanto la
    fecha como la hora, separadas por un espacio.
    
    Valores de entrada:
    id_ (int): número de fila actual de la tabla de atributos
    *carga
    *layer (): capa donde está la info de AMIS
    *cols_str (str list): lista con los nombres de columnas del dataframe
    con la información de ids por circuito
    *idx_ami (int): número de columna del atributo 'AMI'
    *idx_id (int): número de columna del atributo 'ID'
    *name_attribute (str): nombre del atributo donde está el identificador
    del AMI
    *self.df_data (dataframe con la información de los loadshapes por
    medidor
    *self.folder_profile (str): directorio donde 
    
    Valores retornados:
    *total_data (dataframe): dataframe con todos los datos
    *df_fin (dataframe): dataframe con el día seleccionado y los IDs
    seleccionados.
    
    # ################################################################
    # ################################################################
    """
    
    def assign_ids(self, id_, carga, layer, cols_str, idx_ami, idx_id,
               name_attribute='ID'):
        try:
            id_carga = str(carga[name_attribute])
            dtype_ = self.df_data.columns.dtype
            if id_carga not in cols_str:
                return False, None
            if dtype_ != np.dtype('O'):
                try: id_carga = int(id_carga)
                except Exception: pass
            
            col = self.df_data[id_carga]
            filename_ = self.folder_profile + "/amis"
            filename_ += "/" + str(id_carga) + ".txt"
            
            col.to_csv(filename_, header=False, index=False)
            layer.changeAttributeValue(id_, idx_ami, 'Yes')
            layer.changeAttributeValue(id_, idx_id, str(id_carga))
            return True, id_carga
        except Exception:
            self.print_error()
            return False, None
    
    
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    def ReaderDataLoadBT(self, layer, datosCAR, grafoCAR, kWhLVload,
                         toler, indexDSS, grafoBTTotal, grafoBT, grafoACO):
        try:
            # Recibe las caracteristicas de la capa de cargas.
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            idx_ami = auxiliary_functions.getAttributeIndex(self, layer, "AMI")
            idx_id = auxiliary_functions.getAttributeIndex(self, layer, "ID")
            
            """
            filename_ = self.folder_profile +  "/Datos.csv"
            df_data = pd.read_csv(filename_, sep=";", header=0,
                                  index_col=0, decimal=",")
            self.df_data = df_data
            cols_ = df_data.columns
            
            columns = list(cols_)
            
            cols_str = [str(i) for i in columns]
            layer.startEditing() # Activa modo edición
            """
            cargas = layer.getFeatures()
            error = 0
            
            
            for carga in cargas:
                # Lee la geometria de la linea
                point = carga.geometry().asPoint()
                id_ = carga.id()
                try:
                    group = carga['LV_GROUP']
                except KeyError:
                    group = 'N/A'
                nodo = self.CoordPointProcees(carga, toler)
                
                # CONN: atributo opcional
                # (indica si está conectada en delta o en estrella)
                try:
                    conf = str(carga['CONN'])
                    if conf.lower()== "d" or conf.lower()== "delta":
                        conex = "delta"
                    elif (conf.lower()== "y" 
                          or conf.lower()== "estrella"
                          or conf.lower()== "wye"):
                        conex = "wye"
                    else:
                        conex = ""
                except Exception:
                    conex = ""
                #Model: atributo opcional
                try:
                    model = int(carga['MODEL'])
                    if model >= 1 and model <= 8:
                        model = 1
                    model = str(model)
                except Exception:
                    model = "1"
                
                #Service
                try:
                    service = str(carga['SERVICE'])
                    
                    if service == "1" or service =="A" or service=="R":
                        conns = ".1"
                    elif service == "2" or service == "B" or service=="S":
                        conns = ".2"
                    elif service == "3" or service == "C" or service=="T":
                        conns = ".3"
                    elif service == "12" or service == "21" or service =="AB" or service =="BA" or service=="RS" or service=="SR":
                        conns = ".1.2"
                    elif service == "23" or service == "32" or service =="BC" or service =="CB" or service=="ST" or service=="TS":
                        conns = ".2.3"
                    elif service == "13" or service=="31" or service =="AC" or service=="CA" or service=="RT" or service=="TR":
                        conns = ".1.3"
                    elif service == "123" or service=="ABC" or service=="RST":
                        conns = ".1.2.3"
                    else:
                        mensaje = "Favor introduzca un código válido "
                        mensaje += " para el atributo 'SERVICE' "
                        mensaje += "en la capa de cargas BT"
                        title = "QGIS2OpenDSS error BT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0, 0
                except Exception:
                    print('No hay atributo SERVICE')
                    service = "12"
                    conns = ".1.2"
                    
                try:
                    code_nomvolt = carga['NOMVOLT']
                    code_nvolt = int(code_nomvolt)
                    conf_, nom_volt = self.GetNominalVoltBT(code_nvolt,
                                                            service)
                    
                    # caso en que conex no se haya definido
                    if conex == "": 
                        conex = conf_
                    
                    # Caso en que se haya introducido un código no válido
                    # para NOMVOLT
                    if conf_ == 0: 
                        mensaje = "Introduzca un código válido "
                        mensaje += "para NOMVOLT"
                        title = "QGIS2OpenDSS error BT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0, 0
                
                except Exception:
                    self.print_error()
                    mensaje = "Favor introduzca el atributo "
                    mensaje += "'NOMVOLT' en la capa de cargas de BT"
                    title = "QGIS2OpenDSS error BT"
                    QMessageBox.critical(None, title, mensaje)
                    return 0, 0, 0, 0
                    
                # Se llama a la función encargada de determinar si
                # el SERVICE tiene un tipo de cable correcto
             
                sucessfull = self.CableAnalysis(carga, point,
                                                grafoBT, grafoACO, conns, toler)
                                                
                # Para mostrar el mensaje de errores en
                # CableAnalysis sólo una vez
                if sucessfull == 0:
                    error = 1
                
                try:
                # Medición inteligente
                    #ami, id_carga = self.assign_ids(id_, carga, layer,
                                                #cols_str, idx_ami, idx_id,
                                                #name_attribute='ID')

                    ami = carga['AMI']
                    ami = str(ami).lower()
                    if (ami == "yes" or ami== "si" or ami == 'sí' or
                        ami == 'y' or ami == 's' or ami == '1'
                        or ami == 't' or ami == 'true'):
                        ami = True
                    else:
                        ami = False
                    
                except Exception:
                    ami = False
                
                if ami is True:
                    id_carga = carga['ID']
                else:
                    id_carga = None
                
                #Coordenadas
                x1 = point[0]
                y1 = point[1]
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                         "LAYER": layer, "nodo": nodo, 'X1': x1,
                         'Y1': y1, 'kWh': carga['KWHMONTH'],
                         'GRUPO': group, 'kW': 1.0, 'CONNS': conns,
                         'CONF': conex, 'CURVASIG': '',
                         'class': carga['CLASS'],
                         'NOMVOLT': nom_volt, 'MODEL': model,
                         'CODE_NOMVOLT': code_nvolt,
                         'AMI': ami, 'ID_CARGA': id_carga,
                         'idx_bus1': idx_bus1}
                
                datosTotal = {"type": "LOAD"}
                kWhLVload.append(float(carga['KWHMONTH']))
                datosCAR.append(datos)
                grafoCAR.add_node(nodo)
                grafoCAR.nodes[nodo].update(datos)
                
                grafoBTTotal.add_node(nodo)
                grafoBTTotal.nodes[nodo].update(datosTotal)
            
            #Envía un mensaje al usuario
            if error == 1:
                aviso = "Verifique el archivo log, debido a que hubo "
                aviso += "errores asociados al SERVICE y tipo de cable "
                aviso += "asociado a la carga de BT"
                #QMessageBox.warning(None, "Error BT", aviso)
            return datosCAR, grafoCAR, kWhLVload, grafoBTTotal
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que se encuentre la siguiente "
            aviso += "llave en su diccionario: "
            aviso += cause + ".\n Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura cargas BT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de las capas "
            aviso += "de cargas de baja tensión."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura cargas BT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
        finally:
            layer.commitChanges()
            
    
    def ReaderDataLoadLights(self, layer, datosCAR_i, grafoCAR_i,
                     toler, indexDSS, grafoBTTotal):
        try:
            # Recibe las caracteristicas de la capa de cargas.
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            
            """
            filename_ = self.folder_profile +  "/Datos.csv"
            df_data = pd.read_csv(filename_, sep=";", header=0,
                                  index_col=0, decimal=",")
            self.df_data = df_data
            cols_ = df_data.columns
            
            columns = list(cols_)
            
            cols_str = [str(i) for i in columns]
            layer.startEditing() # Activa modo edición
            """
            cargas = layer.getFeatures()
            error = 0
            
          
            for carga in cargas:
                
                # Lee la geometria de la linea
                point = carga.geometry().asPoint()
                id_ = carga.id()
                try:
                    group = carga['LV_GROUP']
                except KeyError:
                    group = 'N/A'
                nodo = self.CoordPointProcees(carga, toler)
                
                # CONN: atributo opcional
                # (indica si está conectada en delta o en estrella)
                try:
                    conf = str(carga['CONN'])
                    if conf.lower()== "d" or conf.lower()== "delta":
                        conex = "delta"
                    elif (conf.lower()== "y" 
                          or conf.lower()== "estrella"
                          or conf.lower()== "wye"):
                        conex = "wye"
                    else:
                        conex = ""
                except Exception:
                    conex = ""
                #Model: atributo opcional
                try:
                    model = int(carga['MODEL'])
                    if model >= 1 and model <= 8:
                        model = 1
                    model = str(model)
                except Exception:
                    model = "1"
                
                #Service
                try:
                    service = carga['SERVICE']
                    service = int(service)
                    service = str(service)
                    
                    if service == "1" or service =="A" or service=="R":
                        conns = ".1"
                    elif service == "2" or service == "B" or service=="S":
                        conns = ".2"
                    elif service == "3" or service == "C" or service=="T":
                        conns = ".3"
                    elif service == "12" or service == "21" or service =="AB" or service =="BA" or service=="RS" or service=="SR":
                        conns = ".1.2"
                    elif service == "23" or service == "32" or service =="BC" or service =="CB" or service=="ST" or service=="TS":
                        conns = ".2.3"
                    elif service == "13" or service=="31" or service =="AC" or service=="CA" or service=="RT" or service=="TR":
                        conns = ".1.3"
                    elif service == "123":
                        conns = ".1.2.3"
                    else:
                        mensaje = "Favor introduzca un código válido "
                        mensaje += " para el atributo 'SERVICE' "
                        mensaje += "en la capa de cargas BT"
                        title = "QGIS2OpenDSS error BT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0, 0
                except Exception:
                    service = "12"
                    conns = ".1.2"
                    
                try:
                    code_nomvolt = carga['NOMVOLT']
                    code_nvolt = int(code_nomvolt)
                    conf_, nom_volt = self.GetNominalVoltBT(code_nvolt,
                                                            service)
                    
                    # caso en que conex no se haya definido
                    if conex == "": 
                        conex = conf_
                    
                    # Caso en que se haya introducido un código no válido
                    # para NOMVOLT
                    if conf_ == 0: 
                        mensaje = "Introduzca un código válido "
                        mensaje += "para NOMVOLT"
                        title = "QGIS2OpenDSS error BT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0, 0
                
                except Exception:
                    self.print_error()
                    mensaje = "Favor introduzca el atributo "
                    mensaje += "'NOMVOLT' en la capa de cargas de BT"
                    title = "QGIS2OpenDSS error BT"
                    QMessageBox.critical(None, title, mensaje)
                    return 0, 0, 0, 0
                    
                
                #Coordenadas
                x1 = point[0]
                y1 = point[1]
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                         "LAYER": layer, "nodo": nodo, 'X1': x1,
                         'Y1': y1, 'kW': carga['KW'],
                         'GRUPO': group, 'CONNS': conns,
                         'CONF': conex, 'CURVASIG': '',
                         'NOMVOLT': nom_volt, 'MODEL': model,
                         'CODE_NOMVOLT': code_nvolt,
                         'idx_bus1': idx_bus1}
                
                datosTotal = {"type": "SL_LOAD"}
                datosCAR_i.append(datos)
                grafoCAR_i.add_node(nodo)
                grafoCAR_i.nodes[nodo].update(datos)
                
                grafoBTTotal.add_node(nodo)
                grafoBTTotal.nodes[nodo].update(datosTotal)
            
            #Envía un mensaje al usuario
            if error == 1:
                aviso = "Verifique el archivo log, debido a que hubo "
                aviso += "errores asociados al SERVICE y tipo de cable "
                aviso += "asociado a la carga de BT"
                #QMessageBox.warning(None, "Error BT", aviso)
            return datosCAR_i, grafoCAR_i, grafoBTTotal
            
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que se encuentre la siguiente "
            aviso += "llave en su diccionario: "
            aviso += cause + ".\n Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura cargas BT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0, 0
            
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de las capas "
            aviso += "de cargas de baja tensión."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura cargas BT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
            
        finally:
            layer.commitChanges()
        
    
    """
    *******************************************************************
    *******************************************************************
    Función ReadDataSub
    Se encarga de la lectura de la capa de Subestación
    
    -Parámetros de entrada:
    *mode (str): forma en la que será modelada la subestación
    *layerSE (QgsVectorLayer): capa en GIS de la subestación
    *toler (float): tolerancia de cercanía entre elementos del circuito
    *grafoMT (nx.Graph): grafo de elementos de media tensión
    *busMT_List (list): lista de buses MT
    *busMTid (int): contador para los buses de MT
    *self.circuitName (str): nombre del circuito
    
    -Valores retornados
    *grafoSubt (nx.Graph): grafo con los datos de la subestación
    *grafoMT (nx.Graph): grafo de elementos de media tensión
    *datosSE (array): array con los datos de la subestación
    *busMT_List (list): lista de buses MT
    *busMTid (int): contador para los buses de MT
    *SubsNode: nodo donde está la subestación
    *******************************************************************
    *******************************************************************
    """
    
    def ReadDataSub(self, mode, layerSE, grafoMT,
                    toler, busMT_List, busMTid):
        grafoSubt = nx.Graph()
        SubsNode = None
        datosSE = []
        # Tipo de modelo de la subestación
        try:
            indexDSS = auxiliary_functions.getAttributeIndex(self, layerSE, "DSSName")
            subests = layerSE.getFeatures()  # Recibe las caracteristicas de la capa de subestación.
            for subest in subests:
                point = subest.geometry().asPoint()  # Lee la geometria de la linea
                node = self.CoordPointProcees(subest, toler)
                CONEXIONAL = ''
                CONEXIONME = ''
                CONEXIONBA = ''
                TAPS = ''
                MINTAP = ''
                MAXTAP = ''
                VOLTAJEMEDLN = str(float(subest['MEDVOLT']) / sqrt(3))
                
                # Corrientes de cortocircuito
                try:
                    isc_1p = str(dataList['ISC_1P'])
                except Exception:
                    isc_1p = '10.5'
                try:
                    isc_3p = str(dataList['ISC_3P'])
                except Exception:
                    isc_3p = '10'
                    
                if mode == "MODEL":
                    if subest['HIGHCONN'] == 'Y':
                        CONEXIONAL = 'wye'
                    if subest['HIGHCONN'] == 'D':
                        CONEXIONAL = 'delta'
                    if subest['MEDCONN'] == 'Y':
                        CONEXIONME = 'wye'
                    if subest['MEDCONN'] == 'D':
                        CONEXIONME = 'delta'
                    if int(subest['TAPS']) > 0:
                        TAPS = subest['TAPS']
                        TAP_MAX_MIN = str(subest['TAPMAX/MI'])
                        TAP_MAX_MIN = TAP_MAX_MIN.split('/')
                        MINTAP = TAP_MAX_MIN[1]
                        MAXTAP = TAP_MAX_MIN[0]
                    setTap = str(subest['TAPSETTING'])
                    if subest['WINDINGS'] == 3:
                        # voltLow =
                        if subest['LOWCONN'] == 'Y':
                            CONEXIONBA = 'wye'
                        if subest['LOWCONN'] == 'D':
                            CONEXIONBA = 'delta'
                        datos = {"INDEXDSS": indexDSS,
                                 "LAYER": layerSE, "TAP": setTap,
                                 "ID": subest.id(),
                                 'X1': point[0], 'Y1': point[1],
                                 'VOLTAJEALT': subest['HIGHVOLT'],
                                 'VOLTAJEMED': subest['MEDVOLT'],
                                 'VOLTAJEMEDLN': VOLTAJEMEDLN,
                                 'VOLTAJEBAJ': subest['LOWVOLT'],
                                 'CONEXIONAL': CONEXIONAL,
                                 'CONEXIONME': CONEXIONME,
                                 'CONEXIONBA': CONEXIONBA,
                                 'KVA_ALTA': subest['KVAHIGH'],
                                 'KVA_MEDIA': subest['KVAMED'],
                                 'KVA_BAJA': subest['KVALOW'],
                                 'PHASEDESIG': '.1.2.3',
                                 'WINDINGS': subest['WINDINGS'],
                                 'TAPS': TAPS, 'MINTAP': MINTAP,
                                 'MAXTAP': MAXTAP, 'XHL': subest['XHL'],
                                 'XHT': subest['XHT'],
                                 'XLT': subest['XLT'], 'ISC_3P': isc_3p,
                                 'ISC_1P': isc_1p}
                    else:
                        datos = {"INDEXDSS": indexDSS, "LAYER": layerSE,
                                 "TAP": setTap, "ID": subest.id(),
                                 'X1': point[0], 'Y1': point[1],
                                 'VOLTAJEALT': subest['HIGHVOLT'],
                                 'VOLTAJEMED': subest['MEDVOLT'],
                                 'VOLTAJEMEDLN': VOLTAJEMEDLN,
                                 'VOLTAJEBAJ': '',
                                 'CONEXIONAL': CONEXIONAL,
                                 'CONEXIONME': CONEXIONME,
                                 'CONEXIONBA': '',
                                 'KVA_ALTA': subest['KVAHIGH'],
                                 'KVA_MEDIA': subest['KVAMED'],
                                 'KVA_BAJA': '',
                                 'PHASEDESIG': '.1.2.3',
                                 'WINDINGS': subest['WINDINGS'],
                                 'TAPS': TAPS, 'MINTAP': MINTAP,
                                 'MAXTAP': MAXTAP, 'XHL': subest['XHL'],
                                 'XHT': subest['XHT'],
                                 'XLT': subest['XLT'],
                                 'ISC_3P': isc_3p, 'ISC_1P': isc_1p}
                    datosSE.append(datos)
                if mode == "AUTO":
                    if int(subest['TAPS']) > 0:
                        TAPS = subest['TAPS']
                        TAP_MAX_MIN = str(subest['TAPMAX/MI'])
                        TAP_MAX_MIN = TAP_MAX_MIN.split('/')
                        MINTAP = TAP_MAX_MIN[1]
                        MAXTAP = TAP_MAX_MIN[0]
                    setTap = str(subest['TAPSETTING'])
                    med_volt_ln = float(subest['MEDVOLT']) / sqrt(3)
                    datos = {"INDEXDSS": indexDSS, "LAYER": layerSE,
                             "TAP": setTap, "ID": subest.id(),
                             'X1': point[0], 'Y1': point[1],
                             'VOLTAJEALT': subest['HIGHVOLT'],
                             "VOLTAJEMEDLN": med_volt_ln,
                             'VOLTAJEMED': subest['MEDVOLT'],
                             'KVA_ALTA': subest['KVAHIGH'],
                             'KVA_MEDIA': subest['KVAMED'],
                             'TAPS': TAPS, 'MINTAP': MINTAP,
                             'MAXTAP': MAXTAP, 'XHL': subest['XHL'],
                             'ISC_3P': isc_3p, 'ISC_1P': isc_1p}
    
                if mode == "NOMODEL":
                    datos = {'INDEXDSS': indexDSS, "LAYER": layerSE,
                             'ID': subest.id(), 'X1': point[0],
                             'Y1': point[1],
                             'VOLTAJEMED': subest['MEDVOLT'],
                             'VOLTAJEMEDLN': VOLTAJEMEDLN,
                             'ISC_3P': isc_3p, 'ISC_1P': isc_1p}
                SubsNode = node
                grafoSubt.add_node(node)
                grafoSubt.nodes[node].update(datos)
                
                
                grafoMT.add_node(node)
                grafoMT.nodes[node].update(datos)
    
            if grafoSubt.number_of_nodes() == 0:
                SEactive = False
            else:
                SEactive = True
                for NODE in list(grafoSubt.nodes(data=True)):
                    dataList = NODE[1]
                    nodo = NODE[0]
                    bus = 'BUSMV' + self.circuitName + str(busMTid)
                    voltage_ln = dataList["VOLTAJEMEDLN"]
                    busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"],
                                        'Y': dataList["Y1"],
                                        'GRAFO': grafoSubt,
                                        'VOLTAGELN': voltage_ln,
                                        'NPHAS': "3",
                                        'PHASES': ".1.2.3"}
                    grafoSubt.nodes[nodo]["BUSMT"] = bus
                    busMTid += 1
            return grafoSubt, grafoMT, datosSE, busMT_List, busMTid, SubsNode
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que la capa de subestación "
            aviso += "tenga el atributo " + cause
            title = "QGIS2OpenDSS Error lectura subestación"
            QMessageBox.critical(None, title, aviso)
            return 0, grafoMT, 0, busMT_List, busMTid, SubsNode
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura la capa de "
            aviso += "subestación. Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura subestación"
            QMessageBox.critical(None, title, aviso)
            return 0, grafoMT, 0, busMT_List, busMTid, SubsNode
    
    
    """
    *******************************************************************
    *******************************************************************
    Función WriteSub
    Se encarga de la escritura del dss de la subestación
    
    
    -Parámetros de entrada:
    *mode (str): tipo de subestación a ser modelada
    *layerSE (QgsVectorLayer): capa en GIS de la subestación
    *grafoSubt (nx.Graph): grafo con los datos de la subestación
    *self.circuitName (str): nombre del circuito
    *self.foldername (str): directorio de la carpeta de salida
    *self.output_filesQGIS2DSS: archivo de salida del azul
    
    -Valores retornados
    *1 si finaliza exitosamente, 0 caso contrario
    *******************************************************************
    *******************************************************************
    """
    
    
    def WriteSub(self, mode, layerSE, grafoSubt):
        try:
            if mode == "MODEL":
                filename = self.circuitName + '_Substation.dss'
                self.output_filesQGIS2DSS.write('\nredirect ' + filename)
                output_sedss = open(self.foldername + '/' + filename, 'w')
                layerSE.startEditing() # Activa modo edición
                # if (ndatosSE!=0): # revisa si hay subestaciones
                output_sedss.write("!UNIT\n")
                for NODE in list(grafoSubt.nodes(data=True)):
                    dataList = NODE[1]
                    nodo = NODE[0]
                    fases = '3'
                    dev =  str(dataList['WINDINGS'])
                    busHV = 'Sourcebus'
                    busMV = str(dataList['BUSMT'])
    
                    busLV = ''
                    normhkva = " normhkva=" + str(dataList['KVA_ALTA'])
                    kVA = ' kVAs=[' + str(float(dataList['KVA_ALTA']))
                    kVA += ' ' + str(float(dataList['KVA_MEDIA']))
                    kV = ' kVs=[' + str(dataList['VOLTAJEALT'])
                    kV += ' ' + str(dataList['VOLTAJEMED'])
                    react = ' xhl=' + str(dataList['XHL'])
                    con = ' conns=[' + dataList['CONEXIONAL']
                    con += ' ' + dataList['CONEXIONME']
                    isc_1p = str(dataList['ISC_1P'])
                    isc_3p = str(dataList['ISC_3P'])
                    indexdss = dataList["INDEXDSS"]
                    id_ = dataList["ID"]
                    if dataList['WINDINGS'] > 2:
                        kVA = kVA + ' ' + str(float(dataList['KVA_BAJA']))
                        kV = kV + ' ' + str(dataList['VOLTAJEBAJ'])
                        busLV = ' BUSMV_TerSub.1.2.3'
                        react = react + ' xht=' + str(dataList['XHT'])
                        react += ' xlt=' + str(dataList['XLT'])
                        con = con + ' ' + dataList['CONEXIONBA']
                    kVA = kVA + ']'
                    kV = kV + ']'
                    buses = ' buses=[Sourcebus.1.2.3 ' + busMV
                    buses += '.1.2.3' + busLV + ']'
                    con = con + ']'
                    taps = ''
                    if dataList['TAPS'] != '':
                        taps = 'wdg=1 numtaps=' + str(dataList['TAPS'])
                        taps += ' tap=' + str(dataList['TAP'])
                        taps += ' maxtap=' + str(dataList['MAXTAP'])
                        taps += ' mintap=' + str(dataList['MINTAP'])
                    line = 'new transformer.HVMV' + buses 
                    line += ' phases=' + fases + ' windings=' + dev + con
                    line += kV + kVA + react + ' %loadloss=0 %noloadloss=0 '
                    line += taps + normhkva + ' !isc1p='
                    line += isc_1p + " isc3p=" + isc_3p + "\n"
                    output_sedss.write(line)
                    dataList["LAYER"].changeAttributeValue(id_, indexdss, "HVMV")
                output_sedss.close()
                layerSE.commitChanges() # Guarda
            
            if mode == "AUTO":
                filename = self.circuitName + '_Substation.dss'
                self.output_filesQGIS2DSS.write('\nredirect ' + filename)
                output_sedss = open(self.foldername + '/' + filename, 'w')
                print("Auto, ", self.foldername + '/' + filename)
                layerSE.startEditing() # Activa modo edición
                # if (ndatosSE!=0): # revisa si hay subestaciones
                for NODE in list(grafoSubt.nodes(data=True)):
                    dataList = NODE[1]
                    nodo = NODE[0]
                    cantFases = '1'
                    fases = ' phases=' + str(cantFases)
                    dev = ' windings=2'
                    busHV = 'Sourcebus'
                    busMV = str(dataList['BUSMT'])
                    Vautohigh = float(dataList['VOLTAJEALT'])/ sqrt(3)
                    Vautolow = float(dataList['VOLTAJEMED'])/ sqrt(3)
                    Zauto = float(dataList['XHL'])
                    kVAauto = float(dataList['KVA_ALTA'])/ 3
                    indexdss = dataList["INDEXDSS"]
                    id_ = dataList["ID"]
                    isc_1p = str(dataList['ISC_1P'])
                    isc_3p = str(dataList['ISC_3P'])
    
                    Vtraf1 = Vautohigh
                    Vtraf2 = Vautohigh - Vautolow
                    nt = Vtraf2 / Vtraf1
                    Ztraf = (1 - nt)* Zauto / nt
                    kVAtraf = nt * kVAauto / (1 - nt)
    
                    normhkva = " normhkva=" + str(kVAtraf)
                    kVA = ' kVAs=[' + str(kVAtraf)+ ' ' + str(kVAtraf)+ "]"
                    kV = ' kVs=[' + str(Vtraf1)+ ' ' + str(Vtraf2)+ "]"
                    react = ' xhl=' + str(Ztraf)
    
                    busesTx1 = ' buses=[bridge.1.0 ' + 'bridge.1.4]'
                    busesTx2 = ' buses=[bridge.2.0 ' + 'bridge.2.5]'
                    busesTx3 = ' buses=[bridge.3.0 ' + 'bridge.3.6]'
    
                    taps = ''
                    if dataList['TAPS'] != '':
                        taps = 'wdg=1 numtaps=' 
                        taps += str(dataList['TAPS'])
                        taps += ' tap=1.00' + ' maxtap='
                        taps += str(dataList['MAXTAP'])
                        taps += ' mintap=' 
                        taps += str(dataList['MINTAP'])
                    line1 = 'new transformer.HVMV_auto1' + busesTx1
                    line1 += fases + dev + kV + kVA
                    line1 += normhkva + react 
                    line1 += ' %loadloss=0 %noloadloss=0 '
                    line1 += taps + ' !isc1p=' + isc_1p
                    line1 += " isc3p=" + isc_3p + "\n"
                    line2 = 'new transformer.HVMV_auto2'
                    line2 += busesTx2 + fases + dev
                    line2 += kV + kVA + normhkva + react
                    line2 += ' %loadloss=0 %noloadloss=0 '
                    line2 += taps + "\n"
                    line3 = 'new transformer.HVMV_auto3'
                    line3 += busesTx3 + fases + dev
                    line3 += kV + kVA + normhkva + react
                    line3 += ' %loadloss=0 %noloadloss=0 '
                    line3 += taps + "\n"
                    jumper1 = "new line.jumper1 bus1="
                    jumper1 += "SourceBus.1.2.3 bus2=bridge"
                    jumper1 += ".1.2.3 R=0 X=0.00001 "
                    jumper1 += "Normamps=7000\n"
                    jumper2 = "new line.jumper2 bus1=bridge"
                    jumper2 += ".4.5.6 bus2=" + busMV
                    jumper2 += ".1.2.3 R=0 X=0.00001 "
                    jumper2 += "Normamps=7000\n"
                    
                    line_t = "!AUTO\n" + jumper1 + jumper2
                    line_t += line1 + line2 + line3
    
                    output_sedss.write(line_t)
                    dataList["LAYER"].changeAttributeValue(id_, indexdss, "HVMV_auto")
                output_sedss.close()
                layerSE.commitChanges() # Guarda
            
            if mode == "NOMODEL":
                filename = self.circuitName + '_Substation.dss'
                output_sedss = open(self.foldername + '/' + filename, 'w')
                for NODE in list(grafoSubt.nodes(data=True)):
                    dataList = NODE[1]
                    nodo = NODE[0]
                    bus = dataList['BUSMT']
                    isc_1p = str(dataList['ISC_1P'])
                    isc_3p = str(dataList['ISC_3P'])
                    line = 'Sourcebus' + " , " + str(dataList["VOLTAJEMED"])
                    line += ' !isc1p=' + isc_1p
                    line += " isc3p=" + isc_3p + " \n"
                    line = "!NOMODEL\n" + line
                    output_sedss.write(line)
                output_sedss.close
                
            return 1
        except Exception:
            self.print_error()
            return 0
        
    
    
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    # *****************************************************************
    
    def ReaderDataBuses(self, layer, datosBuses, grafoBuses, kWhBuses,
                        toler, grafoMT):

        # Se agrega el atributo DSSName si no existe en la capa
        layer_data = layer.dataProvider()
        index_dss = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")

        #Lectura de csv's de cargadores y buses
        buses = layer.getFeatures()
        layer.startEditing()

        error = 0
        for bus in buses:
            # Lee la geometria de la linea
            point = bus.geometry().asPoint() 
            nodo = self.CoordPointProcees(bus, toler)
            
            #Atributo opcional
            try:
                group = bus['MV_GROUP']
            except KeyError:
                group = 'N/A'

            #Nombre del plantel
            try:
                nombre_plantel = bus['PLANTEL']
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'PLANTEL' en la capa de buses"
                QMessageBox.critical(None,"QGIS2OpenDSS error buses",
                                     mensaje)
                return 0, 0, 0

            # ########################
            # Atributos para trafo
            # ########################

            #PHASEDESIG
            phasedesig = "ABC"
            x = phaseOperations.renamePhase(phasedesig)
            fase = x.get('phaseCodeODSS') # define código de OpenDSS
            numfase = x.get('phaseNumberTraf')

            #PRIMVOLT
            try:
                primvolt = bus['PRIMVOLT']
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'PRIMVOLT' en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0
            
            #SECVOLT
            try:
                secvolt = bus['SECVOLT']
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'SECVOLT' en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0
            
            prim_v = int(primvolt)
            sec_v = int(secvolt)
            voltages = trafoOperations.renameVoltage(prim_v, sec_v)
            loadvolt = str(voltages["LVCode"]["LL"])
            loadvoltLN = str(voltages["LVCode"]["LN"])

            if voltages["MVCode"]["LL"] == 0:
                voltoprLL = "UNKNOWN"
                voltoprLN = "UNKNOWN"
                aviso = 'No se encuentra el código de tensión '
                aviso +=  str(primvolt)

            else:
                voltoprLL = str(voltages["MVCode"]["LL"])
                voltoprLN = str(voltages["MVCode"]["LN"])
            
            #PRIMCONN
            try:
                primconn = bus['PRIMCONN']
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'PRIMCONN' en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0
            
            #Se determina la tensión l-n del primario
            if primconn.lower()== "d":
                voltoprLN = voltoprLL
            elif primconn.lower()== "sp":
                voltoprLN = str(2*float(voltoprLL))
                voltoprLN = format(voltoprLN, '.3f')
            elif primconn.lower()== "y":
                voltoprLN = float(voltoprLL)/sqrt(3)
                voltoprLN = format(voltoprLN, '.3f')
            # si no es ninguno de estos casos se deja la tensión
            #  por defecto (según el cuadro del manual)
            
            #SECCONN
            try:
                secconn = bus['SECCONN']
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'SECCONN' en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0

            #Se determina la tensión l-n del secundario
            if secconn.lower()== "d":
                loadvoltLN = loadvolt
            elif secconn.lower()== "sp":
                loadvoltLN = 2*float(loadvolt)
                loadvoltLN = format(loadvoltLN, '.3f')
            elif secconn.lower()== "y":
                loadvoltLN = float(loadvolt)/sqrt(3)
                loadvoltLN = format(loadvoltLN, '.3f')
            # si no es ninguno de estos casos se deja la tensión
            # por defecto (según el cuadro del manual)

            #Preguntar Tavo (4 delta 4 hilos)

            #RATEDKVA
            try:
                ratedkva = bus['RATEDKVA']
                
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo 'RATEDKVA' "
                mensaje += "en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0
            
            #TAPSETTING
            try:
                tapsetting = bus['TAPSETTING']
                tapsetting = format(float(tapsetting), '.3f')
            except KeyError:
                self.print_error()
                mensaje = "Favor introduzca el atributo "
                mensaje += "'TAPSETTING' en la capa de buses"
                title = "QGIS2OpenDSS error buses"
                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0

            #Diccionario de trafos del bus
            dict_trafo = {"NPHAS": numfase, "MVCODE": primvolt,
                          "LVCODE": secvolt, "TAPS": tapsetting,
                          'PHASE': fase, 'KVA': ratedkva,
                          'KVM': primvolt, 'KVL': secvolt,
                          'CONME': primconn, 'CONBA': secconn,
                          'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye',
                          'LOADVOLT': loadvolt,
                          'LOADVOLTLN': loadvoltLN,
                          'GRUPO_MV': group, "VOLTMTLL": voltoprLL,
                          "VOLTMTLN": voltoprLN}

            #########################
            #Atributos para carga BT
            #########################
            
            #Model: atributo opcional
            try:
                model = int(bus['MODEL'])
                if model >= 1 and model <= 8:
                    model = 1
                model = str(model)
            except Exception:
                model = "1"

            #CONN
            conex = "wye"

            #SERVICE
            service = "123"
            conns = ".1.2.3"

            #NOMVOLT
            nom_volt = loadvoltLN #se obtiene del secundario del trafo

            #KWHMONTH
            try:
                kw_month =  str(bus['KWHMONTH'])
                kw_month_t = float(kw_month)
            except Exception:
                self.print_error()
                mensaje = "Favor introduzca el atributo 'KWHMONTH' en "
                mensaje += "la capa de buses y verifique que sea "
                mensaje += " un valor numérico"
                title = "QGIS2OpenDSS error buses",

                QMessageBox.critical(None, title, mensaje)
                return 0, 0, 0

            dict_load_bt = {'kWh': kw_month, 'kW': 1.0, 'CONNS': conns,
                            'CONF': conex, 'CURVASIG': '',
                            'class': bus['CLASS'], 
                            'NOMVOLT': nom_volt, 'MODEL': model}

            excel_name = self.folder_profile + "/AEBs/data_buses_"
            excel_name += nombre_plantel + ".xlsx"
            #Diccionarios buses y cargadores del plantel actual
            dict_buses_plant = excel_to_dict(excel_name, nombre_plantel,
                                             ratedkva, self.foldername)
            if dict_buses_plant == 0:
                return 0, 0, 0

            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            dssname = str("plant_" + nombre_plantel.lower())

            #Cambios en la capa
            idx_dss = bus.fieldNameIndex("DSSName")
            layer.changeAttributeValue(bus.id(), idx_dss, dssname)

            datos = {"INDEXDSS": indexDSS, 'ID': bus.id(),
                     "LAYER": layer, "nodo": nodo, 'X1': x1, 'Y1': y1,
                     'GRUPO': group, 'NOMBRE PLANTEL': nombre_plantel,
                     'DSSName': dssname, 'DICT TRAFO': dict_trafo,
                     'DICT LOAD BT': dict_load_bt,
                     'DICT BUSES': dict_buses_plant}

            kWhBuses.append(float(bus['KWHMONTH']))
            datosBuses.append(datos)
            grafoBuses.add_node(nodo)
            grafoBuses.nodes[nodo].update(datos)

        if error == 1:
            aviso = "Verifique el archivo log, debido a que hubo "
            aviso += "errores asociados al SERVICE y tipo de "
            aviso += "cable asociado al bus"
            QMessageBox.warning(None, "Error MT", aviso)
        layer.commitChanges()
        return datosBuses, grafoBuses, kWhBuses


    # ****************************************************************
    # ****************************************************************
    # ****************************************************************
    # ****************************************************************
    # ****************************************************************
    def ReaderDataLoadMT(self, layer, datosCAR, grafoCAR, kWhMVload,
                         toler, indexDSS, grafoMT):
        try:
            
            # Recibe las caracteristicas de la capa de cargas.
            error = 0
            
            idx_bus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            idx_ami = auxiliary_functions.getAttributeIndex(self, layer, "AMI")
            idx_id = auxiliary_functions.getAttributeIndex(self, layer, "ID")
            
            """
            filename_ = self.folder_profile +  "/Datos.csv"
            df_data = pd.read_csv(filename_, sep=";", header=0,
                                  index_col=0, decimal=",")
            self.df_data = df_data
            cols_ = df_data.columns
            
            columns = list(cols_)
            
            cols_str = [str(i) for i in columns]
            layer.startEditing() # Activa modo edición
            """
            
            cargasMT = layer.getFeatures()
            for carga in cargasMT:
                point = carga.geometry().asPoint()
                id_ = carga.id()
                try:
                    group = carga['MV_GROUP']
                except KeyError:
                    group = 'N/A'
                nodo = self.CoordPointProcees(carga, toler)
                
                #Model: atributo opcional
                try:
                    model = int(carga['MODEL'])
                    if model >= 1 and model <= 8:
                        model = 1
                    model = str(model)
                except Exception:
                    model = "1"
                
                # Atributo opcional (conexión delta o estrella)
                try:
                    conf = str(carga['CONN'])
                    if conf.lower()== "d" or conf.lower()== "delta":
                        conex = "delta"
                    elif (conf.lower()== "y"
                          or conf.lower()== "estrella"
                          or conf.lower()== "wye"):
                        conex = "wye"
                    else:
                        conex = ""
                except Exception:
                    conex = ""
                    
                #PHASEDESIG
                try:
                    phasedesig = carga['PHASEDESIG']
                    data_phasedesig = phaseOperations.renamePhase(phasedesig)
                    conns = data_phasedesig['phaseCodeODSS']
                    cant_fases = data_phasedesig['phaseNumber']
                    
                     # Caso que se haya introducido un código inválido
                    if conns == "NONE":
                        mensaje = "Introduzca un código válido "
                        mensaje += "para 'PHASEDESIG'"
                        title = "QGIS2OpenDSS error cargas MT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0
                except Exception:
                    self.print_error()
                    mensaje = "Favor introduzca el atributo "
                    mensaje += "'PHASEDESIG' en la capa de cargas de MT"
                    title = "QGIS2OpenDSS error MT"
                    QMessageBox.critical(None, title, mensaje)
                    return 0, 0, 0
                
                try:
                    #NOMVOLT
                    code_nomvolt = carga['NOMVOLT']
                    code_nomvolt = int(code_nomvolt)
                    conf_, nom_volt = self.GetNominalVoltMT(code_nomvolt, cant_fases)
                    
                    
                    # Si el tipo de conexion no se introdujo
                    if conex == "":
                        conex = conf_
                    # Si no se introdujo un código válido para NOMVOLT
                    if conf_ == 0:
                        mensaje = "Introduzca un código válido "
                        mensaje += "para 'NOMVOLT'"
                        title = "QGIS2OpenDSS error cargas MT"
                        QMessageBox.critical(None, title, mensaje)
                        return 0, 0, 0
                
                except Exception:
                    self.print_error()
                    mensaje = "Favor introduzca el atributo 'NOMVOLT' "
                    mensaje += "en la capa de cargas de MT"
                    title = "QGIS2OpenDSS error MT"
                    QMessageBox.critical(None, title, mensaje)
                    return 0, 0, 0
                    
                # Factor de potencia
                try:
                    pf_ = float(carga['PF'])
                except Exception:
                    self.print_error()
                    mensaje = "Favor introduzca el atributo PF en la "
                    mensaje += "capa de cargas de MT y verifique que "
                    mensaje += "sea un valor numérico"
                    title = "QGIS2OpenDSS error MT"
                    QMessageBox.critical(None, title, mensaje)
                    return 0, 0, 0
                pf = str(pf_)
                
                
                # Medición inteligente
                try:
                    ami, id_carga = self.assign_ids(id_, carga, layer,
								cols_str, idx_ami, idx_id,
								name_attribute='ID')
                                
                    ami = carga['AMI']
                    ami = str(ami).lower()
                    if (ami == "yes" or ami== "si" or ami == 'sí' or
                        ami == 'y' or ami == 's' or ami == '1'
                        or ami == 't' or ami == 'true'):
                        ami = True
                    else:
                        ami = False
                    
                except Exception:
                    ami = False
                
                if ami is True:
                    id_carga = carga['ID']
                else:
                    id_carga = None
                # Coordenadas
                x1 = point[0]
                y1 = point[1]
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                         "LAYER": layer, "nodo": nodo, 'X1': x1,
                         'Y1': y1, 'kWh': carga['KWHMONTH'],
                         'GRUPO': group, 'kW': 1.0, 'CONNS': conns,
                         'CONF': conex, 'CURVASIG': '',
                         'class': carga['CLASS'], 'NOMVOLT': nom_volt,
                         'MODEL': model, 'CODE_NOMVOLT': code_nomvolt,
                         'PowerFactor':  pf, 'N_FASES': cant_fases,
                         'AMI': ami, 'ID_CARGA': id_carga,
                         'idx_bus1': idx_bus1}
                kWhMVload.append(float(carga['KWHMONTH']))
                datosCAR.append(datos)
                grafoCAR.add_node(nodo)
                grafoCAR.nodes[nodo].update(datos)
                
            if error == 1:
                aviso = "Verifique el archivo log, debido a que hubo "
                aviso += "errores asociados al SERVICE y tipo de cable "
                aviso += "asociado a la carga de MT"
                QMessageBox.warning(None, "Error MT", aviso)
            return datosCAR, grafoCAR, kWhMVload
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que las capas de cargas MT "
            aviso += "tengan el atributo " + cause
            title = "QGIS2OpenDSS Error lectura cargas MT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de las capas "
            aviso += "de cargas de media tensión."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura cargas MT"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
        finally:
            pass
            # layer.commitChanges()
    
    
    """
    *******************************************************************
    *******************************************************************
    Función Write_GD_ls
    Se encarga de crear la sentencia para GDs de gran escala. 
    
    -Parámetros de entrada:
    *dataList (dict): diccionario con los datos del GD
    
    -Valores retornados
    *linea_gd (str): sentencia del nuevo generador
    *linea_lshp (str): sentencia del nuevo loadshape
    *******************************************************************
    *******************************************************************
    """
    
    def Write_GD_ls(self, dataList):
        try:
            dss_name = str(dataList['DSSName'])
            tech = str(dataList['TECH']).upper()
            daily = str(dataList['DAILY'])
            mva = str(dataList["MVA"])
            kv = str(dataList["TENSION"])
            xdpp = str(dataList["XDPP"])
            xdp = str(dataList["XDP"])
            bus = dataList["BUS"]
    
            if tech == "PV" or tech == "WIND":
                model = " model=7"
            elif tech == "HYDRO" or tech== "HIDRO":
                model = " model=1"
                xdp_ = " xdp=" + xdp
            else:
                model=""
                xdp_ = ""
    
            # Sentencia del loadshape
            name_loadshape = "curve_" + dss_name
            name_file = "DG/" + daily
            sent_p = "(file=" + name_file + ", col=1, header=no)"
            sent_q = "(file=" + name_file + ", col=2, header=no)"
            linea_lshp = "new Loadshape." + name_loadshape
            linea_lshp += ' npts=96 minterval=15 Pmult=' + sent_p
            linea_lshp +=  " Qmult=" + sent_q + " useactual=no\n"
    
            linea_gd = "new generator." + dss_name
            linea_gd += " bus1=" + bus + ".1.2.3"
            linea_gd += " kV=" + kv + " phases=3 MVA=" + mva
            linea_gd += " kw=1 kVAr=1 daily=" + name_loadshape
            linea_gd += " conn=wye status=variable" + xdp_
            linea_gd +=  " xdpp=" + xdpp + model + "\n"
            return linea_gd, linea_lshp
        except Exception:
            self.print_error()
            return "", ""
 
    
    
    """
    *******************************************************************
    *******************************************************************
    Función Write_Reg
    Se encarga de la creación de la sentencia que modela los regula-
    dores. Además crea el archivo dss de dichos reguladores
    
    
    -Parámetros de entrada:
    *reg (nx.Graph): diccionario con los datos de reguladores
    *layer (QgsVectorLayer): capa en GIS de los Regs
    
    -Valores retornados
    1 si finaliza exitosamente, 0 caso contrario
    *******************************************************************
    *******************************************************************
    """
    
    def Write_Reg(self, reg, layer):
        try:
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexBus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            index_businst = auxiliary_functions.getAttributeIndex(self, layer,
                                                                  'BUSINST')
            layer.startEditing() # Activa modo edición
            i = -1
            linea_tot = ""
            for reg_ in reg.nodes(data=True):
                reg = reg_[1]
                i += 1
                dss_name = str(reg['DSSName'])
                kv = str(reg['TENSION'])
                kva = str(reg["KVA"])
                pt_ratio = str(reg['PT_RATIO'])
                v_reg = str(reg["VREG"])
                bwt = str(reg["BANDWIDTH"])
                taps = str(reg["TAPS"])
                bus1 = reg["BUSMT"]
                bus2 = reg["BUSBT"]
                numfases = str(reg['NUM_FASES'])
                fase = str(reg['PHASE'])
                tension_ln = str(reg['TENSION_LN'])
                id_ = reg["ID"]
                
                exp_businst = bus1 + fase
                layer.changeAttributeValue(id_, indexBus1, bus1)
                layer.changeAttributeValue(id_, indexBus2, bus2)
                layer.changeAttributeValue(id_, index_businst, exp_businst)
                for j in range(int(numfases)):
                    if numfases == '1':
                        node = numfases
                    else:
                        node = "." + str(j+1)
                    traf_name = dss_name + "_" + str(j)
                    reg_name = dss_name.replace("Reg", "")
                    reg_name = "RegCont" + reg_name + "_" + str(j)
                    linea_traf = "New Transformer." + traf_name
                    linea_traf += " phases=1 bank=reg" + str(i)
                    linea_traf += ' xhl=0.01 kVAs=[' + kva + ' '
                    linea_traf += kva + "] Buses=[" + bus1 + node + " "
                    linea_traf += bus2 + node + "] kVs=[" + tension_ln + " "
                    linea_traf += tension_ln + "] %LoadLoss=0.01 numtaps="
                    linea_traf += taps + " Maxtap=1.0 Mintap=-1.0\n" 
                    
                    linea_trafcont = "New regcontrol." + reg_name
                    linea_trafcont += " transformer=" + traf_name
                    linea_trafcont += " winding=2 vreg=" + v_reg
                    linea_trafcont += " band=" + bwt + " ptratio="
                    linea_trafcont += pt_ratio + " maxtapchange=1\n"
                    
                    linea_mon = "New monitor.Mon" + traf_name 
                    linea_mon += " Element=transformer." + traf_name
                    linea_mon += " Terminal=1 Mode=1\n"
                    
                    linea_tot += linea_traf + linea_trafcont + linea_mon
                    
            # Archivo reguladores
            filename = self.circuitName + '_Reg.dss'
            line_out = '\nredirect ' + filename
            output_Regdss = self.foldername + '/' + filename
            
            with open(output_Regdss , 'w') as file_reg:
                file_reg.write(linea_tot)
            
            #Escritura master
            line_out = '\nredirect ' + filename
            self.output_filesQGIS2DSS.write(line_out)
            return 1
        except Exception:
            self.print_error()
            return 0
        finally:
            layer.commitChanges()

    """
    *******************************************************************
    *******************************************************************
    Función  BusAsignationReg
    Se encarga de asignar el bus correspondiente al regulador
 
 
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *grafo (nx.Graph): grafo de reguladores
    *busMT_List (list): lista de buses de media tensión
    *busMTid (int): contador de buses mt
    
 
    -Valores retornados
    *grafo (nx.Graph): grafo de reguladores
    *busMT_List (list): lista de buses de media tensión
    *busMTid (int): contador de buses mt
    *buslist_reg (list): lista con los buses asociados a reguladores
    *******************************************************************
    *******************************************************************
    """
    
    def BusAsignationReg(self, grafo, busMT_List, busMTid):
        buslist_reg = []
        for NODE in grafo.nodes(data=True):
            dataList = NODE[1]
            nodo = NODE[0]
            if nodo in busMT_List:  # Verifica si el nodo del transformador ya está en los nodos creados en MT
                bus_mt = busMT_List[nodo]["bus"]
                grafo.nodes[nodo]["BUSMT"] = bus_mt
                BTbus = bus_mt + "_reg"
                grafo.nodes[nodo]["BUSBT"] = BTbus
                buslist_reg.append(bus_mt)
            else:
                busMTid += 1
                bus_mt = 'BUSMV' + self.circuitName + str(busMTid)
                busMT_List[nodo] = {"NPHAS": '3', 'bus': bus,
                                    'X': dataList["X1"],
                                    'Y': dataList["Y1"],
                                    "GRAFO": grafo,
                                    "VOLTAGELL": dataList["TENSION"],
                                    "VOLTAGELN": dataList["TENSION_LN"],
                                    "PHASES": ".1.2.3",
                                    'GRUPO_MV': dataList['GRUPO']}
                BTbus = bus_mt + "_reg"
                grafo.nodes[nodo]["BUSBT"] = BTbus
                grafo.nodes[nodo]["BUSMT"] = bus_mt
                buslist_reg.append(bus_mt)
        return grafo, busMT_List, busMTid, buslist_reg

    
    
    """
    *******************************************************************
    *******************************************************************
    Función  ReaderDataReg
    Se encarga de la lectura de la capa de reguladores.
 
 
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los Regs
    *toler (float): tolerancia de cercanía entre elementos del circuito
    
 
    -Valores retornados
    *grafoReg (nx.Graph): grafo con los datos de los switches. 0 si 
    hay errores
    *datosReg (array): array con los datos de los switches. 0 si
    hay errores
    *******************************************************************
    *******************************************************************
    """
 
    def ReaderDataReg(self, layer, toler):
        try:
            grafoReg= nx.Graph()
            datosReg = []
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")
            indexGroup = auxiliary_functions.getAttributeIndex(self, layer, "MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexBus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            
            Regs = layer.getFeatures()
            layer.startEditing() # Activa modo edición
            
            i = -1
            for Reg in Regs:
                i += 1
                dss_name = 'Reg' + self.circuitName + '_' + str(i)
                nodo = self.CoordPointProcees(Reg, toler)
                id_ = Reg.id()
                
                # Coordenadas
                point = Reg.geometry().asPoint()
                x1 = point[0]
                y1 = point[1]
                
                # Lectura de datos capa
                # Se lee el grupo de la capa
                try:
                    group = Reg['MV_GROUP']
                except KeyError:
                    group = ''
                
                # Tensión
                nomvolt = Reg['NOMVOLT']
                tension = lineOperations.renameVoltage(nomvolt)
                tension_ll = tension['LVCode']['LL']
                tension_ln = tension['LVCode']['LN']
                
                # Fases
                phasedesig = Reg['PHASEDESIG']
                x = phaseOperations.renamePhase(phasedesig)
                fase = x.get('phaseCodeODSS') # define código de OpenDSS
                numfase = x.get('phaseNumberTraf')
                # Capacidad nominal
                kva = Reg['KVA']
                
                if str(numfase) == '3':
                    kva = str(float(kva)/3)
                
                # PT RATIO
                pt_rt = Reg['PT_RATIO']
                
                # VREG
                v_reg = Reg['VREG']
                
                # Ancho de banda
                bwt = Reg['BANDWIDTH']
                
                # TAPS
                taps = Reg['TAPS']
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                        "LAYER": layer, 'DSSName': dss_name,
                        "INDEXBUS1": indexBus1, "INDEXBUS2": indexBus2, 
                        'X1': x1, 'Y1': y1, 'GRUPO': group,
                        'TENSION': tension_ll, 'KVA': kva,
                        'TENSION_LN': tension_ln,
                        'PT_RATIO': pt_rt, 'VREG': v_reg,
                        'BANDWIDTH': bwt, 'TAPS': taps,
                        'NUM_FASES': numfase, 'PHASE': fase}
                
                datosReg.append(datos)
                grafoReg.add_node(nodo)
                grafoReg.nodes[nodo].update(datos)
                
                # Cambios en capa
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                layer.changeAttributeValue(id_, indexGroup, group)
            
            return datosReg, grafoReg
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Verifique que la capa de reguladores de tensión "
            aviso += "tenga el atributo " + cause
            title = "QGIS2OpenDSS Error lectura reguladores de tensión"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de reguladores "
            aviso += "de tensión. Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura reguladores"
        finally:
            layer.commitChanges()
       
    
    """
    *******************************************************************
    *******************************************************************
    Función ReaderData_GD_LargeScale
    Se encarga de la lectura de la capa de GDs de gran escala. Además
    escribe el dss correspondiente a este tipo de datos
 
 
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los GDs
    *toler (float): tolerancia de cercanía entre elementos del circuito
    *Graph_T3F_single (nx.Graph): grafo de transformadores trifásicos
    de una sola unidad
 
    -Valores retornados
    *grafoGD (nx.Graph): grafo con los datos de los switches. 0 si 
    hay errores
    *datosGD (array): array con los datos de los switches. 0 si
    hay errores
    *******************************************************************
    *******************************************************************
    """
 
    def ReaderDataGD_LargeScale(self, layer, toler, Graph_T3F_single):
        try:
            grafoGD = nx.Graph()
            datosGD = []
            
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")
            indexGroup = auxiliary_functions.getAttributeIndex(self, layer, "MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            
            GDs = layer.getFeatures()
            layer.startEditing() # Activa modo edición
            
            i = -1
            linea_gd = ""
            linea_lshp = ""
            for gd in GDs:
                i += 1
                dss_name = 'gd_ls_' + self.circuitName + '_' + str(i)
                nodo = self.CoordPointProcees(gd, toler)
                id_ = gd.id()
                
                # Coordenadas
                point = gd.geometry().asPoint()
                x1 = point[0]
                y1 = point[1]
                
                
                # Datos de lineas MT
                nodo_trafo = ""
                exists = Graph_T3F_single.has_node(nodo)
                if exists is True:
                    nodo_trafo = nodo
                    inf_trafo = Graph_T3F_single.nodes[nodo]
                    mv_mv = inf_trafo['MV_MV']
                    #bus
                    bus = inf_trafo['BUSBT']
                    if mv_mv == False:
                        aviso = "El transformador asociado al generador"
                        aviso += " distribuido a gran escala "
                        aviso +=  dss_name + " no es mv_mv"
                        self.mensaje_log_gral += aviso + "\n"
                else:
                    inf_trafo = ''
                    aviso = "No se encontró trafo MT/MT para GD a gran "
                    aviso += "escala " + dss_name
                    self.mensaje_log_gral += aviso + "\n"
                    #bus
                    bus = 'UNKNOWN'
                
                # Lectura de datos capa
                # Se lee el grupo de la capa
                try:
                    group = gd['MV_GROUP']
                except KeyError:
                    group = ''
                
                # Si no se encuentra se lee el dato de la línea asociada
                if group == None or group == '' or group == 'N/A':
                    # Caso en que haya alguna linea de MT
                    if inf_trafo:
                        group = inf_trafo['GRUPO_MV']
                    else:
                        group = 'N/A'
    
                # Tecnología
                tech = str(gd['TECH']).lower()
                
                #Loadshape
                daily = gd['DAILY']
                
                # Tensión 
                nomvolt = gd['NOMVOLT']
                tension = lineOperations.renameVoltage(nomvolt)
                tension_ll = tension['LVCode']['LL']
                # Capacidad nominal
                mva = gd['MVA']
                
                # xdp
                xdp = gd['XDP']
                
                # xdpp
                xdpp = gd['XDPP']
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                        "LAYER": layer, 'DSSName': dss_name,
                        "INDEXBUS1": indexBus1, 
                        'X1': x1, 'Y1': y1, 'GRUPO': group,
                        'TECH': tech, 'MVA': mva,
                        'DAILY': daily, 'TENSION': tension_ll,
                        'XDP': xdp, 'XDPP': xdpp,
                        'NODO_TRAFO': nodo_trafo, 'BUS': bus}
                
                datosGD.append(datos)
                grafoGD.add_node(nodo)
                grafoGD.nodes[nodo].update(datos)
                
                # Lineas dss
                temp = self.Write_GD_ls(datos)
                linea_gd += temp[0]
                linea_lshp += temp[1]
                
                # Cambios en capa
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                layer.changeAttributeValue(id_, indexGroup, group)
                layer.changeAttributeValue(id_, indexBus1, bus)
            
            # Escritura dss
            # Loadshapes
            filename_lshp = self.circuitName + '_LoadshapesGD_ls.dss'
            name_lshp = self.foldername + '/' + filename_lshp
            with open(name_lshp, 'w') as lshp_gd:
                lshp_gd.write(linea_lshp)

            # Archivo gd
            filename_gdls = self.circuitName + '_DGls.dss'
            line_out = '\nredirect ' + filename_gdls
            output_GDdss = self.foldername + '/' + filename_gdls
            
            linea_gd = "redirect " + filename_lshp + "\n" + linea_gd
            with open(output_GDdss , 'w') as file_gd:
                file_gd.write(linea_gd)
            return datosGD, grafoGD, filename_gdls
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que la capa de generación "
            aviso += "distribuida gran escala tenga el atributo "
            aviso += cause
            title = "QGIS2OpenDSS Error lectura GD larga escala"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de GD de gran "
            aviso += "escala. Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura generación "
            title += "distribuida gran escala"
            QMessageBox.critical(None, title, aviso)
            return 0, 0, 0
        finally:
            layer.commitChanges()

    
    
    """
    *******************************************************************
    *******************************************************************
    Función ReaderDataSwitches
    Se encarga de la lectura de la capa de switches. Crea el grafo de
    switches y además busca el grupo del switch en el grafo de líneas de
    MT si este dato no está en la capa.
    
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los switches
    *toler (float): tolerancia de cercanía entre elementos del circuito
    *grafoMT (nx.Graph): grafo de líneas de MT
    
    -Valores retornados
    *grafoSwitch (nx.Graph): grafo con los datos de los switches. 0 si 
    hay errores
    *datosSwitch (array): array con los datos de los switches. 0 si
    hay errores
    *******************************************************************
    *******************************************************************
    """
    
    def ReaderDataSwitches(self, layer, toler, grafoMT):
        try:
            grafoSwitch = nx.Graph()
            datosSwitch = []
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")
            indexGroup = auxiliary_functions.getAttributeIndex(self, layer, "MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexBus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            indexBusInt = auxiliary_functions.getAttributeIndex(self, layer, "bus_int")

            switches = layer.getFeatures()
            layer.startEditing() # Activa modo edición

            i = -1
            for switch in switches:
                i += 1
                dss_name = 'SWT_' + self.circuitName + '_' + str(i)
                nodo = self.CoordPointProcees(switch, toler)
                id_ = switch.id()

                # Coordenadas
                point = switch.geometry().asPoint()
                x1 = point[0]
                y1 = point[1]

                # Datos de lineas MT
                exists = grafoMT.has_node(nodo)
                if exists is True:
                    nodos = [n for n in grafoMT.neighbors(nodo)]
                    nodo2 = nodos[0] # Sólo debería tener un nodo vecino
                    inf_line_MT = grafoMT[nodo][nodo2]
                else:
                    inf_line_MT = ''
                    aviso = "No se encontro linea MT para switch "
                    aviso += dss_name
                    self.mensaje_log_gral += aviso + "\n"
                
                # Lectura de datos capa
                # Se lee el grupo de la capa
                try:
                    group = switch['MV_GROUP']
                except KeyError:
                    group = ''
                
                # Si no se encuentra se lee el dato de la línea asociada
                if group == None or group == '' or group == 'N/A':
                    #Caso en que haya alguna linea de MT
                    if inf_line_MT != '':
                        group = inf_line_MT['MV_GROUP']
                    else:
                        group = 'N/A'

                # Normalmente cerrado
                NC = str(switch['NC']).lower()                
                if (NC == "yes" or NC == "si" or NC == 'sí' or NC == 'y'
                    or NC == 's' or NC == '1' or NC == 't'
                    or NC == 'true'):
                    NC = True
                else:
                    NC = False
                    dss_name = None
                    # Se elimina el nodo del grafo MT y guardan datos en NULL
                    if exists is True:                        
                        grafoMT.remove_node(nodo)

                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                         "LAYER": layer, 'DSSName': dss_name,
                         "INDEXBUS1": indexBus1, "INDEXBUS2": indexBus2,
                         "INDEXBUS_INT": indexBusInt,
                         'X1': x1, 'Y1': y1, 'GRUPO': group, 'NC': NC}
                
                # Sólo se debe escribir en el grafo si es NC
                if NC is True:
                    datosSwitch.append(datos)
                    grafoSwitch.add_node(nodo)
                    grafoSwitch.nodes[nodo].update(datos)
                
                # Cambios en capa
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                layer.changeAttributeValue(id_, indexGroup, group)
                layer.changeAttributeValue(id_, indexBus1, NULL)
                layer.changeAttributeValue(id_, indexBus2, NULL)
                layer.changeAttributeValue(id_, indexBusInt, NULL)
            return datosSwitch, grafoSwitch
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que la capa de switches tenga "
            aviso += "el atributo " + cause
            title = "QGIS2OpenDSS Error lectura switches"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de switches."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura switches"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        finally:
            layer.commitChanges()

   
    """
    *******************************************************************
    *******************************************************************
    Función ReaderDataReclosers
    Se encarga de la lectura de la capa de reclosers. Crea el grafo de
    switches y además busca el grupo del switch en el grafo de líneas de
    MT si este dato no está en la capa.
    
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los reclosers
    *toler (float): tolerancia de cercanía entre elementos del circuito
    *grafoMT (nx.Graph): grafo de líneas de MT
    
    -Valores retornados
    *grafoReclosers (nx.Graph): grafo con los datos de los switches. 
    0 si hay errores
    *datosreclosers (array): array con los datos de los switches. 0 si
    hay errores
    *******************************************************************
    *******************************************************************
    """
    
    def ReaderDataReclosers(self, layer, toler, grafoMT):
        try:
            grafoReclosers = nx.Graph()
            datosreclosers = []
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")
            indexGroup = auxiliary_functions.getAttributeIndex(self, layer, "MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexBus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")
            
            reclosers = layer.getFeatures()
            layer.startEditing() # Activa modo edición
            
            i = -1
            for recloser in reclosers:
                i += 1
                dss_name = 'recloser_' + self.circuitName + '_' + str(i)
                nodo = self.CoordPointProcees(recloser, toler)
                id_ = recloser.id()
                
                # Coordenadas
                point = recloser.geometry().asPoint()
                x1 = point[0]
                y1 = point[1]
                
                
                # Datos de lineas MT
                exists = grafoMT.has_node(nodo)
                if exists is True:
                    nodos = [n for n in grafoMT.neighbors(nodo)]
                    nodo2 = nodos[0] # Sólo debería tener un nodo vecino
                    inf_line_MT = grafoMT[nodo][nodo2]
                else:
                    inf_line_MT = ''
                    aviso = "No se encontro linea MT para recloser "
                    aviso += dss_name
                    self.mensaje_log_gral += aviso + "\n"
                
                # Lectura de datos capa
                # Se lee el grupo de la capa
                try:
                    group = recloser['MV_GROUP']
                except KeyError:
                    group = ''
                
                # Si no se encuentra se lee el dato de la línea asociada
                if group == None or group == '' or group == 'N/A':
                    # Caso en que haya alguna linea de MT
                    if inf_line_MT != '':
                        group = inf_line_MT['MV_GROUP']
                    else:
                        group = 'N/A'
                
                # Normalmente cerrado
                NC = str(recloser['NC']).lower()                
                if (NC == "yes" or NC == "si" or NC == 'sí'
                    or NC == 'y' or NC == 's' or NC == '1'
                    or NC == 't' or NC == 'true'):
                    NC = True
                else:
                    NC = False
                    dss_name = None
                    # Se elimina el nodo del grafo MT y guardan datos en NULL
                    if exists is True:                        
                        grafoMT.remove_node(nodo)
                
                # Ground Delayed
                try:
                    grd_d = recloser['GRD_D']
                    if grd_d == None or grd_d == '':
                        grd_d = None
                except KeyError:
                    grd_d = None
                
                # Phase Delayed    
                try:
                    ph_d = recloser['PH_D']
                    if ph_d == None or ph_d  == '':
                        ph_d  = None
                except KeyError:
                    ph_d  = None
                
                # Atributos no opcionales
                # Ground Fast
                try:
                    grd_f = recloser['GRD_F']
                    if grd_f == None or grd_f == '':
                        grd_f = None
                except KeyError:
                    grd_f = None
                
                # Ground Inst
                try:
                    grd_i = recloser['GRD_I']
                    if grd_i == None or grd_i == '':
                        grd_i = '100'
                except KeyError:
                    grd_i = '100'
                
                # Ground Trip
                try:
                    grd_trip = recloser['GRD_TRIP']
                    if grd_trip == None or grd_trip == '':
                        grd_trip = '100'
                except KeyError:
                    grd_trip = '100'
                
                # Phase Fast
                try:
                    ph_f = recloser['PH_F']
                    if ph_f == None or ph_f == '':
                        ph_f = None
                except KeyError:
                    ph_f = None

                # Phase Inst
                try:
                    ph_i = recloser['PH_I']
                    if ph_i == None or ph_i == '':
                        ph_i = '100'
                except KeyError:
                    ph_i = '100'
                
                # Phase Trip
                try:
                    ph_trip = recloser['PH_TRIP']
                    if ph_trip == None or ph_trip == '':
                        ph_trip = '100'
                except KeyError:
                    ph_trip = '100'

                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                         "LAYER": layer, 'DSSName': dss_name,
                         "INDEXBUS1": indexBus1, "INDEXBUS2": indexBus2,
                         'X1': x1, 'Y1': y1, 'GRUPO': group, 'NC': NC,
                         'GRD_D': grd_d, 'GRD_F': grd_f,
                         'GRD_I': grd_i, 'GRD_TRIP': grd_trip,
                         'PH_D': ph_d, 'PH_F': ph_f,
                         'PH_I': ph_i, 'PH_TRIP': ph_trip}
                
                # Sólo se debe escribir en el grafo si es NC
                if NC is True:
                    datosreclosers.append(datos)
                    grafoReclosers.add_node(nodo)
                    grafoReclosers.nodes[nodo].update(datos)
                
                # Cambios en capa
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                layer.changeAttributeValue(id_, indexGroup, group)
                layer.changeAttributeValue(id_, indexBus1, NULL)
                layer.changeAttributeValue(id_, indexBus2, NULL)
            return datosreclosers, grafoReclosers
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que la capa de reconectadores  "
            aviso += "tenga el atributo " + cause
            title = "QGIS2OpenDSS Error lectura reconectadores"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de reconectadores."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura reconectadores"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        finally:
            layer.commitChanges()
       
   
   
    """
    *******************************************************************
    *******************************************************************
    Función ReaderDataFuses
    Se encarga de la lectura de la capa de fusibles. Crea el grafo de
    switches y además busca el grupo del switch en el grafo de líneas de
    MT si este dato no está en la capa.
    
    -Parámetros de entrada:
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los fusibles
    *toler (float): tolerancia de cercanía entre elementos del circuito
    *grafoMT (nx.Graph): grafo de líneas de MT
    
    -Valores retornados
    *grafoFuses (nx.Graph): grafo con los datos de los switches. 0 si 
    hay errores
    *datosFuses (array): array con los datos de los switches. 0 si
    hay errores
    *******************************************************************
    *******************************************************************
    """
    
    def ReaderDataFuses(self, layer, toler, grafoMT):
        try:
            grafoFuses = nx.Graph()
            datosFuses = []
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer, "DSSName")
            indexGroup = auxiliary_functions.getAttributeIndex(self, layer, "MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer, "bus1")
            indexBus2 = auxiliary_functions.getAttributeIndex(self, layer, "bus2")

            fusibles = layer.getFeatures()
            layer.startEditing() # Activa modo edición
            
            i = -1
            for fuse in fusibles:
                i += 1
                dss_name = 'fuse_' + self.circuitName + '_' + str(i)
                nodo = self.CoordPointProcees(fuse, toler)
                id_ = fuse.id()
                
                # Coordenadas
                point = fuse.geometry().asPoint()
                x1 = point[0]
                y1 = point[1]
                
                
                # Datos de lineas MT
                exists = grafoMT.has_node(nodo)
                if exists is True:
                    nodos = [n for n in grafoMT.neighbors(nodo)]
                    nodo2 = nodos[0] # Sólo debería tener un nodo vecino
                    inf_line_MT = grafoMT[nodo][nodo2]
                else:
                    inf_line_MT = ''
                    aviso = "No se encontro linea MT para fusible "
                    aviso += dss_name
                    self.mensaje_log_gral += aviso + "\n"
                
                # Lectura de datos capa
                # Se lee el grupo de la capa
                try:
                    group = fuse['MV_GROUP']
                except KeyError:
                    group = ''
                
                # Si no se encuentra se lee el dato de la línea asociada
                if group == None or group == '' or group == 'N/A':
                    # Caso en que haya alguna linea de MT
                    if inf_line_MT != '':
                        group = inf_line_MT['MV_GROUP']
                    else:
                        group = 'N/A'

                # Normalmente cerrado
                NC = str(fuse['NC']).lower()
                if (NC == "yes" or NC == "si" or NC == 'sí'
                    or NC == 'y' or NC == 's' or NC == '1'
                    or NC == 't' or NC == 'true'):
                    NC = True
                else:
                    NC = False
                    dss_name = None
                    # Se elimina el nodo del grafo MT y guardan datos en NULL
                    if exists is True:                        
                        grafoMT.remove_node(nodo)
                # Curva
                try:
                    curve = fuse['CURVE']
                    if curve == None or curve == '':
                        curve = '10T_CLEARING'
                except Exception:
                    curve = '10T_CLEARING'
               
               # Corriente
                try:
                    ic_current = fuse['RATED_C']
                    if ic_current == None or ic_current == '':
                        ic_current = '30'
                    else:
                        ic_current = str(ic_current)
                except Exception:
                    ic_current = '30'
                
                
                datos = {"INDEXDSS": indexDSS, 'ID': id_,
                        "LAYER": layer, 'DSSName': dss_name,
                        "INDEXBUS1": indexBus1, "INDEXBUS2": indexBus2,
                        'X1': x1, 'Y1': y1, 'GRUPO': group, 'NC': NC,
                        'RATED_CURRENT': ic_current,
                        'FUSE_CURVE': curve}
                # Solo se debe crear en el grago  en caso de que sea NC
                if NC is True:
                    datosFuses.append(datos)
                    grafoFuses.add_node(nodo)
                    grafoFuses.nodes[nodo].update(datos)
                
                # Cambios en capa
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                layer.changeAttributeValue(id_, indexGroup, group)
                layer.changeAttributeValue(id_, indexBus1, NULL)
                layer.changeAttributeValue(id_, indexBus2, NULL)
            return datosFuses, grafoFuses
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique que se la capa de fusibles "
            aviso += "tenga el siguiente atributo " + cause
            title = "QGIS2OpenDSS Error lectura fusibles"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de fusibles."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS Error lectura fusibles"
            QMessageBox.critical(None, title, aviso)
            return 0, 0
        finally:
            layer.commitChanges()
            
    """
    *******************************************************************
    *******************************************************************
    Función ReaderData_Capacitors
    Se encapi de la lectura de la capa de GDs de gran escala. Además
    escribe el dss correspondiente a este tipo de datos
    
    -Parámetros de entrada:
    *layer (QgsVectorLayer): capa en GIS de los GDs
    *toler (float): tolerancia de cercanía entre elementos del circuito
    
    -Valores retornados
    *graph_cap (nx.Graph): grafo de capacitores. 0 si hay errores
    *******************************************************************
    *******************************************************************
    """
    
    def ReaderDataCapacitors(self, layer, toler):
        try:
            grafoCap = nx.Graph()
            indexDSS = auxiliary_functions.getAttributeIndex(self, layer,
                                                             "DSSName")
            indexGroup = layer.dataProvider().fieldNameIndex("MV_GROUP")
            indexBus1 = auxiliary_functions.getAttributeIndex(self, layer,
                                                              "bus1")
    
            capacitors = layer.getFeatures()
            i = -1
            for capi in capacitors:
                point = capi.geometry().asPoint()
                i += 1
                # MV_GROUP
                if indexGroup == -1:
                    group = 'N/A'
                else:
                    group = str(capi['MV_GROUP'])

                nodo = self.CoordPointProcees(capi, toler)

                dss_name = "cap" + str(i)
                # NOMVOLT
                nomvolt = capi['NOMVOLT']
                tension = lineOperations.renameVoltage(nomvolt)
                tension_ll = tension['LVCode']['LL']
                tension_ln = tension['LVCode']['LN']
                config = tension['config']

                # KVAR
                try:
                    kvar = capi['KVAR']
                    kvar = str(float(kvar))
                    
                except ValueError:
                    msg = self.print_error()
                    aviso = "El valor " + str(kvar) + " suministrado en el "
                    aviso += " atributo 'KVAR' no es un valor numérico. "
                    aviso += "Favor corrija y vuelva a ejecutar."
                    self.mensaje_log_gral += aviso + "\n"
                    title = "QGIS2OpenDSS error lectura capacitores"
                    QMessageBox.critical(None, title, aviso)
                    return 0

                # Fases
                phasedesig = capi['PHASEDESIG']
                x = phaseOperations.renamePhase(phasedesig)
                service = x.get('phaseCodeODSS') # define código de OpenDSS
                phases = x.get('phaseNumberTraf')
                
                if str(phases) == '3':
                        kvar = str(float(kvar)/3)
                
                # Coordenadas
                x1 = point[0]
                y1 = point[1]
    
                datos = {"INDEXDSS": indexDSS, 'idx_bus1': indexBus1,
                         'idx_group': indexGroup, 'ID': capi.id(),
                         "LAYER": layer, "nodo": nodo, 'X1': x1,
                         'Y1': y1, 'GRUPO': group, 'KVAR': kvar,
                         'CONF': config, 'SERVICE': service,
                         'PHASES': phases,
                         'VOLT_LL': tension_ll, 'VOLT_LN': tension_ln,
                         'DSS_NAME': dss_name}
                grafoCap.add_node(nodo)
                grafoCap.nodes[nodo].update(datos)
            return grafoCap
        except KeyError as e:
            cause = e.args[0]
            msg = self.print_error()
            aviso = "Favor verifique la capa de capacitores "
            aviso += "tenga el atributo " + cause
            title = "QGIS2OpenDSS error lectura capacitores"
            QMessageBox.critical(None, title, aviso)
            return 0
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la lectura de las capas "
            aviso += "de capacitores de media tensión."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS error lectura capacitores"
            QMessageBox.critical(None, title, aviso)
            return 0

    """
    *******************************************************************
    *******************************************************************
    Función WriteCapacitors
    Se encapi de la lectura de la capa de GDs de gran escala. Además
    escribe el dss correspondiente a este tipo de datos
    
    
    -Parámetros de entrada:
    *graph_cap (nx.Graph): grafo de capacitores
    *self.circuitName (str): nombre del circuito
    *layer (QgsVectorLayer): capa en GIS de los capacitores
    
    -Valores retornados
    1 si finaliza exitosamente. 0 si hay errores
    *******************************************************************
    *******************************************************************
    """
    
    def WriteCapacitors(self, layer, graph_cap):
        try:
            indexBus1 = layer.dataProvider().fieldNameIndex("bus1")
            indexDSS = layer.dataProvider().fieldNameIndex("DSSName")
            layer.startEditing()  # Activa modo edición
            linea_cap = ""
            for cap_ in graph_cap.nodes(data=True):
                cap = cap_[1]
                
                id_ = cap['ID']
                group = str(cap['GRUPO'])
                kvar = str(cap['KVAR'])
                conex = str(cap['CONF'])
                service = str(cap['SERVICE'])
                phases = str(cap['PHASES'])
                tens_ll = str(cap['VOLT_LL'])
                tens_ln = str(cap['VOLT_LN'])
                bus1 = str(cap["BUS"])
                dss_name = cap["DSS_NAME"]
                layer.changeAttributeValue(id_, indexBus1, bus1)
                layer.changeAttributeValue(id_, indexDSS, dss_name)
                
                linea_cap += "New Capacitor." + dss_name + " bus1="
                linea_cap += bus1 + service + " phases=" + phases
                linea_cap += ' kVAr=' + kvar + ' kV=' + tens_ln 
                linea_cap += ' conn=' + conex + " !group=" + group + "\n"
    
            # Archivo capuladores
            filename = self.circuitName + '_Capacitors.dss'
            line_out = '\nredirect ' + filename
            output_Regdss = self.foldername + '/' + filename
    
            with open(output_Regdss, 'w') as file_cap:
                file_cap.write(linea_cap)
    
            # Escritura master
            line_out = '\nredirect ' + filename
            self.output_filesQGIS2DSS.write(line_out)
            return 1
        except Exception:
            msg = self.print_error()
            aviso = "Ocurrió un error en la escritura de los archivos "
            aviso += "de capacitores."
            aviso += "Para más detalles:\n" + msg
            title = "QGIS2OpenDSS escritura capacitores"
            QMessageBox.critical(None, title, aviso)
            return 0
        finally:
            layer.commitChanges()

    

    """
    Esta función encuentra el primer último 0 en determinada columna
    Retorna un -1 si se encuentra en la última posición, un x + 1
     caso contrario, donde x es el número de fila donde se encontró
    
    Parámetros:
    *column (columna de un dataframe, pero también puede ser un vector
    
    Valores retornados:
    *t (int): número de columna + 1 en la que fue encontrado
    el primer cero
    """
    def find_t(self, column):
        n_columnas = len(column)
        # se recorren al revés la columnas
        for j in reversed(range(n_columnas)):
            dato = column[j]
            if dato == 0 and j == n_columnas - 1:
                return -1 
            elif dato == 0:
                return j + 1 

   
   
    """
    # ********************************************************
    # ********************************************************
    ############################### WriteFileEV ###############################
    # ********************************************************
    # ********************************************************    
    Función encargada de crear el archivo dss que corresponde a los VEs. 
    También crea el archivo con los perfiles de carga.
    
    Parámetros de entrada:
    *toler (int): valor utilizado para ver si las coordenadas de ciertos datos coinciden
    *grafoCAR (networkx graph): grafo de cargas de BT
    *grafoEV (networkx undirected graph): grafo de vehículos eléctricos    
    
    Otros parámetros utilizados:
    *self.foldername (str): carpeta donde se guardan los archivos dss
    *self.circuitName (str): nombre del circuito, determinado por el usuario
    *self.output_filesQGIS2DSS (file): archivo de salida de QGIS2OpenDSS
    *self.name_loadshapes (str): path relativa del archivo donde se encuentran los perfiles de carga de VEs
    *self.evs_loadshapes (dataframe): dataframe donde se encuentran los perfiles de carga de los EV
    
    Valores retornados:
    0: si hubo errores
    1: si la escritura finalizó exitosamente
    """
    def WriteFileEV(self, toler, grafoCAR, grafoEV):
        try:
            name_dss = self.circuitName + "_EV.dss"
            path_csv = self.name_loadshapes.split("/")#path relativo del csv
            name_csv = path_csv[1] #Nombre del archivo csv
            folder_csv = path_csv[0] #Nombre de la carpeta donde está el csv
            
            name_loadshapes = folder_csv + "/" + self.circuitName + "_EVLoadshapes.dss" #Path relativo de los loadshapes dss
            filename_ev_dss = self.foldername + '/' + name_dss #Path absoluto
            
            filename_loadshapes = self.foldername + "/" + name_loadshapes
            print(filename_ev_dss, ' ', filename_loadshapes, ' ', name_csv)
            
            linea_ev = ""
            linea_loadshape = ""
            
            columns_evs_loadshapes = self.evs_loadshapes.columns            
            contador = -1 #Ya que se aumenta al principio, no al final
            
            linea = "redirect " +  name_loadshapes + "\n"
            linea_ev += linea
            #file_ev.write(linea)
            
            for ve in grafoEV.nodes(data=True):
                contador += 1
                ve_ = ve[1]
                #Datos de BT
                x1 = str(ve_['X1'])
                y1 = str(ve_['Y1'])
                x1_ = int(float(x1)/toler)
                y1_ = int(float(y1)/toler)
                nodo1 = (x1_, y1_)
                inf_load_BT = grafoCAR.nodes[nodo1]            
                bus = inf_load_BT['BUS']
                
                #Datos de VEs
                name_ev_dss = ve_['DSSName']
                kW = ve_['kW']
                kWh = ve_['kWh']
                kV = ve_['NOMVOLT'] 
                conns = ve_['CONNS']
                soc = ve_['SOC_INIC']
                grupo = ve_['GRUPO']
                #Nombre del daily curve
                daily_curve = str("EV_" + str(contador))
                cant_soc = len(soc)
                extra_sentence = "!Group=" + str(grupo)
                if len(soc)!= 1: #Caso en que se tengan varios estados de carga iniciales
                    #Obtener t (para soc)(donde se da el último 0 en la columna)
                    name_column = columns_evs_loadshapes[contador]
                    columna = self.evs_loadshapes[name_column]
                    t = self.find_t(columna)
                    for i in range(1, cant_soc):
                        extra_sentence += " !stored=" + str(soc[i])+ ' ' 
                    extra_sentence += "!t=" + str(t)+ ' '
                else:
                    extra_sentence += " !t=-1"
                
                sentence = str("new storage." + name_ev_dss)
                sentence += " bus1=" + bus + conns
                sentence += " phases=1 model=1"
                sentence += " kW=" + str(kW)
                sentence += " kV=" + str(kV)
                sentence += " pf=0.98"
                sentence += " kWrated=" + str(kW)
                sentence += " kWhrated=" + str(kWh)
                sentence += " %reserve=0"
                sentence += " %stored=" + str(soc[0])
                sentence += " %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW daily=" + daily_curve
                sentence += ' ' + extra_sentence + "\n"
                #file_ev.write(sentence)
                linea_ev += sentence
                
                sentence_loadshape = "New Loadshape." + daily_curve  + " npts=96 mInterval=15 mult=(file="
                sentence_loadshape += name_csv + ", col=" + str(contador + 1)+ ", header=no)useactual=no\n"
                linea_loadshape += sentence_loadshape
                #file_loadshape.write(sentence_loadshape)
            
            #se escriben y cierran los archivos
            
            with open(filename_ev_dss, 'w')as file_ev:
                file_ev.write(linea_ev)            
            with open(filename_loadshapes, 'w')as file_loadshape:
                file_loadshape.write(linea_loadshape)

            #Escritura master
            self.output_filesQGIS2DSS.write('\nredirect ' + name_dss)

            return 1
        except Exception:
            msg = "Hubo un error al escribir el archivo de VE.\n"
            msg += "Para más información corrija el siguiente error\n"
            msg += self.print_error()
            QMessageBox.critical(None, "QGIS2OpenDSS escritura VE", msg)
            return 0
        
    """
    # ********************************************************
    # ********************************************************
    ############################### WriteFileEV ###############################
    # ********************************************************
    # ********************************************************    
    Función encargada de crear el archivo dss que corresponde a los VEs. 
    También crea el archivo con los perfiles de carga.
    
    Parámetros de entrada:
    *toler (int): valor utilizado para ver si las coordenadas de ciertos datos coinciden
    *grafoBuses (networkx graph): grafo del plantel de buses
    *busBT_List (dict): diccionario con los buses de BT
    
    Otros parámetros utilizados:
    *self.foldername (str): carpeta donde se guardan los archivos dss
    *self.circuitName (str): nombre del circuito, determinado por el usuario
    *self.output_filesQGIS2DSS (file): archivo de salida de QGIS2OpenDSS
    
    Valores retornados:
    0: si hubo errores
    1: si la escritura finalizó exitosamente
    """
    def WriteFileBuses(self, toler, grafoBuses, busBT_List):
        try:
            name_dss = self.circuitName + "_PlantelesBuses.dss"
            filename_bus_dss = self.foldername + '/' + name_dss #Path absoluto
            file_bus = open(filename_bus_dss, 'w')
            
            name_bus = self.circuitName + "_StorageBuses.dss"
            filename_bus_real = self.foldername + '/' + name_bus #Path absoluto
            file_bus_real = open(filename_bus_real, 'w')
            
            #Extra loadshapes (temporal)
            name_loadshape_dss = self.circuitName + "_LoadshapesBuses.dss"
            filename_loadshape_buses = self.foldername + '/' + name_loadshape_dss #Path absoluto
            file_loadshape_bus = open(filename_loadshape_buses, 'w')
            sentence_loadshape = ""
            
            contador = -1
            
            for bus in grafoBuses.nodes(data=True):                
                bus_ = bus[1]
                #Datos de BT
                x1 = str(bus_['X1'])
                y1 = str(bus_['Y1'])
                nodo = bus[0]
                x1_ = int(float(x1)/toler)
                y1_ = int(float(y1)/toler)
                nodo1 = (x1_, y1_)
                
                #Datos generales
                datos_trafo = bus_['DICT TRAFO']
                datos_carga_bt = bus_['DICT LOAD BT']
                datos_buses = bus_['DICT BUSES']
                nombre_plantel = bus_['NOMBRE PLANTEL']
                group = str(bus_['GRUPO'])
                dssname = str(bus_['DSSName'])
                plantel = bus_['NOMBRE PLANTEL']
                
                #Trafo
                cantFases = '3'                
                kVA = str(datos_trafo['KVA'])
                normhkva = " normhkva=" + kVA
                kV_LowLL = datos_trafo["LOADVOLT"]
                kV_LowLN = datos_trafo["LOADVOLTLN"]
                kV_MedLL = datos_trafo['VOLTMTLL']
                kV_MedLN = datos_trafo['VOLTMTLN']
                busMV = str(bus_['BUS'])
                
                tap = datos_trafo['TAPS']
                if (datos_trafo['CONME'] == 'Y'):
                    confMV = 'wye'
                else:
                    confMV = 'delta'
                if (datos_trafo['CONBA'] == 'Y'):
                    confLV = 'wye'
                else:
                    confLV = 'delta'
                phaseMV = datos_trafo['PHASE']
                impedance = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get('Z')
                noloadloss = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get('Pnoload')
                imag = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get('Im')
                trafName = self.circuitName + cantFases + 'P_' + dssname
                
                #Bus al que se conectan los buses y carga BT
                busLV = "barralv_" + dssname
                datos_nodo = {'bus': busLV, 'X': x1, 'Y': y1, "GRAFO": grafoBuses,
                              "VOLTAGELN": str(kV_LowLN), 'PHASES': ".1.2.3",
                              'GRUPO': group}               
                busBT_List[nodo] = datos_nodo
                
                #Línea trafo
                trafo_line = 'new transformer.' + trafName + ' phases=3 windings=2 '
                trafo_line += noloadloss + ' ' + imag + ' buses=[' + busMV + '.1.2.3 '
                trafo_line += busLV + '.1.2.3]' + ' conns=[' + confMV + ' ' + confLV + ']'
                trafo_line += ' kvs=[' + kV_MedLL + ' ' +  kV_LowLL + ']'
                trafo_line += ' kvas=[' + kVA + ' ' + kVA + '] ' + impedance + ' Taps=[' + tap + ', 1]'
                trafo_line += normhkva + ' !GroupMV=' + group + "\n"
                line_monitor = "new monitor.Mon" + trafName + " Element=transformer."
                line_monitor += trafName + " Terminal=1 Mode=1\n"
                trafo_line += line_monitor
                #Bus
                kV = kV_LowLN
                
                i = -1
                bus_line = ""
                
                sentence_loadshape = ""
                monitor_line = ""
                for dato in datos_buses:
                    contador += 1
                    i += 1
                    
                    #Recupera datos cargador
                    kw_carg = datos_buses[dato]['KW']
                    
                    #Datos del bus
                    kWh = datos_buses[dato]['KWHBATTERY']
                    soc = datos_buses[dato]['SOCi']
                    csv_loadshapes = datos_buses[dato]['CSV_NAME']
               
                    
                    name_bus_dss = dssname + "_" + str(i)
                    
                    conns = ".1.2.3"
                    cantFases = "3"
                        
                    #Nombre del daily curve
                    daily_curve = "daily_" + dssname + str(contador)
                    
                    extra_sentence = "daily=" + daily_curve + " !GroupMV=" + str(group)
                    
                    #Línea bus
                    bus_line += "new storage." + name_bus_dss
                    bus_line += " bus1=" + busLV + conns
                    bus_line += " phases=3 model=1"
                    bus_line += " kW=" + str(kw_carg)
                    bus_line += " kV=" + str(kV_LowLL)
                    bus_line += " pf=0.98"
                    bus_line += " kWrated=" + str(kw_carg)
                    bus_line += " kWhrated=" + str(kWh)
                    bus_line += " %reserve=0"
                    bus_line += " %stored=" + str(soc)
                    bus_line += " %EffCharge=0.9 %IdlingkW=0 enabled=y dispmode=FOLLOW"
                    bus_line += ' ' + extra_sentence + "\n"
                    monitor_line += "new monitor.mon_" + name_bus_dss + " element=storage."
                    monitor_line += name_bus_dss + " mode=1\n"
                    
                    #Temporal
                    sentence_loadshape += "New Loadshape." + daily_curve  + " npts=96 mInterval=15 mult=(file="
                    sentence_loadshape += csv_loadshapes + ", col=" + str(contador + 1)+ ", header=no)useactual=no\n"
                
                #Carga BT
                kW = str(datos_carga_bt['kW'])
                conns = str(datos_carga_bt['CONNS'])
                conf = str(datos_carga_bt['CONF'])
                model = str(datos_carga_bt['MODEL'])
                kWhmonth = str(datos_carga_bt['kWh'])
                loadclass = str(datos_carga_bt['class'])
                daily = str(datos_carga_bt['CURVASIG'])
                pf = "0.9"
                loadName = "n_LV_" + dssname
                btload_line = "new load." + loadName + " bus1=" + busLV + conns + " kV=" 
                btload_line += str(kV_LowLL) + " model=" + model + " conn=" + conf
                btload_line += " kW=" + kW + " pf=" + pf + " status=variable phases=" +  cantFases + ' ' + daily
                btload_line += " !kWh="  + kWhmonth + " class=" + loadclass + " !GroupMV=" + group + "\n"
                
                sentence = trafo_line + btload_line
                file_bus_real.write(bus_line + monitor_line)
                file_bus.write(sentence)
                
                #Temporal
                file_loadshape_bus.write(sentence_loadshape)
                
                
                #Monitorear carga de buses
                
                
                
            #Temporal
            file_bus.write('redirect ' + name_loadshape_dss + "\n")
                
            file_bus_real.close()
            file_bus.close()
            
            #Temporal
            file_loadshape_bus.close()           
            
            #Escritura master            
            self.output_filesQGIS2DSS.write('\nredirect ' + name_dss)
            return busBT_List
        except Exception:
            msg = "Hubo un error al escribir el archivo de buses.\n"
            msg += "Para más información revise el siguiente error:\n"
            msg += self.print_error()
            title = "QGIS2OpenDSS escritura buses"
            QMessageBox.critical(None, title, msg)
            return 0
       
    
    
    # ********************************************************
    # ********************************************************
    # ********************************************************
    # ********************************************************
    """    
    Función encargada de realizar funciones según el tipo de estudio seleccionado para VEs.
    Es llamada dentro de la lectura de VEs. Se encarga de determinar la lista de
    VEs que se asignarán en la simulación 
    
    Parámetros de entrada:
    *studytype:
    *evs_aux
    *grafoCAR
    *toler
    *percent_ve
    """
    # ********************************************************
    # ********************************************************
    # ********************************************************
    # ********************************************************
    
    def VETypeStudy(self, studytype, evs_aux, grafoCAR, toler, percent_ve = 0):
        n_carros = 0
        
        #Tipo de estudio en que se proporciona la capa de VES
        if studytype == "layer":
            for ev in evs_aux:
                DSSName = "EV_" + self.circuitName + "_" + str(n_carros)
                DSSName_List.append(DSSName)
                n_carros += 1 
            print("n_carros = ", n_carros)
        
        #Tipo de estudio random
        if studytype == "random":
            for ev in evs_aux:
                DSSName = "EV_" + self.circuitName + "_" + str(n_carros)
                class_ = ev['class']
                n_carros += 1
                if class_ == "R":
                    DSSName_List.append(DSSName)#sólo deben utilizarse cargas residenciales
                
            percent_ve = float(percent_ve / 100)
            n_carros = int(n_carros*percent_ve)#cantidad de carros tomados en cuenta para el estudio con base en el porcentaje indicado por el usuario
            print("n_carros = ", n_carros)
            if n_carros <= 0:
                mensaje = "Favor introduzca el atributo SERVICE en "
                mensaje += "la capa de EV con el formato indicado en el manual"
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0
                
            DSSName_List = np.random.shuffle(DSSName_List)
            DSSName_List = DSSName_List[: n_carros] #se reajusta el vector aleatoriamente para que sólo seleccione la cantidad de VE seleccionada
        
        #Tipo de estudio por probabilidad
        if studytype == "prob":
            #Crear diccionario a partir del csv
            for ev in evs_aux:
                point = ev.geometry().asPoint() # Lee la geometria de la linea
                x1 = point[0]
                y1 = point[1]
                x1 = int(x1/toler)
                y1 = int(y1/toler)
                nodo1 = (x1, y1)
                try:
                    inf_load_BT = grafoCAR.nodes[nodo1]
                    kwh = inf_load_BT['kWh']
                    class_ = inf_load_BT['class']
                except Exception:
                    continue
                
                prob = 10 #asignar del csv (diccionario)
                n = 1000 #cantidad de iteraciones
                ve_enable = np.random.binomial(n, prob)
                
                if ve_enable == 1 and class_ == "R":
                    DSSName = "EV_" + self.circuitName + "_" + str(n_carros)
                    DSSName_List.append(DSSName)
                n_carros += 1
    
    # ********************************************************
    # ********************************************************
    # ********************************************************
    # ********************************************************
    #************ LECTURA DE CAPA DE CARROS ELÉCTRUCOS *******
    # ********************************************************
    # ********************************************************
    # ********************************************************
    # ********************************************************
    # ********************************************************
    def ReaderDataEV(self, layer, datosEV, grafoEV, kWhLVev, toler,
                     indexDSS, grafoBTTotal, grafoCAR, grafoBT):
        error = 0
        evs = layer.getFeatures() # Caracteristicas de la capa de evs.
        # Se necesita la cantidad de carros para obtener los 
        # datos aleatorios, en caso de requerirse
        n_carros = 0
        for ev in evs:
            n_carros += 1 
        print("n_carros = ", n_carros)
        
        #Se agrega el atributo DSSName si no existe en la capa respectiva
        names = layer.dataProvider().fields().names()
        if "DSSName" not in names:
            index_dss = layer.dataProvider().addAttributes([QgsField("DSSName", QVariant.String)])
            layer.updateFields()
        if "LV_GROUP" not in names:
            index_dss = layer.dataProvider().addAttributes([QgsField("LV_GROUP", QVariant.String)])
            layer.updateFields()
            
        #Llamar funcion datos aleatorios
        sample_wd, sample_we, sample_gnal, power_val, kwh_val = AnalizarEncuestas()
        evs_loadshapes, evs_power, evs_initial_soc, evs_kwh = CreacionPerfilesEV(power_val, kwh_val, total_cars =  n_carros)
        self.evs_loadshapes = evs_loadshapes #se utiliza como parámetro en la escritura de VEs
        path_loadshapes = self.foldername + "/profiles"
            
        #Crea la carpeta donde se crearán los perfiles aleatorios, si esta no existe
        if not os.path.exists(path_loadshapes):
            os.makedirs(path_loadshapes)
        
        self.name_loadshapes = "profiles/EV_profiles.csv" #Parámetro utilizado por la función que escribe archivo dss de VEs para saber donde están guardados los perfiles de VEs
        name_loadshapes = str(self.foldername + "/" + self.name_loadshapes)
        evs_loadshapes.to_csv(name_loadshapes, header=False, index=False)
        kw_random = list(evs_power.iloc[0].values)
        kwH_random = list(evs_kwh.iloc[0].values)
        
        contador = -1 #Porque se aumenta al inicio del for, no al final
        layer.startEditing()
        evs = layer.getFeatures()
        for ev in evs:
            contador = contador + 1            
            DSSName = "EV_" + self.circuitName + "_" + str(contador)
            
            nodo = self.CoordPointProcees(ev, toler)
            
            #Model: atributo opcional
            try:
                model = int(ev['MODEL'])
                if model >= 1 and model <= 8:
                    model = 1
                model = str(model)
            except Exception:
                model = "1"
                
            point = ev.geometry().asPoint() # Lee la geometria de la linea
            x1 = point[0]
            y1 = point[1]
            x1_ = int(x1/toler)
            y1_ = int(y1/toler)
            nodo1 = (x1_, y1_)
            
            try:
                inf_load_BT = grafoCAR.nodes[nodo1]
                kWh = inf_load_BT['kWh']
                
            except Exception:
                aviso = "El EV " + DSSName + " no se encuentra en el grafo de baja tensión. Verifique que se encuentre conectado\n"
                self.mensaje_log_gral += aviso
                
            try:
                group = inf_load_BT['LV_GROUP']
            except KeyError:
                group = 'N/A'
            
            #Lectura de datos de capa
            try:
                
                #CONNECTION
                connection = ev['SERVICE']
                connection = int(connection)
                connection = str(connection)
                
                if connection == "1":
                    conns = ".1"
                elif connection == "2":
                    conns = ".2"
                elif connection == "12":
                    conns = ".1.2"
                elif connection == "123":
                    conns = ".1.2.3"
                else:
                    mensaje = "Favor introduzca un código válido para "
                    mensaje += "el atributo SERVICE en la capa de EV"
                    QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                    return 0, 0, 0, 0
            except Exception:
                self.print_error()
                msg = "Favor introduzca el atributo SERVICE en la capa "
                msg += "de EV con el formato indicado en el manual"
                QMessageBox.critical(None, "QGIS2OpenDSS error BT", msg)
                return 0, 0, 0
            #Fin de datos de capa
            
            #NOMVOLT
            code_nomvolt = inf_load_BT['CODE_NOMVOLT']
            code_nomvolt = int(code_nomvolt)
            conf, nom_volt = self.GetNominalVoltBT(code_nomvolt, connection)
            
            if conf == 0: #Caso en que se haya introducido un código no válido para NOMVOLT
                mensaje = str("Introduzca un código válido para NOMVOLT en la capa de cargas BT")
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0
                
            #CLASS
            class_ = inf_load_BT['class']
            #Fin lectura de datos de cargas BT
            
            
            #Se llama a la función encargada de determinar si SERVICE tiene un tipo de cable correcto asociado
            self.CableAnalysis(ev, point, grafoBT, grafoACO, conns, toler)
            
            #Lectura de datos de capa
            
            #kWH
            try:
                kwHrated = float(ev['KWHBATTERY'])
                if kwHrated == None or kwHrated == "":
                    #Asignar datos aleatorios
                    kwHrated = float(kwH_random[contador])
                
            except Exception:
                #Asignar datos aleatorios
                kwHrated = float(kwH_random[contador])
            
            #kW
            try:
                kw = ev['KW']
                if kw == None or kw == "":
                    #Asignar datos aleatorios
                    kw = float(kw_random[contador])
            except Exception:
                #Asignar datos aleatorios
                kw = float(kw_random[contador])
            
            #Vector de estado de carga inicial (state of charge initial). Es un dato aleatorio siempre
            n_filas_soc = len(evs_initial_soc)
            vect_soc = []
            for k in range(n_filas_soc):
                data_soc = evs_initial_soc.iloc[n_filas_soc - k - 1][contador] #Para que asigne primero el último valor
                if data_soc != None:
                    data_soc = float(data_soc)
                    if np.isnan(data_soc)is False: 
                        vect_soc.append(data_soc)
            vect_soc.sort(reverse = True)#Se ordena el vector descendentemente
                        
            #Cambios en la capa
            idx_dss = ev.fieldNameIndex("DSSName")
            layer.changeAttributeValue(ev.id(), idx_dss, DSSName)
            idx_grup = ev.fieldNameIndex("LV_GROUP")
            layer.changeAttributeValue(ev.id(), idx_grup, group)
                
            
            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            
            
            datos = {"INDEXDSS": indexDSS, 'ID': ev.id(), "LAYER": layer, "nodo": nodo, 'X1': x1,
                     'Y1': y1, 'kWh': kwHrated, 'GRUPO': group, 'kW': kw, 'CONNS': conns,
                     'CONF': conf, 'CURVASIG': '', 'class': class_, 'NOMVOLT': nom_volt, 'MODEL': model, 'SOC_INIC': vect_soc, 'DSSName': DSSName}
            datosTotal = {"type": "EV"}
            datosEV.append(datos)
            grafoEV.add_node(nodo)
            grafoEV.nodes[nodo].update(datos)
            grafoBTTotal.add_node(nodo)
            grafoBTTotal.nodes[nodo].update(datosTotal)
        
        layer.commitChanges()
        return datosEV, grafoEV, grafoBTTotal

    # ##########################################
    # ##########################################
    def BusAsignationTraf(self, circuitName, grafo, busMT_List, busMTid, busBT_List, busBTid, tipo, grafoMT):
        graphNodes = list(grafo.nodes(data=True))
        for NODE in graphNodes:
            dataList = NODE[1]
            nodo = NODE[0]
            mv_mv = dataList['MV_MV']
            if mv_mv is False:
                BTbus = 'BUSLV' + circuitName + str(busBTid)
                grafo.nodes[nodo]["BUSBT"] = BTbus
                
                busBT_List[nodo] = {'bus': BTbus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafo, "VOLTAGELN": dataList["LOADVOLTLN"],
                                    'LOADCONNS': dataList['LOADCONNS'], 'GRUPO': dataList['GRUPO_LV']}
                busBTid += 1
            if nodo in busMT_List:  # Verifica si el nodo del transformador ya está en los nodos creados en MT
                bus_mt = busMT_List[nodo]["bus"]
                grafo.nodes[nodo]["BUSMT"] = bus_mt
                if mv_mv is True:
                    grafo.nodes[nodo]["BUSBT"] = bus_mt + "_tx"
                for secondNode in grafoMT[nodo]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                    dataLine = grafoMT[nodo][secondNode]
                    if phaseOperations.trafoPhaseMT(dataLine['PHASE'], dataList['PHASE'])== 0:
                        layer = grafo.nodes[nodo]["LAYER"]
                        indexPhase = auxiliary_functions.getAttributeIndex(self, layer, "PHASEDESIG")
                        indexPowerA = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEA")
                        indexPowerB = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEB")
                        indexPowerC = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEC")
                        grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPhase,
                                                                       dataLine['PHASEDESIG'])
                        ############# Solo se realiza la autocorreción si la línea y el transformador tienen la misma cantidad de fases ######
                        ###CORRECIÓN DE FASE DE TRANSFORMADOR
                        PowerA = float(dataList["KVA_FA"])
                        PowerB = float(dataList["KVA_FB"])
                        PowerC = float(dataList["KVA_FC"])

                        if (dataList['PHASE'] == ".1.2" and dataLine['PHASE'] == ".1.3")or (
                                dataList['PHASE'] == ".1.3" and dataLine['PHASE'] == ".1.2")or (
                                dataList['PHASE'] == ".2" and dataLine['PHASE'] == ".3")or (
                                dataList['PHASE'] == ".3" and dataLine['PHASE'] == ".2"):
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerB, PowerC)
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerC, PowerB)
                            grafo.nodes[nodo]["KVA_B"] = PowerC
                            grafo.nodes[nodo]["KVA_C"] = PowerB
                            aviso = "Conexión entre fases distintas corregida en ("
                            aviso += str(dataList["X1"])+ ',' + str(dataList["Y1"])
                            aviso += '). Línea MT con fase ' + str(dataLine['PHASE'])
                            aviso += ' y transformador con fase ' + str(dataList['PHASE']) + '\n'
                            self.mensaje_log_gral += aviso
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.nodes[nodo]["PHASE"] = dataLine['PHASE']
                        elif (dataList['PHASE'] == ".1.2" and dataLine['PHASE'] == ".2.3")or (
                                dataList['PHASE'] == ".2.3" and dataLine['PHASE'] == ".1.2")or (
                                dataList['PHASE'] == ".1" and dataLine['PHASE'] == ".3")or (
                                dataList['PHASE'] == ".3" and dataLine['PHASE'] == ".1"):
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerA, PowerC)
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerC, PowerA)
                            grafo.nodes[nodo]["KVA_A"] = PowerC
                            grafo.nodes[nodo]["KVA_C"] = PowerA
                            aviso = 'Conexión entre fases distintas corregida en ('
                            aviso += str(dataList["X1"])+ ',' + str(dataList["Y1"])
                            aviso += '). Línea MT con fase '
                            aviso += str(dataLine['PHASE']) + ' y transformador con fase '
                            aviso += str(dataList['PHASE']) + "\n"
                            self.mensaje_log_gral += aviso
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.nodes[nodo]["PHASE"] = dataLine['PHASE']
                        elif (dataList['PHASE'] == ".2.3" and dataLine['PHASE'] == ".1.3")or (
                                dataList['PHASE'] == ".1.3" and dataLine['PHASE'] == ".2.3")or (
                                dataList['PHASE'] == ".2" and dataLine['PHASE'] == ".1")or (
                                dataList['PHASE'] == ".1" and dataLine['PHASE'] == ".2"):
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerB, PowerA)
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPowerA, PowerB)
                            grafo.nodes[nodo]["KVA_B"] = PowerA
                            grafo.nodes[nodo]["KVA_A"] = PowerB
                            aviso = 'Conexión entre fases distintas corregida en ('
                            aviso += str(dataList["X1"]) + ',' + str(dataList["Y1"])
                            aviso += '). Línea MT con fase ' + str(dataLine['PHASE'])
                            aviso += ' y transformador con fase ' + str(dataList['PHASE']) + "\n"
                            self.mensaje_log_gral += aviso
                            grafo.nodes[nodo]["LAYER"].changeAttributeValue(grafo.nodes[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.nodes[nodo]["PHASE"] = dataLine['PHASE']
                        else:
                            aviso = 'Conexión entre fases distintas en ('
                            aviso += str(dataList["X1"])+ ',' + str(dataList["Y1"])
                            aviso += '). Línea MT con fase ' + str(dataLine['PHASE'])
                            aviso += ' y transformador con fase ' + str(dataList['PHASE']) + "\n"
                            self.mensaje_log_gral += aviso


            else:
                busMTid += 1
                bus = 'BUSMV' + circuitName + str(busMTid)
                busMT_List[nodo] = {"NPHAS": dataList["NPHAS"], 'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"],
                                    "GRAFO": grafo, "VOLTAGELL": dataList["VOLTMTLL"],
                                    "VOLTAGELN": dataList["VOLTMTLN"], "PHASES": dataList["PHASE"], 'GRUPO_MV': dataList['GRUPO_MV']}
                grafo.nodes[nodo]["BUSMT"] = bus
                
                if mv_mv is True:
                    grafo.nodes[nodo]["BUSBT"] = bus + "_tx"
                aviso = QCoreApplication.translate('dialog',
                                                   'Hay 1 transformador ')+ tipo + QCoreApplication.translate('dialog',
                                                                                                               ' sin red primaria: (')+ str(
                    dataList["X1"])+ ',' + str(dataList["Y1"])+ ')'
                QgsMessageLog.logMessage(aviso, QCoreApplication.translate('dialog', 'Alerta Transformadores'), Qgis.Warning)#QgsMessageLog.WARNING
        return grafo, busMT_List, busMTid, busBT_List, busBTid

    def BusAdapterLines(self, GRAFO, SOURCE, DATOS):
        nodo1_old = DATOS['nodo1']  # Recibe el nombre del bus de inicio de la linea
        nodo2_old = DATOS['nodo2']  # Recibe el nombre del bus de final de la linea
        X1Y1_old = (DATOS['X1'], DATOS['Y1'])
        X2Y2_old = (DATOS['X2'], DATOS['Y2'])
        EQUAL = False
        if SOURCE == "NULL":
            nodofrom = nodo1_old
            nodoto = nodo2_old
            X1Y1 = X1Y1_old
            X2Y2 = X2Y2_old
            connected = False
        else:
            try:
                dist1 = len(nx.shortest_path(GRAFO, source=SOURCE, target=nodo1_old))
                dist2 = len(nx.shortest_path(GRAFO, source=SOURCE, target=nodo2_old))
                connected = True
                if dist2 == dist1:
                    nodofrom = nodo1_old
                    nodoto = nodo2_old
                    X1Y1 = X1Y1_old
                    X2Y2 = X2Y2_old
                    EQUAL = True
                elif dist1 < dist2:
                    nodofrom = nodo1_old
                    nodoto = nodo2_old
                    X1Y1 = X1Y1_old
                    X2Y2 = X2Y2_old
                elif dist1 > dist2:
                    nodofrom = nodo2_old
                    nodoto = nodo1_old
                    X1Y1 = X2Y2_old
                    X2Y2 = X1Y1_old
            except Exception:
                nodofrom = nodo1_old
                nodoto = nodo2_old
                X1Y1 = X1Y1_old
                X2Y2 = X2Y2_old
                connected = False
                EQUAL = True
        return nodofrom, nodoto, connected, EQUAL, X1Y1, X2Y2

    def IslandIdentification(self, grafoBTTotal, grafoBT, grafoACO, grafoCAR):  ######SI NO FUNCIONA: IMPORTAR LOS GRAFOS A LA FUNCION.
        # iDENTIFICA CUAL ES EL TRANSFORMADOR CONECTADO A CADA LINEA DE BAJA TENSION, ACOMETIDAS Y CARGAS.
        
        print (os.path.dirname(os.path.abspath(__file__)))
        
        dir_scripts = os.path.dirname(os.path.abspath(__file__))                      # La carpeta donde todas los archivos .py se encuentran
        connected_components = (grafoBTTotal.subgraph(c) for c in nx.connected_components(grafoBTTotal))
        connected_components = list(connected_components)
        # list(nx.connected_component_subgraphs(grafoBTTotal))
        
        # print(connected_components)

        for graph in connected_components:
            TrafoNode = "NULL"
            TRAFVOLTLL = "NULL"
            TRAFVOLTLN = "NULL"
            TRAFNPHASES = "NULL"
            
            for node in list(graph.nodes(data=True)):  # Identifica el nodo del transformador
                if len(node[1])!= 0 and node[1]['type'] == 'TRAF':
                    TrafoNode = node[0]
                    TRAFVOLTLL = node[1]["LOADVOLT"]
                    TRAFVOLTLN = node[1]["LOADVOLTLN"]
                    TRAFNPHASES = node[1]["NPHAS"]
                    break
                    
            for edge in list(graph.edges(data=True)):     
                             
                datos = edge[2]
                nodo1 = edge[0]
                nodo2 = edge[1]
                
                
                if datos["type"] == "LBT":
                    grafoBT[nodo1][nodo2]["TRAFNODE"] = TrafoNode
                    grafoBT[nodo1][nodo2]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoBT[nodo1][nodo2]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoBT[nodo1][nodo2]["TRAFNPHAS"] = TRAFNPHASES
                elif datos["type"] == "ACO":
                    grafoACO[nodo1][nodo2]["TRAFNODE"] = TrafoNode
                    grafoACO[nodo1][nodo2]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoACO[nodo1][nodo2]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoACO[nodo1][nodo2]["TRAFNPHAS"] = TRAFNPHASES

            for vertice in list(graph.nodes(data=True)):  # Identifica el nodo del transformador
                if len(vertice[1])!= 0 and vertice[1]['type'] == 'LOAD':
                    datos = vertice[1]
                    nodo = vertice[0]
                    grafoCAR.nodes[nodo]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoCAR.nodes[nodo]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoCAR.nodes[nodo]["TRAFNPHAS"] = TRAFNPHASES
        return grafoBT, grafoACO, grafoCAR
        
    #####################################################
    """    
    Esta función lo que hace es pasar de un MultiLineString a una matriz.
    """    
    def MultiStringToMatrix(self, geom):
        try:
            line = geom.asMultiPolyline()
            line = line[0]
            matriz = []
            
            for punto in line:
                x = punto.x()
                y = punto.y()
                punto_t = [x, y]
                matriz.append(punto_t)
            return matriz
        except Exception:
            self.print_error()
            mensaje = "Seleccione una capa del tipo línea y repita "
            mensaje += "la simulación"
            QMessageBox.critical(None,"QGIS2OpenDSS error", mensaje)
            return 0
        
    #####################################################
    
    def ChangeName(self, Dir):
        try:
            Dir = str(Dir)
            
            for filename in os.listdir(Dir):
                nombre = "" 
                # Si no hay guión lo añade, y cambién ceros
                if filename.startswith("Curve")or filename.startswith("curve"):
                    
                    #Cambia la primera c a minúscula
                    nombre = list(filename)
                    if nombre[0] == 'C':
                        nombre[0] = "c"
                    
                    # Busca el "_", si no lo encuentra añade un "_" y 00 antes de la letra
                    posicion_ = filename.find("_")
                            
                    if posicion_ == -1:
                        
                        n = len(nombre)
                        inicio = nombre[: n - 5]
                        letra = nombre[n - 5]
                        final = str("_00" + letra + ".txt")
                        inicio.append(final)
                        nombre = str("".join(inicio))
                        
                        
                    
                    #Cambia la extensión del archivo a .txt	
                    nombre = list(nombre)
                    n = len(nombre)
                    nombre[n - 4 :] = ".txt"
                        
                    nombre = "".join(nombre)
                    os.rename(filename, nombre)
        except Exception:
            self.print_error()
			
    #####################################################
    #####################################################
    #####################################################
    #############  Ruta más larga, funciones  ###########
    #####################################################
    #####################################################
    #####################################################
    
    """
    Algoritmo que calcula la ruta más larga. Como este grafo
    representa una red eléctrica de distribución,
    entonces tiene una estructura particular, ya que sólo habrá
    un único camino para llegar a cualquier nodo. Por tanto,
    se utiliza el algoritmo de Dijkstra que ya está implementado en la librería Networkx.
    Básicamente lo que se hace es iterar por desde el nodo origen
    hasta todos los nodos para encontrar la ruta más larga.
    """	
    def RutaMasLargaReal(self, Grafo, nodo_origen, dep, err, dir_archivo):
        graf = 0
        peso_runta_mas_larga = 0
        ruta_mas_larga = []
        archivo = open(dir_archivo, 'a')
        
        try:
            ruta_mas_larga = ""
            peso_ruta_mas_larga = 0
            
            cantidad_nodos = len(Grafo)
            nodos = list(Grafo.nodes())
            #archivo.write(("Lista de nodos:" + str(nodos)))
                        
            #Aquí inicia el algoritmo en sí. Este primer ciclo sirve
            #para tener como nodo origen todos los nodos existentes
            for nodo_destino in nodos: #range(1, cantidad_nodos, 1):
                #Este ciclo lo que va cambiando es el nodo destino.
                try:
                    ruta = nx.dijkstra_path(Grafo, nodo_origen, nodo_destino)
                    peso_ruta = nx.dijkstra_path_length(Grafo, nodo_origen, nodo_destino)
                    
                    #Opciones de depuración, muestra cómo va analizando nodo por nodo
                    if dep == "1":
                        print("****************************************")
                        msg = "Nodo origen: " + nodo_origen
                        msg += " nodo destino: " + nodo_destino
                        print(msg)
                        print("Ruta: ", ruta)
                        print("Peso ruta: ", peso_ruta)
                        
                    if peso_ruta > peso_ruta_mas_larga:
                        ruta_mas_larga = ruta
                        peso_ruta_mas_larga = peso_ruta
                    if dep == "1":
                        msg = "La ruta más larga, luego de analizar el "
                        msg += "nodo destino" + nodo_destino + " es: "
                        msg += ruta_mas_larga
                        print(msg)
                    
                except Exception:
                    if err == "1":
                        archivo.close()
                        self.print_error()
                        
            if dep == "1":
                # fix_print_with_import
                print("\n\n********************************************")
            if ruta_mas_larga == "":
                msg = "Este grafo está vacío, por lo que no hay una "
                msg += "ruta más larga.\n"
                archivo.write(msg)
                peso_runta_mas_larga = 0
                ruta_mas_larga = []
            else:
                msg = "La ruta más larga desde el nodo origen "
                msg += str(nodo_origen)+ " es: \n" + str(ruta_mas_larga) + "\n"
                archivo.write(msg)
            archivo.close()
            return ruta_mas_larga, peso_ruta_mas_larga
                
        except Exception as e:
            archivo.close()
            if err == "1":
                self.print_error()
                            
        except nx.NetworkXError:
            archivo.close()
            if err == "1":
                self.print_error()
                                    
        except Exception:
            if err == "1":
                self.print_error()
                                        
        return ruta_mas_larga, peso_ruta_mas_larga

    # #####################################################
    """
    Función que obtiene ciertos datos de un arista entre dos nodos dados
    No se implementa el uso de las excepciones, debido a que precisamente
    las excepciones son útiles en el main
    """
    def ObtenerDatosArista(self, Grafo, nodo1, nodo2, DatoBuscado):
        dato = 0
        dato = Grafo.get_edge_data(nodo1, nodo2, "weight")
        dato = dato.get("weight", 0)
        dato = dato.get(DatoBuscado, 0)
                     
        return dato
    
    # #####################################################
    """
    Subestación
    Retorna tres grafos con un nuevo nodo añadido (el que representa a la subestación).
    La diferencia radica en la utilización que 
    se hace de cada nodo desde el main.
    También retorna el nodo en el que está ubicada la subestación,
    para ser utilizado como nodo origen en el cálculo de la ruta más larga.
    """
    
    def SubEst(self, layerSE_, grafoTotal, grafo_Subest,
               grafoDistancias, tolerancia, dir_archivo):
        subests_ = layerSE_.getFeatures()
        indexDSS = auxiliary_functions.getAttributeIndex(self, layerSE_, "DSSName")
        
        for subest in subests_:
            point = subest.geometry().asPoint()  # Lee la geometria de la linea
            node = self.CoordPointProcees(subest, tolerancia)
            
            archivo = open(dir_archivo, 'w')
            msg = "La ubicación del nodo de la subestación, es: "
            msg += str(node)+ "\n"
            archivo.write(msg)
            archivo.close()
            grafo_Subest.add_node(node, pos=node)
            grafoDistancias.add_node(node, pos=node)
            grafoTotal.add_node(node, pos=node)
            
        return grafoTotal, grafo_Subest, grafoDistancias, node
            
    # #####################################################
    """
    Media tensión
    Se encarga de leer la capa de media tensión.
    Retorna el grafo de media tensión, el grafo total y el grafo de distancias.
    Estos dos últimos son utilizados desde el main como grafos que representan todo el circuito.
    El grafoTotal lo único que sirve es para hacer pruebas, y el grafo
    de distancias es el utilizado en el cálculo de la ruta más larga.
    """
    def MT(self, layerMT, GrafoTotal, GrafoMT_, GrafoDistancias,
           tolerancia, subterranea):
        
        indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT,
                                                         "DSSName")
        lineasMT_ = layerMT.getFeatures()

        for lineaMT in lineasMT_:
            geom = lineaMT.geometry()
            line = self.MultiStringToMatrix(geom)
            if line == 0:
                return 0, 0, 0
            n = len(line)  # Cantidad de vértices de la línea
            
            fase = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseCodeODSS')
            faseOrig= lineaMT['PHASEDESIG']
            cantFases = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseNumber')
            dat = lineOperations.renameVoltage(lineaMT['NOMVOLT'])
            opervoltLN = dat.get('LVCode')['LN']
            opervoltLL = dat.get('LVCode')['LL']
            config = dat.get('config')
            nodo1, nodo2 = self.CoordLineProcess(lineaMT, tolerancia)
            
            # Se agregan los nodos
            GrafoDistancias.add_node(nodo1, pos=nodo1)
            GrafoDistancias.add_node(nodo2, pos=nodo2)
            GrafoMT_.add_node(nodo1, pos=nodo1)
            GrafoMT_.add_node(nodo2, pos=nodo2)
            GrafoTotal.add_node(nodo1, pos=nodo1)
            GrafoTotal.add_node(nodo2, pos=nodo2)
            
            LineLength = lineaMT.geometry().length()
            
            # Subterranea
            if subterranea:
                air_ugnd ='ugnd'
                datosLinea = {"PHASEDESIG": faseOrig,
                              "INDEXDSS": indexDSS, 'ID': lineaMT.id(),
                              "LAYER": layerMT, "nodo1": nodo1,
                              "nodo2": nodo2, "X1": line[0][0] ,
                              "Y1": line[0][1], "X2": line[n-1][0] ,
                              "Y2": line[n-1][1], 'NEUMAT': lineaMT['NEUTMAT'],
                              'NEUSIZ': lineaMT['NEUTSIZ'],
                              'PHAMAT': lineaMT['PHASEMAT'],
                              'PHASIZ': lineaMT['PHASESIZ'],
                              'NOMVOLT': lineaMT['INSULVOLT'],
                              'PHASE': fase,'SHLEN': LineLength,
                              'AIR_UGND': air_ugnd, 'INSUL': lineaMT['INSULMAT'],
                              'NPHAS': cantFases, 'VOLTOPRLL': opervoltLL,
                              'VOLTOPRLN': opervoltLN, "SHIELD": lineaMT["SHIELDING"]}
            # Aerea
            else:
                air_ugnd ='air'
                datosLinea = {"PHASEDESIG": faseOrig, "INDEXDSS": indexDSS,
                              'ID': lineaMT.id(), "LAYER": layerMT,
                              "nodo1": nodo1, "nodo2": nodo2,
                              "X1": line[0][0] , "Y1": line[0][1],
                              "X2": line[n-1][0], "Y2": line[n-1][1],
                              'NEUMAT': lineaMT['NEUTMAT'],
                              'NEUSIZ': lineaMT['NEUTSIZ'],
                              'PHAMAT': lineaMT['PHASEMAT'],
                              'PHASIZ': lineaMT['PHASESIZ'],
                              'CCONF': lineaMT['LINEGEO'],
                              'PHASE': fase, 'SHLEN': LineLength,
                              'AIR_UGND': air_ugnd, 'NPHAS': cantFases,
                              'VOLTOPRLL': opervoltLL, 'VOLTOPRLN': opervoltLN}
            
            tension = "mv"
            geometry_code = self.DeterminarGeometryCode(subterranea, datosLinea, tension)
            Impedancia = self.DeterminarImpedancia(geometry_code)
            Impedancia = Impedancia*LineLength/1000
            datosLinea["IMPEDANCIA"] = Impedancia
            if Impedancia == 0: # Caso en que no se haya encontrado un geometry code, se retornarán ceros para saber que no se continuará
                mensaje = str("No se encontró el código " + str(geometry_code)+ " en la biblioteca de cables.\n Favor agregar esta información para obtener la ruta más larga")
                QMessageBox.critical(None,"QGIS2OpenDSS error Ruta Más Larga", mensaje)
                return 0, 0, 0
            GrafoMT_.add_edge(nodo1, nodo2, weight=datosLinea)
            GrafoDistancias.add_edge(nodo1, nodo2, weight=LineLength)
            GrafoTotal.add_edge(nodo1, nodo2, weight=datosLinea)
            
        return GrafoTotal, GrafoMT_, GrafoDistancias

    # #####################################################
    """
    Baja tensión
    Función que lee la capa de baja tensión y crea los grafos de
    Distancias (un grafo con sólo las distancias, el grafoTotal,
    y el grafo de baja tensión.
    Se llama desde el main para crear un grafo total de distancias,
    el grafo de baja tensión solamente, y el grafo total que lo único que sirve es para 
    realizar pruebas.
    """
    def BT(self, layer_BT, grafoTotal, grafoBT, grafoDistancias, tolerancia, subterranea):
        
        indexDSS = auxiliary_functions.getAttributeIndex(self, layer_BT,
                                                         "DSSName")
        lineas = layer_BT.getFeatures()
        
        for linea in lineas:
            geom = linea.geometry()
            line = self.MultiStringToMatrix(geom)
            if line == 0:
                return 0, 0, 0
            
            LineLength = linea.geometry().length()
            n = len(line)#Cantidad de vértices de la línea
            LVCode = linea['NOMVOLT']
            nodo1, nodo2 = self.CoordLineProcess(linea, tolerancia)

            #Se agregan los nodos
            grafoDistancias.add_node(nodo1, pos = nodo1)
            grafoDistancias.add_node(nodo2, pos = nodo2)
            grafoBT.add_node(nodo1, pos = nodo1)
            grafoBT.add_node(nodo2, pos = nodo2)
            grafoTotal.add_node(nodo1, pos = nodo1)
            grafoTotal.add_node(nodo2, pos = nodo2)
            
            cantFases = lineOperations.renameVoltage(linea['NOMVOLT']).get('cantFases')# 1 or 3 phases
            conns = lineOperations.renameVoltage(linea['NOMVOLT']).get('conns')# phaseCodeOpenDSS
            config = lineOperations.renameVoltage(linea['NOMVOLT']).get('config')# wye or delta
            
            try:
                group=linea['LV_GROUP']
            except KeyError:
                group='N/A'
            if subterranea: #Determina si la línea es aérea o subterránea
                air_ugnd ='ugnd'
                datosLinea= {"LVCODE": LVCode,
                            "INDEXDSS": indexDSS, "LAYER": layer_BT,
                            "ID": linea.id(), "nodo1": nodo1, "nodo2": nodo2,
                            'NEUMAT': linea['NEUTMAT'], 'NEUSIZ': linea['NEUTSIZ'],
                            'PHAMAT': linea['PHASEMAT'],'PHASIZ': linea['PHASESIZ'],
                            'X1': line[0][0], 'Y1': line[0][1],
                            'X2': line[n-1][0], 'Y2': line[n-1][1],
                            'SHLEN': LineLength, 'AIR_UGND': air_ugnd,
                            'NPHAS': cantFases, 'CONNS': conns,
                            'CONF': config, 'INSUL': linea['INSULMAT'],
                            'GRUPO': group}
            else:
                air_ugnd ='air'
                datosLinea = {"LVCODE": LVCode,
                            "INDEXDSS": indexDSS, "LAYER": layer_BT,
                            "ID": linea.id(), "nodo1": nodo1,
                            "nodo2": nodo2, 'NEUMAT': linea['NEUTMAT'],
                            'NEUSIZ': linea['NEUTSIZ'], 'PHAMAT': linea['PHASEMAT'],
                            'PHASIZ': linea['PHASESIZ'], 'X1': line[0][0],
                            'Y1': line[0][1], 'X2': line[n-1][0],
                            'Y2': line[n-1][1], 'SHLEN': LineLength,
                            'AIR_UGND': air_ugnd, 'NPHAS': cantFases,
                            'CONNS': conns, 'CONF': config,
                            'GRUPO': group, 'TIPO': linea['TYPE']}
            
            tension = "lv"
            geometry_code = self.DeterminarGeometryCode(subterranea, datosLinea, tension)
            Impedancia = self.DeterminarImpedancia(geometry_code)
            Impedancia = Impedancia*LineLength/1000
            datosLinea["IMPEDANCIA"] = Impedancia
            if Impedancia == 0: #Caso en que no se haya encontrado un geometry code, se retornarán ceros para saber que no se continuará
                mensaje = str("No se encontró el código " + str(geometry_code)+ " en la biblioteca de cables.\n Favor agregar esta información para obtener la ruta más larga")
                QMessageBox.critical(None,"QGIS2OpenDSS error Ruta Más Larga", mensaje)
                return 0, 0, 0
            
            grafoBT.add_edge(nodo1, nodo2, weight=datosLinea)
            grafoDistancias.add_edge(nodo1, nodo2, weight=LineLength)
            grafoTotal.add_edge(nodo1, nodo2, weight=datosLinea)
                
        return grafoTotal, grafoBT, grafoDistancias
        
     
    #####################################################
    """
    Esta función lo que hace es determinar qué grafo tiene más nodos consecutivos en una ruta dada.
    Se le pasan como parámetros:
        -La posición a partir de la cual se quieren empezar a recorrer ambos grafos
        -Los dos grafos a recorrer
        -La ruta que se va a recorrer
        
    Retorna un 1 en caso de que el primer grafo tenga más nodos
    consecutivos en la ruta, un 2 si es el grafo 2, o un 0 si se llega
    al final del grafo y
    ambos tienen la misma cantidad de nodos consecutivos presentes.
    """
    def DeterminarGrafo(self, posicion_nodo_en_ruta, ruta, Grafo1, Grafo2):
        graf = 0
        n_nodos = len(ruta)
        
        #Caso en que sea el último nodo, entonces recorre de atrás para adelante
        if posicion_nodo_en_ruta == n_nodos - 1:
            for i in range(posicion_nodo_en_ruta, 1, -1):
                nodo = ruta[i]
                nodo_anterior = ruta[i - 1]
                
                try:
                    Grafo1.get_node(nodo, nodo_anterior)
                    graf = 1
                except Exception:
                    pass
                
                try:
                    Grafo2.get_node(nodo, nodo_anterior)
                    
                    if graf == 0:
                        graf = 2
                    
                    elif graf == 1:
                        graf = 0
                    
                except Exception:
                    pass
            return graf
                
        #Caso en que no sea el último nodo        
        else:
            for i in range(posicion_nodo_en_ruta, n_nodos - 2):
                nodo = ruta[i]
                nodo_siguiente = ruta[i + 1]
                                              
                try:
                    Grafo1.get_node(nodo, nodo_siguiente)
                    graf = 1
                except Exception:
                    pass

                try:
                    Grafo2.get_node(nodo, nodo_siguiente)
                    if graf == 0:
                        graf = 2
                    
                    elif graf == 1:
                        graf = 0
                    
                except Exception:
                    pass
                        
            return graf
        return graf

    #####################################################
    """
    Esta función lee el archivo generado por OpenDSS y a partir de él
    determina la impedancia para un geometry code dado.
    Entonces lo que hace es recorrer el archivo Datos_LineConstants.txt
    hasta que encuentra la palabra buscada. Dependiendo de si es trifásico o no
    busca los componentes simétricos para determinar la impedancia,
    o en caso contrario determina la impedancia a partir de la primera posición de la matriz
    de resistencias y de la primera también de la de inductancias.
    Realiza la comparación con todas las palabras en minúscula, para que no existan errores.
    Además, escribe en Resultatos.txt para opciones de depuración
    """

    def DeterminarImpedancia(self, Codigo_buscado, dir_archivo = ""):
        palabra = "Geometry Code = " + Codigo_buscado + "\n"
        
        if dir_archivo == "":
            dir_scripts = os.path.dirname(os.path.abspath(__file__))
            dir_archivo = dir_scripts + "\Datos_LineConstants.txt"
        archivo = open(dir_archivo, 'r')
        lines = archivo.readlines()
        
        #dir_archivo = dir_scripts + "\Resultados.txt"
        #archivoDepuracion = open(dir_archivo, 'w')
        #archivoDepuracion.write(str(Codigo_buscado + "\n"))
        #archivoDepuracion.write(str(palabra + "\n"))
        
        palabra = palabra.lower()
        contador = 0
        resultado = 0
        
        #Se busca en qué línea del archivo está la palabra buscada.
        for line in lines:
            line_ = line.lower()
            if line_ == palabra:
                resultado = contador
                break
            contador = contador + 1
        
        #Si no encontró el valor buscado
        if resultado == 0:
            #archivoDepuracion.write("Error, no encontró el geometry code\n")
            #archivoDepuracion.write("*********************************************\n")
            return complex(0, 0)
            
        #archivoDepuracion.write(str("El geometry code está en la línea " + str(resultado + 1)+ "\n"))
                
        geometry_code = lines[resultado]
        contador = geometry_code.find("=")
        fases = geometry_code[contador + 2]
               
        #Si es trifásico o monofásico se debe buscar la impedancia de forma distinta
        #Caso trifásico
        if fases == "3":
        
            valor_buscado = "-------------------Equiv Symmetrical Component --------------------\n"
            i = 1
            #Busca hasta 50 líneas más abajo de donde se econtró el geometru code la frase buscada (valor_buscado)
            for i in range(1, 50):
                if valor_buscado == lines[resultado + i]:
                    posicion_equiv = resultado + i
                    break
                    
            lin_imp = lines[posicion_equiv + 4]
            contador = lin_imp.find("=")
            lin_imp = lin_imp[contador + 2:]
            contador = lin_imp.find("(")
            
            impedancia = lin_imp[:contador - 1]
            contador = impedancia.find("j")
            R = Decimal( impedancia[: contador - 2])
            
            X = Decimal(impedancia[contador + 1 :])
            
            impedancia = complex(R, X)
        
        #Caso monofásico o bifásico
        else:
            valor_buscado_r = "R MATRIX, ohms per km\n"
            valor_buscado_x = "jX MATRIX, ohms per km\n"
            i = 1
            
            #Busca hasta 50 líneas más abajo de donde se econtró el
            #geometry code la frase buscada (valor_buscado_r y valor_buscado_x)
            for i in range(1, 50):
                if lines[resultado + i] == valor_buscado_r:
                    posicion_r = resultado + i
                if lines[resultado + i] == valor_buscado_x:
                    posicion_y = resultado + i
                    break
                    
            R = lines[posicion_r + 1]
            contador = R.find(",")
            R = Decimal(R[: contador - 1])
            
            X = lines[posicion_y + 1]
            contador = X.find(",")
            X = Decimal(X[: contador - 1])
            impedancia = complex(R, X)
        
        #archivoDepuracion.write(str("Impedancia (Ohm/km)= " + str(impedancia)+ "\n"))
        #archivoDepuracion.write("*********************************************\n")
        return impedancia

    #####################################################
    """    
    Esta función lo que hace es redondear un número complejo.
    Para ello divide el número complejo en parte real e imaginaria,
    hace uso de la función "round", y vuelve a convertir el número a complejo.
    """    
    def RedondearComplejo(self, complejo, numero_decimales):
        try:
            Re = round(complejo.real, numero_decimales)
            Im = round(complejo.imag, numero_decimales)
            numero = complex(Re, Im)
        
        except Exception:
            numero = complejo
                
        return numero

    #####################################################
    """
    Función que determina el geometry code de una línea
    de MT o BT
    """
    def DeterminarGeometryCode(self, subterranea, linea, tension):
        GeometryCode = ""
        cantFases = linea['NPHAS']
        # Baja tensión
        if tension == "lv":
            # Aereo
            if subterranea is False:
                if cantFases == '1':
                    if linea['TIPO'] == 'TPX':
                        GeometryCode = 'TRPX'+ str(linea['PHASIZ'])
                        GeometryCode += str(linea['PHAMAT'])
                    else:
                        GeometryCode = '1FLV' + str(linea['PHASIZ'])
                        GeometryCode += str(linea['PHAMAT'])
                        GeometryCode += str(linea['NEUSIZ'])
                        GeometryCode += str(linea['NEUMAT'])
                elif cantFases == '3' or cantFases == '2':
                    if linea['TIPO'] == 'QPX':
                        GeometryCode = 'QDPX' + str(linea['PHASIZ'])
                        GeometryCode += str(linea['PHAMAT'])
                    else:
                        GeometryCode = '3FLV' + str(linea['PHASIZ'])
                        GeometryCode += str(linea['PHAMAT'])
                        GeometryCode += str(linea['NEUSIZ'])
                        GeometryCode += str(linea['NEUMAT'])
            # Subterraneo
            else:
                if cantFases == '1':
                    GeometryCode = '1FLV' + str(linea['PHASIZ'])
                    GeometryCode += str(linea['PHAMAT'])
                    GeometryCode += str(linea['PHASIZ'])
                    GeometryCode += str(linea['PHAMAT'])
                    GeometryCode += '_' + str(linea['INSUL'])
                elif cantFases == '3' or cantFases == '2':
                    GeometryCode = '3FLV' + str(linea['PHASIZ'])
                    GeometryCode += str(linea['PHAMAT'])
                    GeometryCode += str(linea['PHASIZ'])
                    GeometryCode += str(linea['PHAMAT'])
                    GeometryCode += '_' + str(linea['INSUL'])
        # MT
        else:
            if subterranea is False:  # Aereas
                GeometryCode = str(cantFases) + 'FMV'
                GeometryCode += str(linea['PHASIZ'])
                GeometryCode += str(linea['PHAMAT'])
                GeometryCode += str(linea['NEUSIZ'])
                GeometryCode += str(linea['NEUMAT'])
                GeometryCode += '_' + str(linea['CCONF'])
            else:  # Subterráneas
                GeometryCode = str(cantFases) + 'FMV'
                GeometryCode += str(linea['PHASIZ'])
                GeometryCode += str(linea['PHAMAT'])
                GeometryCode += '_' + str(linea['NOMVOLT'])
                GeometryCode += str(linea['INSUL'])
        
        return GeometryCode 
    
    # ###############################################################
    # ###############################################################

    def RutaMasLargaMain(self, grafoDistancias, grafoMT_subt,
                         grafoMT_aer, grafoBT_subt, grafoBT_aer,
                         nodo_origen, dir_archivo):
        try:
            archivo = open(dir_archivo, 'w')
            archivo.write(str(""))
            archivo.close()
        
        except Exception:
            msg = u"Debe introducirse una ruta válida donde se "
            msg += "mostrarán los resultados"
            QMessageBox.critical(None, "QGIS2OpenDSS Error",
                                 QCoreApplication.translate('dialog', msg))
            return
        try:
            tolerancia = 1
            error = 0
            dep = 0
            
            # Calculo de la ruta más larga
            ruta, distancia = self.RutaMasLargaReal(grafoDistancias, nodo_origen,
                                                    dep, error, dir_archivo)

            #Declaración de variables para cálculos de distancias en impedancias        
            distancia_total = 0
            distancia_bt = 0
            distancia_mt = 0
            distancia_bta = 0
            distancia_mta = 0
            distancia_bts = 0
            distancia_mts = 0
            impedancia_t = 0
            impedancia_mt = 0
            impedancia_lt = 0
            impedancia_ma = 0
            impedancia_la = 0
            impedancia_ms = 0
            impedancia_ls = 0

            long_ = "SHLEN"
            impedancia = "IMPEDANCIA"
            cantidad = len(ruta)
            # Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
            foldername = self.dlg.lineEdit_dirOutput.text()
            archivo = open(dir_archivo, 'a')
            msg = "La cantidad de nodos en la ruta es de " + str(cantidad) + "\n"
            archivo.write(msg)

            valor_m = 0
            valor_b = 0
            imp_ma = 0
            imp_la = 0
            contador_imp_ma = 0
            contador_imp_ms = 0
            contador_imp_la = 0
            contador_imp_ls = 0
            contador_dist_ma = 0
            contador_dist_ms = 0
            contador_dist_la = 0
            contador_dist_ls = 0

            turno = self.DeterminarGrafo(0, ruta, grafoMT_aer, grafoBT_subt)
            contador = 0
            i = 0
            
            #Se recorren todos los nodos de la ruta más larga.
            # Se dividen por turnos, debido a que pueden haber nodos en distintas capas con idénticas ubicaciones
            while i < cantidad - 1:
                nodo_i = ruta[i]
                nodo_d = ruta[i + 1]
                
                #Media tensión aérea
                if turno == 1 or turno == 0:
                    try:
                        valor_m = self.ObtenerDatosArista(grafoMT_aer, nodo_i, nodo_d, long_)
                        distancia_mta += valor_m
                        
                        imp_ma = self.ObtenerDatosArista(grafoMT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_ma += imp_ma
                        turno = 1
                        contador = 0
                        #archivo.write("Media tensión \n")
                        #archivo.write(str("Nodo origen = " + str(nodo_i)+ "\n"))
                        #archivo.write(str("Nodo destino = " + str(nodo_d)+ "\n"))
                        
                        if valor_m != 0:
                            contador_dist_ma += 1
                                             
                        if imp_ma != 0:
                            contador_imp_ma += 1
                            
                    except Exception:
                        #archivo.write("Excepción en media tensión!\n")
                        turno = 0
                        
                #Media tensión subterránea
                if turno == 2 or turno == 0:
                    try:
                        valor_m = self.ObtenerDatosArista(grafoMT_subt, nodo_i, nodo_d, long_)
                        distancia_mts += valor_m
                        
                        imp_ma = self.ObtenerDatosArista(grafoMT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_ms += imp_ma
                        turno = 2
                        contador = 0
                        
                        #archivo.write("Media tensión \n")
                        #archivo.write(str("Nodo origen = " + str(nodo_i)+ "\n"))
                        #archivo.write(str("Nodo destino = " + str(nodo_d)+ "\n"))
                                            
                        if valor_m != 0:
                            contador_dist_ms += 1
                            
                        if imp_ma != 0:
                            contador_imp_ms += 1
                    except Exception:
                        #archivo.write("Excepción en media tensión!\n")
                        turno = 0
                    
                #Baja tensión aérea
                if turno == 3 or turno == 0:
                    try:
                        valor_b = self.ObtenerDatosArista(grafoBT_aer, nodo_i, nodo_d, long_)
                        distancia_bta += valor_b
                        imp_la = self.ObtenerDatosArista(grafoBT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_la += imp_la
                        
                        turno = 3
                        contador = 0
                        #archivo.write("Baja tensión \n")
                        #archivo.write(str("Nodo origen = " + str(nodo_i)+ "\n"))
                        #archivo.write(str("Nodo destino = " + str(nodo_d)+ "\n"))
                        if valor_b != 0:
                            contador_dist_la += 1
                        if imp_la != 0:
                            contador_imp_la += 1
                    except Exception:
                        turno = 0
                #Baja tensión subterránea        
                if turno == 4 or turno == 0:
                    try:
                        valor_b = self.ObtenerDatosArista(grafoBT_subt, nodo_i, nodo_d, long_)
                        distancia_bts += valor_b
                        imp_la = self.ObtenerDatosArista(grafoBT_subt, nodo_i, nodo_d, impedancia)
                        impedancia_ls += imp_la
                        
                        turno = 4
                        contador = 0
                        #archivo.write("Baja tensión \n")
                        #archivo.write(str("Nodo origen = " + str(nodo_i)+ "\n"))
                        #archivo.write(str("Nodo destino = " + str(nodo_d)+ "\n"))
                        if valor_b != 0:
                            contador_dist_ls += 1
                        if imp_la != 0:
                            contador_imp_ls += 1
                    except Exception:
                        turno = 0
                if turno == 0 and contador < 2:
                    contador = contador + 1
                    i -= 1
                i += 1
            #Distancias
            distancia_mt = distancia_mta + distancia_mts
            distancia_bt = distancia_bta + distancia_bts
            distancia_total = distancia_mt + distancia_bt
            
            contador_dist_m = contador_dist_ma + contador_dist_ms + 1
            contador_dist_l = contador_dist_la + contador_dist_ls
            contador_dist_t = contador_dist_m + contador_dist_l
            
            #Promedios (en caso de que no se haya encontrado algún geometry code)
            if contador_dist_ma != 0:
                impedancia_ma += impedancia_ma*(contador_dist_ma - contador_imp_ma)/(contador_dist_ma)
            
            if contador_dist_ms != 0:
                impedancia_ms += impedancia_ms*(contador_dist_ms - contador_imp_ms)/(contador_dist_ms)
            
            if contador_dist_la != 0:
                impedancia_la += impedancia_la*(contador_dist_la - contador_imp_la)/(contador_dist_la)
            
            if contador_dist_ls != 0:
                impedancia_ls += impedancia_ls*(contador_dist_ls - contador_imp_ls)/(contador_dist_ls)
            
            #Impedancias
            impedancia_mt = impedancia_ma + impedancia_ms
            impedancia_lt = impedancia_la + impedancia_ls
            impedancia_total = impedancia_mt + impedancia_lt
            
            contador_imp_m = contador_imp_ma + contador_imp_ms + 1
            contador_imp_l = contador_imp_la + contador_imp_ls
            contador_imp_t = contador_imp_m + contador_imp_l
            
            #Escritura de los datos en un archivo de texto (en la ruta que seleccione el usuario)
            msg = "La distancia total es de: " + str(round(distancia_total/1000, 3))
            msg += " km, con un total de " + str(contador_dist_t) + " nodos\n"
            msg += "La impedancia total es de: " + str(self.RedondearComplejo(impedancia_total, 4))
            msg += " Ohms, con un total de " + str(contador_imp_t) + " nodos\n\n\n"
            msg += "La distancia en media tensión es de: " + str(round(distancia_mt/1000, 3))
            msg += " km, con un total de " + str(contador_dist_m) + " nodos\n"
            msg +=  "La distancia en baja tensión es de: " + str(round(distancia_bt, 3))
            msg += " m, con un total de " + str(contador_dist_l) + " nodos\n"
            msg += "La impedancia en media tensión es de: "
            msg += str(self.RedondearComplejo(impedancia_mt, 4))
            msg += " Ohms, con un total de " + str(contador_imp_m) + " nodos\n"
            msg += "La impedancia en baja tensión es de: "
            msg += str(self.RedondearComplejo(impedancia_lt, 4))
            msg += " Ohms, con un total de " + str(contador_imp_l) + " nodos\n\n\n"
            
            msg += "La distancia en media tensión aérea es de: "
            msg += str(round(distancia_mta/1000, 3)) + " km\n"
            msg +="La distancia en media tensión subterránea es de: "
            msg += str(round(distancia_mts, 3)) + " m\n"
            
            msg += "La impedancia en media tensión aérea es de: "
            msg += str(self.RedondearComplejo(impedancia_ma, 4)) + " Ohms\n"
            msg += "La impedancia en media tensión subterránea es de: "
            msg += str(self.RedondearComplejo(impedancia_ms, 4)) + " Ohms\n\n"
            
            msg += "La distancia en baja tensión aérea es de: "
            msg += str(round(distancia_bta, 3))+ " m\n"
            msg += "La distancia en baja tensión subterránea es de: "
            msg += str(round(distancia_bts, 3))+ " m\n"
            
            msg += "La impedancia en baja tensión aérea es de: "
            msg += str(self.RedondearComplejo(impedancia_la, 4))+ " Ohms\n"
            msg += "La impedancia en baja tensión subterránea es de: "
            msg += str(self.RedondearComplejo(impedancia_ls, 4))+ " Ohms\n"
                  
              
            #archivo.write(str("La ruta más larga es:\n" + str(ruta)+ "\n"))
                           
            tiempo_f = time.time()
            tiempo_total = (tiempo_f - self.time)/60
            msg += "El tiempo de ejecución es de " + str(tiempo_total)+ " minutos\n"
            archivo.write(msg)
            
            """
            
            nodos_MT= list(GrafoMT_.nodes()
            nodos_BT= list(GrafoBT.nodes())
            
            archivo.write(str("Nodos media tensión: \n" + str(nodos_MT)+ "\n"))
            archivo.write(str("Nodos baja tensión: \n" + str(nodos_BT)+ "\n"))
            
            """
            
            archivo.close()
            aviso = "Se exportaron los resultados de la ruta más larga "
            aviso += "en el siguiente archivo:\n" + dir_archivo
            QMessageBox.information(None,
                                    QCoreApplication.translate('dialog', "Ruta más larga"),
                                    aviso)
            print("Final ruta más larga")
            return 1
        
        except Exception:
            self.print_error()
            return 0
    
    #####################################################
    #####################################################
    #####################################################
    #######  Final de funciones de ruta más larga  ######
    #####################################################
    #####################################################
    #####################################################
    
    
    # Función que se encarga de imprimir los errores que han ocurrido
    def print_error(self):
        exc_info = sys.exc_info()
        msg = str(exc_info)
        msg = "\nError: " + str(exc_info)
        msg += "\n********  Información detallada del error **********"  
        for tb in traceback.format_tb(sys.exc_info()[2]):
            msg += "\n" + tb

        print(msg)
        return msg

    """
    Función que se encarga de instalar una librería en la versión de python de QGIS
    -Parámetros de entrada:
    *library_name (string): nombre de la librería a instalar (tal como se le debe pasar a pip)
    
    -Valores retornados:
    *1 en caso de finalizar exitosamente
    *0 en caso de ocurrir algún error
    """
    
    def install_libraries(self, library_name):
    
        """
        Función que se encarga de instalar una librería en
        la versión de python de QGIS
        -Parámetros de entrada:
        *library_name (string): nombre de la librería a instalar
        (tal como se le debe pasar a pip)

        -Valores retornados:
        *1 en caso de finalizar exitosamente
        *0 en caso de ocurrir algún error
        """
        
        try:
            # Se obtiene el path de QGIS
            directorio = str(os.path)
            fin_dir = directorio.find("\\apps")
            first_letter_in = directorio.find(":\\") - 1
            first_letter = directorio[first_letter_in:first_letter_in+1]
            first_letter += ":\\"
            inic_dir = directorio.find(first_letter)
            path = directorio[inic_dir:fin_dir]
            # Se obtiene version de Python en QGIS
            info = sys.version_info
            verspy1 = str(info[0])
            verspy2 = str(info[1])
            carp_python = verspy1 + verspy2
            carp_python = "Python" + carp_python

            # Se copia los archivos
            dir_origen = path + "/bin/"
            name_file_or = "python" + verspy1 + ".dll"
            archivo_origen = str(dir_origen + name_file_or)
            dir_destino = path + "/apps/" + carp_python
            name_dest = dir_destino + "/" + name_file_or

            if os.path.exists(name_dest) is False:
                # Copia python3.dll
                self.copy(archivo_origen, dir_destino)

            # Copia python37.dll
            name_file_or = "python" + verspy1 + verspy2 + ".dll"
            archivo_origen = dir_origen + name_file_or
            name_dest = dir_destino + "/" + name_file_or

            if os.path.exists(name_dest) is False:
                # Copia python37.dll
                self.copy(archivo_origen, dir_destino)

            # Instalación de librerías
            # Actualización de pip
            sentencia = dir_origen + 'python.exe -m pip install –upgrade pip'
            subprocess.call(sentencia, cwd=dir_destino, shell=True)

            # Instalación libreria
            sentencia = dir_origen + "python.exe -m pip install " + library_name
            x = subprocess.call(sentencia, cwd=dir_destino, shell=True)

            print("Instalación de librería ", library_name, " finalizada.")
            return 1

        except Exception:
            self.print_error()
            return 0
            
            
    """
    Función que se encarga de crear y asignar los loadshapes
    
    -Parámetros de entrada:
    *grafoCAR (nx graph): grafo al que se le deseen asignar los loadshapes
    *folder_profile (str): directorio donde se encuentran los perfiles de carga
    *foldername (str): directorio de salida de los dss
    *circuitName (str): nombre del circuito
    *Buses (bool): indica si se está leyendo los grafos y datos referentes a buses
    
    
    -Valores retornados
    *grafoCAR (nx graph): grafo actualizado con loadshapes asignados
    *errorLoadShape (bool): True si hubo errores, False en caso contrario.
    """
    def AssignLoadshapes(self, grafoCAR, folder_profile, foldername, circuitName, output_shpdss, Buses = False, StreetLightning=False):
        errorLoadShape = False
        try:
            # De las curvas disponibles, toma las existentes y las pone en un vector
            curv_disp = {"residential": [], "commercial": [], "industrial": []}
            for sector in ["residential", "commercial", "industrial"]:
                try:
                    os.chdir(folder_profile + "\\" + sector) # Se ubica en la carpeta
                    directorio = str(folder_profile + "\\" + sector)
                    self.ChangeName(directorio)
                    
                    for file in glob.glob("*.txt")or glob.glob("*.dss"):
                        if file[:5].lower()== "curve":
                            energ = file[:(len(file)- 5)]
                            energ = energ.replace("curve", "")
                            energ = energ.replace("_", ".")
                            curv_disp[sector].append(float(energ))
                except FileNotFoundError: #no necesariamente existen todas las carpetas residencial, comercial e industrial
                    continue
                except Exception:
                    self.print_error()
            graphNodes = list(grafoCAR.nodes(data=True))
            # print grafoCAR.number_of_nodes()
            i = 1
            for NODE in graphNodes:
                dataList = NODE[1]
                
                nodo = NODE[0]
                if Buses is True:
                    class_ = dataList['DICT LOAD BT']["class"]
                else:
                    class_ = dataList["class"]
                class_t = str(class_).lower()
                
                #Caso MT
                
                #Revisar, MT
                if class_t == "mv_c":
                    sector = "commercial"
                    final_letter_loadshape = "C"
                elif class_t == "mv_i":
                    sector = "industrial"
                    final_letter_loadshape = "I"
                elif class_t == "mv_s":
                    sector = "services"
                    final_letter_loadshape = "S"
                
                #Baja tensión
                elif class_t == "r":
                    sector = "residential"
                    final_letter_loadshape = class_
                elif class_t == "c":
                    sector = "commercial"
                    final_letter_loadshape = class_
                elif class_t == "i":
                    sector = "industrial"
                    final_letter_loadshape = class_
            
                else:
                    self.print_error()
                    errorLoadShape = True
                    msg = "Error en la creación de los loadshapes\n"
                    msg += "Verifique que se asigne una clase válida"
                    title = "QGIS2OpenDSS Error"
                    QMessageBox.critical(None, title, msg)
                    return grafoCAR, errorLoadShape, output_shpdss
                
                if Buses is True:
                    kwh = dataList['DICT LOAD BT']["kWh"]
                else:
                    kwh = dataList["kWh"]
                
                energLoad = format(float(kwh), '.2f')
                aux = list(abs(np.array(curv_disp[sector])- np.array(len(curv_disp[sector])* [float(energLoad)]))) # Diferencia en kwh de la carga y las curvas existentes
                
                if len(aux)== 0: #caso en que no se haya encontrado el dato correspondiente a las curvas de cierto sector 
                    print("Aux = 0, sector = ", sector, " energLoad = ", energLoad)
                    pass #revisar
                
                error = min(aux)/ float(energLoad)
                enerLoadStr = str(energLoad)
                                                                        
                enerLoadStr = enerLoadStr.replace(".", "_")
                ClosCurvEnerg = float(curv_disp[sector][aux.index(min(aux))])
                                            
                ClosCurvEnergStr = str(format(ClosCurvEnerg, '.2f'))
                ClosCurvEnergStr = ClosCurvEnergStr.replace(".", "_")
                                            
                if error <= 0.02:  # Significa que la diferencia de energía con la curva más cercana es menor al 2%
                    if Buses is True:
                        grafoCAR.nodes[nodo]['DICT LOAD BT']['CURVASIG'] = 'daily=curve' + ClosCurvEnergStr + final_letter_loadshape
                        
                    else:
                        grafoCAR.nodes[nodo]['CURVASIG'] = 'daily=curve' + ClosCurvEnergStr + final_letter_loadshape

                else:  # A las curvas asignadas, si no existen, crea una curva artificial adaptada del valor más cercano existentes
                    if Buses is True:
                        grafoCAR.nodes[nodo]['DICT LOAD BT']['CURVASIG'] = 'daily=curve' + enerLoadStr + final_letter_loadshape
                    else:
                        grafoCAR.nodes[nodo]['CURVASIG'] = 'daily=curve' + enerLoadStr + final_letter_loadshape
                    
                    os.chdir(folder_profile + "\\" + sector)
                    file_name = 'curve' + str(ClosCurvEnergStr)+ final_letter_loadshape + '.txt'
                    file_data = []
                    with open(file_name)as f:
                        file_data = f.readlines()
                    file_data_parse = []
                    w = QWidget()
                    aux = 0.0
                    for j in range(96):
                        file_data_parse.append(float(file_data[j]))
                        aux = aux + file_data_parse[j]
                    if aux == 0:
                        k = 1
                    else:
                        k = float(ClosCurvEnerg)/ (aux * 0.25 * 30)

                    file_ = open('curve' + enerLoadStr + final_letter_loadshape + '.txt', "w") # _new
                    for j in range(96):
                        file_.write(str(format(k * file_data_parse[j], '.2f'))+ ' \n')
                    file_.close()
                    curv_disp[sector].append(float(energLoad))

            # Create a file with all the loadshapes
            output_shpdss.write('!' + folder_profile + '\n')
            for sector in ["residential", "commercial", "industrial", 'amis']:
                try:
                    os.chdir(folder_profile + "\\" + sector) # Se ubica en la carpeta
                    for file in glob.glob("*.txt"):
                        out = 'New Loadshape.' + file.replace('.txt', '')
                        out += ' npts=96 minterval=15 mult=(file='
                        out += folder_profile + '\\' + sector + '\\'
                        out += file + ') useactual=no \n'
                        output_shpdss.write(out)
                
                except FileNotFoundError: #no necesariamente existen todas las carpetas residencial, comercial e industrial
                    continue
                except Exception:
                    self.print_error()
                
            if StreetLightning==True:
            
                #Las iluminarias se activan de 6pm a 6am
            
                array_1 = ["1" for x in range(4*6)] #media noche a 6am -> 6 horas
                array_2 = ["0" for x in range(4*12)] #6am a 6pm -> 12 horas
                array_3 = ["1" for x in range(4*6)] #6pm a media noche -> 6 horas
                
                ls_lshp = '(' + ','.join(array_1 + array_2 + array_3) +')'
            
                out = 'New Loadshape.StreetLightning' 
                out += ' npts=96 minterval=15 mult='
                out += ls_lshp
                out += ' useactual=no \n'
                output_shpdss.write(out)

            return grafoCAR, errorLoadShape, output_shpdss
        except Exception:
            self.print_error()
            errorLoadShape = True
            if Buses is True:
                msg = "Error en la creación de loadshapes de buses\n"
                msg += "Verifique que existen los archivos de curvas "
                msg += "en la carpeta indicada.\n*NOTA: no se creará "
                msg += "el archivo de cargas ni el de LoadShapes."
                QMessageBox.critical(None, "QGIS2OpenDSS Error", msg)
            else:
                msg = "Error en la creación de loadshapes de BT/MT\n"
                msg += "Verifique que existen los archivos de curvas "
                msg += "en la carpeta indicada.\n*NOTA: no se creará "
                msg += "el archivo de cargas ni el de LoadShapes."
                QMessageBox.critical(None, "QGIS2OpenDSS Error", msg)
            return grafoCAR, errorLoadShape, output_shpdss
    
    """
    Función encargada de leer un csv y convertirlo a un diccionario, agrupados por el nombre de la
    primera columna de cada fila del csv.
    Se hizo para leer los csv con información de los buses y cargadores, donde la primera columna representa
    el nombre del plantel al que corresponde dicha información
    
    Parámetros de entrada:
    *csv_name (str): dirección del csv a ser leído
    
    Parámetros de salida:
    *dict_buses (dict): diccionario con la información del csv agrupada como se mencionó anteriormente.
    Retorna un 0 si ocurre algún error.
    """
    
    def csv_to_dict(self, csv_name):
        dict_buses = {}
        
        try:
            dataframe = pd.read_csv(csv_name, index_col = 0)#se convierte el csv a un dataframe con la primera columna como índice
            
            for index, row in dataframe.iterrows():
                row_dict = row.to_dict()#se convierte la columna del dataframe a diccionario
                
                #Asignación de valores al diccionario
                try:
                    i = len(dict_buses[index])
                    dict_buses[index][i] = row_dict
                except Exception:
                    dict_buses[index] = {}
                    dict_buses[index][0] = row_dict
            return dict_buses
        
        
        except Exception:
            self.print_error()
            return 0
        
    
    #========================================================================================================================================
    #========================================================================================================================================
    #========================================================================================================================================
    def run(self):
        self.mensaje_log_gral = ""
        self.msg_trafos = ""
        self.nombres_capas = ""
        self.install_libraries("networkx --upgrade")
        """
        #Instalación librerías
        try:
            import dss
        except Exception:
            self.install_libraries("dss_python")#Instalación de librerías extra requeridas
            import dss
        """

        """Método que ejecuta las funciones del complemento"""
        # Carga los nombres de las capas actualmente abiertas y las muestra en las listas desplegables
        lista_ant =  [self.dlg.comboBox_LMT1.itemText(i) for i in range(self.dlg.comboBox_LMT1.count())] #Lista anterior en combobox
        
        layers = QgsProject.instance().mapLayers().values()
        layer_list = []
        layer_list.append("")
        
        for layer in layers:
            layer_list.append(layer.name())
        if lista_ant != layer_list:
            self.configurate_combobox(layer_list)
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop_
        result = self.dlg.exec_()
        circuitName = self.dlg.lineEdit_nameCircuit.text().upper() # Recibe el nombre del circuito indicado en la ventana de dialogo.
        foldername = self.dlg.lineEdit_dirOutput.text() # Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
        self.foldername = foldername     
        folder_profile = self.dlg.lineEdit_AC.text()
        self.folder_profile = folder_profile
        cargas1 = self.dlg.comboBox_CA1.currentIndex()
        cargas2 = self.dlg.comboBox_CA2.currentIndex()
        cargas3 = self.dlg.comboBox_CA3.currentIndex()
        cargas = cargas1 + cargas2 + cargas3
        
        cargas_iluminacion1 = self.dlg.comboBox_CI1.currentIndex()
        cargas_iluminacion2 = self.dlg.comboBox_CI2.currentIndex()
        cargas_iluminacion3 = self.dlg.comboBox_CI3.currentIndex()
        cargas_iluminacion = cargas_iluminacion1+cargas_iluminacion2+cargas_iluminacion3

        MTL1 = self.dlg.comboBox_LMT1.currentIndex()
        MTL2 = self.dlg.comboBox_LMT2.currentIndex()
        MTL3 = self.dlg.comboBox_LMT3.currentIndex()
        MTL = MTL1 + MTL2 + MTL3

        BTL1 = self.dlg.comboBox_LBT1.currentIndex()+ self.dlg.comboBox_ACO1.currentIndex()
        BTL2 = self.dlg.comboBox_LBT2.currentIndex()+ self.dlg.comboBox_ACO2.currentIndex()
        BTL3 = self.dlg.comboBox_LBT3.currentIndex()+ self.dlg.comboBox_ACO3.currentIndex()
        BTL = BTL1 + BTL2 + BTL3
        
        EVs = self.dlg.comboBox_EV.currentIndex()

        tx1 = self.dlg.comboBox_TR1.currentIndex()
        tx2 = self.dlg.comboBox_TR2.currentIndex()
        tx3 = self.dlg.comboBox_TR3.currentIndex()
        TX = tx1 + tx2 + tx3
        projPath = str(QgsProject.instance().homePath())
        
        SubsNode = NULL
        dir_scripts = os.path.dirname(os.path.abspath(__file__))
        self.circuitName = circuitName
        
        
        # Verifica que se haya indicado el nombre del circuito, la carpeta de destino y se ejecuta al presionar OK.
        if result and (not projPath):
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate('dialog', "Debe crear un proyecto de QGIS"))
            return 0
        elif result and (
                not circuitName or not foldername):  # Se asegura de que el complemento se ejecute solo si se tiene completa la informacin necesaria
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate(
                'dialog', "Debe indicar el nombre del circuito y la carpeta de destino"))
            return 0
        elif result and (self.dlg.comboBox_SE.currentIndex()== 0)and (MTL != 0):
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate(
                'dialog', u"Debe seleccionar la capa de la subestación"))
            return 0

        elif result and (self.dlg.comboBox_SE.currentIndex()== 0)and TX == 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate(
                'dialog', u"Al menos debe seleccionar la capa de la subestación o transformadores"))
            return 0
            
        
        elif result and (BTL != 0)and TX == 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate(
                'dialog', "Debe seleccionar la capa de la transformadores"))
            return 0
        elif result and not folder_profile and cargas > 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar")+ "\n" + QCoreApplication.translate(
                'dialog', "Para modelar la carga debe ingresar la carpeta de perfiles"))
            return 0
            
        elif result and circuitName and foldername: #Estas son las opciones para que funcione el plugin
            
            dir_logs = foldername + "/error"
            self.dir_logs = dir_logs
            
            #Se crea el directorio de errores si no existe
            if not os.path.exists(dir_logs):
                os.makedirs(dir_logs)
            #shutil.rmtree(dir_logs, ignore_errors=True)
            
            
            if self.dlg.check_rutamaslarga.isChecked(): #Primero hace el cálculo de la ruta más larga
                # ################################################
                # ################################################
                ##############  RUTA MÁS LARGA  ##################
                # ################################################
                # ################################################
                
                #Se crea el archivo con los datos de impedancias
                LlamarOpenDSS()
                #Esto es sólo para limpiar el archivo donde se va a escribir
                try:
                    #Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
                    foldername = self.dlg.lineEdit_dirOutput.text()
                    name_file = "/ResultadosRutaMasLarga" + circuitName + ".txt"
                    dir_archivo = foldername + name_file
                    print(dir_archivo)
                    archivo = open(dir_archivo, 'w')
                    archivo.write("")
                    archivo.close()
                
                except Exception:
                    self.print_error()
                    msg = "Debe introducirse una ruta válida donde se "
                    msg += "mostrarán los resultados"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error", msg)
                    return 0
                        
                tolerancia = 1
                
                #Declaración de los grafos
                grafo_Subest = nx.Graph()
                
                grafoMT_aer = nx.Graph()
                grafoMT_subt = nx.Graph()
                grafoBT_aer = nx.Graph()
                grafoBT_subt = nx.Graph()
                
                grafoDistancias = nx.Graph()
                grafoTotal = nx.Graph()
                
                datosLMT = []
                        
                self.time = time.time()
                        
                #Lectura subestación
                try:
                    NombreSE = self.dlg.comboBox_SE.currentText()
                    layerSE = QgsProject.instance().mapLayersByName(NombreSE)[0]
                    grafoTotal, grafo_Subest, grafoDistancias, nodo_origen = self.SubEst(layerSE, grafoTotal, grafo_Subest,
                                                                                         grafoDistancias, tolerancia, dir_archivo)
                
                except Exception:
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor inserte una capa para la subestación"))
                    return 0
                
                #Media tensión
                NombreMT1 = self.dlg.comboBox_LMT1.currentText()
                NombreMT2 = self.dlg.comboBox_LMT2.currentText()
                NombreMT3 = self.dlg.comboBox_LMT3.currentText()
                
                #Lectura media tensión 1
                try:
                    if len(NombreMT1)!=0:
                        layerMT1 = QgsProject.instance().mapLayersByName(NombreMT1)[0] #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                        
                        if self.dlg.checkBox_LMT1.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaMT1 = True
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT1, grafoTotal, grafoMT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaMT1)
                        else:
                            subterraneaMT1 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT1, grafoTotal, grafoMT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaMT1)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0

                    else:
                        msg = u"Debe seleccionar al menos una capa de media tensión"
                        QMessageBox.critical(None,"QGIS2OpenDSS Error", QCoreApplication.translate('dialog', msg))
                        return 0
                except Exception:
                    self.print_error()
                    msg = u"Favor revise que su tabla de atributos de media "
                    msg += "tensión tenga todos los atributos necesarios"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                    return 0
                
                #Lectura media tensión 2
                try:
                    if len(NombreMT2)!=0:
                        layerMT2 = QgsProject.instance().mapLayersByName(NombreMT2)[0]
                        if self.dlg.checkBox_LMT2.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaMT2 = True
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT2, grafoTotal, grafoMT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaMT2)
                            
                        else:
                            subterraneaMT2 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT2, grafoTotal, grafoMT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaMT2)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0

                except Exception:
                    self.print_error()
                    msg = u"Favor revise que su tabla de atributos de media tensión "
                    msg += "tenga todos los datos necesarios"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                    return 0

                #Lectura media tensión 3
                try:
                    if len(NombreMT3)!=0:
                        #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                        layerMT3 = QgsProject.instance().mapLayersByName(NombreMT3)[0]
                        if self.dlg.checkBox_LMT3.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaMT3 = True
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT3, grafoTotal, grafoMT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaMT3)
                        else:
                            subterraneaMT3 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT3, grafoTotal, grafoMT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaMT3)

                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0

                except Exception:
                    self.print_error()
                    msg = u"Favor revise que su tabla de atributos de media "
                    msg += "tensión tenga todos los datos necesarios"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                    return 0

                #Baja tensión    
                NombreBT1 = self.dlg.comboBox_LBT1.currentText()
                NombreBT2 = self.dlg.comboBox_LBT2.currentText()
                NombreBT3 = self.dlg.comboBox_LBT3.currentText()
                datosLBT = []  #datosLBT guarda informacion de ubicacion y fase de las lineas BT3
                        
                #Baja tensión 1
                try:
                    if len(NombreBT1)!=0:
                        layerBT1 = QgsProject.instance().mapLayersByName(NombreBT1)[0]
                        
                        if self.dlg.checkBox_LBT1.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaBT1 = True
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT1, grafoTotal, grafoBT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaBT1)
                            
                        else:
                            subterraneaBT1 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT1, grafoTotal, grafoBT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaBT1)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0
                        
                    else:
                        msg = u"Debe seleccionar al menos una capa de baja tensión"
                        QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                        
                except Exception:
                    self.print_error()
                    msg = u"Favor revise que su tabla de atributos de baja "
                    msg = "tensión tenga todos los datos necesarios"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                    return 0
                    
                    
                #Baja tensión 2
                try:
                    if len(NombreBT2)!=0:
                        layerBT2 = QgsProject.instance().mapLayersByName(NombreBT2)[0]
                        indexDSS=auxiliary_functions.getAttributeIndex(self, layerBT2, "DSSName")
                        
                        if self.dlg.checkBox_LBT2.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaBT2 = True
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT2, grafoTotal, grafoBT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaBT2)
                        else:
                            subterraneaBT2 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT2, grafoTotal, grafoBT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaBT2)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0
                except Exception:
                    self.print_error()
                    msg = u"Favor revise que su tabla de atributos de "
                    msg += "baja tensión tenga todos los datos necesarios"
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', msg))
                    return 0
                
                #Baja tensión 3
                try:
                    if len(NombreBT3)!=0:
                        layerBT3 = QgsProject.instance().mapLayersByName(NombreBT3)[0]
                        indexDSS=auxiliary_functions.getAttributeIndex(self, layerBT3, "DSSName")
                        
                        if self.dlg.checkBox_LBT3.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaBT3 = True
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT3, grafoTotal, grafoBT_subt,
                                                                                grafoDistancias, tolerancia, subterraneaBT3)
                        else:
                            subterraneaBT3 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT3, grafoTotal, grafoBT_aer,
                                                                               grafoDistancias, tolerancia, subterraneaBT3)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return 0
                except Exception:
                    self.print_error()
                    msg = "Favor revise que su tabla de atributos de "
                    msg += "baja tensión tenga todos los "
                    msg += "datos necesarios"
                    title = "QGIS2OpenDSS Error"
                    QMessageBox.critical(None, title, msg)
                    return 0
                    
                self.RutaMasLargaMain(grafoDistancias, grafoMT_subt,
                                      grafoMT_aer, grafoBT_subt,
                                      grafoBT_aer, nodo_origen,
                                      dir_archivo)
                # ####################################################
                # ####################################################
                # ##############  FIN RUTA MÁS LARGA #################
                # ####################################################
                # ####################################################
            
            self.progress.show()
            self.progress.progressBar.setRange(0, 100)
            self.progress.progressBar.setValue(0)

            startpluginTime = timeit.default_timer() # starts runTime counter

            Error = False  # Se inicializa esta variable. Cambia a True si ocurre un error crítico.
            # Time meters init
            startTime = time.time()
            toler = 0.1  # tolerancia para los grafos en metros
            grafoBTTotal = nx.Graph()
            """1-Se inicia contador de barras MT y BT"""
            busnumMT = 1  # inicializa contador de barras de MT
            busnumBT = 1  # inicializa contador de barras de BT
            # 2.1-Crea lista con las coordenadas de Inicio, Final y Fase de las lineas de media tension.
            
            
            selectedLayerMT1 = self.dlg.comboBox_LMT1.currentText()
            selectedLayerMT2 = self.dlg.comboBox_LMT2.currentText()
            selectedLayerMT3 = self.dlg.comboBox_LMT3.currentText()
            
            
            busMT_List = {}  # Lista de Bus MT
            busMTid = 1
            busBT_List = {}  # Lista de Bus BT

            ###Crea lista con datos de subestación
            
            grafoMT = nx.Graph()
            
            ############################################################
            ################## Lectura subestacion #####################
            ############################################################
            
            startTimeSub = time.time()
            
            if self.dlg.Model.isChecked():
                mode = "MODEL"
            elif self.dlg.checkBox_AutoTraf.isChecked():
                mode = "AUTO"
            elif self.dlg.noModel.isChecked():
                mode = "NOMODEL"

            SEactive = False
            # Recibe la capa de subestación seleccionada en la lista desplegable
            selectedLayerSE = self.dlg.comboBox_SE.currentText()
            if len(selectedLayerSE)!= 0:
                self.nombres_capas += "\n!Layer Substation: "
                self.nombres_capas += str(selectedLayerSE)
                layerSE = QgsProject.instance().mapLayersByName(selectedLayerSE)[0]
                subt_ = self.ReadDataSub(mode, layerSE, grafoMT, toler,
                                         busMT_List, busMTid)
                grafoSubt = subt_[0]
                grafoMT = subt_[1]
                datosSE = subt_[2]
                busMT_List = subt_[3]
                busMTid = subt_[4]
                SubsNode = subt_ [5]

                if grafoSubt == 0:
                    self.progress.close()
                    return 0
                SEactive = True

            endTimeSub = time.time()
            aviso_bus = False
            LMTactive = False
            Error = False
            aviso_bus = False
            aviso_fases = False
            first_line_mt_aer = True
            first_line_mt_sub = True
            line_mt_aer = ""
            line_mt_sub = ""
            
            try:  ###Lectura y Conectividad de media tensión
                datosLMT = []
                mensaje_mt = ""
                if len(selectedLayerMT1)!= 0:
                    layerMT1 = QgsProject.instance().mapLayersByName(selectedLayerMT1)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    # Subterránea
                    if self.dlg.checkBox_LMT1.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                        # Se agrega el nombre de la capa sub
                        line_mt_sub += "\n!Layers LinesMV_sub: "
                        line_mt_sub += str(selectedLayerMT1) + ","
                        first_line_mt_sub = False
                    # Aérea
                    else:
                        subterranea = False
                        line_mt_aer += "\n!Layers LinesMV_aer: "
                        line_mt_aer += str(selectedLayerMT1) + ","
                        first_line_mt_aer = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT1, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT1, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    LMTactive = True
                    if grafoMT == 0:
                        self.progress.close()
                        return 0
                if len(selectedLayerMT2)!= 0:
                    layerMT2 = QgsProject.instance().mapLayersByName(selectedLayerMT2)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    # Subterránea
                    if self.dlg.checkBox_LMT2.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                        # Se agrega el nombre de la capa sub
                        if first_line_mt_sub is True:
                            line_mt_sub += "\n!Layers LinesMV_sub: "
                            line_mt_sub += str(selectedLayerMT2) + ","
                            first_line_mt_sub = False
                        else:
                            line_mt_sub += str(selectedLayerMT2) + ","
                    # Aérea
                    else:
                        subterranea = False
                        if first_line_mt_aer is True:
                            line_mt_aer += "\n!Layers LinesMV_aer: "
                            line_mt_aer += str(selectedLayerMT2) + ","
                            first_line_mt_aer = False
                        else:
                            line_mt_aer += str(selectedLayerMT2) + ","
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT2, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT2, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    if grafoMT == 0:
                        self.progress.close()
                        return 0
                    LMTactive = True
                if len(selectedLayerMT3)!= 0:
                    layerMT3 = QgsProject.instance().mapLayersByName(selectedLayerMT3)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    # Subterránea
                    if self.dlg.checkBox_LMT3.isChecked():
                        subterranea = True
                        if first_line_mt_sub is True:
                            line_mt_sub += "\n!Layers LinesMV_sub: "
                            line_mt_sub += str(selectedLayerMT3) + ","
                            first_line_mt_sub = False
                        else:
                            line_mt_sub += str(selectedLayerMT3) + ","
                    # Aérea
                    else:
                        subterranea = False
                        if first_line_mt_aer is True:
                            line_mt_aer += "\n!Layers LinesMV_aer: "
                            line_mt_aer += str(selectedLayerMT3) + ","
                            first_line_mt_aer = False
                        else:
                            line_mt_aer += str(selectedLayerMT3) + ","
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT3, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT3, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    if grafoMT == 0:
                        self.progress.close()
                        return 0
                    LMTactive = True
            except KeyError:  # Si los nombres de las columnas no son correctos, el programa genera un aviso y no se ejecuta
                self.print_error()
                msg = "Verifique los nombres de las columnas de las "
                msg += "tablas de atributos de líneas de media tensión"
                QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', msg))
                LMTactive = False
                Error = True
                aviso_bus = False
                aviso_fases = False
            
            # Se agrega el nombre de las capas
            self.nombres_capas += line_mt_aer + line_mt_sub

            ############################################################
            ################### Lectura switches #######################
            ############################################################
            
            capa_switches = False
            # Recibe la capa de switches
            selectedLayerSwitches = self.dlg.comboBox_secc.currentText()
            if len(selectedLayerSwitches)!= 0:
                self.nombres_capas += "\n!Layer Switches: "
                self.nombres_capas += str(selectedLayerSwitches)
                layerSwitches = QgsProject.instance().mapLayersByName(selectedLayerSwitches)[0]
                datosSwitch, grafoSwitch = self.ReaderDataSwitches(layerSwitches, toler, grafoMT)
                capa_switches = True
                if datosSwitch == 0:
                    self.progress.close()
                    return 0
                    
            # ##########################################################
            # ################## Lectura fusibles ######################
            # ##########################################################
            
            capa_fuses = False
            # Recibe la capa de fuses
            selectedLayerFuses = self.dlg.comboBox_fusib.currentText()
            if len(selectedLayerFuses)!= 0:
                self.nombres_capas += "\n!Layer Fuses: "
                self.nombres_capas += str(selectedLayerFuses)
                layerFuses = QgsProject.instance().mapLayersByName(selectedLayerFuses)[0]
                datosFuses, grafoFuses = self.ReaderDataFuses(layerFuses, toler, grafoMT)
                capa_fuses = True
                if datosFuses == 0:
                    self.progress.close()
                    return 0
                    
                    
            # ##########################################################
            # ################## Lectura capacitores ###################
            # ##########################################################
            
            capa_cap = False
            # Recibe la capa de fuses
            selectedLayerCap = self.dlg.comboBox_cap.currentText()
            if len(selectedLayerCap)!= 0:
                self.nombres_capas += "\n!Layer Capacitors: "
                self.nombres_capas += str(selectedLayerCap)
                layerCap = QgsProject.instance().mapLayersByName(selectedLayerCap)[0]
                grafoCap = self.ReaderDataCapacitors(layerCap, toler)
                capa_cap = True
                if grafoCap == 0:
                    self.progress.close()
                    return 0
            
            # ##########################################################
            # ################## Lectura reclosers #####################
            # ##########################################################
            
            capa_reclosers = False
            # Recibe la capa de reclosers
            selectedLayerReclosers = self.dlg.comboBox_recon.currentText()
            if len(selectedLayerReclosers)!= 0:
                self.nombres_capas += "\n!Layer Reclosers: "
                self.nombres_capas += str(selectedLayerReclosers)
                layerReclosers = QgsProject.instance().mapLayersByName(selectedLayerReclosers)[0]
                datosReclosers, grafoReclosers = self.ReaderDataReclosers(layerReclosers, toler, grafoMT)
                capa_reclosers = True
                if datosReclosers == 0:
                    self.progress.close()
                    return 0
                    
            # ##########################################################
            # ################## Conectividad LMT  #####################
            # ##########################################################    
            
            if grafoMT.number_of_edges()== 0:
                LMTactive = False
            else:  # Se identifica la conectividad de las líneas de Media Tensión.
                startTimeMT = time.time()
                LMTactive = True
                nodesMT = grafoMT.nodes() # guarda todos los nodos del grafoMT en una lista
                ################## Asignación de Bus a líneas MT
                for node in nodesMT:  # node es el nodo en nombramiento
                    i = 1
                    #print(" --------------------------------------------------------------------- ")
                    #print(grafoMT[node])
                    for secondNode in grafoMT[
                        node]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                        dataLine = grafoMT[node][secondNode]  # info de la linea
                        
                        if dataLine['PHASE'] == '.1.2.3':
                            if node in busMT_List:
                                bus = busMT_List[node]['bus']
                            else:
                                bus = 'BUSMV' + circuitName + str(busMTid)
                                busMTid += 1

                            nodeFrom, nodeTo, connec, EQUAL, X1Y1, X2Y2 = self.BusAdapterLines(grafoMT, SubsNode, dataLine)
                            dataLine['X1'] = X1Y1[0]
                            dataLine['Y1'] = X1Y1[1]
                            dataLine['X2'] = X2Y2[0]
                            dataLine['Y2'] = X2Y2[1]
                            dataLine['nodo1'] = nodeFrom
                            dataLine['nodo2'] = nodeTo
                            if dataLine['nodo1'] == dataLine['nodo2']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                                aviso = "Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (" 
                                aviso += str(busMT_List[node]['X'])+ ", " + str(busMT_List[node]['Y'])+ ")"
                                aviso_bus = True
                                mensaje_mt += aviso + "\n"
                                
                            elif node == dataLine['nodo1']:  #############CONDICION DE MAS CERCANO
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                            elif node == dataLine['nodo2']:  #############CONDICION DE MAS CERCANO
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X2'],
                                                        'Y': dataLine['Y2'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}

                            # otherNodes = grafoMT[node].keys()
                            # for j in range(i, len(otherNodes)):

                            i += 1

                        else:
                            pass
                for node in nodesMT:  # node es el nodo en nombramiento
                    i = 1
                    for secondNode in grafoMT[
                        node]:  # itera sobre las lineas que contienen el nodo. secondNode es el otro nodo de la linea
                        dataLine = grafoMT[node][secondNode]  # info de la linea
                        if dataLine['PHASE'] == '.1.2' or dataLine['PHASE'] == '.2.3' or dataLine['PHASE'] == '.1.3':
                            if node in busMT_List:
                                bus = busMT_List[node]['bus']
                            else:
                                bus = 'BUSMV' + circuitName + str(busMTid)
                                busMTid += 1

                            nodeFrom, nodeTo, connec, EQUAL, X1Y1, X2Y2 = self.BusAdapterLines(grafoMT, SubsNode, dataLine)
                            dataLine['X1'] = X1Y1[0]
                            dataLine['Y1'] = X1Y1[1]
                            dataLine['X2'] = X2Y2[0]
                            dataLine['Y2'] = X2Y2[1]
                            dataLine['nodo1'] = nodeFrom
                            dataLine['nodo2'] = nodeTo
                            if dataLine['nodo1'] == dataLine['nodo2']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                                aviso = QCoreApplication.translate('dialog',
                                                                   u'Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (')+ str(
                                    busMT_List[node]['X'])+ ', ' + str(busMT_List[node]['Y'])+ ')'
                                    
                                aviso_bus = True
                                mensaje_mt += aviso + "\n"
                                
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X2'],
                                                        'Y': dataLine['Y2'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}

                            i += 1
                        else:
                            pass
                for node in nodesMT:  # node es el nodo en nombramiento
                    i = 1
                    for secondNode in grafoMT[
                        node]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                        dataLine = grafoMT[node][secondNode]  # info de la linea
                        if dataLine['PHASE'] == '.1' or dataLine['PHASE'] == '.2' or dataLine['PHASE'] == '.3':
                            if node in busMT_List:
                                bus = busMT_List[node]['bus']
                            else:
                                bus = 'BUSMV' + circuitName + str(busMTid)
                                busMTid += 1

                            nodeFrom, nodeTo, connec, EQUAL, X1Y1, X2Y2 = self.BusAdapterLines(grafoMT, SubsNode, dataLine)
                            dataLine['X1'] = X1Y1[0]
                            dataLine['Y1'] = X1Y1[1]
                            dataLine['X2'] = X2Y2[0]
                            dataLine['Y2'] = X2Y2[1]
                            dataLine['nodo1'] = nodeFrom
                            dataLine['nodo2'] = nodeTo
                            if dataLine['nodo1'] == dataLine['nodo2']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                                aviso = QCoreApplication.translate('dialog',
                                                                   u'Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (')+ str(
                                    busMT_List[node]['X'])+ ', ' + str(busMT_List[node]['Y'])+ ')'
                                aviso_bus = True
                                mensaje_mt += aviso + "\n"
                                
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X1'],
                                                        'Y': dataLine['Y1'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}  #
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoMT
                                if node not in busMT_List:
                                    busMT_List[node] = {"NPHAS": dataLine["NPHAS"], 'bus': bus, 'X': dataLine['X2'],
                                                        'Y': dataLine['Y2'], "GRAFO": grafoMT,
                                                        "VOLTAGELL": dataLine["VOLTOPRLL"],
                                                        "VOLTAGELN": dataLine["VOLTOPRLN"],
                                                        "PHASES": dataLine["PHASE"]}
                            i += 1

                

                for node in nodesMT:  # Revisión de fases entre líneas
                    for secondNode in grafoMT[node]:
                        
                        otherNodes = list(grafoMT[node].keys())
                        for j in range(i, len(otherNodes)):
                            if secondNode != list(grafoMT[node].keys())[-1] and grafoMT[node][secondNode] != \
                                    grafoMT[node][otherNodes[j]]:  # Evita comparar líneas 2 veces o más
                                if (node == grafoMT[node][secondNode]['nodo1'])and (node == grafoMT[node][otherNodes[j]]['nodo2']):
                                    NodoLineaCerc = otherNodes[j]
                                    NodoLinealejan = secondNode
                                    if (phaseOperations.linePhaseMT(grafoMT[node][NodoLineaCerc]['PHASE'],
                                                                    grafoMT[node][NodoLinealejan]['PHASE'])== 0):
                                        aviso = QCoreApplication.translate('dialog',
                                                                           u'Conexión de fases distintas en (')+ str(
                                            busMT_List[node]['X'])+ ',' + str(
                                            busMT_List[node]['Y'])+ QCoreApplication.translate('dialog',
                                                                                                u'). Línea MT ')+ str(
                                            grafoMT[node][secondNode]['NPHAS'])+ QCoreApplication.translate(
                                            'dialog', 'F con fase ')+ str(
                                            grafoMT[node][secondNode]['PHASE'])+ QCoreApplication.translate(
                                            'dialog', ' y MT ')+ str(
                                            grafoMT[node][otherNodes[j]]['NPHAS'])+ QCoreApplication.translate(
                                            'dialog', 'F con fase ')+ str(grafoMT[node][otherNodes[j]]['PHASE'])
                                        aviso_fases = True
                                        mensaje_mt += aviso + " 1\n"
                                        
                                        
                                elif (node == grafoMT[node][secondNode]['nodo2'])and (node == grafoMT[node][otherNodes[j]]['nodo1']):
                                    NodoLineaCerc = secondNode
                                    NodoLinealejan = otherNodes[j]
                                    if (phaseOperations.linePhaseMT(grafoMT[node][NodoLineaCerc]['PHASE'],
                                                                    grafoMT[node][NodoLinealejan]['PHASE'])== 0):
                                        aviso = QCoreApplication.translate('dialog',
                                                                           u'Conexión de fases distintas en (')+ str(
                                            busMT_List[node]['X'])+ ',' + str(
                                            busMT_List[node]['Y'])+ QCoreApplication.translate('dialog',
                                                                                                u'). Línea MT ')+ str(
                                            grafoMT[node][secondNode]['NPHAS'])+ QCoreApplication.translate(
                                            'dialog', 'F con fase ')+ str(
                                            grafoMT[node][secondNode]['PHASE'])+ QCoreApplication.translate(
                                            'dialog', ' y MT ')+ str(
                                            grafoMT[node][otherNodes[j]]['NPHAS'])+ QCoreApplication.translate(
                                            'dialog', 'F con fase ')+ str(grafoMT[node][otherNodes[j]]['PHASE'])
                                        aviso_fases = True
                                        mensaje_mt += aviso + " 2\n"
                i += 1
            
                
            endTimeMT = time.time()
                
            self.progress.progressBar.setValue(5)
            
            # Recibe las capas con transformadores seleccionada
            selectedLayerTR1 = self.dlg.comboBox_TR1.currentText()
            selectedLayerTR2 = self.dlg.comboBox_TR2.currentText()
            selectedLayerTR3 = self.dlg.comboBox_TR3.currentText()

            datosT1F = []
            datosT2F = []
            datosT3F_Multi = []
            datosT3F_Single = []
            
            # Transformadores Grafos
            Graph_T1F = nx.Graph()
            Graph_T2F = nx.Graph()
            Graph_T3F_multi = nx.Graph()
            Graph_T3F_single = nx.Graph()
            busBTid = 1

            try:  ## Lectura y CONECTIVIDAD TRANSFORMADORES
                
                #### Lectura de datos y creación de grafos de transformadores
                if len(selectedLayerTR1)!= 0:
                    layerT1 = QgsProject.instance().mapLayersByName(selectedLayerTR1)[0]
                    layerT1.startEditing()
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT1, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                                   layerT1, toler, datosT3F_Multi,
                                   datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                                   Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal)
                    if datosT3F_Multi == 0:
                        self.progress.close()
                        return 0

                if len(selectedLayerTR2)!= 0:
                    layerT2 = QgsProject.instance().mapLayersByName(selectedLayerTR2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT2, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                                   layerT2, toler, datosT3F_Multi, datosT3F_Single,
                                   datosT2F, datosT1F, Graph_T3F_multi,
                                   Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal)
                    if datosT3F_Multi == 0:
                        self.progress.close()
                        return 0
                    layerT2.startEditing()
                if len(selectedLayerTR3)!= 0:
                    layerT3 = QgsProject.instance().mapLayersByName(selectedLayerTR3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT3, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                                   layerT3, toler, datosT3F_Multi, datosT3F_Single,
                                   datosT2F, datosT1F, Graph_T3F_multi,
                                   Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS,
                                   grafoBTTotal)
                    if datosT3F_Multi == 0:
                        self.progress.close()
                        return 0
                    layerT3.startEditing()
                if (len(datosT1F)== 0 and len(datosT2F)== 0 and len(datosT3F_Multi)== 0 and len(
                        datosT3F_Single)== 0):
                    LTRactive = False
                else:  ##### Asignación de bus a transformadores
                    LTRactive = True
                    if Graph_T1F.number_of_nodes()> 0:  # Verifica que existen transformadores monofásicos
                        Graph_T1F, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T1F, busMT_List, busMTid,
                                                                                                     busBT_List, busBTid, u"monofásico", grafoMT)
                    if Graph_T3F_multi.number_of_nodes()> 0:  # Verifica que existen transformadores trifásicos de 3 unidades monofásicas
                        Graph_T3F_multi, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T3F_multi, busMT_List, busMTid, 
                                                                                                           busBT_List, busBTid, u"trifásico", grafoMT)
                    if Graph_T3F_single.number_of_nodes()> 0:  # Verifica que existen transformadores trifásicos de 1 unidad
                        Graph_T3F_single, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T3F_single, busMT_List,
                                                                                                            busMTid, busBT_List, busBTid, u"trifásico", grafoMT)
                    if Graph_T2F.number_of_nodes()> 0:  # Verifica que existen transformadores bifásicos
                        Graph_T2F, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T2F, busMT_List, busMTid,
                                                                                                     busBT_List, busBTid, u"bifásico", grafoMT)
                        

            except KeyError:
                self.print_error()
                QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate('dialog', "de las tablas de atributos de transformadores"))
                    
                LTRactive = False
                Error = True
            endTimeTraf = time.time()
            self.progress.progressBar.setValue(10)
            
            
            
            # ##########################################################
            # ############### Lectura Reguladores ###################
            # ##########################################################
            capa_reg = False
            # Recibe la capa de reguladores
            selectedLayer_reg = self.dlg.comboBox_reg.currentText()
            if len(selectedLayer_reg)!= 0:
                self.nombres_capas += "\n!Layer Regulators: "
                self.nombres_capas += str(selectedLayer_reg)
                layer_reg = QgsProject.instance().mapLayersByName(selectedLayer_reg)[0]
                datos_reg, grafo_reg = self.ReaderDataReg(layer_reg,
                                                          toler)
                capa_reg = True
                if datos_reg == 0:
                    self.progress.close()
                    return 0
                Bus_reg = self.BusAsignationReg(grafo_reg, busMT_List,
                                                busMTid)
                grafo_reg, busMT_List, busMTid, buslist_reg = Bus_reg
                    
            ############################################## Baja tensión
            
            selectedLayerBT1 = self.dlg.comboBox_LBT1.currentText() # Índice de layer_list con lineas BT seleccionada en la lista desplegable
            selectedLayerBT2 = self.dlg.comboBox_LBT2.currentText() # Índice de layer_list con lineas BT seleccionada en la lista desplegable
            selectedLayerBT3 = self.dlg.comboBox_LBT3.currentText() # Índice de layer_list con lineas BT seleccionada en la lista desplegable

            
            datosLBT = []  # datosLBT guarda informacion de ubicacion y fase de las lineas BT3
            grafoBT = nx.Graph()
            LBTactive = False
            first_line_bt_aer = True
            first_line_bt_sub = True
            line_bt_aer = ""
            line_bt_sub = ""
			
           ###  Lectura de datos y creación de grafo de LINEAS BAJA TENSION
                
            if len(selectedLayerBT1)!= 0:
                try:
                    layerBT1 = QgsProject.instance().mapLayersByName(selectedLayerBT1)[0]
                    LBTactive = True
                    #Subterránea
                    if self.dlg.checkBox_LBT1.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                        # Se agrega el nombre de la capa sub
                        line_bt_sub += "\n!Layers LinesLV_sub: "
                        line_bt_sub += str(selectedLayerBT1) + ","
                        first_line_bt_sub = False
                    # Aérea
                    else:
                        subterranea = False
                        line_bt_aer += "\n!Layers LinesLV_aer: "
                        line_bt_aer += str(selectedLayerBT1) + ","
                        first_line_bt_aer = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT1, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT1, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                    
                    if datosLBT == 0:
                        self.progress.close()
                        return 0
                
                except Exception:
                    self.print_error()       
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 1 de líneas de baja tensión"))
                    
                    LBTactive = False
                    Error = True

            if len(selectedLayerBT2)!= 0:
                try:
                    layerBT2 = QgsProject.instance().mapLayersByName(selectedLayerBT2)[0]
                    LBTactive = True
                    # Subterránea
                    if self.dlg.checkBox_LBT2.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                        # Se agrega el nombre de la capa sub
                        if first_line_bt_sub is True:
                            line_bt_sub += "\n!Layers LinesLV_sub: "
                            line_bt_sub += str(selectedLayerBT2) + ","
                            first_line_bt_sub = False
                        else:
                            line_bt_sub += str(selectedLayerBT2) + ","
                    # Aérea
                    else:
                        subterranea = False
                        if first_line_bt_aer is True:
                            line_bt_aer += "\n!Layers LinesLV_aer: "
                            line_bt_aer += str(selectedLayerBT2) + ","
                            first_line_bt_aer = False
                        else:
                            line_bt_aer += str(selectedLayerBT2) + ","
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT2, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT2, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                    if datosLBT == 0:
                        self.progress.close()
                        return 0
                    
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 2 de líneas de baja tensión"))
                    
                    LBTactive = False
                    Error = True
                                                                         
            if len(selectedLayerBT3)!= 0:
                try:
                    layerBT3 = QgsProject.instance().mapLayersByName(selectedLayerBT3)[0]
                    LBTactive = True
                    # Subterránea
                    if self.dlg.checkBox_LBT3.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                        # Se agrega el nombre de la capa sub
                        if first_line_bt_sub is True:
                            line_bt_sub += "\n!Layers LinesLV_sub: "
                            line_bt_sub += str(selectedLayerBT3) + ","
                            first_line_bt_sub = False
                        else:
                            line_bt_sub += str(selectedLayerBT3) + ","
                        
                    # Aérea
                    else:
                        subterranea = False
                        if first_line_bt_aer is True:
                            line_bt_aer += "\n!Layers LinesLV_aer: "
                            line_bt_aer += str(selectedLayerBT3) + ","
                            first_line_bt_aer = False
                        else:
                            line_bt_aer += str(selectedLayerBT3) + ","
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT3, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT3, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                    if datosLBT == 0:
                        self.progress.close()
                        return 0
                
                except Exception:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 3 de líneas de baja tensión"))
                    LBTactive = False
                    Error = True

            endTimeBT = time.time()

            # Se agregan los nombres de capas de BT
            self.nombres_capas += line_bt_sub + line_bt_aer
            self.progress.progressBar.setValue(15)
            # 2.3-Crea lista con las coordenadas de Inicio, Final y Fase de las lineas de media tension.
                  
            selectedLayerACO1 = self.dlg.comboBox_ACO1.currentText() # Índice de layer_list con acometidas en la lista desplegable
            selectedLayerACO2 = self.dlg.comboBox_ACO2.currentText() # Índice de layer_list con acometidas en la lista desplegable
            selectedLayerACO3 = self.dlg.comboBox_ACO3.currentText() # Índice de layer_list con acometidas en la lista desplegable
            
            datosACO = []  # datosACO guarda informacion de ubicacion de acometidas
            grafoACO = nx.Graph()
            ACOactive = False
            ### Lectura de datos y construcción de grafo de acometidas
                
            if len(selectedLayerACO1)!= 0:
                try:
                    self.nombres_capas += "\n!Layers Acometidas: "
                    self.nombres_capas += str(selectedLayerACO1) + ","
                    layerACO1 = QgsProject.instance().mapLayersByName(selectedLayerACO1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO1, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO1, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return 0
                    ACOactive = True
                except Exception:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate('dialog', "de la tabla 1 de atributos de acometidas"))
                    ACOactive = False
                    Error = True
            if len(selectedLayerACO2)!= 0:
                try:
                    if not ACOactive:
                        self.nombres_capas += "\n!Layers Acometidas: "
                    self.nombres_capas += str(selectedLayerACO2) + ","
                    layerACO2 = QgsProject.instance().mapLayersByName(selectedLayerACO2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO2, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO2, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return 0
                    ACOactive = True
                except Exception:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate('dialog', "de la tabla 2 de atributos de acometidas"))
                    ACOactive = False
                    Error = True
            if len(selectedLayerACO3)!= 0:
                try:
                    if not ACOactive:
                        self.nombres_capas += "\n!Layers Acometidas: "
                    self.nombres_capas += str(selectedLayerACO3)
                    layerACO3 = QgsProject.instance().mapLayersByName(selectedLayerACO3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO3, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO3, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return 0
                    ACOactive = True
                except Exception:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas")+ "\n" + QCoreApplication.translate('dialog', "de la tabla 3 de atributos de acometidas"))
                    ACOactive = False
                    Error = True
                    
            ### Lectura de datos y construcción de grafo de CARGAS MT
            selectedLayerCA_MT1 = self.dlg.comboBox_CAMT1.currentText()
            selectedLayerCA_MT2 = self.dlg.comboBox_CAMT2.currentText()
            selectedLayerCA_MT3 = self.dlg.comboBox_CAMT3.currentText()
            
            datosMVCAR = []
            kWhMVload = []
            grafoCAR_mv = nx.Graph()
            CARmvactive = False
            
            if len(selectedLayerCA_MT1)!= 0:
                try:
                    if not CARmvactive:
                        self.nombres_capas += "\n!Layers LoadsMV: "
                    self.nombres_capas += str(selectedLayerCA_MT1) + ","
                    layerCAMV1 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV1, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV1, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT)
                    if datosMVCAR == 0:
                        self.progress.close()
                        return 0
                    CARmvactive = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de cargas de media tensión")
                    Error = True
                    
            if len(selectedLayerCA_MT2)!= 0:
                try:
                    if not CARmvactive:
                        self.nombres_capas += "\n!Layers LoadsMV: "
                    self.nombres_capas += str(selectedLayerCA_MT2) + ","
                    layerCAMV2 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV2, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV2, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT)
                    if datosMVCAR == 0:
                        self.progress.close()
                        return 0
                    CARmvactive = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de cargas de media tensión")
                    Error = True
                    
            if len(selectedLayerCA_MT3)!= 0:
                try:
                    if not CARmvactive:
                        self.nombres_capas += "\n!Layers LoadsMV: "
                    self.nombres_capas += str(selectedLayerCA_MT3)
                    layerCAMV3 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT3)[0]
                    
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV3, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV3, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT)
                    if datosMVCAR == 0:
                        self.progress.close()
                        return 0
                    CARmvactive = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de cargas de media tensión")
                    Error = True
    
            # 2.4-Crea listas con coordenadas y Fase de las cargas
            ### Lectura de datos y construcción de grafo de CARGAS BT
            selectedLayerCA1 = self.dlg.comboBox_CA1.currentText()
            selectedLayerCA2 = self.dlg.comboBox_CA2.currentText()
            selectedLayerCA3 = self.dlg.comboBox_CA3.currentText()
            
            datosCAR = []
            kWhLVload = []
            grafoCAR = nx.Graph()
            CARactive = False
            
            if len(selectedLayerCA1)!= 0:
                try:
                    if not CARactive:
                        self.nombres_capas += "\n!Layers LoadsLV: "
                    self.nombres_capas += str(selectedLayerCA1) + ","
                    layerCA1 = QgsProject.instance().mapLayersByName(selectedLayerCA1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA1, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA1, datosCAR, grafoCAR, kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT, grafoACO)
                    if datosCAR == 0:
                        self.progress.close()
                        return 0
                    CARactive = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCA2)!= 0:
                try:
                    if not CARactive:
                        self.nombres_capas += "\n!Layers LoadsLV: "
                    self.nombres_capas += str(selectedLayerCA2) + ","
                    layerCA2 = QgsProject.instance().mapLayersByName(selectedLayerCA2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA2, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA2, datosCAR, grafoCAR, kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT, grafoACO)
                    if datosCAR == 0:
                        self.progress.close()
                        return 0
                    CARactive = True
                    
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCA3)!= 0:
                try:
                    if not CARactive:
                        self.nombres_capas += "\n!Layers LoadsLV: "
                    self.nombres_capas += str(selectedLayerCA3)
                    layerCA3 = QgsProject.instance().mapLayersByName(selectedLayerCA3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA3, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA3, datosCAR, grafoCAR,  kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT, grafoACO)
                    if datosCAR == 0:
                        self.progress.close()
                        return 0
                    CARactive = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                    
            # 2.5-Crea listas con coordenadas y Fase de las cargas iluminación
            ### Lectura de datos y construcción de grafo de CARGAS BT_iluminación
            selectedLayerCI1 = self.dlg.comboBox_CI1.currentText()
            selectedLayerCI2 = self.dlg.comboBox_CI2.currentText()
            selectedLayerCI3 = self.dlg.comboBox_CI3.currentText()
            
            datosCAR_i = []
            grafoCAR_i = nx.Graph()
            CAR_i_active = False
            
            if len(selectedLayerCI1)!= 0:
                try:
                    if not CAR_i_active:
                        self.nombres_capas += "\n!Layers StreetLightning: "
                    self.nombres_capas += str(selectedLayerCI1) + ","
                    layerCI1 = QgsProject.instance().mapLayersByName(selectedLayerCI1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCI1, "DSSName")
                    datosCAR_i, grafoCAR_i, grafoBTTotal = self.ReaderDataLoadLights(layerCI1, datosCAR_i, grafoCAR_i, toler, indexDSS, grafoBTTotal)#######
                    if datosCAR_i == 0:
                        self.progress.close()
                        return 0
                    CAR_i_active = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCI2)!= 0:
                try:
                    if not CAR_i_active:
                        self.nombres_capas += "\n!Layers StreetLightning: "
                    self.nombres_capas += str(selectedLayerCI1) + ","
                    layerCI2 = QgsProject.instance().mapLayersByName(selectedLayerCI2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCI2, "DSSName")
                    datosCAR_i, grafoCAR_i, grafoBTTotal = self.ReaderDataLoadBT(layerCI2, datosCAR_i, grafoCAR_i, toler, indexDSS, grafoBTTotal)#######
                    if datosCAR_i == 0:
                        self.progress.close()
                        return 0
                    CAR_i_active = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCI3)!= 0:
                try:
                    if not CAR_i_active:
                        self.nombres_capas += "\n!Layers StreetLightning: "
                    self.nombres_capas += str(selectedLayerCI3) + ","
                    layerCI1 = QgsProject.instance().mapLayersByName(selectedLayerCI3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCI3, "DSSName")
                    datosCAR_i, grafoCAR_i, grafoBTTotal = self.ReaderDataLoadBT(layerCI3, datosCAR_i, grafoCAR_i, toler, indexDSS, grafoBTTotal)#######
                    if datosCAR_i == 0:
                        self.progress.close()
                        return 0
                    CAR_i_active = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
            
            ### Lectura de datos y construcción de grafo de Buses eléctricos
            selectedLayerBuses = self.dlg.comboBox_plantelbuses.currentText()
                        
            datosBuses = []
            kWhBuses = []
            grafoBuses = nx.Graph()
            buses_act = False
            Error_Buses = False
            
            if len(selectedLayerBuses)!= 0:
                try:
                    """
                    dir_csvbuses = self.dlg.lineEdit_csvbus.text()
                    dir_csvcharg = self.dlg.lineEdit_dircsvcarg.text()
                    
                    if dir_csvbuses == "" or dir_csvcharg == "":
                        QMessageBox.critical(None, "QGIS2OpenDSS Error", "Favor indique el nombre de los csv para realizar la simulación de buses eléctricos")
                        self.progress.close()
                        return 0
                    """ 
                    
                    layerBuses = QgsProject.instance().mapLayersByName(selectedLayerBuses)[0]
                    datosBuses, grafoBuses, kWhBuses = self.ReaderDataBuses(layerBuses, datosBuses, grafoBuses, kWhBuses, toler, grafoMT)
                    
                    print("Final buses")
                    if datosBuses == 0:
                        self.progress.close()
                        return 0
                    buses_act = True
                
                except Exception:
                    self.print_error()   
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de buses")
                    Error_Buses = True
                    
            
            
            self.progress.progressBar.setValue(20)
            grafoBT, grafoACO, grafoCAR = self.IslandIdentification(grafoBTTotal, grafoBT, grafoACO, grafoCAR)
            self.progress.progressBar.setValue(25)
            
            # ######################################
            # ######################################
            # ######  VEHÍCULOS ELÉCTRICOS  ########
            # ######################################
            # ######################################
            name_layer_evs = self.dlg.comboBox_EV.currentText()#Recibe el nombre de la capa de vehículos eléctricos seleccionada
            if (name_layer_evs != "" and name_layer_evs != None
                and CARactive is True): #Verifica que se haya seleccionado alguna capa en el combobox de vehículos eléctricos, y que se seleccione una capa de cargas
                try:
                    #Declaración de variables necesarias para EV
                    datosEV = []
                    kWhLVev = []
                    grafoEV = nx.Graph()
                    
                    layerEV = QgsProject.instance().mapLayersByName(name_layer_evs)[0] #Se selecciona la capa de vehículos eléctricos
                    datosEV, grafoEV, grafoBTTotal = self.ReaderDataEV(layerEV, datosEV, grafoEV, kWhLVev,
                                                                       toler, indexDSS, grafoBTTotal, grafoCAR, grafoBT)#Lectura de capa de EV
                    
                    if datosEV == 0: #caso en que haya un error leyendo la capa de VE
                        self.print_error()
                        mensaje =  "Verifique que la capa de vehículos "
                        mensaje += "eléctricos tenga todos los "
                        mensaje += "datos solicitados"
                        title = "QGIS2OpenDSS Error"
                        QMessageBox.critical(None, title, mensaje)
                        self.progress.close()
                        return 0
                    print("VEHÍCULOS ELÉCTRICOS FINAL")
                except Exception:
                    self.print_error()
                    title = "QGIS2OpenDSS Error"
                    msg = "Verifique que la capa de vehículos "
                    msg += "eléctricos tenga todos los "
                    msg += "datos solicitados"
                    QMessageBox.critical(None, title, msg)
                    Error = True
            
            
            self.progress.progressBar.setValue(30)
            dir_archivo = dir_scripts + "\Pruebas.txt"
                  
            #archivo = open(dir_archivo, 'a')
            #archivo.write("\n\nData line error final\n")
            
        
            ## CONECTIVIDAD LINEAS DE BAJA TENSION
            mensaje_bt = ""
            aviso_busBT = False
            
            if len(datosLBT)== 0:  
                LBTactive = False
            else:
                startTimeBT = time.time()
                LBTactive = True
                nodesBT = grafoBT.nodes() # guarda todos los nodos del grafoBT en una lista
                ################## Asignación de Bus a líneas BT
                
                for node in nodesBT:  # node es el nodo en nombramiento
                    try:
                        if node in busBT_List:  # Verifica si el nodo existe (significa que está conectada a un transformador)
                            bus = busBT_List[node]["bus"]
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBTid += 1
                        for secondNode in grafoBT[node]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                            dataLine = grafoBT[node][secondNode]  # info de la linea
                            #archivo.write(str(str(dataLine)+ "\n"))
                            #print("Data Line error final", dataLine)
                            if dataLine['nodo1'] == dataLine['nodo2']:  # Verifica si la línea empieza y termina en el mismo punto
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoBT
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'],
                                                    "GRAFO": grafoBT, "VOLTAGELL": dataLine["TRAFVOLTLL"],
                                                    "VOLTAGELN": dataLine['TRAFVOLTLN'],
                                                    "GRUPO": dataLine["GRUPO"]}  #
                                msg = u'Existe una línea de BT con bus1 igual a bus2 dada su cercanía en ('
                                aviso = QCoreApplication.translate('dialog', msg) + str(busBT_List[node]['X'])+ ', ' + str(busBT_List[node]['Y'])+ ')'
                                mensaje_bt += aviso + "\n"
                                aviso_busBT = True
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoBT,
                                                    "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine['TRAFVOLTLN'],
                                                    "GRUPO": dataLine["GRUPO"]}  #
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X2'], 'Y': dataLine['Y2'], "GRAFO": grafoBT,
                                                    "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"],
                                                    "GRUPO": dataLine["GRUPO"]}

                    except Exception:
                        self.print_error()
                
            
            ## CONECTIVIDAD DE ACOMETIDAS
            mensaje_aco = ""
            contador_err = 0
            aviso_busAco = False
                        
            if len(datosACO)== 0:  
                ACOactive = False
            else:
                startTimeAco = time.time()
                ACOactive = True
                nodesACO = grafoACO.nodes() # guarda todos los nodos del grafoACO en una lista
                
                ################## Asignación de Bus a líneas Acometidas
                for node in nodesACO:  # node es el nodo en nombramiento
                    try:
                        if node in busBT_List:  # Verifica si el nodo existe (significa que está conectada a un transformador o línea de baja tensión)
                            bus = busBT_List[node]["bus"]
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBTid += 1
                        for secondNode in grafoACO[node]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                            dataLine = grafoACO[node][secondNode]  # info de la linea
                            if dataLine['nodo1'] == dataLine['nodo2']:  # Verifica si la línea empieza y termina en el mismo punto
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoACO
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoACO,
                                                    "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"],
                                                     "GRUPO": dataLine['GRUPO']}
                                msg = u'Existe una línea de acometidas con bus1 igual a bus2 dada su cercanía en ('
                                aviso = QCoreApplication.translate('dialog', msg)+ str(busBT_List[node]['X'])+ ', ' + str(busBT_List[node]['Y'])+ ')'
                                mensaje_aco += aviso + "\n"
                                aviso_busAco = True
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'],
                                                    "GRAFO": grafoACO, "VOLTAGELL": dataLine["TRAFVOLTLL"],
                                                    "VOLTAGELN": dataLine["TRAFVOLTLN"], "GRUPO": dataLine['GRUPO']}
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X2'], 'Y': dataLine['Y2'], "GRAFO": grafoACO,
                                                    "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"],
                                                    "GRUPO": dataLine['GRUPO']}
                        endTimeAco = time.time()
                    except Exception:
                        self.print_error()
                        if contador_err == 0:
                            contador_err = 1
                
            
            ### CONECTIVIDAD DE PLANTELES DE BUSES
            #nuevo
            if buses_act is True:  
                #Datos de la subestación (necesarios sólo si la carga no tiene un bus asociado)
                datos_subest = list(grafoSubt.nodes(data=True))
                dato = datos_subest[0][1]
                tension_ll = dato['VOLTAJEMED']
                tension_ln = dato['VOLTAJEMEDLN']               
                
                startTimeLoad = time.time()
                graphNodes = list(grafoBuses.nodes(data=True))
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        
                        if nodo in busMT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de MT
                            grafoBuses.nodes[nodo]["BUS"] = busMT_List[nodo]["bus"]
                            grafoBuses.nodes[nodo]["VOLTAGELL"] = busMT_List[nodo]["VOLTAGELL"]
                            grafoBuses.nodes[nodo]["VOLTAGELN"] = busMT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSMV_plant_buses_' + circuitName + str(busMTid)
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoBuses,
                                                "VOLTAGELN": "0.12", 'NPHAS': dataList["PHASE"], 'PHASES': dataList['LOADCONNS']}
                            grafoBuses.nodes[nodo]["BUS"] = bus
                            grafoBuses.nodes[nodo]["VOLTAGELL"] = tension_ll #revisar con Tavo
                            grafoBuses.nodes[nodo]["VOLTAGELN"] = tension_ln #revisar con Tavo
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (')+ str(dataList["X1"])+ ',' + str(dataList["Y1"])+ ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busMTid += 1
                        endTimeLoad = time.time()
                    except Exception:
                        self.print_error()
            
            #nuevo
            CARmv_active = False
            if len(datosMVCAR)!= 0:  ### CONECTIVIDAD DE CARGAS MT
                #Datos de la subestación (necesarios sólo si la carga no tiene un bus asociado)
                datos_subest = list(grafoSubt.nodes(data=True))
                dato = datos_subest[0][1]
                tension_ll = dato['VOLTAJEMED']
                tension_ln = dato['VOLTAJEMEDLN']
                                
                CARmv_active = True
                startTimeLoad = time.time()
                graphNodes = list(grafoCAR_mv.nodes(data=True))
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busMT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de MT
                            grafoCAR_mv.nodes[nodo]["BUS"] = busMT_List[nodo]["bus"]
                            grafoCAR_mv.nodes[nodo]["VOLTAGELL"] = busMT_List[nodo]["VOLTAGELL"]
                            grafoCAR_mv.nodes[nodo]["VOLTAGELN"] = busMT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSMV' + circuitName + str(busMTid)
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCAR_mv,
                                                "VOLTAGELN": "0.12", 'NPHAS': dataList["N_FASES"], 'PHASES': dataList['CONNS']}
                            grafoCAR_mv.nodes[nodo]["BUS"] = bus
                            #Asigna los datos de la subestacion si no existe ub bus asociado
                            grafoCAR_mv.nodes[nodo]["VOLTAGELL"] = str(tension_ll)#revisar con Tavo
                            grafoCAR_mv.nodes[nodo]["VOLTAGELN"] = str(tension_ln)#revisar con Tavo
                            aviso = 'Hay 1 carga MT desconectada: (' + str(dataList["X1"])
                            aviso += ',' + str(dataList["Y1"]) + ')'
                            self.mensaje_log_gral += aviso + "\n"
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busMTid += 1
                        endTimeLoad = time.time()
                    except Exception:
                        self.print_error()
                        
                        
            # Capacitores
            if capa_cap is True:  ### CONECTIVIDAD DE Capacitores
                #Datos de la subestación (necesarios sólo si la carga no tiene un bus asociado)
                datos_subest = list(grafoSubt.nodes(data=True))
                dato = datos_subest[0][1]
                tension_ll = dato['VOLTAJEMED']
                tension_ln = dato['VOLTAJEMEDLN']
                
                graphNodes = list(grafoCap.nodes(data=True))
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busMT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de MT
                            grafoCap.nodes[nodo]["BUS"] = busMT_List[nodo]["bus"]
                            grafoCap.nodes[nodo]["VOLTAGELL"] = busMT_List[nodo]["VOLTAGELL"]
                            grafoCap.nodes[nodo]["VOLTAGELN"] = busMT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSMV' + circuitName + str(busMTid)
                            volt_ln = dataList['VOLT_LN']
                            n_phases = dataList['PHASES']
                            fases = dataList['SERVICE']
                            dss_name = dataList["DSS_NAME"]
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCap,
                                                "VOLTAGELN": "0.12", 'NPHAS': n_phases, 'PHASES': fases}
                            grafoCap.nodes[nodo]["BUS"] = bus
                            #Asigna los datos de la subestacion si no existe ub bus asociado
                            grafoCap.nodes[nodo]["VOLTAGELL"] = str(tension_ll)#revisar con Tavo
                            grafoCap.nodes[nodo]["VOLTAGELN"] = str(tension_ln)#revisar con Tavo
                            aviso = "Hay 1 capacitor desconectado: " + dss_name
                            self.mensaje_log_gral += aviso + "\n"
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busMTid += 1
                        endTimeLoad = time.time()
                    except Exception:
                        self.print_error()


            # CONECTIVIDAD DE CARGAS BT
            if len(datosCAR)== 0:
                CARactive = False
            else:
                                        
                CARactive = True
                startTimeLoad = time.time()
                graphNodes = list(grafoCAR.nodes(data=True))
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busBT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de BT
                            grafoCAR.nodes[nodo]["BUS"] = busBT_List[nodo]["bus"]
                            try:
                                grafoCAR.nodes[nodo]["VOLTAGELL"] = busBT_List[nodo]["VOLTAGELL"]
                                grafoCAR.nodes[nodo]["VOLTAGELN"] = busBT_List[nodo]["VOLTAGELN"]
                            except KeyError:
                                grafoCAR.nodes[nodo]["VOLTAGELL"] = "0.24"
                                grafoCAR.nodes[nodo]["VOLTAGELN"] = "0.12"
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCAR,
                                                "VOLTAGELN": "0.12", 'GRUPO': dataList["GRUPO"]}
                            grafoCAR.nodes[nodo]["BUS"] = bus
                            grafoCAR.nodes[nodo]["VOLTAGELL"] = "0.24"
                            grafoCAR.nodes[nodo]["VOLTAGELN"] = "0.12"
                            
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (')+ str(dataList["X1"])+ ',' + str(dataList["Y1"])+ ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busBTid += 1
                        endTimeLoad = time.time()
                    except Exception:
                        self.print_error()
            
            # CONECTIVIDAD DE CARGAS DE ILUMINACIÓN
            if len(datosCAR_i)== 0:
                CAR_i_active = False
            else:
                                        
                CAR_i_active = True
                startTimeLoad = time.time()
                graphNodes = list(grafoCAR_i.nodes(data=True))
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busBT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de BT
                            grafoCAR_i.nodes[nodo]["BUS"] = busBT_List[nodo]["bus"]
                            try:
                                grafoCAR_i.nodes[nodo]["VOLTAGELL"] = busBT_List[nodo]["VOLTAGELL"]
                                grafoCAR_i.nodes[nodo]["VOLTAGELN"] = busBT_List[nodo]["VOLTAGELN"]
                            except KeyError:
                                grafoCAR_i.nodes[nodo]["VOLTAGELL"] = "0.24"
                                grafoCAR_i.nodes[nodo]["VOLTAGELN"] = "0.12"
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCAR_i,
                                                "VOLTAGELN": "0.12", 'GRUPO': dataList["GRUPO"]}
                            grafoCAR_i.nodes[nodo]["BUS"] = bus
                            grafoCAR_i.nodes[nodo]["VOLTAGELL"] = "0.24"
                            grafoCAR_i.nodes[nodo]["VOLTAGELN"] = "0.12"
                            
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (')+ str(dataList["X1"])+ ',' + str(dataList["Y1"])+ ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busBTid += 1
                        endTimeLoad = time.time()
                    except Exception:
                        self.print_error()
                        
    
            self.progress.progressBar.setValue(35)

            
            #######################################################  LOADSHAPE BUSES
            startTimeLoadShape = time.time()
            errorLoadShapeBuses = False
            LoadShapesFile = False #bandera que especifica si el archivo de loadshapes ya se abrió
            if (buses_act is True and folder_profile != ""):  # revisa si hay cargas
                filename = circuitName + '_Loadshapes.dss'
                output_shpdss = open(foldername + '/' + filename, 'w')
                LoadShapesFile = True
                #Asigna los loadshapes de Buses
                grafoBuses, errorLoadShapeBuses, output_shpdss = self.AssignLoadshapes(grafoBuses, folder_profile, foldername, circuitName, output_shpdss, Buses = True, StreetLightning=False)
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(40)
            startTimeWriting = time.time()
            
            #######################################################  LOADSHAPE CARGAS MT
            startTimeLoadShape = time.time()
            errorLoadShapeMV = False
            if (CARmvactive is True and folder_profile != ""):  # revisa si hay cargas
                if LoadShapesFile is False: #bandera que especifica si el archivo de loadshapes ya se abrió
                    filename = circuitName + '_Loadshapes.dss'
                    output_shpdss = open(foldername + '/' + filename, 'w')
                    LoadShapesFile = True
                
                #Asigna los loadshapes de MT
                grafoCAR_mv, errorLoadShapeMV, output_shpdss = self.AssignLoadshapes(grafoCAR_mv, folder_profile, foldername, circuitName, output_shpdss, Buses = False, StreetLightning=False)
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(45)
            startTimeWriting = time.time()

            #######################################################  LOADSHAPE CARGAS BT #revisar para agregar cargas MT
            startTimeLoadShape = time.time()
            errorLoadShape = False
            if (CARactive is True and folder_profile != ""):  # revisa si hay cargas
                if LoadShapesFile is False: #bandera que especifica si el archivo de loadshapes ya se abrió
                    filename = circuitName + '_Loadshapes.dss'
                    output_shpdss = open(foldername + '/' + filename, 'w')
                    LoadShapesFile = True
                #Asigna los loadshapes de BT
                if CAR_i_active == False:
                    grafoCAR, errorLoadShape, output_shpdss = self.AssignLoadshapes(grafoCAR, folder_profile, foldername, circuitName, output_shpdss, Buses = False, StreetLightning=False)
                else:
                    grafoCAR, errorLoadShape, output_shpdss = self.AssignLoadshapes(grafoCAR, folder_profile, foldername, circuitName, output_shpdss, Buses = False, StreetLightning=True)
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(50)
            startTimeWriting = time.time()

            ####9-Crea archivos de salida en la carpeta seleccionada
            """SALIDAS PARA OPENDSS"""
            output_filesQGIS2DSS = open(foldername + '/' + circuitName + '_OutputQGIS2OpenDSS.dss', 'w') # genera el archivo con la lista de archivos creados
            self.output_filesQGIS2DSS = output_filesQGIS2DSS
            # Líneas de media tensión y monitores de líneas de media tensión.
            output_filesQGIS2DSS.write('\nredirect Bibliotecas/bibliotecas.dss')
            
            # ##########################################################
            # ############### Lectura GD gran escala ###################
            # ##########################################################
            
            capa_gd_ls = False
            # Recibe la capa de reclosers
            selectedLayerGD_ls = self.dlg.comboBox_GD_ls.currentText()
            if len(selectedLayerGD_ls)!= 0:
                layerGD_ls = QgsProject.instance().mapLayersByName(selectedLayerGD_ls)[0]
                datosGD_ls, grafoGD_ls, filename_gdls = self.ReaderDataGD_LargeScale(layerGD_ls, toler, Graph_T3F_single)
                capa_gd_ls = True
                if datosGD_ls == 0:
                    self.progress.close()
                    return 0
            
            ###LECTURA Y CONECTIVIDAD DE GD LV
            selectedLayerGD = self.dlg.comboBox_GD_lv.currentText()

            if len(selectedLayerGD)!= 0:
                try:
                    layerGD = QgsProject.instance().mapLayersByName(selectedLayerGD)[0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerGD, "DSSName")
                    grafoGD = nx.Graph()
                    grafoGD, busBTid, busBT_List = self.ReaderDataGD(toler, layerGD, grafoGD, indexDSS, Graph_T3F_multi,
                                                                     Graph_T3F_single, Graph_T2F, Graph_T1F, grafoCAR,
                                                                     circuitName, busBTid, busBT_List, busMT_List)
                    if grafoGD == 0:
                        self.progress.close()
                        return 0
                except Exception:
                    self.print_error()   
            
            ###################################  Escritura LMT

            if LMTactive is True:
                try:
                    filename = circuitName + '_LinesMV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_lmtdss = open(foldername + '/' + filename, 'w')
                    filenameMon = circuitName + '_Monitors.dss'
                    # output_filesQGIS2DSS.write('\nredirect '+filenameMon)
                    output_monitorsdss = open(foldername + '/' + filenameMon, 'w')
                    
                    #First line definition
                    lineName = 'MV3P'+circuitName+'00'
                    
                    if mode != 'NOMODEL':
                        busfrom = 'BUSMV'+circuitName+'1'
                    else:
                        busfrom = 'Sourcebus'
                    
                    configFase = '.1.2.3'
                    busto = 'AFTERMETER'
                    value='0.00001'
             
                    line = 'new line.' + lineName + ' bus1=' + busfrom + configFase
                    line += ' bus2=' +  busto + configFase + ' r1=' + value + ' x1=' + value
                    line += ' length=' + value + ' units=m \n' # Se usan las variables anteriores en formar el string de salida
                    output_lmtdss.write(line)
                    
                    
                    # Switches
                    if capa_switches is True:
                        filename_switch = circuitName + '_Switches.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename_switch)
                        output_switches = open(foldername + '/' + filename_switch, 'w')
                        layerSwitches.startEditing()
                    
                    # Fusibles
                    if capa_fuses is True:
                        filename_fuses = circuitName + '_Fuses.dss'
                        line_output = '\nredirect Bibliotecas/Fuse_Curves.dss'
                        line_output += '\nredirect ' + filename_fuses
                        output_filesQGIS2DSS.write(line_output)
                        output_fuses = open(foldername + '/' + filename_fuses, 'w')
                        layerFuses.startEditing()
                    
                    # Reclosers
                    if capa_reclosers is True:
                        filename_reclosers = circuitName + '_Reclosers.dss'
                        line_output = '\nredirect ' + filename_reclosers
                        output_filesQGIS2DSS.write(line_output)
                        output_reclosers = open(foldername + '/' + filename_reclosers, 'w')
                        layerReclosers.startEditing()
    
                    if len(selectedLayerMT1)!= 0:
                        layerMT1.startEditing() # Activa modo edición
                    if len(selectedLayerMT2)!= 0:
                        layerMT2.startEditing() # Activa modo edición
                    if len(selectedLayerMT3)!= 0:
                        layerMT3.startEditing() # Activa modo edición
                    
                    n = 0
                    lista_switches = []
                    lista_fuses = []
                    lista_reclosers = []
                    for linea in grafoMT.edges(data=True):
                        DATOS = linea[2]
                        air_or_ugnd = DATOS['AIR_UGND']
                        configFase = DATOS['PHASE']  # Recibe la fase del bus en formato ODSS
                        cantFases = DATOS['NPHAS']  # Recibe la cantidad de fases de la linea
                        opervoltLN = DATOS['VOLTOPRLN']  # ,' !1ph_basekV=',opervolt
                        nodo1 = linea[0]
                        nodo2 = linea[1]
                        
                        # Recibe la informacion del tipo de conductor, cantidad y aislamiento
                        if air_or_ugnd == 'air':
                            equipment = str(cantFases)+ 'FMV' + str(DATOS['PHASIZ'])+ str(DATOS['PHAMAT'])
                            equipment += str(DATOS['NEUSIZ'])+ str(DATOS['NEUMAT'])+ '_' + str(DATOS['CCONF']) 
                        else:
                            equipment = str(cantFases)+ 'FMV' + str(DATOS['PHASIZ'])
                            equipment += str(DATOS['PHAMAT'])+ '_' + str(DATOS['NOMVOLT'])+ str(DATOS['INSUL'])
                        busfrom = DATOS['bus1']
                        busto = DATOS['bus2']
                        if float(DATOS['SHLEN'])== 0:
                            DATOS['SHLEN'] = 0.0001
                        sh_len = "{0:.4f}".format(DATOS['SHLEN']) # Recibe la longitud de la linea
                        if (busfrom == 'BUSMV' + circuitName + str(1))or (busto == 'BUSMV' + circuitName + str(1)):
                            lineName = "MV" + str(cantFases)+ 'P' + circuitName + str(n)
                        else:
                            lineName = "MV" + str(cantFases)+ 'P' + circuitName + str(n)
                        n += 1
                        
                        
                        # Switch
                        if capa_switches is True:
                            exists = grafoSwitch.has_node(nodo2) or grafoSwitch.has_node(nodo1)
                            if exists is True:
                                if grafoSwitch.has_node(nodo2) is True:
                                    info_switch = grafoSwitch.nodes[nodo2]
                                else:
                                    info_switch = grafoSwitch.nodes[nodo1]
                                dss_name = info_switch['DSSName']
                                # Verifica que no se haya escrito la línea anteriormente
                                if dss_name not in lista_switches:
                                    nc = info_switch['NC'] # normalmente cerrado
                                    grupo = info_switch['GRUPO']
                                    
                                    line_switch = "new line." + dss_name  + " bus1="
                                    line_switch += busto + "_swt" + configFase 
                                    line_switch += " bus2=" + busto + configFase 
                                    line_switch += " phases=" + cantFases + " r1=1e-3 r0=1e-3 "
                                    line_switch += "x1=0.000 x0=0.000 c1=0.000 c0=0.000"
                                    line_switch += " Length=0.001 switch=yes"
                                    line_switch += " !Grupo=" + str(grupo) + "\n"
                                    
                                    if nc is False: # si es normalmente abierto
                                        line_switch += "open line." + dss_name + " term=1\n"
                                    output_switches.write(line_switch)
                                    lista_switches.append(dss_name)
                                    
                                    # Se escriben datos en la capa de switches
                                    id_swt = info_switch["ID"]
                                    idxbus1_swt = info_switch["INDEXBUS1"]
                                    idxbus2_swt = info_switch["INDEXBUS2"]
                                    layerSwitches.changeAttributeValue(id_swt, idxbus1_swt, busfrom)
                                    layerSwitches.changeAttributeValue(id_swt, idxbus2_swt, busto)
                                    
                                    # Se ajusta el busto para la línea
                                    busto = busto + "_swt"
                                    idxbus_int = info_switch["INDEXBUS_INT"]
                                    layerSwitches.changeAttributeValue(id_swt, idxbus_int, busto)
                        
                        # Fusibles
                        if capa_fuses is True:
                            exists = grafoFuses.has_node(nodo2) or grafoFuses.has_node(nodo1)
                            if exists is True:
                                if grafoFuses.has_node(nodo2) is True:
                                    info_fuses = grafoFuses.nodes[nodo2]
                                else:
                                    info_fuses = grafoFuses.nodes[nodo1]
                                dss_name = info_fuses['DSSName']
                                # Verifica que no se haya escrito la línea anteriormente
                                if dss_name not in lista_fuses:
                                    nc = str(info_fuses['NC']) # normalmente cerrado
                                    grupo = str(info_fuses['GRUPO'])
                                    rated_i = str(info_fuses['RATED_CURRENT'])
                                    curve = str(info_fuses['FUSE_CURVE'])
                                    line_fuse = "new fuse." + dss_name
                                    line_fuse += ' MonitoredObj=line.' + lineName
                                    line_fuse += ' MonitoredTerm=1 FuseCurve=' + curve
                                    line_fuse += ' RatedCurrent='
                                    if nc is True:
                                        line_fuse += rated_i + '\n'
                                    else:
                                        line_fuse += '0\n'
                                    
                                    output_fuses.write(line_fuse)
                                    lista_fuses.append(dss_name)
                                    
                                    # Se escriben datos en la capa de switches
                                    id_swt = info_fuses["ID"]
                                    idxbus1_swt = info_fuses["INDEXBUS1"]
                                    idxbus2_swt = info_fuses["INDEXBUS2"]
                                    layerFuses.changeAttributeValue(id_swt, idxbus1_swt, busfrom)
                                    layerFuses.changeAttributeValue(id_swt, idxbus2_swt, busto)
                        
                        # Reclosers
                        if capa_reclosers is True:
                            exists = grafoReclosers.has_node(nodo2) or grafoReclosers.has_node(nodo1)
                            if exists is True:
                                if grafoReclosers.has_node(nodo2) is True:
                                    info_reclosers = grafoReclosers.nodes[nodo2]
                                else:
                                    info_reclosers = grafoReclosers.nodes[nodo1]
                                dss_name = info_reclosers['DSSName']
                                # Verifica que no se haya escrito la línea anteriormente
                                if dss_name not in lista_reclosers:
                                    nc = info_reclosers['NC'] # normalmente cerrado
                                    grupo = str(info_reclosers['GRUPO'])
                                    grd_d = info_reclosers['GRD_D']
                                    grd_f = info_reclosers['GRD_F']
                                    grd_i = info_reclosers['GRD_I']
                                    grd_trp = info_reclosers['GRD_TRIP']
                                    ph_d = info_reclosers['PH_D']
                                    ph_f = info_reclosers['PH_F']
                                    ph_i = info_reclosers['PH_I']
                                    ph_trp = info_reclosers['PH_TRIP']

                                    line_recloser = "new recloser." + dss_name
                                    line_recloser += ' MonitoredObj=line.' + lineName
                                    line_recloser += ' MonitoredTerm=1 '

                                    if nc is True:
                                        if grd_d != None:
                                            line_recloser += "GroundDelayed=" + str(grd_d)
                                        if ph_d != None:
                                            line_recloser += "PhaseDelayed=" + str(ph_d)

                                        line_recloser += ' groundTrip=' + str(grd_trp)
                                        line_recloser += ' phaseTrip=' + str(ph_trp)
                                        
                                        if grd_f != None:
                                            line_recloser += " GroundFast=" + str(grd_f)
                                        if ph_f != None:
                                            line_recloser += " PhaseFast=" + str(ph_f)
                                        
                                        line_recloser += ' groundInst=' + str(grd_i)
                                        line_recloser += ' phaseInst=' + str(ph_i) + "\n"
                                    else:
                                        line_recloser += 'groundTrip=0 phaseTrip=0 '
                                        line_recloser += 'groundInst=0 phaseInst=0\n'
                                    
                                    
                                    output_reclosers.write(line_recloser)
                                    lista_reclosers.append(dss_name)
                                    
                                    # Se escriben datos en la capa de switches
                                    id_rec = info_reclosers["ID"]
                                    idxbus1_rec = info_reclosers["INDEXBUS1"]
                                    idxbus2_rec = info_reclosers["INDEXBUS2"]
                                    layerReclosers.changeAttributeValue(id_rec, idxbus1_rec, busfrom)
                                    layerReclosers.changeAttributeValue(id_rec, idxbus2_rec, busto)
                                    
                        # Reguladores
                        if capa_reg is True:
                            if busfrom in buslist_reg:
                                busfrom = busfrom + "_reg"                    
                        
                        if lineName == "MV" + str(cantFases)+ 'P' + circuitName + '0':
                            busfrom = 'AFTERMETER' #Para coincidir con la línea 00
                        
                        line = 'new line.' + lineName + ' bus1=' + busfrom + configFase
                        line += ' bus2=' +  busto + configFase + ' geometry=' + equipment
                        line += ' length=' + sh_len + ' units=m' + ' !1ph_basekV='
                        line += str(opervoltLN) + "\n" # Se usan las variables anteriores en formar el string de salida
                        output_lmtdss.write(line)
                        
                        element = 'line.' + lineName
                        line = 'new monitor.Mon' + lineName + ' Element=' + element
                        line += ' Terminal=1 Mode=0 !1ph_basekV=' + str(opervoltLN) + "\n"
                        output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                        #Cambios en capa
                        layer_ = DATOS["LAYER"]
                        id_ = DATOS["ID"]
                        layer_.changeAttributeValue(id_, DATOS["INDEXDSS"], lineName)
                        layer_.changeAttributeValue(id_, DATOS["idx_bus1"], busfrom)
                        layer_.changeAttributeValue(id_, DATOS["idx_bus2"], busto)
                    output_lmtdss.close() # Cierra el archivo de salida
                    output_monitorsdss.close() # Cierra el archivo de salida
                        
                except Exception:
                    self.print_error()
                finally:
                    #Guarda cambios en capas
                    if capa_switches is True:
                        output_switches.close()
                        layerSwitches.commitChanges()
                    if capa_fuses is True:
                        output_fuses.close()
                        layerFuses.commitChanges()
                    if capa_reclosers is True:
                        output_reclosers.close()
                        layerReclosers.commitChanges()
                    
                    if len(selectedLayerMT1)!= 0:
                        layerMT1.commitChanges() # Guarda
                    if len(selectedLayerMT2)!= 0:
                        layerMT2.commitChanges() # Guarda
                    if len(selectedLayerMT3)!= 0:
                        layerMT3.commitChanges() # Guarda
                        
                        
                #######################################

            ##Buses de media tensión con coordenadas
            if len(busMT_List)> 0:
                try:
                    filename = circuitName + '_BusListMV.csv'
                    filename2 = circuitName + '_MV_BaseKV_LN.dss'
                    # output_filesQGIS2DSS.write('\nBusCoords '+filename)
                    # output_filesQGIS2DSS.write('\nredirect '+filename2)
                    output_buslistmt = open(foldername + '/' + filename, 'w')
                    output_baseKV_DSS = open(foldername + '/' + filename2, 'w')
                    layerMTBusName = "Bus_MT_Layer"
                    attrs_MtBusLayer = ['BUS', 'BASEKV_LN', 'PHASES', 'NODES']
                    layer = auxiliary_functions.newShape(layerMTBusName, attrs_MtBusLayer, "POINT", projPath)
                    layer.startEditing()
                    pr = layer.dataProvider()
                    # layer.beginEditCommand("Feature triangulation")
                    index = layer.dataProvider().fieldNameIndex('BUS')
    
                    for bus in list(busMT_List.keys()):
                        # print busMT_List[bus]
                        busName = busMT_List[bus]['bus']
                        MTvoltage = busMT_List[bus]['VOLTAGELN']
                        Nphases = busMT_List[bus]['NPHAS']
                        line = '%s,%s,%s\n' % (busMT_List[bus]['bus'], busMT_List[bus]['X'], busMT_List[bus]['Y']) # Se usan las variables anteriores en formar el string de salida
                        output_buslistmt.write(line) # Escribe el string de salida en el archivo
                        lineBase = "%s%s%s%s\n" % ("SetKVBase bus=", busName, " kvln=", MTvoltage)
                        output_baseKV_DSS.write(lineBase)
                        feat = QgsFeature(pr.fields())
                        feat.setGeometry(QgsGeometry.fromPointXY(QgsPointXY(float(busMT_List[bus]['X']), float(busMT_List[bus]['Y']))))
                        feat["BUS"] = busName
                        feat["BASEKV_LN"] = MTvoltage
                        feat["PHASES"] = Nphases
                        feat["NODES"] =  str(busMT_List[bus]['PHASES']).replace(".","")
                        pr.addFeatures([feat])
                    output_buslistmt.close() # Cierra el archivo de salida
                    output_baseKV_DSS.close()
                    layer.updateExtents()
                    layer.commitChanges()
                
                except Exception:
                    self.print_error()
    
            self.progress.progressBar.setValue(55)
            ############################################################
            ################# Escritura subestacion ####################
            ############################################################
            if SEactive is True:
                self.WriteSub(mode, layerSE, grafoSubt)
            
            filenameEM = circuitName + '_EnergyMeters.dss'
            output_energymdss = open(foldername + '/' + filenameEM, 'w')
            line_em = 'new Energymeter.Met_Sub' + ' Element=line.MV3P'+circuitName+'00'
            line_em += ' Terminal=1 \n'
            output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                
            ##################################################
            ##   ESCRITURA DE TRANSFORMADORES
            ##################################################
            column_names_trafo = ['DSS_NAME', 'KVA', 'X', 'Y', 'GROUP_LV', 'BUS_MT']
            df_trafo = pd.DataFrame(columns = column_names_trafo)
            
            # Se genera la salida de transformadores y monitores de transformadores para OpenDSS
            #Lista de buses mt_mt 
            list_tx_buses_mv_mv = []
            if (LTRactive is True):
                try:
                    filename = circuitName + '_Transformers.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_trdss = open(foldername + '/' + filename, 'w')
                    filenameMon = circuitName + '_Monitors.dss'
                    filenameEM = circuitName + '_EnergyMeters.dss'
                    #output_filesQGIS2DSS.write('\nredirect ' + filenameMon)
                    output_monitorsdss = open(foldername + '/' + filenameMon, 'a')
                    output_energymdss = open(foldername + '/' + filenameEM, 'a')
                    
                    self.nombres_capas += "\n!Layers Transformers: "
    
                    if len(selectedLayerTR1)!= 0:
                        self.nombres_capas += str(selectedLayerTR1) + ","
                        layerT1.startEditing() # Activa modo edición
                    if len(selectedLayerTR2)!= 0:
                        self.nombres_capas += str(selectedLayerTR2) + ","
                        layerT2.startEditing() # Activa modo edición
                    if len(selectedLayerTR3)!= 0:
                        self.nombres_capas += str(selectedLayerTR3)
                        layerT3.startEditing() # Activa modo edición
    
                    if (Graph_T1F.number_of_nodes()!= 0):  # revisa si hay trafos monofásicos
                        output_trdss.write('//Transformadores Monofasicos\n') # Escribe el string de salida en el archivo
                        # for n in range(ndatosT1F):
                        n = 0
                        for TRAFO1F in Graph_T1F.nodes(data=True):
                            dataList = TRAFO1F[1]
                            nodo = TRAFO1F[0]
                            cantFases = '1'
                            kVA = str(dataList['KVA'])
                            normhkva = " normhkva=" + kVA
                            kV_MedLL = dataList['VOLTMTLL']
                            kV_MedLN = dataList['VOLTMTLN']
                            kV_LowLL = dataList['LOADVOLT']
                            kV_LowLN = dataList['LOADVOLTLN']
                            X_ = dataList['X1']
                            Y_ = dataList['Y1']
                            
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            phaseMV = dataList['PHASE'] + ".0"
                            tap = dataList['TAPS']
                            tap = float(tap)
                            tap = "{:.2f}".format(tap)
                            mv_mv = dataList['MV_MV']
                            idx_lv_group = dataList['INDEX_LV_GROUP']
    
                            grupo_trafo_lv = str(dataList['GRUPO_LV'])
                            grupo_trafo_mv = str(dataList['GRUPO_MV'])
                            
                            datos_tafo = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA, mv_mv)
                            reactance = datos_tafo.get('X')
                            resistance = datos_tafo.get('R')
                            noloadloss = datos_tafo.get('Pnoload')
                            imag = datos_tafo.get('Im')
                            trafName = circuitName + cantFases + 'P_' + str(n + 1)
                            
                            # Solucion cargas pegadas a trafo
                            exists = False
                            if CARactive is True:
                                exists = grafoCAR.has_node(nodo)
                            if exists is True:
                                inf_car = grafoCAR.nodes[nodo]
                                group_car = inf_car['GRUPO']
                                grupo_trafo_lv  = str(group_car)
                            
                            if mv_mv is True:
                                busLV = busMV + "_tx"
                                list_tx_buses_mv_mv.append(busMV)
                                line = 'new transformer.' + trafName + ' phases=1 windings=2 ' + reactance + ' '
                                line += resistance + ' ' + noloadloss + ' ' + imag + ' Buses=[' + busMV + phaseMV 
                                line += ' ' + busLV + phaseMV + '] kvs=[' + kV_MedLN + ' '
                                line += kV_LowLN + '] kVAs=[' + kVA + ' ' + kVA + '] conns=[wye wye] '
                                line += 'Taps=[' + tap + ', 1]' + normhkva +  " !GroupMV=" + grupo_trafo_mv
                            
                            else:
                                line = 'new transformer.' + trafName + ' phases=1 windings=3 ' + reactance + ' '
                                line += resistance + ' ' + noloadloss + ' ' + imag + ' Buses=[' + busMV + phaseMV
                                line += ' ' + busLV + '.1.0 ' + busLV + '.0.2] kvs=[' + kV_MedLN + ' '
                                line += kV_LowLN + ' ' + kV_LowLN + '] kVAs=[' + kVA + ' ' + kVA + ' ' + kVA
                                line += '] conns=[wye wye wye] Taps=[' + tap + ', 1, 1]'
                                line += normhkva +  " !GroupMV=" + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                
                            output_trdss.write(line + "\n")
                            
                            element = 'transformer.' + trafName
                            line = 'new monitor.Mon' + trafName + ' Element=' + element
                            line += ' Terminal=1 Mode=1\n'
                            output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                            
                            if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                line_em = 'new Energymeter.Met_' + trafName + ' Element=' + element
                                line_em += ' Terminal=1 \n'
                                output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXBUS1"], busMV)
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], idx_lv_group, grupo_trafo_lv)
                            
                            
                            #Dataframe de trafos
                            datos = [[trafName, kVA, X_, Y_, grupo_trafo_lv, busMV]]
                            df_temp = pd.DataFrame(data=datos, columns = column_names_trafo)
                            df_trafo = df_trafo.append(df_temp, ignore_index=True)
    
                            n += 1
                    if (Graph_T3F_single.number_of_nodes()!= 0):  # revisa si hay trafos trifásicos Single
                        output_trdss.write(
                            '\n//Transformadores Trifasicos Simples\n') # Escribe el string de salida en el archivo
                        n = 0
                        for TRAFO3F in Graph_T3F_single.nodes(data=True):
                            cantFases = '3'
                            dataList = TRAFO3F[1]
                            nodo = TRAFO3F[0]
                            kVA = str(dataList['KVA'])
                            normhkva = " normhkva=" + kVA
                            kV_LowLL = dataList["LOADVOLT"]
                            kV_LowLN = dataList["LOADVOLTLN"]
                            kV_MedLL = dataList['VOLTMTLL']
                            kV_MedLN = dataList['VOLTMTLN']
                            X_ = dataList['X1']
                            Y_ = dataList['Y1']
                            
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            tap = dataList['TAPS']
                            tap = float(tap)
                            tap = "{:.2f}".format(tap)
                            mv_mv = dataList['MV_MV']
                            
                            if (dataList['CONME'] == 'Y'):
                                confMV = 'wye'
                            else:
                                confMV = 'delta'
                            if (dataList['CONBA'] == 'Y'):
                                confLV = 'wye'
                            else:
                                confLV = 'delta'
                            phaseMV = dataList['PHASE']
                            grupo_trafo_lv = str(dataList['GRUPO_LV'])
                            grupo_trafo_mv = str(dataList['GRUPO_MV'])
                            datos_tafo = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA, mv_mv)
                            impedance = datos_tafo.get('Z')
                            noloadloss = datos_tafo.get('Pnoload')
                            imag = datos_tafo.get('Im')
                            trafName = circuitName + cantFases + 'P_' + str(n + 1)
                            
                            if mv_mv is True:
                                busLV = busMV + "_tx"
                                list_tx_buses_mv_mv.append(busMV)
                                group_lv = ""
                            else:
                                group_lv = ' !GroupLV=' + grupo_trafo_lv
                                
                            line = 'new transformer.' + trafName + ' phases=3 windings=2 ' + noloadloss + ' '
                            line += imag + ' buses=[' + busMV + '.1.2.3 ' + busLV + '.1.2.3] '
                            line += 'conns=[' + confMV + ' ' + confLV + '] kvs=[' + kV_MedLL + ' ' +  kV_LowLL + ']'
                            line += ' kvas=[' + kVA + ' ' + kVA + '] ' + impedance + ' Taps=[' + tap + ', 1]' + normhkva
                            line += ' !GroupMV=' + grupo_trafo_mv + group_lv
                            
                            output_trdss.write(line + "\n")
                            
                            element = 'transformer.' + trafName
                            line = 'new monitor.Mon' + trafName + ' Element=' + element + ' Terminal=1 Mode=1\n'
                            output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                            
                            if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                line_em = 'new Energymeter.Met_' + trafName + ' Element=' + element
                                line_em += ' Terminal=1 \n'
                                output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXBUS1"], busMV)
                            
                            datos = [[trafName, kVA, X_, Y_, grupo_trafo_lv, busMV]]
                            df_temp = pd.DataFrame(data=datos, columns = column_names_trafo)
                            df_trafo = df_trafo.append(df_temp, ignore_index=True)
                            n += 1
                    if (Graph_T3F_multi.number_of_nodes()!= 0):  # revisa si hay trafos trifásicos Multi
                        output_trdss.write(
                            '\n//Transformadores Trifasicos de tres unidades Monofasicas\n') # Escribe el string de salida en el archivo
                        n = 0
                        for TRAFO3F_multi in Graph_T3F_multi.nodes(data=True):
                            dataList = TRAFO3F_multi[1]
                            nodo = TRAFO3F_multi[0]
                            kVA_A = str(float(dataList['KVA_FA']))
                            kVA_B = str(float(dataList['KVA_FB']))
                            kVA_C = str(float(dataList['KVA_FC']))
                            normhkva_A = " normhkva=" + kVA_A
                            normhkva_B = " normhkva=" + kVA_B
                            normhkva_C = " normhkva=" + kVA_C
                            kV_MedLL = dataList['VOLTMTLL']
                            kV_MedLN = dataList['VOLTMTLN']
                            kV_LowLN = str(
                                trafoOperations.renameVoltage(int(dataList['KVM']), int(dataList['KVL'])).get('LVCode')[
                                    'LN'])
                            kV_LowLL = str(
                                trafoOperations.renameVoltage(int(dataList['KVM']), int(dataList['KVL'])).get('LVCode')[
                                    'LL'])
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            phaseMV = dataList['PHASE']
                            tap = dataList['TAPS']
                            tap = float(tap)
                            tap = "{:.2f}".format(tap)
                            mv_mv = dataList['MV_MV']
                            X_ = dataList['X1']
                            Y_ = dataList['Y1']
    
                            grupo_trafo_lv = str(dataList['GRUPO_LV'])
                            grupo_trafo_mv = str(dataList['GRUPO_MV'])
    
                            datos_trafo = trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV)
                            impedanceA = datos_trafo.get('impA')['Za']
                            impedanceB = datos_trafo.get('impB')['Zb']
                            resistanceB = datos_trafo.get('impB')['Rb']
                            reactanceB = datos_trafo.get('impB')['Xb']
                            impedanceC = datos_trafo.get('impC')['Zc']
                            imagA = datos_trafo.get('impA')['ImA']
                            imagB = datos_trafo.get('impB')['ImB']
                            imagC = datos_trafo.get('impC')['ImC']
                            noloadlossA = datos_trafo.get('impA')['PnoloadA']
                            noloadlossB = datos_trafo.get('impB')['PnoloadB']
                            noloadlossC = datos_trafo.get('impC')['PnoloadC']
                            
                            trafName1 = circuitName + '3U3P_1_' + str(n + 1)
                            trafName2 = circuitName + '3U3P_2_' + str(n + 1)
                            trafName3 = circuitName + '3U3P_3_' + str(n + 1)
                                
                            if (dataList['CONBA'] == '4D'):  # si el transformador es delta 4 hilos en baja tensión
                                line_A = 'new transformer.' + trafName1 + ' phases=1 windings=2 ' +  imagA
                                line_A += ' buses=[' + busMV + '.1.0 ' + busLV + '.3.1' + ']' + ' ' + noloadlossA + ' '
                                line_A += impedanceA + ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + ']'
                                line_A += ' kvas=[' + kVA_A + ' ' + kVA_A + ']' + ' conns=[wye wye] Taps=[' + tap + ', 1]'
                                line_A += normhkva_A + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                line_B = 'new transformer.' + trafName2 + ' phases=1 windings=3 '
                                line_B += imagB + ' buses=[' + busMV + '.2.0 ' + busLV + '.1.0 ' + busLV + '.0.2] '
                                line_B += noloadlossB + ' ' + reactanceB + ' '
                                line_B += resistanceB + ' kvs=[' + kV_MedLN + ' ' + kV_LowLN + ' ' + kV_LowLN + '] kvas=['
                                line_B += kVA_B + ' ' + kVA_B + ' ' + kVA_B + ']'
                                line_B += ' conns=[wye wye wye] Taps=[' + tap + ', 1, 1]' + normhkva_B
                                line_B += ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                line_C = 'new transformer.' + trafName3 + ' phases=1 windings=2 ' + imagC
                                line_C += ' buses=[' + busMV + '.3.0 ' + busLV + '.2.3]' + ' ' + noloadlossC + ' ' + impedanceC
                                line_C += ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + '] kvas=[' + kVA_C + ' ' + kVA_C
                                line_C += '] conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_C
                                line_C += ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                output_trdss.write(line_A + "\n")
                                output_trdss.write(line_B + "\n")
                                output_trdss.write(line_C + "\n")
                                trafName = circuitName + '3U3P_' + str(n + 1)
                                
                                element = 'transformer.' + trafName1
                                line = 'new monitor.Mon' + trafName1
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName1 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                    
                                element = 'transformer.' + trafName2
                                line = 'new monitor.Mon' + trafName2
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName2 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                element = 'transformer.' + trafName3
                                line = 'new monitor.Mon' + trafName3
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName3 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXBUS1"], busMV)
                            elif (dataList['CONBA'] == 'Y'):
                                if mv_mv is True: # Caso trafo mv-mv
                                    busLV = busMV + "_tx"
                                    list_tx_buses_mv_mv.append(busMV)
                                    line_group = ""
                                else:
                                    line_group = ' !GroupLV=' + grupo_trafo_lv
                                    
                                line_A = 'new transformer.' + trafName1 + ' phases=1 windings=2 ' + imagA
                                line_A += ' buses=[' + busMV + '.1.0' + ' ' + busLV + '.3.1]' + ' ' + noloadlossA + ' '
                                line_A +=  impedanceA + ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + ']'
                                line_A += ' kvas=[' + kVA_A + ' ' + kVA_A + '] conns=[wye wye] Taps=[' + tap + ', 1]'
                                line_A +=  normhkva_A + ' !GroupMV=' + grupo_trafo_mv + line_group
    
                                line_B = 'new transformer.' + trafName2 + ' phases=1 windings=2 ' + imagB
                                line_B += ' buses=[' + busMV + '.2.0 ' + busLV + '.1.2]' + ' ' + noloadlossB + ' ' + impedanceB
                                line_B +=  ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + '] kvas=[' + kVA_B + ' ' + kVA_B + ']'
                                line_B += ' conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_B
                                line_B +=  ' !GroupMV=' + grupo_trafo_mv + line_group
    
                                line_C = 'new transformer.' + trafName3 + ' phases=1 windings=2 ' + imagC
                                line_C += ' buses=[' +  busMV + '.3.0 ' + busLV + '.2.3]' + ' ' + noloadlossC + ' ' + impedanceC
                                line_C += ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + ']'
                                line_C += ' kvas=[' + kVA_C + ' ' +  kVA_C + '] conns=[wye wye] Taps=[' + tap + ', 1]'
                                line_C +=  normhkva_C + ' !GroupMV=' + grupo_trafo_mv + line_group
                                    
                                    
                                output_trdss.write(line_A + "\n") # res
                                output_trdss.write(line_B + "\n")
                                output_trdss.write(line_C + "\n")
                                trafName = circuitName + '3U3P_' + str(n + 1)
                                
                                element = 'transformer.' + trafName1
                                line = 'new monitor.Mon' + trafName1 
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName1 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                element = 'transformer.' + trafName2
                                line = 'new monitor.Mon' + trafName2
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName2 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                element = 'transformer.' + trafName3
                                line = 'new monitor.Mon' + trafName3
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                if mv_mv is False: #Solo si es un transformador mv/lv tiene un energymeter
                                    line_em = 'new Energymeter.Met_' + trafName3 + ' Element=' + element
                                    line_em += ' Terminal=1 \n'
                                    output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXBUS1"], busMV)
                            
							
                            else:
                                msg = "Los transformadores trifásicos "
                                msg += "de tres unidades monofásicos"
                                msg += "sólo puede tener una conexión "
                                msg += "en el secundario tipo delta "
                                msg += " cuatro hilos o estrella. "
                                msg += "Usted indicó una conexión "
                                msg += str(dataList['CONBA'])
                                msg += ". Favor corrija y corra de "
                                msg += "nuevo."
                                self.msg_trafos += msg + "\n"
                            datos = [[trafName1, kVA_A, X_, Y_, grupo_trafo_lv, busMV]]
                            df_temp = pd.DataFrame(data=datos, columns = column_names_trafo)
                            df_trafo = df_trafo.append(df_temp, ignore_index=True)
                            n += 1
                    if (Graph_T2F.number_of_nodes()!= 0):  # revisa si hay trafos bifásicos
                        output_trdss.write(
                            '\n//Transformadores bifásicos (Conexiones especiales de dos transformadores)\n') # Escribe el string de salida en el archivo
                        n = 0
                        for TRAFO2F in Graph_T2F.nodes(data=True):
                            dataList = TRAFO2F[1]
                            nodo = TRAFO2F[0]
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            kV_MedLL = dataList['VOLTMTLL']
                            X_ = dataList['X1']
                            Y_ = dataList['Y1']
                            kV_MedLN = dataList['VOLTMTLN']
                            kV_LowLN = str(
                                trafoOperations.renameVoltage(int(dataList['KVM']), int(dataList['KVL'])).get('LVCode')[
                                    'LN'])
                            kV_LowLL = str(
                                trafoOperations.renameVoltage(int(dataList['KVM']), int(dataList['KVL'])).get('LVCode')[
                                    'LL'])
                            kVA_A = str(int(float(dataList['KVA_FA'])))
                            kVA_B = str(int(float(dataList['KVA_FB'])))
                            kVA_C = str(int(float(dataList['KVA_FC'])))
                            phase = dataList['PHASE']
                            tap = str(dataList['TAPS'])
                            # tap="1"
                            grupo_trafo_lv = str(dataList['GRUPO_LV'])
                            grupo_trafo_mv = str(dataList['GRUPO_MV'])
                            
                            if (dataList['CONBA'] == '4D'):  # si el transformador es delta 4 hilos en baja tensión
                                if phase == '.1.2':  # Las variables conexME y conexBA se utilizan para escribir a qué nodos de la barra se conecta la estrella abierta
                                    if float(dataList['KVA_FA'])>= float(dataList['KVA_FB']):
                                        buff_kVA_A = kVA_A
                                        buff_kVA_B = kVA_B
                                        kVA_A = buff_kVA_B
                                        kVA_B = buff_kVA_A
    
                                    conexME_trafoA = '.1.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.3.1'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.2.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.1.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.2'  # Conexión en barra de baja tensión del transformador B
                                    datos_trafo = trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase)
                                    # Se obtienen las impedancias según las fases del banco en las que hay transformadores
                                    impedanceA = datos_trafo.get('impA')['Za']
                                    reactanceB = datos_trafo.get('impB')['Rb']
                                    resistanceB = datos_trafo.get('impB')['Xb']
                                    imagA = datos_trafo.get('impA')['ImA']
                                    imagB = datos_trafo.get('impB')['ImB']
                                    noloadlossA = datos_trafo.get('impA')['PnoloadA']
                                    noloadlossB = datos_trafo.get('impB')['PnoloadB']
                                    kVA_trafoA = kVA_A  # Potencia del transformador A
                                    kVA_trafoB = kVA_B  # Potencia del transformador B
                                elif phase == '.1.3':
                                    if float(dataList['KVA_FA'])>= float(dataList['KVA_FC']):
                                        buff_kVA_A = kVA_A
                                        buff_kVA_C = kVA_C
                                        kVA_A = buff_kVA_C
                                        kVA_C = buff_kVA_A
    
                                    conexME_trafoA = '.1.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.2.1'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.3.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.1.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.3'  # Conexión en barra de baja tensión del transformador B
                                    datos_trafo = trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase)
                                    impedanceA = datos_trafo.get('impA')['Za']
                                    reactanceB = datos_trafo.get('impB')['Rb']
                                    resistanceB = datos_trafo.get('impB')['Xb']
                                    imagA = datos_trafo.get('impA')['ImA']
                                    imagB = datos_trafo.get('impB')['ImB']
                                    noloadlossA = datos_trafo.get('impA')['PnoloadA']
                                    noloadlossB = datos_trafo.get('impB')['PnoloadB']
                                    kVA_trafoA = kVA_A
                                    kVA_trafoB = kVA_C
                                elif phase == '.2.3':
                                    if float(dataList['KVA_FB'])>= float(dataList['KVA_FC']):
                                        buff_kVA_B = kVA_B
                                        buff_kVA_C = kVA_C
                                        kVA_B = buff_kVA_C
                                        kVA_C = buff_kVA_B
                                    conexME_trafoA = '.2.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.1.2'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.3.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.2.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.3'  # Conexión en barra de baja tensión del transformador B
                                    datos_trafo = trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase)
                                    impedanceA = datos_trafo.get('impA')['Za']
                                    reactanceB = datos_trafo.get('impB')['Rb']
                                    resistanceB = datos_trafo.get('impB')['Xb']
                                    imagA = datos_trafo.get('impA')['ImA']
                                    imagB = datos_trafo.get('impB')['ImB']
                                    noloadlossA = datos_trafo.get('impA')['PnoloadA']
                                    noloadlossB = datos_trafo.get('impB')['PnoloadB']
                                    kVA_trafoA = kVA_B
                                    kVA_trafoB = kVA_C
                                
                                else:
                                    msg = "Los transformadores "
                                    msg += "con conexión delta 4 hilos "
                                    msg += 'en el secundario sólo puede'
                                    msg += "tener fases .1.2, .1.3 o "
                                    msg += ".2.3, y usted indicó "
                                    msg += str(phase) + "\n"
                                    self.msg_trafos += msg
                                    
                                normhkva_A = " normhkva=" + kVA_trafoA
                                normhkva_B = " normhkva=" + kVA_trafoB
                                
                                trafName1 = circuitName + '2U3P_1_' + str(n + 1)
                                trafName2 = circuitName + '2U3P_2_' + str(n + 1)
                                
                                line_A = 'new transformer.' + trafName1 + ' phases=1 windings=2 ' + imagA + ' '
                                line_A += impedanceA + ' ' + noloadlossA + ' buses=[' + busMV + conexME_trafoA + ' '
                                line_A +=  busLV + conexBA_trafoA + '] '
                                line_A += ' kvs=[' + kV_MedLN + ' ' + kV_LowLL + '] kvas=[' + kVA_trafoA + ' ' + kVA_trafoA + ']'
                                line_A += ' conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_A + ' !GroupMV=' + grupo_trafo_mv
                                line_B = 'new transformer.' + trafName2 + ' phases=1 windings=3 ' + imagB
                                line_B += ' ' + reactanceB + ' ' + resistanceB + ' ' + noloadlossB + ' buses=[' + busMV + conexME_trafoB
                                line_B += ' ' + busLV + conexBA1_trafoB + ' ' + busLV + conexBA2_trafoB 
                                line_B += '] kvs=[' + kV_MedLN + ' ' + kV_LowLN + ' ' + kV_LowLN + '] kvas=[' + kVA_trafoB + ' '
                                line_B += kVA_trafoB + ' ' + kVA_trafoB + '] conns=[wye wye wye] Taps=[' + tap + ', 1, 1]'
                                line_B +=  normhkva_B + ' !GroupMV=' + grupo_trafo_mv 
                                line_A += ' !GroupLV=' + grupo_trafo_lv
                                line_B += ' !GroupLV=' + grupo_trafo_lv
                                    
                                    
                                output_trdss.write(line_A + "\n")
                                output_trdss.write(line_B + "\n")
                                trafName = circuitName + '2U3P_' + str(n + 1)
                                
                                element = 'transformer.' + trafName1
                                line = 'new monitor.Mon' + trafName1
                                line += ' Element=' + element +' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                line_em = 'new Energymeter.Met_' + trafName1 + ' Element=' + element
                                line_em += ' Terminal=1 \n'
                                output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                element = 'transformer.' + trafName2
                                line = 'new monitor.Mon' + trafName2
                                line += ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line) # Escribe el string de salida en el archivo
                                line_em = 'new Energymeter.Met_' + trafName2 + ' Element=' + element
                                line_em += ' Terminal=1 \n'
                                output_energymdss.write(line_em) # Escribe el string de salida en el archivo
                                
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXBUS1"], busMV)
                                
                                datos = [[trafName1, kVA_trafoA, X_, Y_, grupo_trafo_lv, busMV]]
                                df_temp = pd.DataFrame(data=datos, columns = column_names_trafo)
                                df_trafo = df_trafo.append(df_temp, ignore_index=True)
                                
                            else:
                                msg = "Los transformadores bifásicos "
                                msg += "sólo puede tener una conexión "
                                msg += "en el secundario tipo "
                                msg += "delta cuatro hilos. "
                                msg += "Usted indicó una conexión "
                                msg += str(dataList['CONBA']) + "\n"
                                self.msg_trafos += msg
                            n += 1
                    
                    output_trdss.close()
                    output_monitorsdss.close()
                    output_energymdss.close()
                    namefile_ = foldername + "/TrafosList.csv"
                    df_trafo.to_csv(namefile_)
                    if len(selectedLayerTR1)!= 0:
                        layerT1.commitChanges() # Activa modo edición
                    if len(selectedLayerTR2)!= 0:
                        layerT2.commitChanges() # Activa modo edición
                    if len(selectedLayerTR3)!= 0:
                        layerT3.commitChanges() # Activa modo edición
                    
                    # Realiza cambios en archivo de lineas debido a trafos mt_mt
                    if LMTactive is True and list_tx_buses_mv_mv != []:
                        filename = circuitName + '_LinesMV.dss'
                        output_lmtdss_ = foldername + '/' + filename
                        time_in = time.time()
                        self.replace_lines_mt_trafo_mt(output_lmtdss_, list_tx_buses_mv_mv)
                        time_fin = time.time()

                except Exception:
                    self.print_error()
    
                    
            #####################################
            ##   FIN ESCRITURA DE TRANSFORMADORES
            #####################################
            
            # Escritura GD large scale
            if capa_gd_ls is True:
                line_out = '\nredirect ' + filename_gdls
                self.output_filesQGIS2DSS.write(line_out)
            
            # Escritura Reguladores
            if capa_reg is True:
                self.Write_Reg(grafo_reg, layer_reg)
                
            # Escritura Capacitores
            if capa_cap is True:
                self.WriteCapacitors(layerCap, grafoCap)
            
            self.progress.progressBar.setValue(65)
            # Se genera la salida de transformadores y monitores de transformadores para OpenDSS
            
            ##################################
            ### ESCRITURA DE LÍNEAS BAJA TENSIÓN ####
            ##################################
            if (LBTactive is True):
                try:
                    filename = circuitName + '_LinesLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_lbtdss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerBT1)!= 0:
                        layerBT1.startEditing() # Activa modo edición
                    if len(selectedLayerBT2)!= 0:
                        layerBT2.startEditing() # Activa modo edición
                    if len(selectedLayerBT3)!= 0:
                        layerBT3.startEditing() # Activa modo edición
                    n = 1
                    
                    column_names_lines = ['DSS_NAME', 'bus1', 'bus2', 'GROUP_LV']
                    df_lv_lines1 = pd.DataFrame(columns = column_names_lines)
                    
                    for line in grafoBT.edges(data=True):
                        dataLine = line[2]
                        TrafNode = dataLine["TRAFNODE"]
                        busfrom = line[2]['bus1']
                        busto = line[2]['bus2']
                        
                        # Si la linea no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                        if dataLine['TRAFNPHAS'] == "NULL":
                            cantFases = dataLine['NPHAS']
                            desc = " Disconnected"
                            busBT_List[line[0]]['VOLTAGELN'] = "0.12"
                            busBT_List[line[1]]['VOLTAGELN'] = "0.12"
                        else:
                            cantFases = dataLine['TRAFNPHAS']
                            desc = ""
                        # Aereo
                        if dataLine['AIR_UGND'] == 'air':
                            if cantFases == '1':
                                if dataLine['TIPO'] == 'TPX':
                                    equipment = 'Linecode=TRPX'
                                    equipment += str(dataLine['PHASIZ'])
                                    equipment += str(dataLine['PHAMAT'])
                                else:
                                    equipment = 'Geometry=1FLV'
                                    equipment += str(dataLine['PHASIZ'])
                                    equipment += str(dataLine['PHAMAT'])
                                    equipment += str(dataLine['NEUSIZ'])
                                    equipment += str(dataLine['NEUMAT'])
                                conns = ".1.2"
                            elif cantFases == '3' or cantFases == '2':
                                if dataLine['TIPO'] == 'QPX':
                                    equipment = 'Linecode=QDPX'
                                    equipment += str(dataLine['PHASIZ'])
                                    equipment += str(dataLine['PHAMAT'])
                                else:
                                    equipment = 'Geometry=3FLV'
                                    equipment += str(dataLine['PHASIZ'])
                                    equipment += str(dataLine['PHAMAT'])
                                    equipment += str(dataLine['NEUSIZ'])
                                    equipment += str(dataLine['NEUMAT'])
                                conns = ".1.2.3"
                            else:
                                equipment = 'NONE'
                        # Subterraneo
                        else:
                            if cantFases == '1':
                                equipment = 'LineCode=1FLV'
                                equipment += str(dataLine['PHASIZ'])
                                equipment += str(dataLine['PHAMAT'])
                                equipment += str(dataLine['PHASIZ'])
                                equipment += str(dataLine['PHAMAT'])
                                equipment += '_' + str(dataLine['INSUL'])
                                conns = ".1.2"
                            elif cantFases == '3' or cantFases == '2':
                                equipment = 'LineCode=3FLV'
                                equipment += str(dataLine['PHASIZ'])
                                equipment += str(dataLine['PHAMAT'])
                                equipment += str(dataLine['PHASIZ'])
                                equipment += str(dataLine['PHAMAT'])
                                equipment += '_' + str(dataLine['INSUL'])
                                conns = ".1.2.3"
                            else:
                                equipment = 'NONE'
                                conns = "NONE"
                        if float(dataLine['SHLEN'])== 0:
                            dataLine['SHLEN'] = 0.0001
                        
                        #Se agrega la información de los nodos a los buses
                        """
                        A veces invierte el orden de los buses en BusBT_List
                        (a veces busfrom está en line[0] y a veces está en line[1]
                        En este caso no tiene importancia donde esté ubicado
                        """
                        #busfrom
                        if busBT_List[line[0]]['bus'] == busfrom:
                            if 'PHASES' in busBT_List[line[0]]:
                                # esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                if busBT_List[line[0]]['PHASES'] != ".1.2.3":
                                    busBT_List[line[0]]['PHASES'] = conns
                            else:
                                busBT_List[line[0]]['PHASES'] = conns
                        
                        elif busBT_List[line[1]]['bus'] == busfrom:
                            if 'PHASES' in busBT_List[line[1]]:
                                # esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                if busBT_List[line[1]]['PHASES'] != ".1.2.3":
                                    busBT_List[line[1]]['PHASES'] = conns
                            else:
                                busBT_List[line[1]]['PHASES'] = conns
                            
                        #busto
                        if busBT_List[line[1]]['bus'] == busto:
                            if 'PHASES' in busBT_List[line[1]]:
                                # esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                if busBT_List[line[1]]['PHASES'] != ".1.2.3":
                                    busBT_List[line[1]]['PHASES'] = conns
                            else:
                                busBT_List[line[1]]['PHASES'] = conns
                        
                        elif busBT_List[line[0]]['bus'] == busto:
                            if 'PHASES' in busBT_List[line[0]]:
                                # esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                if busBT_List[line[0]]['PHASES'] != ".1.2.3":
                                    busBT_List[line[0]]['PHASES'] = conns
                            else:
                                busBT_List[line[0]]['PHASES'] = conns
                       
                        sh_len = "{0:.4f}".format(dataLine['SHLEN']) # Recibe la longitud de la linea
                        opervoltLN = dataLine["TRAFVOLTLN"]
                        grupo_aco = dataLine['GRUPO']
                        lineName = "LV" + cantFases + 'F' + circuitName + str(n)
                        line = 'new line.' + lineName + ' bus1=' + busfrom
                        line += conns + ' bus2=' + busto + conns + " "
                        line += equipment + ' length=' + str(sh_len)
                        line += ' units=m !1ph_basekV=' + str(opervoltLN)
                        line += ' Group=' + str(grupo_aco) + desc + "\n"
                        output_lbtdss.write(line) # Escribe el string de salida en el archivo
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"],
                                                               dataLine["INDEXDSS"],
                                                               lineName)
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"],
                                                               dataLine["idx_bus1"],
                                                               busfrom)
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"],
                                                               dataLine["idx_bus2"],
                                                               busto)
                        
                        
                        datos = [[lineName, busfrom, busto, grupo_aco ]]
                        df_temp = pd.DataFrame(data=datos, columns = column_names_lines)
                        df_lv_lines1 = df_lv_lines1.append(df_temp, ignore_index=True)
                        
                        n += 1
                    output_lbtdss.close() # Cierra el archivo de salida
                    if len(selectedLayerBT1)!= 0:
                        layerBT1.commitChanges() # Guarda
                    if len(selectedLayerBT2)!= 0:
                        layerBT2.commitChanges() # Guarda
                    if len(selectedLayerBT3)!= 0:
                        layerBT3.commitChanges() # Guarda
    
                except Exception:
                    self.print_error()

            ##############################
            ### FIN ESCRITURA LÍNEAS BAJA TENSIÓN
            ##############################
            self.progress.progressBar.setValue(70)
            ##############################
            ###  ESCRITURA ACOMETIDAS
            ##############################
            if (ACOactive is True):
                try:
                    
                
                    filename = circuitName + '_ServicesLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_acodss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerACO1)!= 0:
                        layerACO1.startEditing() # Activa modo edición
                    if len(selectedLayerACO2)!= 0:
                        layerACO2.startEditing() # Activa modo edición
                    if len(selectedLayerACO3)!= 0:
                        layerACO3.startEditing() # Activa modo edición
    
                    n = 1
                    
                    column_names_lines = ['DSS_NAME', 'bus1', 'bus2', 'GROUP_LV']
                    df_lv_lines2 = pd.DataFrame(columns = column_names_lines)
                    
                    for line in grafoACO.edges(data=True):
                        dataLine = line[2]
                        TrafNode = dataLine["TRAFNODE"]
                        busfrom = line[2]['bus1']
                        busto = line[2]['bus2']

                        # Si la linea no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                        if dataLine[
                            'TRAFNPHAS'] == "NULL":
                            cantFases = dataLine['NPHAS']
                            desc = " Disconnected"
                            busBT_List[line[0]]['VOLTAGELN'] = "0.12"
                            busBT_List[line[1]]['VOLTAGELN'] = "0.12"
                        else:
                            cantFases = dataLine['TRAFNPHAS']
                            desc = ""
                        if (cantFases == '1'):
                            equipment = 'TRPX' + str(dataLine['PHASIZ'])+ str(dataLine['PHAMAT'])
                            conns = ".1.2"
                        elif (cantFases == '3' or cantFases == '2'):
                            equipment = 'QDPX' + str(dataLine['PHASIZ'])+ str(dataLine['PHAMAT'])
                            conns = ".1.2.3"
                        else:
                            equipment = 'NONE'
                            conns = "NONE"
                        if float(dataLine['SHLEN'])== 0:
                            dataLine['SHLEN'] = 0.0001
                            
                        #Se agrega la información de los nodos a los buses
                        """
                        A veces invierte el orden de los buses en BusBT_List (a veces busfrom está en line[0] y a veces está en line[1]
                        En este caso no tiene importancia donde esté ubicado
                        """
                        #busfrom
                        if busBT_List[line[0]]['bus'] == busfrom:
                            if 'PHASES' in busBT_List[line[0]]:
                                if busBT_List[line[0]]['PHASES'] != ".1.2.3": #esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                    busBT_List[line[0]]['PHASES'] = conns
                            else:
                                busBT_List[line[0]]['PHASES'] = conns
                        
                        elif busBT_List[line[1]]['bus'] == busfrom:
                            if 'PHASES' in busBT_List[line[1]]:
                                if busBT_List[line[1]]['PHASES'] != ".1.2.3": #esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                    busBT_List[line[1]]['PHASES'] = conns
                            else:
                                busBT_List[line[1]]['PHASES'] = conns
                        
                            
                        #busto
                        if busBT_List[line[1]]['bus'] == busto:
                            if 'PHASES' in busBT_List[line[1]]:
                                if busBT_List[line[1]]['PHASES'] != ".1.2.3": #esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                    busBT_List[line[1]]['PHASES'] = conns
                            else:
                                busBT_List[line[1]]['PHASES'] = conns
                        
                        elif busBT_List[line[0]]['bus'] == busto:
                            if 'PHASES' in busBT_List[line[0]]:
                                if busBT_List[line[0]]['PHASES'] != ".1.2.3": #esto es porque si ya está en .1.2.3 no debería cambiar a otro valor
                                    busBT_List[line[0]]['PHASES'] = conns
                            else:
                                busBT_List[line[0]]['PHASES'] = conns
                            
                            
                        sh_len = "{0:.4f}".format(dataLine['SHLEN']) # Recibe la longitud de la linea
                        opervoltLN = dataLine['TRAFVOLTLN']  # ,' !1ph_basekV=',opervoltLN
                        grupo_aco = dataLine['GRUPO']
                        lineName = "SRV" + cantFases + 'F' + circuitName + str(n)
                        text = 'new line.' + lineName + ' bus1=' + busfrom
                        text += conns + ' bus2=' + busto + conns
                        text += ' linecode=' + equipment + ' length='
                        text += str(sh_len) + ' units=m' + ' !1ph_basekV='
                        text += str(opervoltLN) + ' Group=' + str(grupo_aco) 
                        text += desc + "\n"
                        output_acodss.write(text) # Escribe el string de salida en el archivo
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"], dataLine["INDEXDSS"], lineName)
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"], dataLine["idx_bus1"], busfrom)
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"], dataLine["idx_bus2"], busto)
                        
                        datos = [[lineName, busfrom, busto, grupo_aco ]]
                        df_temp = pd.DataFrame(data=datos, columns = column_names_lines)
                        df_lv_lines2 = df_lv_lines2.append(df_temp, ignore_index=True)
                        
                        n += 1
                    output_acodss.close() # Cierra el archivo de salida
                    
                    if len(selectedLayerACO1)!= 0:
                        layerACO1.commitChanges() # Guarda
                    if len(selectedLayerACO2)!= 0:
                        layerACO2.commitChanges() # Guarda
                    if len(selectedLayerACO3)!= 0:
                        layerACO3.commitChanges() # Guarda
                
                except Exception:
                    self.print_error()
                    
            ##############################
            ###   FIN ESCRITURA ACOMETIDAS
            ##############################
            
            #Código para crear csv de lineas de baja tensión
            if (ACOactive is True) and (LBTactive is True):
                linesLVlist = [df_lv_lines1, df_lv_lines2]
                df_linesLV = pd.concat(linesLVlist)
                namefile_ = foldername + "/LinesLVList.csv"
                df_linesLV.to_csv(namefile_)
            elif (ACOactive is False) and (LBTactive is True):
                namefile_ = foldername + "/LinesLVList.csv"
                df_lv_lines1.to_csv(namefile_)
            elif (ACOactive is True) and (LBTactive is False):
                namefile_ = foldername + "/LinesLVList.csv"
                df_lv_lines2.to_csv(namefile_)
            
            self.progress.progressBar.setValue(75)
            
            
            #nuevo
            #################
            ### Escritura Cargas MT
            #################
            write_loadshape_MT = False
            if (CARmvactive is True)and not errorLoadShape:
                try:
                    if folder_profile != "":
                        filename = circuitName + '_Loadshapes.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                    filename = circuitName + '_LoadsMV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    #output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_cadss_mv = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerCA_MT1)!= 0:
                        layerCAMV1.startEditing() # Guarda
                    if len(selectedLayerCA_MT2)!= 0:
                        layerCAMV2.startEditing() # Guarda
                    if len(selectedLayerCA_MT3)!= 0:
                        layerCAMV3.startEditing() # Guarda
                    n = 0
                    self.progress.progressBar.setValue(80)
                    
                    for carga in grafoCAR_mv.nodes(data=True):
                                                                        
                        dataList = carga[1]
                        nodo = carga[0]
                        bus = str(dataList['BUS'])
                        kW = str(dataList['kW'])
                        conns = str(dataList['CONNS'])
                        conf = str(dataList['CONF'])
                        model = str(dataList['MODEL'])
                        kWhmonth = str(dataList['kWh'])
                        loadclass = str(dataList['class'])
                        daily = str(dataList['CURVASIG'])
                        Grupo = str(dataList['GRUPO'])
                        kV = str(dataList['NOMVOLT'])
                        cantFases = str(dataList['N_FASES'])
                        pf = str(dataList['PowerFactor'])
                        id_carga = dataList['ID_CARGA']
                        id_ = dataList["ID"]
                        idx_dss = dataList["INDEXDSS"]
                        idx_bus1 = dataList['idx_bus1']
                        layer = dataList["LAYER"]

                        loadName = "MV_" + cantFases + 'F' + circuitName + str(n + 1)                        
                        # AMI
                        if id_carga:
                            daily= 'daily=' + str(id_carga)
                            loadName = "a_" + loadName
                        else:
                            loadName = "n_" + loadName

                        line = "new load." + loadName + " bus1=" + bus + conns
                        line += " kV=" + kV + " model=" + model + " conn=" + conf
                        line += " kW=" + kW + " pf=" +pf + " status=variable "
                        line += "phases=" +  cantFases + ' ' + daily
                        line += " !kWh="  + kWhmonth + " class="
                        line += loadclass + " !Group=" + Grupo + "\n"
                        output_cadss_mv.write(line)
                        # layer.changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], loadName)
                        layer.changeAttributeValue(id_, idx_dss, loadName)
                        layer.changeAttributeValue(id_, idx_bus1, bus)
                        
                        n += 1
                        if cantFases == "3" and conns != ".1.2.3":
                            self.mensaje_log_gral += "Revise la carga " + loadName + " debido a que hay"
                            self.mensaje_log_gral += " inconsistencias entre las fases y el número de nodos a los que está"
                            self.mensaje_log_gral += " conectada. Número de fases: " + cantFases + ", nodos: " + conns + "\n"
                        elif cantFases == "1" and conns == ".1.2.3":
                            self.mensaje_log_gral += "Revise la carga " + loadName + " debido a que hay"
                            self.mensaje_log_gral += " inconsistencias entre las fases y el número de nodos a los que está"
                            self.mensaje_log_gral += " conectada. Número de fases: " + cantFases + ", nodos: " + conns + "\n"
                    
                    output_cadss_mv.close()
                    if len(selectedLayerCA_MT1)!= 0:
                        layerCAMV1.commitChanges() # Guarda
                    if len(selectedLayerCA_MT2)!= 0:
                        layerCAMV2.commitChanges() # Guarda
                    if len(selectedLayerCA_MT3)!= 0:
                        layerCAMV3.commitChanges() # Guarda
                    
                    write_loadshape_MT = True
    
                except Exception:
                    self.print_error()

            #################
            ### Escritura Cargas BT
            #################
            if (CARactive is True) and not errorLoadShape:
                try:
                    #write_loadshape_MT se utiliza para que no se escriba de nuevo el archivo de Loadshapes si ya fue escrito en las cargas de MT
                    if folder_profile != "" and write_loadshape_MT is False: 
                        filename = circuitName + '_Loadshapes.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                        
                    filename = circuitName + '_LoadsLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_cadss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerCA1)!= 0:
                        layerCA1.startEditing() # Guarda
                    if len(selectedLayerCA2)!= 0:
                        layerCA2.startEditing() # Guarda
                    if len(selectedLayerCA3)!= 0:
                        layerCA3.startEditing() # Guarda
                    n = 0
                    self.progress.progressBar.setValue(85)
                    
                    column_names_load = ['DSS_NAME', 'KWH', 'BUS_BT', 'X', 'Y',
                                         'EUCLID_DISTANCE', 'SECTOR', 'GROUP_LV']
                    df_load = pd.DataFrame(columns = column_names_load)
                    for carga in grafoCAR.nodes(data=True):                                                
                        dataList = carga[1]
                        nodo = carga[0]
                        bus = str(dataList['BUS'])
                        kW = str(dataList['kW'])
                        conns = str(dataList['CONNS'])
                        conf = str(dataList['CONF'])
                        model = str(dataList['MODEL'])
                        kWhmonth = str(dataList['kWh'])
                        loadclass = str(dataList['class'])
                        daily = str(dataList['CURVASIG'])
                        Grupo = str(dataList['GRUPO'])
                        kV = str(dataList['NOMVOLT'])
                        X_ = dataList['X1']
                        Y_ = dataList['Y1']
                        id_carga = dataList['ID_CARGA']
                        id_ = dataList["ID"]
                        idx_dss = dataList["INDEXDSS"]
                        idx_bus1 = dataList['idx_bus1']
                        layer = dataList["LAYER"]
                        
                        # Si la carga no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                        if dataList['TRAFNPHAS'] == "NULL":  
                            cantFases = "1"
                            desc = " disconnected"
                        else:
                            cantFases = dataList['TRAFNPHAS']
                            desc = ""
                        
                        if loadclass == "R":
                            kvar = str(0.1)
                        else:
                            kvar = str(0.3)
    
                        
                        loadName = cantFases + 'F' + circuitName + str(n + 1)
                        # AMI
                        if id_carga:
                            daily= 'daily=' + str(id_carga)
                            loadName = "a_" + loadName
                        else:
                            loadName = "n_" + loadName
                        line = "new load." + loadName + " bus1=" + bus + conns
                        line += " kV=" + kV + " model=" + model + " conn=" + conf
                        line += " kW=" + kW + " kvar=" + kvar
                        line += " status=variable phases=" +  cantFases 
                        line += ' ' + daily + " !kWh="  + kWhmonth + " class="
                        line +=  loadclass + " !Group=" + Grupo +  desc + "\n" 
                        output_cadss.write(line)
                        layer.changeAttributeValue(id_, idx_dss, loadName)
                        layer.changeAttributeValue(id_, idx_bus1, bus)
                        
                        #Calculo de distancia entre trafo y carga
                        datos_dist = df_trafo[df_trafo['GROUP_LV'] == Grupo]
                        if len(datos_dist)== 1:
                            datos_dist = datos_dist[['X', 'Y']].values.tolist()
                            datos_dist = datos_dist[0]
                            X_trafo = float(datos_dist[0])
                            Y_trafo = float(datos_dist[1])
                            x_diff = X_trafo - float(X_)
                            y_diff = Y_trafo - float(Y_)
                            sum_ = math.pow(x_diff, 2)
                            sum_ += math.pow(y_diff, 2) 
                            dist = math.sqrt(sum_)
                        #Si hay varios del mismo grupo se pone una distancia negativa
                        else:
                            dist = -1
                        datos = [[loadName, kWhmonth, bus, X_, Y_, dist, loadclass, Grupo]]
                        df_temp = pd.DataFrame(data=datos, columns = column_names_load)
                        df_load = df_load.append(df_temp, ignore_index=True)

                        n += 1
                        
                        if cantFases == "3" and conns != ".1.2.3":
                            self.mensaje_log_gral += "Revise la carga " + loadName + " debido a que hay"
                            self.mensaje_log_gral += " inconsistencias entre las fases y el número de nodos a los que está"
                            self.mensaje_log_gral += " conectada. Número de fases: " + cantFases + ", nodos: " + conns + "\n"
                        elif cantFases == "1" and conns == ".1.2.3":
                            self.mensaje_log_gral += "Revise la carga " + loadName + " debido a que hay"
                            self.mensaje_log_gral += " inconsistencias entre las fases y el número de nodos a los que está"
                            self.mensaje_log_gral += " conectada. Número de fases: " + cantFases + ", nodos: " + conns + "\n"
                        
                    output_cadss.close()
                    namefile_ = foldername + "/LoadsLVList.csv"
                    df_load.to_csv(namefile_)
                    if len(selectedLayerCA1)!= 0:
                        layerCA1.commitChanges() # Guarda
                    if len(selectedLayerCA2)!= 0:
                        layerCA2.commitChanges() # Guarda
                    if len(selectedLayerCA3)!= 0:
                        layerCA3.commitChanges() # Guarda
    
                except Exception:
                    self.print_error()
                    
            
            #################
            ### Escritura Cargas Iluminación
            #################
            if (CAR_i_active is True) and not errorLoadShape:
                try:
                        
                    filename = circuitName + '_StreetLightning.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_cadss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerCI1)!= 0:
                        layerCI1.startEditing() # Guarda
                    if len(selectedLayerCI2)!= 0:
                        layerCI2.startEditing() # Guarda
                    if len(selectedLayerCI3)!= 0:
                        layerCI3.startEditing() # Guarda
                    n = 0
                    self.progress.progressBar.setValue(85)
                    
                    column_names_load = ['DSS_NAME', 'KW', 'BUS_BT', 'X', 'Y',
                                         'EUCLID_DISTANCE', 'GROUP_LV']
                    df_load = pd.DataFrame(columns = column_names_load)
                    for carga in grafoCAR_i.nodes(data=True):                                                
                        dataList = carga[1]
                        nodo = carga[0]
                        bus = str(dataList['BUS'])
                        kW = str(dataList['kW'])
                        conns = str(dataList['CONNS'])
                        conf = str(dataList['CONF'])
                        model = "1"
                        daily = "StreetLightning"
                        Grupo = str(dataList['GRUPO'])
                        kV = str(dataList['NOMVOLT'])
                        X_ = dataList['X1']
                        Y_ = dataList['Y1']
                        id_ = dataList["ID"]
                        idx_dss = dataList["INDEXDSS"]
                        idx_bus1 = dataList['idx_bus1']
                        layer = dataList["LAYER"]
                        kvar = "0"
                       
                        loadName = 'sl_1F' + circuitName + str(n + 1)

                        line = "new load." + loadName + " bus1=" + bus + conns
                        line += " kV=" + kV + " model=" + model + " conn=" + conf
                        line += " kW=" + kW + " kvar=" + kvar
                        line += " status=variable phases=" +  cantFases 
                        line += " daily=" + daily + "!Group="+ Grupo + " \n" 
                        output_cadss.write(line)
                        
                        layer.changeAttributeValue(id_, idx_dss, loadName)
                        layer.changeAttributeValue(id_, idx_bus1, bus)
                        
                        #Calculo de distancia entre trafo y carga
                        datos_dist = df_trafo[df_trafo['GROUP_LV'] == Grupo]
                        if len(datos_dist)== 1:
                            datos_dist = datos_dist[['X', 'Y']].values.tolist()
                            datos_dist = datos_dist[0]
                            X_trafo = float(datos_dist[0])
                            Y_trafo = float(datos_dist[1])
                            x_diff = X_trafo - float(X_)
                            y_diff = Y_trafo - float(Y_)
                            sum_ = math.pow(x_diff, 2)
                            sum_ += math.pow(y_diff, 2) 
                            dist = math.sqrt(sum_)
                        #Si hay varios del mismo grupo se pone una distancia negativa
                        else:
                            dist = -1
                        datos = [[loadName, kW, bus, X_, Y_, dist, Grupo]]
                        df_temp = pd.DataFrame(data=datos, columns = column_names_load)
                        df_load = df_load.append(df_temp, ignore_index=True)

                        n += 1
                        
                        if cantFases == "1" and conns == ".1.2.3":
                            self.mensaje_log_gral += "Revise la carga " + loadName + " debido a que hay"
                            self.mensaje_log_gral += " inconsistencias entre las fases y el número de nodos a los que está"
                            self.mensaje_log_gral += " conectada. Número de fases: " + cantFases + ", nodos: " + conns + "\n"
                        
                    output_cadss.close()
                    namefile_ = foldername + "/StreetLightningList.csv"
                    df_load.to_csv(namefile_)
                    if len(selectedLayerCI1)!= 0:
                        layerCI1.commitChanges() # Guarda
                    if len(selectedLayerCI2)!= 0:
                        layerCI2.commitChanges() # Guarda
                    if len(selectedLayerCI3)!= 0:
                        layerCI3.commitChanges() # Guarda
    
                except Exception:
                    self.print_error()
                    
            ########################
            ### Fin escritura Cargas
            ########################

            ########################
            ### Escritura de GD
            ########################
            
            
            if len(selectedLayerGD)!= 0:
                try:
                    
                    if LoadShapesFile is False: #bandera que especifica si el archivo de loadshapes ya se abrió
                        filename = circuitName + '_Loadshapes.dss'
                        output_shpdss = open(foldername + '/' + filename, 'a+')
                        LoadShapesFile = True
                    layerGD = QgsProject.instance().mapLayersByName(selectedLayerGD)[
                        0]
                    self.progress.progressBar.setValue(90)
                    filename = circuitName + '_DG.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_GDdss = open(foldername + '/' + filename, 'w')
                    layerGD.startEditing() # Guarda
                    
                    if errorLoadShape or cargas == 0:
                        output_shpdss.write('!' + folder_profile + '\n')
                    n = 1
                    i = 1
                    shapewritten = {}
                    for GD in grafoGD.nodes(data=True):
                        dataList = GD[1]
                        nodo = GD[0]
                        bus = str(dataList['BUS'])
                        kVA = str(dataList['KVA'])
                        conf = str(dataList['CONF'])
                        NPHAS = str(dataList["NPHAS"])
                        CURVE1 = dataList["CURVE1"]
                        CURVE2 = dataList["CURVE2"]
                        kV = str(dataList["VOLTAGELL"])
                        if NPHAS == "1":
                            conn = ".1.2"
                        else:
                            conn = ".1.2.3"
                        if dataList["TECH"] == "PV":
                            if "MyPvsT" not in shapewritten:
                                shapewritten["MyPvsT"] = 0
                                out = 'New XYCurve.MyPvsT npts=4 xarray=[.001 25 75 100] '
                                out += ' yarray=[1.2 1.0 0.8 0.6]\n'
                                out += 'New XYCurve.MyEff npts=4 xarray=[.1 .2 .4 1.0] '
                                out += 'yarray=[.86 .9 .93 .97]\n'
                                output_shpdss.write(out)
    
                            if CURVE1 not in shapewritten:
                                shapewritten[CURVE1] = 0
                                name1 = CURVE1.replace('.txt', '')
                                name1 = name1.replace('.dss', '')
                                out = 'New Loadshape.' + name1 + ' npts=96 '
                                out += 'minterval=15 csvfile=' + folder_profile
                                out += '\\DG\\' + CURVE1 + '\n'
                                output_shpdss.write(out)
                            if CURVE2 not in shapewritten:
                                shapewritten[CURVE2] = 0
                                name2 = CURVE2.replace('.txt', '')
                                name2 = name2.replace('.dss', '')
                                out = 'New Tshape.' + name2 + ' npts=96 '
                                out += 'minterval=15 csvfile=' + folder_profile
                                out += '\\DG\\' + CURVE2 + '\n'
                                output_shpdss.write(out)
                            pvname = "PV" + NPHAS + "F" 
                            pvname += circuitName + str(n)
                            pvSentence = "New PVSystem." + pvname
                            pvSentence += " bus1=" + bus + conn
                            pvSentence += " kV=" + kV + " phases="
                            pvSentence += NPHAS + " kVA=" + kVA
                            pvSentence += " PF=1 conn=" + conf
                            pvSentence += " irrad=0.90 Pmpp=" + kVA
                            pvSentence += " temperature=25 effcurve="
                            pvSentence += "Myeff P-TCurve=MyPvsT Daily="
                            pvSentence += name1 + " TDaily=" + name2 
                            pvSentence +=" %cutin=0.01 %cutout=0.01 "
                            pvSentence += "enabled=yes \n"
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], pvname)
                            output_GDdss.write(pvSentence)
                            n += 1
                        else:
                            if (CURVE1, CURVE2)not in shapewritten:
                                shapewritten[(CURVE1, CURVE2)] = "curveDG" + str(i)
                                out = "New Loadshape.curveDG" + str(i) 
                                out += " npts=96 minterval=15 mult=(file="
                                out += folder_profile + '\\DG\\' + CURVE1
                                out += ")Qmult=(file=" + folder_profile
                                out += '\\DG\\' + CURVE2 + ")useactual=no \n"
                                output_shpdss.write(out)
                                i += 1
                            GDName = "DG" + NPHAS + "F" + circuitName + str(n)
                            DGsentence = "New Generator." + GDName
                            DGsentence += " Bus1=" + bus + conn + " phases="
                            DGsentence += NPHAS + " conn=" + conf
                            DGsentence +=  " kVA=" + kVA + " kV=" + kV
                            DGsentence += " kW=1 kVAR=1 Model=1 daily="
                            DGsentence += shapewritten[(CURVE1, CURVE2)] 
                            DGsentence += " status=variable\n"
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], GDName)
                            output_GDdss.write(DGsentence)
                            n += 1
                    output_GDdss.close()
                    layerGD.commitChanges()
                except Exception:
                    self.print_error()
                    
                    
            ########################
            ### Fin de escritura de GD
            ########################
            
            if LoadShapesFile is True: #bandera que especifica si el archivo de loadshapes ya se abrió
                output_shpdss.close()
                    
            
            ########################
            ### Escritura de plantel de buses
            ########################
            writeBuses = False
            if selectedLayerBuses != "" and selectedLayerBuses != None and CARactive is True:
                busBT_List = self.WriteFileBuses(toler, grafoBuses, busBT_List)
                if busBT_List == 0: #caso en que hubo errores escribiendo los buses
                    return 0
                writeBuses = True
                
            
            #Se escribe el archivo de monitores
            if LTRactive is True or writeBuses is True:
                output_filesQGIS2DSS.write('\nredirect ' + filenameMon)
                output_filesQGIS2DSS.write('\nredirect ' + filenameEM)
            
            ########################
            ### Escritura de VE
            ########################
            if name_layer_evs != "" and name_layer_evs != None and CARactive is True:
                self.WriteFileEV(toler, grafoCAR, grafoEV)
            ########################
            ### Fin de escritura de VE
            ########################

            self.progress.progressBar.setValue(95)
            if len(busBT_List)> 0:  ###Buses de baja tensión con coordenadas
                try:
                    filename = circuitName + '_BusListLV.csv'
                    filename2 = circuitName + '_LV_KVBaseLN.dss'
                    # output_filesQGIS2DSS.write('\nBusCoords '+filename)
                    # output_filesQGIS2DSS.write('\nredirect '+filename2)
                    output_buslistbt = open(foldername + '/' + filename, 'w')
                    output_baseKV_DSS = open(foldername + '/' + filename2, 'w')
                    layerBTBusName = "Bus_BT_Layer"
                    attrs_BtBusLayer = ["BUS", "BASEKV_LN", "NODES", 'GROUP']
                    layer = auxiliary_functions.newShape(layerBTBusName, attrs_BtBusLayer, "POINT", projPath)
                    layer.startEditing()
                    pr = layer.dataProvider()
                    index = layer.dataProvider().fieldNameIndex('BUS')
                    for bus in list(busBT_List.keys()):
                        busName = busBT_List[bus]['bus']
                        group = busBT_List[bus]['GRUPO']
                        BTvoltage = busBT_List[bus]['VOLTAGELN']
                        line = '%s,%s,%s\n' % (busBT_List[bus]['bus'], busBT_List[bus]['X'], busBT_List[bus][
                            'Y']) # Se usan las variables anteriores en formar el string de salida
                        output_buslistbt.write(line) # Escribe el string de salida en el archivo
                        lineBase = "%s%s%s%s\n" % ("SetKVBase bus=", busName, " kvln=", BTvoltage)
                        output_baseKV_DSS.write(lineBase)
                        feat = QgsFeature(pr.fields())
                        feat.setGeometry(
                            QgsGeometry.fromPointXY(QgsPointXY(float(busBT_List[bus]['X']), float(busBT_List[bus]['Y']))))
                        feat["BUS"] = busName
                        feat["GROUP"] = group
                        feat["BASEKV_LN"] = BTvoltage
                        try:
                            phases = busBT_List[bus]['PHASES']
                            phases = str(phases).replace(".","")
                            feat["NODES"] = phases
                        except Exception:
                            try:
                                #Caso en que no se encuentre el valor dado por líneas se toma de LOADCONNS, asignado por los trafos
                                phases = busBT_List[bus]['LOADCONNS']
                                phases = str(phases).replace(".","")
                                feat["NODES"] = phases
                            except Exception:
                                pass
                                
                        pr.addFeatures([feat])
                    output_buslistbt.close() # Cierra el archivo de salida
                    output_baseKV_DSS.close() # Cierra el archivo de salida
                    layer.updateExtents()
                    layer.commitChanges()
                
                except Exception:
                    self.print_error()
            
            output_filesQGIS2DSS.write(self.nombres_capas) #se escriben los nombres de las capas de líneas MV, LV, y trafos
            output_filesQGIS2DSS.close() # cierra el archivo con la lista de archivos creados
            
            starRevLinesDisc = time.time()
            self.progress.progressBar.setValue(97)
            ############  Revisión de líneas desconectas
            connected_components_MT = (grafoMT.subgraph(c) for c in nx.connected_components(grafoMT))
            connected_components_MT = list(connected_components_MT)
            connected_components_BT = (grafoBTTotal.subgraph(c) for c in nx.connected_components(grafoBTTotal))
            connected_components_BT = list(connected_components_BT)
            connected_components_ACO = (grafoACO.subgraph(c) for c in nx.connected_components(grafoACO))
            connected_components_ACO = list(connected_components_ACO)
            
            msg_lines_mt = ""
            msg_lines_bt = ""
            for i, graph in enumerate(connected_components_MT):
                if graph.number_of_edges()== 1:
                    for edge in list(graph.edges(data=True)):
                        msg_lines_mt += "Hay un segmento de línea MT  desconectado en: ("
                        msg_lines_mt += str(edge[2]['X1'])+ ',' + str(edge[2]['Y1'])+ '); '
                        msg_lines_mt += '(' + str(edge[2]['X2'])+ ',' + str(edge[2]['Y2'])+ ')\n'
            endRevLinesDisc = time.time()
            
            for i, graph in enumerate(connected_components_BT):
                if graph.number_of_edges()== 1:
                    for edge in list(graph.edges(data=True)):
                        if (not Graph_T1F.has_node(edge[0]))and (not Graph_T2F.has_node(edge[0]))and (
                        not Graph_T3F_multi.has_node(edge[0]))and (not Graph_T3F_single.has_node(edge[0]))and (
                        not Graph_T1F.has_node(edge[1]))and (not Graph_T2F.has_node(edge[1]))and (
                        not Graph_T3F_multi.has_node(edge[1]))and (not Graph_T3F_single.has_node(edge[1])):
                            msg_lines_bt += "Hay un segmento de línea BT  desconectado en: ("
                            msg_lines_bt +=  str(edge[2]['X1']) + ',' + str(edge[2]['Y1'])+ ');'
                            msg_lines_bt += ' (' + str(edge[2]['X2'])+ ',' + str(edge[2]['Y2'])+ ')\n'
          
            # #### Escritura mensajes de errores al usuario  # ####
            # Escritura archivo de errores MT
            filename_mt = "error_MT" + circuitName + ".log"
            dir_archivo_MTlog = dir_logs + "/" + filename_mt
            if mensaje_mt != "":
                with open(dir_archivo_MTlog, 'w')as archivo_errlogMT:
                    archivo_errlogMT.write(mensaje_mt)
                aviso = "Favor revise el archivo " + filename_mt 
                aviso += ", ya que la capa de MT presenta los "
                aviso += " siguientes problemas que pueden "
                aviso += "ser corregidos: "
                if aviso_bus is True:
                    aviso += "\nLíneas de MT con bus1 = bus2"
                if aviso_fases is True:
                    aviso += "\nConexiones de fases distintas en MT"
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Advertencias MT'), aviso)
            else: #Si no hay mensajes de error se elimina el archivo para que no exista un archivo de errores falso
                try:
                    os.remove(dir_archivo_MTlog)
                except Exception:
                    pass
           
            # Escritura del archivo log de errores en BT
            filename_bt = "error_BT" + circuitName + ".log"
            dir_archivo_BTlog = dir_logs + "/" + filename_bt
            if aviso_busBT is True and mensaje_bt != "":
                aviso = "Favor revise el archivo " + filename_bt
                aviso += " ya que habían líneas de BT con "
                aviso += "bus1 = bus2 en BT"
                with open(dir_archivo_BTlog, 'w')as archivo_errBTlog:
                    archivo_errBTlog.write(mensaje_bt)
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Advertencias BT'), aviso)
            else: #Si no hay mensajes de error se elimina el archivo para que no exista un archivo de errores falso
                try:
                    os.remove(dir_archivo_BTlog)
                except Exception:
                    pass
            
            # Escritura de archivos de error acometidas
            filename_aco = "error_ACO" + circuitName + ".log"
            dir_archivo_ACOlog = dir_logs + "/" + filename_aco
            if aviso_busAco is True and mensaje_aco != "":
                with open(dir_archivo_ACOlog, 'w')as archivo_errACOlog:
                    archivo_errACOlog.write(mensaje_aco)
                aviso = "Favor revise el archivo " + filename_aco
                aviso += " ya que habían acometidas con bus1 = bus2"
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Advertencias acometidas'), aviso)
                
            else: # Si no hay mensajes de error se elimina el archivo para que no exista un archivo de errores falso
                try:
                    os.remove(dir_archivo_ACOlog)
                except Exception:
                    pass
            self.progress.progressBar.setValue(98)
           
           
            # Mensaje sobre transformadores con conexiones no aceptadas
           
            if self.msg_trafos != "":
                filename_traf_err = "trafo_errors" + circuitName + ".log"
                dir_traf_err = dir_logs + "/" + filename_traf_err
                with open(dir_traf_err, 'w') as err_trafos:
                    err_trafos.write(self.msg_trafos)
                aviso = "Favor revise el archivo " + dir_traf_err
                aviso += " ya que hay varios transformadores con "
                aviso += "tipos de conexiones no aceptadas"
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Transformadores"), aviso)
            
           
            # Mensaje para que el usuario revise archivo con líneas desconectadas en MT
            if msg_lines_mt != "":
                name_file_discon_mt = "disconect_mt" + circuitName + ".log"
                dir_archivo_disc = dir_logs + "/" + name_file_discon_mt
                with open(dir_archivo_disc, 'w') as archivo_errlog:
                    archivo_errlog.write(msg_lines_mt)
                aviso = "Favor revise el archivo " + dir_archivo_disc
                aviso += " ya que hay varias líneas en MT desconectadas."
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Líneas primarias"), aviso)
                
            # Mensaje para que el usuario revise archivo con líneas desconectadas en BT
            if msg_lines_bt != "":
                name_file_discon = "disconect_bt" + circuitName + ".log"
                dir_archivo_disc = dir_logs + "/" + name_file_discon
                with open(dir_archivo_disc, 'w') as archivo_errlog:
                    archivo_errlog.write(msg_lines_bt)
                aviso = "Favor revise el archivo " + dir_archivo_disc
                aviso += " ya que hay varias líneas en BT desconectadas."
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Líneas secundarias"), aviso)
            endRevLinesDisc = time.time()
            
            # Escribe archivo de errores general
            name_fileev = "errors" + circuitName + ".log"
            dir_archivo_log = dir_logs + "/" + name_fileev
            if self.mensaje_log_gral != "":
                with open(dir_archivo_log, 'w')as archivo_errlog:
                    archivo_errlog.write(self.mensaje_log_gral)
                aviso = "Favor revise el archivo " + name_fileev 
                aviso += ", ya que hay mensajes importantes que "
                aviso += "deben ser revisados."
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Advertencias'), aviso)
            else: # Si no hay mensajes de error se elimina el archivo para que no exista un archivo de errores falso
                try:
                    os.remove(dir_archivo_log)
                except Exception:
                    with open(dir_archivo_log, 'w') as archivo_errlog:
                        archivo_errlog.write("")
                
            # Copia las bibliotecas a la carpeta de salida
            try:
                bibPath = foldername + '/Bibliotecas'
                # self.mkdir_p(bibPath)
                pluginPath = str(os.path.dirname(os.path.abspath(inspect.stack()[0][1])))
                shutil.copytree(pluginPath + '/Bibliotecas', bibPath)
            except Exception:
                aviso = QCoreApplication.translate('dialog',
                                               u"No se exportó la biblioteca de conductores porque ya existe en la carpeta de salida.")
                
                print(aviso)
              
            if Error is True:
                self.iface.messageBar().pushCritical("QGIS2OpenDSS", QCoreApplication.translate('dialog',
                                                                                               u'Sucedió un error grave y  no fue posible completar la operación')) # Aviso de finalizado en barra de QGIS
            else:

                self.iface.messageBar().pushInfo("QGIS2OpenDSS", QCoreApplication.translate('dialog',
                                                                                               'Finalizado. Los archivos fueron creados correctamente en')+ ":\n" + foldername) # Aviso de finalizado en barra de QGIS
            self.progress.close()
            finalTime = time.time()
            return 0

            

            """
            # ################ TIEMPOS
            if LMTactive is True:
                pass
               
                aviso = QCoreApplication.translate('dialog',
                                                   u"Tiempo de conectividad de líneas de media tensión: ")+ str(
                    -startTimeMT + endTimeMT)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if LBTactive is True:
                aviso = QCoreApplication.translate('dialog',
                                                   u"Tiempo de conectividad de líneas de baja tensión: ")+ str(
                    -startTimeBT + endTimeBT)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if LTRactive is True:
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de transformadores: ")+ str(
                    -startTimeTraf + endTimeTraf)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if (ACOactive is True):
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de líneas de acometidas: ")+ str(
                    -startTimeAco + endTimeAco)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if SEactive is True:
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de la subestación: ")+ str(
                    -startTimeSub + endTimeSub)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if (CARactive is True):
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de las cargas: ")+ str(
                    -startTimeLoad + endTimeLoad)+ QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
        
            # print "El tiempo es: " + str(finalTime-startTime)
            # print "El tiempo de escritura fue "+ str(finalTime-startTimeWriting)
            
            """
            
            
            
            
