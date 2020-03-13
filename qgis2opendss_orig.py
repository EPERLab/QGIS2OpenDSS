    # -*- coding: utf-8 -*-
"""
/***************************************************************************
 QGIS2OpenDSS
                                 A QGIS plugin
 This plugin reads geographic information of electric distribution circuits and exports command lines for OpenDSS
                              -------------------
        begin                : 2015-11-22
        git sha              : $Format:%H$
        copyright            : (C) 2015 by EPERLAB / Universidad de Costa Rica
        email                : gvalverde@eie.ucr.ac.cr, abdenago.guzman@ucr.ac.cr, aarguello@eie.ucr.ac.cr
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

from . import auxiliary_functions
from .LlamarOpenDSS import LlamarOpenDSS
# from dateTime import *
from . import lineOperations
from . import phaseOperations  # Realiza diferentes tareas con las fases de los elementos electricos.
from . import trafoOperations  # Operaciones con transformadores
from . import resources
from . evs_code.EVsFunctions import CreacionPerfilesEV, AnalizarEncuestas #funciones necesarias para VEs
# Initialize Qt resources from file resources.py
# Import the code for the dialog
from .qgis2opendss_dialog import QGIS2OpenDSSDialog
from .qgis2opendss_progress import Ui_Progress

import sys
from decimal import Decimal

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
            locale = (u'en')  # en

        locale_path = os.path.join(self.plugin_dir, 'i18n', 'QGIS2OpenDSS_{}.qm'.format(locale))  # translation file

        if os.path.exists(locale_path):
            self.translator = QTranslator()
            self.translator.load(locale_path)

            if qVersion() > '4.3.3':
                QCoreApplication.installTranslator(self.translator)

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
        
        #Planteles de buses
        self.dlg.pushButton_bus.clicked.connect( self.select_csv_buses )
        self.dlg.pushButton_carg.clicked.connect( self.select_csv_cargadores )
        self.dlg.comboBox_plantelbuses.activated.connect( self.activate_csvs_buses )
        
        self.dlg.lineEdit_csvbus.setEnabled( False )
        self.dlg.lineEdit_dircsvcarg.setEnabled( False )
        self.dlg.pushButton_bus.setEnabled( False )
        self.dlg.pushButton_carg.setEnabled( False )
        
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
        self.dlg.comboBox_SE.clear()
        #self.dlg.comboBox_GD.clear()
        self.dlg.comboBox_CAMT1.clear()
        self.dlg.comboBox_CAMT2.clear()
        self.dlg.comboBox_CAMT3.clear()
        self.dlg.comboBox_GD_lv.clear()
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
        self.dlg.comboBox_SE.addItems(layer_list)
        #self.dlg.comboBox_GD.addItems(layer_list)
        self.dlg.comboBox_CAMT1.addItems(layer_list)
        self.dlg.comboBox_CAMT2.addItems(layer_list)
        self.dlg.comboBox_CAMT3.addItems(layer_list)
        self.dlg.comboBox_GD_lv.addItems(layer_list)
        self.dlg.comboBox_EV.addItems(layer_list)
        
        #Plantel de buses
        self.dlg.comboBox_plantelbuses.clear()
        self.dlg.comboBox_plantelbuses.addItems(layer_list)

    
    """
    Función que habilita las líneas de csvs de buses cuando se pone el nombre de una capa en el
    espacio del nombre de la capa de planteles de buses
    """
    def activate_csvs_buses( self ):
        #Se define el nombre de la capa de buses como una variable de clase
        self.name_layer_buses = self.dlg.comboBox_plantelbuses.currentText()
        
        #Caso en que no haya ninguna capa seleccionada
        if self.name_layer_buses == "":
            self.dlg.lineEdit_csvbus.setEnabled( False )
            self.dlg.lineEdit_dircsvcarg.setEnabled( False )
            self.dlg.pushButton_bus.setEnabled( False )
            self.dlg.pushButton_carg.setEnabled( False )
            
            self.dlg.lineEdit_csvbus.clear()
            self.dlg.lineEdit_dircsvcarg.clear()
        
        #Caso en que haya alguna capa seleccionada
        else:
            self.dlg.lineEdit_csvbus.setEnabled( True )
            self.dlg.lineEdit_dircsvcarg.setEnabled( True )
            self.dlg.pushButton_bus.setEnabled( True )
            self.dlg.pushButton_carg.setEnabled( True )
        
            
        
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
            path (e.g. ':/plugins/foo/bar.png') or a normal file system path.
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
        foldername = QFileDialog.getExistingDirectory(self.dlg, "Seleccione carpeta de destino","", )
        self.dlg.lineEdit_dirOutput.setText(foldername)
    
    #Función para cargar el csv de buses
    def select_csv_buses(self):
        csv_buses, _ = QFileDialog.getOpenFileName(self.dlg, "Seleccione el csv con la información de los parámetros de buses eléctricos", "", "*.csv")
        self.csv_buses = csv_buses #variable de la clase
        self.dlg.lineEdit_csvbus.setText( csv_buses )
    
    #Función para cargar el csv de los cargadores
    def select_csv_cargadores(self):
        csv_cargadores, _ = QFileDialog.getOpenFileName(self.dlg, "Seleccione el csv con la información de los cargadores de los buses eléctricos", "", "*.csv")
        self.csv_cargadores = csv_cargadores #variable de la clase
        self.dlg.lineEdit_dircsvcarg.setText( csv_cargadores )

    def select_load_profile(self):
        """Método para seleccionar el archivo de asignación de perfil de consumo"""
        # filename=QFileDialog.getOpenFileName(self.dlg,"Seleccione el archivo.txt para designar curva de carga conforme al consumo mensual promedio","",)
        foldername = QFileDialog.getExistingDirectory(self.dlg, "Seleccione la carpeta con las curvas de carga", "", )
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
        #line = ObjetoLinea.geometry().asPolyline()  # Lee la geometria de la linea
        
        geom = ObjetoLinea.geometry()
        line = self.MultiStringToMatrix(geom)
        
        n = len(line)  # Cantidad de vértices de la línea
        X1 = int(float(line[0][0] / tolerancia))
        Y1 = int(float(line[0][1] / tolerancia))
        X2 = int(float(line[n - 1][0] / tolerancia))
        Y2 = int(float(line[n - 1][1] / tolerancia))
        P1 = (X1, Y1)
        P2 = (X2, Y2)
        return P1, P2

    def CoordPointProcees(self, ObjetoLinea, tolerancia):
        point = ObjetoLinea.geometry().asPoint()  # Lee la geometria de la linea
        X1 = int(float(point[0] / tolerancia))
        Y1 = int(float(point[1] / tolerancia))
        P1 = (X1, Y1)
        return P1

    def ReaderDataLMT(self, layer, Grafo, datosLMT, toler, subterranea,
                      indexDSS):  # Lee los datos de capa de línea y las agrega al grafo
        lineasMT = layer.getFeatures()  # Recibe las caracteristicas de la capa de lineas de media tension.
        
        for lineaMT in lineasMT:
            geom = lineaMT.geometry()
            line = self.MultiStringToMatrix(geom)
            if line == 0:
                return 0, 0
            n = len(line)  # Cantidad de vértices de la línea
            fase = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseCodeODSS')
            faseOrig = lineaMT['PHASEDESIG']
            cantFases = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseNumber')
            opervoltLN = lineOperations.renameVoltage(lineaMT['NOMVOLT']).get('LVCode')['LN']
            opervoltLL = lineOperations.renameVoltage(lineaMT['NOMVOLT']).get('LVCode')['LL']
            nodo1, nodo2 = self.CoordLineProcess(lineaMT, toler)
            LineLength = lineaMT.geometry().length()
            if subterranea:  # Determina si la línea es aérea o subterránea
                air_ugnd = 'ugnd'
                datosLinea = {"PHASEDESIG": faseOrig, "INDEXDSS": indexDSS, 'ID': lineaMT.id(), "LAYER": layer,
                              "nodo1": nodo1, "nodo2": nodo2, "X1": line[0][0], "Y1": line[0][1], "X2": line[n - 1][0],
                              "Y2": line[n - 1][1], 'NEUMAT': lineaMT['NEUTMAT'], 'NEUSIZ': lineaMT['NEUTSIZ'],
                              'PHAMAT': lineaMT['PHASEMAT'], 'PHASIZ': lineaMT['PHASESIZ'],
                              'NOMVOLT': lineaMT['INSULVOLT'], 'PHASE': fase, 'SHLEN': LineLength, 'AIR_UGND': air_ugnd,
                              'INSUL': lineaMT['INSULMAT'], 'NPHAS': cantFases, 'VOLTOPRLL': opervoltLL,
                              'VOLTOPRLN': opervoltLN, "SHIELD": lineaMT["SHIELDING"]}
            else:
                air_ugnd = 'air'
                datosLinea = {"PHASEDESIG": faseOrig, "INDEXDSS": indexDSS, 'ID': lineaMT.id(), "LAYER": layer,
                              "nodo1": nodo1, "nodo2": nodo2, "X1": line[0][0], "Y1": line[0][1], "X2": line[n - 1][0],
                              "Y2": line[n - 1][1], 'NEUMAT': lineaMT['NEUTMAT'], 'NEUSIZ': lineaMT['NEUTSIZ'],
                              'PHAMAT': lineaMT['PHASEMAT'], 'PHASIZ': lineaMT['PHASESIZ'], 'CCONF': lineaMT['LINEGEO'],
                              'PHASE': fase, 'SHLEN': LineLength, 'AIR_UGND': air_ugnd, 'NPHAS': cantFases,
                              'VOLTOPRLL': opervoltLL, 'VOLTOPRLN': opervoltLN}

            if Grafo.get_edge_data(nodo1, nodo2) == None:  # se asegura que la línea no existe
                Grafo.add_edge(nodo1, nodo2)
                Grafo[nodo1][nodo2].update( datosLinea )  # Agrega la línea al grafo con todos los datos
            else:  # Si la línea existe es porque están en paralelo
                newLength = float(datosLinea["SHLEN"]) / 2
                datosLinea["SHLEN"] = newLength
                paralelNode = "Paralel" + str(nodo1)
                datosLinea["nodo2"] = paralelNode
                Grafo.add_edge(nodo1, paralelNode)
                Grafo[nodo1][paralelNode].update( datosLinea )  # Agrega la línea al grafo con todos los datos

                datosLinea["nodo2"] = nodo2
                datosLinea["nodo1"] = paralelNode
                Grafo.add_edge(paralelNode, nodo2)
                Grafo[paralelNode][nodo2].update( datosLinea )  # Agrega la línea al grafo con todos los datos

        return Grafo, datosLMT

    def ReaderDataTrafos(self, layer, toler, datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                         Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal):
        trafos1 = layer.getFeatures()  # Recibe las caracteristicas de la capa de transformadores.
        for trafo1 in trafos1:  # Separa los transformadores en tres listas: Monofasicos, Bifasicos y Trifasicos
            
            nodo = self.CoordPointProcees(trafo1, toler)
            point = trafo1.geometry().asPoint()  # Lee la geometria de la linea
            fase = phaseOperations.renamePhase(trafo1['PHASEDESIG']).get('phaseCodeODSS')  # define código de OpenDSS
            numfase = phaseOperations.renamePhase(trafo1['PHASEDESIG']).get(
                'phaseNumberTraf')  # define código de OpenDSS
            MVCode = trafo1['PRIMVOLT']
            LVCode = trafo1['SECVOLT']
            tap = str(format(float(trafo1['TAPSETTING']), '.4f'))
            voltages = trafoOperations.renameVoltage(int(MVCode), int(LVCode))
            #print( "Voltages = ", voltages )
            if voltages["LVCode"]["LL"] == 0:
                loadvolt = "UNKNOWN"
                loadvoltLN = "UNKNOWN"
                aviso = QCoreApplication.translate('dialog', u'No se encuentra el código de tensión ') + str(LVCode)
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
            else:
                loadvolt = str(voltages["LVCode"]["LL"])
                loadvoltLN = str(voltages["LVCode"]["LN"])
            if voltages["MVCode"]["LL"] == 0:
                voltoprLL = "UNKNOWN"
                voltoprLN = "UNKNOWN"
                aviso = QCoreApplication.translate('dialog', 'No se encuentra el código de tensión ') + str(MVCode)
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
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
            

            if fase == '.1.2.3':  # Divide los transformadores trifasicos en transformadores simples y de multiples unidades monofasicas
                # Revisa si es un banco de tres transformadores con placa diferente o una sola unidad
                if (trafo1['SECCONN'] == '4D'):
                    datosMulti = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "INDEXDSS": indexDSS,
                                  'ID': trafo1.id(), "TAPS": tap, "LAYER": layer, "nodo": nodo, 'X1': point[0],
                                  'Y1': point[1], 'PHASE': fase, 'KVA_FA': trafo1['KVAPHASEA'],
                                  'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                                  'KVM': trafo1['PRIMVOLT'], 'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'],
                                  'CONBA': trafo1['SECCONN'], 'LOADCONNS': '.1.2.3', 'LOADCONF': 'delta',
                                  'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                                  "VOLTMTLN": voltoprLN}
                    datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}

                    datosT3F_Multi.append(datosMulti)
                    if (nodo in Graph_T3F_multi.nodes()) and (
                            Graph_T3F_multi.node[nodo]['PHASE'] == datosMulti['PHASE']):
                        Graph_T3F_multi.node[nodo]['KVA_FA'] = float(datosMulti['KVA_FA']) + float(
                            Graph_T3F_multi.node[nodo]['KVA_FA'])
                        Graph_T3F_multi.node[nodo]['KVA_FB'] = float(datosMulti['KVA_FB']) + float(
                            Graph_T3F_multi.node[nodo]['KVA_FB'])
                        Graph_T3F_multi.node[nodo]['KVA_FC'] = float(datosMulti['KVA_FC']) + float(
                            Graph_T3F_multi.node[nodo]['KVA_FC'])
                        aviso = QCoreApplication.translate('dialog',
                                                           'Se aumento la capacidad de un transformador trifasico de 3 unidades debido a su cercanía con otro banco de transformadores en: (') + str(
                            Graph_T3F_multi.node[nodo]['X1']) + ', ' + str(Graph_T3F_multi.node[nodo]['Y1']) + ')'
                        QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                    else:
                        Graph_T3F_multi.add_node(nodo)
                        Graph_T3F_multi.nodes[nodo].update( datosMulti )  # Agrega el trafo al grafo con todos los datos
                        
                        grafoBTTotal.add_node(nodo)
                        grafoBTTotal.nodes[nodo].update( datosTotalGraph )
                        
                if (trafo1['SECCONN'] == 'Y') and (trafo1['PRIMCONN'] == 'Y' or trafo1['PRIMCONN'] == 'D'):

                    if float(trafo1['KVAPHASEA']) == float(trafo1['KVAPHASEB']) == float(trafo1['KVAPHASEC']):

                        datosSingleY = {'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'],
                                        'KVA_FC': trafo1['KVAPHASEC'], "NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode,
                                        "INDEXDSS": indexDSS, 'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, "TAPS": tap,
                                        'X1': point[0], 'Y1': point[1], 'PHASE': fase,
                                        'KVA': int(float(trafo1['RATEDKVA'])), 'KVM': trafo1['PRIMVOLT'],
                                        'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'], 'CONBA': trafo1['SECCONN'],
                                        'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye', 'LOADVOLT': loadvolt,
                                        'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                                        "VOLTMTLN": voltoprLN}
                        datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt,
                                           'LOADVOLTLN': loadvoltLN}
                        datosT3F_Single.append(datosSingleY)

                        if (nodo in Graph_T3F_single.nodes()) and (
                                Graph_T3F_single.node[nodo]['PHASE'] == datosSingleY['PHASE']):
                            Graph_T3F_single.node[nodo]['KVA'] = float(datosSingleY['KVA']) + float(
                                Graph_T3F_single.node[nodo]['KVA'])
                            aviso = QCoreApplication.translate('dialog',
                                                               'Se aumento la capacidad de un transformador trifasico debido a su cercanía con otro transformador en: (') + str(
                                Graph_T3F_single.node[nodo]['X1']) + ', ' + str(Graph_T3F_single.node[nodo]['Y1']) + ')'
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                        else:
                            Graph_T3F_single.add_node(nodo)
                            Graph_T3F_single.nodes[nodo].update( datosSingleY )  # Agrega el trafo al grafo con todos los datos
                            
                            grafoBTTotal.add_node(nodo)
                            grafoBTTotal.nodes[nodo].update( datosTotalGraph )

                    else:
                        # fix_print_with_import
                        
                        datosMulti = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "INDEXDSS": indexDSS,
                                  'ID': trafo1.id(), "TAPS": tap, "LAYER": layer, "nodo": nodo, 'X1': point[0],
                                  'Y1': point[1], 'PHASE': fase, 'KVA_FA': trafo1['KVAPHASEA'],
                                  'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                                  'KVM': trafo1['PRIMVOLT'], 'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'],
                                  'CONBA': trafo1['SECCONN'], 'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye',
                                  'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL,
                                  "VOLTMTLN": voltoprLN}

                        datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt,
                                           'LOADVOLTLN': loadvoltLN}
                        datosT3F_Multi.append(datosMulti)

                        if (nodo in Graph_T3F_multi.nodes()) and (
                                Graph_T3F_multi.node[nodo]['PHASE'] == datosMulti['PHASE']):
                            Graph_T3F_multi.node[nodo]['KVA_FA'] = float(datosMulti['KVA_FA']) + float(
                                Graph_T3F_multi.node[nodo]['KVA_FA'])
                            Graph_T3F_multi.node[nodo]['KVA_FB'] = float(datosMulti['KVA_FB']) + float(
                                Graph_T3F_multi.node[nodo]['KVA_FB'])
                            Graph_T3F_multi.node[nodo]['KVA_FC'] = float(datosMulti['KVA_FC']) + float(
                                Graph_T3F_multi.node[nodo]['KVA_FC'])
                            aviso = QCoreApplication.translate('dialog',
                                                               'Se aumento la capacidad de un transformador trifasico de 3 unidades debido a su cercanía con otro banco de transformadores en: (') + str(
                                Graph_T3F_multi.node[nodo]['X1']) + ', ' + str(Graph_T3F_multi.node[nodo]['Y1']) + ')'
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                        else:
                            Graph_T3F_multi.add_node(nodo)
                            Graph_T3F_multi.nodes[nodo].update( datosMulti )  # Agrega el trafo al grafo con todos los datos
                            
                            grafoBTTotal.add_node(nodo)
                            grafoBTTotal.nodes[nodo].update( datosTotalGraph )

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
                                    "VOLTMTLN": voltoprLN}
                    datosT3F_Single.append(datosSingleD)
                    datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                    if (nodo in Graph_T3F_single.nodes()) and (
                            Graph_T3F_single.node[nodo]['PHASE'] == datosSingleD['PHASE']):
                        Graph_T3F_single.node[nodo]['KVA'] = float(datosSingleD['KVA']) + float(
                            Graph_T3F_single.node[nodo]['KVA'])
                        aviso = QCoreApplication.translate('dialog',
                                                           'Se aumento la capacidad de un transformador trifasico debido a su cercanía con otro transformador en: (') + str(
                            Graph_T3F_single.node[nodo]['X1']) + ', ' + str(Graph_T3F_single.node[nodo]['Y1']) + ')'
                        QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                    else:
                        Graph_T3F_single.add_node(nodo)
                        Graph_T3F_single.nodes[nodo].update( datosSingleD )  # Agrega el trafo al grafo con todos los datos
                        
                        grafoBTTotal.add_node(nodo)
                        grafoBTTotal.nodes[nodo].update( datosTotalGraph )
            if fase == '.2.3' or fase == '.1.3' or fase == '.1.2':
                datos2F = {"NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "TAPS": tap, "INDEXDSS": indexDSS,
                           'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                           'PHASE': fase, 'KVA': int(float(trafo1['RATEDKVA'])), 'KVM': trafo1['PRIMVOLT'],
                           'KVL': trafo1['SECVOLT'], 'CONME': trafo1['PRIMCONN'], 'CONBA': trafo1['SECCONN'],
                           'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                           'LOADCONNS': '.1.2.3', 'LOADCONF': 'delta', 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN,
                           'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN}
                datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                datosT2F.append(datos2F)
                if (nodo in Graph_T2F.nodes()) and (Graph_T2F.node[nodo]['PHASE'] == datos2F['PHASE']):
                    Graph_T2F.node[nodo]['KVA'] = float(datos2F['KVA']) + float(Graph_T2F.node[nodo]['KVA'])
                    aviso = QCoreApplication.translate('dialog',
                                                       'Se aumento la capacidad de un transformador bifasico debido a su cercania con otro transformador en: (') + str(
                        Graph_T2F.node[nodo]['X1']) + ', ' + str(Graph_T2F.node[nodo]['Y1']) + ')'
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                else:
                    Graph_T2F.add_node(nodo)
                    Graph_T2F.nodes[nodo].update( datos2F )  # Agrega el trafo al grafo con todos los datos
                    
                    grafoBTTotal.add_node(nodo)
                    grafoBTTotal.nodes[nodo].update( datosTotalGraph )
            if fase == '.3' or fase == '.2' or fase == '.1':
                datos1F = {'KVA_FA': trafo1['KVAPHASEA'], 'KVA_FB': trafo1['KVAPHASEB'], 'KVA_FC': trafo1['KVAPHASEC'],
                           "NPHAS": numfase, "MVCODE": MVCode, "LVCODE": LVCode, "TAPS": tap, "INDEXDSS": indexDSS,
                           'ID': trafo1.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                           'PHASE': fase, 'KVA': trafo1['RATEDKVA'], 'KVM': trafo1['PRIMVOLT'],
                           'KVL': trafo1['SECVOLT'], 'LOADCONF': 'delta', 'LOADCONNS': '.1.2', 'LOADVOLT': loadvolt,
                           'LOADVOLTLN': loadvoltLN, 'GRUPO_MV': group_mv, 'GRUPO_LV': group_lv, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN}
                datosTotalGraph = {"NPHAS": numfase, "type": "TRAF", 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN}
                datosT1F.append(datos1F)
                if (nodo in Graph_T1F.nodes()) and (Graph_T1F.node[nodo]['PHASE'] == datos1F['PHASE']):
                    Graph_T1F.node[nodo]['KVA'] = float(datos1F['KVA']) + float(Graph_T1F.node[nodo]['KVA'])
                    aviso = QCoreApplication.translate('dialog',
                                                       'Se aumento la capacidad de un transformador monofasico debido a su cercania con otro transformador en: (') + str(
                        Graph_T1F.node[nodo]['X1']) + ', ' + str(Graph_T1F.node[nodo]['Y1']) + ')'
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Transformadores'), aviso)
                else:
                    Graph_T1F.add_node(nodo)
                    Graph_T1F.nodes[nodo].update( datos1F )  # Agrega el trafo al grafo con todos los datos
                    
                    grafoBTTotal.add_node(nodo)
                    grafoBTTotal.nodes[nodo].update( datosTotalGraph )
        return datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal

    def ReaderDataLBT(self, layer, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS):

        # self.dlg.label_Progreso.setText('Linea MT 1...')
        lineas = layer.getFeatures()  # Recibe las caracteristicas de la capa de lineas de baja tension.
        for linea in lineas:
            geom = linea.geometry()
            line = self.MultiStringToMatrix(geom)
            
            if line == 0:
                return 0, 0, 0
            
            LineLength = linea.geometry().length()
            n = len(line)  # Cantidad de vértices de la línea
            LVCode = linea['NOMVOLT']
            nodo1, nodo2 = self.CoordLineProcess(linea, toler)
            conns = lineOperations.renameVoltage(linea['NOMVOLT']).get('conns')  # phaseCodeOpenDSS
            cantFases = lineOperations.renameVoltage(linea['NOMVOLT']).get('cantFases')  # 1 or 3 phases
            config = lineOperations.renameVoltage(linea['NOMVOLT']).get('config')  # wye or delta

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
                              'INSUL': linea['INSULMAT'],
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
                              'TIPO': linea['TYPE']}  # , 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                datosTotalGraph = {"type": "LBT", 'X1': line[0][0], 'Y1': line[0][1], 'X2': line[n - 1][0],
                                   'Y2': line[n - 1][1]}  # , 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
            datosLBT.append(datosLinea)  ### Código viejo

            if grafoBT.get_edge_data(nodo1, nodo2) == None:  # se asegura que la línea no existe
                
                grafoBT.add_edge(nodo1, nodo2)
                grafoBT[nodo1][nodo2].update( datosLinea )  # Agrega la línea al grafo con todos los datos
                
                grafoBTTotal.add_edge(nodo1, nodo2)
                grafoBTTotal[nodo1][nodo2].update( datosTotalGraph )  # Agrega la línea al grafo con todos los datos
                
            else:  # Si la línea existe es porque están en paralelo
                newLength = float(datosLinea["SHLEN"]) / 2
                datosLinea["SHLEN"] = newLength
                paralelNode = "Paralel" + str(nodo1)
                datosLinea["nodo2"] = paralelNode

                grafoBT.add_edge(nodo1, paralelNode)
                grafoBT[nodo1][paralelNode].update( datosLinea )  # Agrega la línea al grafo con todos los datos
                
                grafoBTTotal.add_edge(nodo1, paralelNode)
                grafoBTTotal[nodo1][paralelNode].update( datosTotalGraph )  # Agrega la línea al grafo con todos los datos

                datosLinea["nodo2"] = nodo2
                datosLinea["nodo1"] = paralelNode

                grafoBT.add_edge(paralelNode, nodo2)
                grafoBT[paralelNode][nodo2].update( datosLinea )  # Agrega la línea al grafo con todos los datos
                
                grafoBTTotal.add_edge(paralelNode, nodo2)
                grafoBTTotal[paralelNode][nodo2].update( datosTotalGraph )  # Agrega la línea al grafo con todos los datos
        return datosLBT, grafoBT, grafoBTTotal

    def ReaderDataAcom(self, layer, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT):
        lineasACO = layer.getFeatures()  # Recibe las caracteristicas de la capa de acometidas.
        for lineaACO in lineasACO:
            
            #line = lineaACO.geometry().asPolyline()  # Lee la geometria de la linea
            
            geom = lineaACO.geometry()
            line = self.MultiStringToMatrix(geom)
            
            if line == 0:
                return 0, 0, 0
            
            LineLength = lineaACO.geometry().length()
            n = len(line)  # Cantidad de vértices de la línea
            nodo1, nodo2 = self.CoordLineProcess(lineaACO, toler)
            conns = lineOperations.renameVoltage(lineaACO['NOMVOLT']).get('conns')  # phaseCodeOpenDSS
            cantFases = lineOperations.renameVoltage(lineaACO['NOMVOLT']).get('cantFases')  # 1 or 3 phases
            config = lineOperations.renameVoltage(lineaACO['NOMVOLT']).get('config')  # wye or delta
            LVCode = lineaACO['NOMVOLT']
            # opervoltLN=lineOperations.renameVoltage(lineaACO['NOMVOLT']).get('LVCode')['LN']
            # opervoltLL=lineOperations.renameVoltage(lineaACO['NOMVOLT']).get('LVCode')['LL']
            try:
                group = lineaACO['GRUPO']
            except KeyError:
                group = 'N/A'
            datos = {"LVCODE": LVCode, "INDEXDSS": indexDSS, "LAYER": layer, "ID": lineaACO.id(), "nodo1": nodo1,
                     "nodo2": nodo2, 'PHAMAT': lineaACO['PHASEMAT'], 'PHASIZ': lineaACO['PHASESIZ'], 'X1': line[0][0],
                     'Y1': line[0][1], 'X2': line[n - 1][0], 'Y2': line[n - 1][1], 'SHLEN': LineLength,
                     'NPHAS': cantFases, 'CONNS': conns, 'CONF': config, 'GRUPO': group,
                     'TIPO': lineaACO["TYPE"]}  # 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN,
            datosTotalGraph = {"type": "ACO", 'X1': line[0][0], 'Y1': line[0][1], 'X2': line[n - 1][0],
                               'Y2': line[n - 1][1]}  # 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN,
            datosACO.append(datos)

            if grafoBT.get_edge_data(nodo1,
                                     nodo2) != None:  # Se asegura que la línea no se ha creado en el grafo de LBT
                # print "Linea acometida ya existia en grafoTOTAL"
                pass
            else:
                if grafoACO.get_edge_data(nodo1, nodo2) == None:  # se asegura que la línea no existe
                    
                    grafoACO.add_edge(nodo1, nodo2)
                    grafoACO[nodo1][nodo2].update( datos )  # Agrega la línea al grafo con todos los datos
                    
                    grafoBTTotal.add_edge(nodo1, nodo2)
                    grafoBTTotal[nodo1][nodo2].update( datosTotalGraph )
                    
                else:  # Si la línea existe es porque están en paralelo
                    newLength = float(datos["SHLEN"]) / 2
                    datos["SHLEN"] = newLength
                    paralelNode = "Paralel" + str(nodo1)
                    datos["nodo2"] = paralelNode
                    
                    grafoACO.add_edge(nodo1, paralelNode)
                    grafoACO[nodo1][paralelNode].update( datos )  # Agrega la línea al grafo con todos los datos
                    
                    grafoBTTotal.add_edge(nodo1, paralelNode)
                    grafoBTTotal[nodo1][paralelNode].update( datosTotalGraph )

                    datos["nodo2"] = nodo2
                    datos["nodo1"] = paralelNode
                    
                    grafoACO.add_edge(paralelNode, nodo2)
                    grafoACO[paralelNode][nodo2].update( datos )  # Agrega la línea al grafo con todos los datos
                    
                    grafoBTTotal.add_edge(paralelNode, nodo2)
                    grafoBTTotal[paralelNode][nodo2].update( datosTotalGraph )
                    
        return datosACO, grafoACO, grafoBTTotal

    def ReaderDataGD(self, toler, layer, grafoGD, indexDSS, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F,
                     grafoCAR, circuitName, busBTid, busBT_List, busMT_List):
        GDs = layer.getFeatures()  # Recibe las caracteristicas de la capa de cargas.
        for GD in GDs:
            point = GD.geometry().asPoint()  # Lee la geometria de la linea
            nodo = self.CoordPointProcees(GD, toler)
            nodoInTraf = False
            if (nodo in grafoCAR.nodes()):
                bus = grafoCAR.node[nodo]["BUS"]
                if grafoCAR.node[nodo]["TRAFNPHAS"] != "NULL":
                    VOLTAGELL = grafoCAR.node[nodo]["TRAFVOLTLL"]
                    VOLTAGELN = grafoCAR.node[nodo]["TRAFVOLTLN"]
                    NPHAS = grafoCAR.node[nodo]["TRAFNPHAS"]
                    conf = grafoCAR.node[nodo]["CONF"]
                else:
                    VOLTAGELL = "0.24"
                    VOLTAGELN = "0.12"
                    NPHAS = "1"
                    conf = "wye"
            elif (nodo in Graph_T3F_multi.nodes()):
                nodoInTraf == True
                bus = Graph_T3F_multi.node[nodo]["BUSBT"]
                VOLTAGELL = Graph_T3F_multi.node[nodo]["LOADVOLT"]
                VOLTAGELN = Graph_T3F_multi.node[nodo]["LOADVOLTLN"]
                NPHAS = Graph_T3F_multi.node[nodo]["NPHAS"]
                conf = Graph_T3F_multi.node[nodo]["LOADCONF"]

            elif (nodo in Graph_T3F_single.nodes()):
                nodoInTraf == True
                bus = Graph_T3F_single.node[nodo]["BUSBT"]
                VOLTAGELL = Graph_T3F_single.node[nodo]["LOADVOLT"]
                VOLTAGELN = Graph_T3F_single.node[nodo]["LOADVOLTLN"]
                NPHAS = Graph_T3F_single.node[nodo]["NPHAS"]
                conf = Graph_T3F_single.node[nodo]["LOADCONF"]

            elif (nodo in Graph_T2F.nodes()):
                nodoInTraf == True
                bus = Graph_T2F.node[nodo]["BUSBT"]
                VOLTAGELL = Graph_T2F.node[nodo]["LOADVOLT"]
                VOLTAGELN = Graph_T2F.node[nodo]["LOADVOLTLN"]
                NPHAS = Graph_T2F.node[nodo]["NPHAS"]
                conf = Graph_T2F.node[nodo]["LOADCONF"]

            elif (nodo in Graph_T1F.nodes()):
                nodoInTraf == True
                bus = Graph_T1F.node[nodo]["BUSBT"]
                VOLTAGELL = Graph_T1F.node[nodo]["LOADVOLT"]
                VOLTAGELN = Graph_T1F.node[nodo]["LOADVOLTLN"]
                NPHAS = Graph_T1F.node[nodo]["NPHAS"]
                conf = Graph_T1F.node[nodo]["LOADCONF"]
            elif (not nodoInTraf) and (nodo in busMT_List):
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
                aviso = QCoreApplication.translate('dialog', 'Hay 1 generador desconectado en: (') + str(
                    point[0]) + ',' + str(point[1]) + ')'
                QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Generador'), aviso)

            datos = {"CONF": conf, "NPHAS": NPHAS, "VOLTAGELN": VOLTAGELN, "VOLTAGELL": VOLTAGELL, "BUS": bus,
                     "INDEXDSS": indexDSS, 'ID': GD.id(), "LAYER": layer, "nodo": nodo, 'X1': point[0], 'Y1': point[1],
                     'KVA': GD['KVA'], "CURVE1": GD["CURVE1"], "CURVE2": GD["CURVE2"], "TECH": GD["TECH"]}
            
            grafoGD.add_node(nodo)
            grafoGD.nodes[nodo].update( datos )
            
        return grafoGD, busBTid, busBT_List
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
        voltageCodes = lineOperations.renameVoltage( Code )
        conf = voltageCodes['config']
        
        #Se elige tensión LL si service es 12 o 123
        if service == "12" or service == "123":
            nomvolt = str( voltageCodes['LVCode']['LL'] )
        #Si no, se elige la tensión LN
        else:
            nomvolt = str( voltageCodes['LVCode']['LN'] )
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
        voltageCodes = lineOperations.renameVoltage( Code )
        conec = voltageCodes['config']
        
        #Se elige tensión LL si es bifásico o trifásico
        if cant_fases == "2" or cant_fases == "3":
            nomvolt = str( voltageCodes['LVCode']['LL'] )
        #Si no, se elige la tensión LN
        else:
            nomvolt = str( voltageCodes['LVCode']['LN'] )
        return conec, nomvolt
        
        
    #**************************************************************************
    #**************************************************************************
    """
    Esta función se encarga de determinar si el SERVICE asociado a cierta carga
    tiene un tipo que cable que acepta este tipo de conexion
    
    Retorna un 0 en caso de haber errores, un 1 en caso contrario
    """
    #**************************************************************************
    #**************************************************************************
    def CableAnalysis(self, carga, point, grafoBT, conns, toler, tipo_analisis = "BT" ):
        
        #Se determina a qué línea está conectada la carga para averiguar si el código de cable acepta el tipo de SERVICE indicado
        index_carga = carga['DSSName']
        x1 = point[0]
        y1 = point[1]
        x1_ = int( x1/toler )
        y1_ = int( y1/toler )
        nodo1 = ( x1_, y1_ )
        
        #Analiza los nodos vecinos
        try:
            nodo2 = nx.neighbors( grafoBT, nodo1 ) #por la topología de la red sólo tendrá un nodo vecino
        except:
            if index_carga != None:
                aviso = "**La carga/ev " + str(index_carga) + " no se encuentra en el grafo de " + tipo_analisis + ". Verifique que se encuentre conectada**"
            else:
                aviso = "**La carga/ev ubicada en (" + str(x1) + "," + str(y1) + ") no se encuentra en el grafo de " + tipo_analisis + ". Verifique que se encuentre conectada**"
            aviso = str( aviso + "\n")
            self.archivo_errlog.write( aviso )
            return 0
            
        #Recupera datos del grafo de BT (línea asociada a la carga)
        for nodo_ in nodo2:  #se parte del hecho que en una red de distribucion las cargas (nodos) sólo tendrán un arista (y por tanto, un vecino)
                nodo2 = nodo_
        line_type = grafoBT.get_edge_data(nodo1, nodo2)['AIR_UGND']
        
        #Todo el análisis posterior sólo aplica si el tipo de línea es aérea
        if line_type.lower() != "air":
            return 1
        cable_type = grafoBT.get_edge_data(nodo1, nodo2)['TIPO']
            
        #Verifica que el tipo de cable y la conexión tengan sentido
        error_carga = 0
        if (cable_type == "DPX" and conns != ".1") or (cable_type == "DPX" and conns != ".2"):
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
                aviso = "**Verifique que la carga " + tipo_analisis + " /ev de ubicada en (" + str(x1) + "," + str(y1) + ") tenga el SERVICE correcto, ya que el tipo de línea " + cable_type + " no acepta estar conectada a la fase " + conns + "\n"
            else:
                aviso = "**Verifique que la carga" + tipo_analisis + " /ev " + str(index_carga) + " tenga el SERVICE correcto, ya que el tipo de línea " + cable_type + " no acepta estar conectada a la fase " + conns + "\n"                
            self.archivo_errlog.write( aviso )
            return 0
        return 1
    
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    def ReaderDataLoadBT(self, layer, datosCAR, grafoCAR, kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT):

        cargas = layer.getFeatures()  # Recibe las caracteristicas de la capa de cargas.
        error = 0
        for carga in cargas:
            point = carga.geometry().asPoint()  # Lee la geometria de la linea
            try:
                group = carga['LV_GROUP']
            except KeyError:
                group = 'N/A'
            nodo = self.CoordPointProcees(carga, toler)
            
            #CONN: atributo opcional (indica si está conectada en delta o en estrella)
            try:
                conf = str( carga['CONN'] )
                if conf.lower() == "d" or conf.lower() == "delta":
                    conex = "delta"
                elif conf.lower() == "y" or conf.lower() == "estrella" or conf.lower() == "wye":
                    conex = "wye"
                else:
                    conex = ""
            except:
                conex = ""
            #Model: atributo opcional
            try:
                model = int(carga['MODEL'])
                if model >= 1 and model <= 8:
                    model = 1
                model = str( model )
            except:
                model = "1"
            
            #Service
            try:
                service = carga['SERVICE']
                service = int( service )
                service = str( service )
                
                if service == "1":
                    conns = ".1"
                elif service == "2":
                    conns = ".2"
                elif service == "12":
                    conns = ".1.2"
                elif service == "123":
                    conns = ".1.2.3"
                else:
                    mensaje = str( "Favor introduzca un código válido para el atributo SERVICE en la capa de cargas BT")
                    QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                    return 0, 0, 0, 0
            except:
                service = "12"
                conns = ".1.2"
                
            try:
                code_nomvolt = carga['NOMVOLT']
                code_nomvolt = int( code_nomvolt )
                conf_, nom_volt = self.GetNominalVoltBT(code_nomvolt, service)
                
                if conex == "": #caso en que conex no se haya definido anteriormente
                    conex = conf_
                
                if conf_ == 0: #Caso en que se haya introducido un código no válido para NOMVOLT
                    mensaje = str( "Introduzca un código válido para NOMVOLT")
                    QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                    return 0, 0, 0, 0
            
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo NOMVOLT en la capa de cargas de BT")
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0, 0
                
            #Se llama a la función encargada de determinar si el SERVICE tiene un tipo de cable correcto
            sucessfull = self.CableAnalysis( carga, point, grafoBT, conns, toler )
            
            #Para mostrar el mensaje de errores en CableAnalysis sólo una vez
            if sucessfull == 0:
                error = 1
            
            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            
            datos = {"INDEXDSS": indexDSS, 'ID': carga.id(), "LAYER": layer, "nodo": nodo, 'X1': x1,
                     'Y1': y1, 'kWh': carga['KWHMONTH'], 'GRUPO': group, 'kW': 1.0, 'CONNS': conns,
                     'CONF': conex, 'CURVASIG': '', 'class': carga['CLASS'], 'NOMVOLT': nom_volt, 'MODEL': model, 'CODE_NOMVOLT': code_nomvolt}
            datosTotal = {"type": "LOAD"}
            kWhLVload.append(float(carga['KWHMONTH']))
            datosCAR.append(datos)
            grafoCAR.add_node(nodo)
            grafoCAR.nodes[nodo].update( datos )
            
            grafoBTTotal.add_node(nodo)
            grafoBTTotal.nodes[nodo].update( datosTotal )
        
        #Envía un mensaje al usuario
        if error == 1:
            aviso = "Verifique el archivo log, debido a que hubo errores asociados al SERVICE y tipo de cable asociado a la carga de BT"
            QMessageBox.warning(None, "Error BT", aviso)
        return datosCAR, grafoCAR, kWhLVload, grafoBTTotal
        
    
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    def ReaderDataBuses(self, layer, datosBuses, grafoBuses, kWhBuses, toler, indexDSS, grafoMT, csv_buses, csv_cargadores):
        
        #Se agrega el atributo DSSName si no existe en la capa respectiva
        names = layer.dataProvider().fields().names()
        if "DSSName" not in names:
            index_dss = layer.dataProvider().addAttributes([QgsField("DSSName", QVariant.String)])
            layer.updateFields()
        
        #Lectura de csv's de cargadores y buses
        dict_buses = self.csv_to_dict( csv_buses )
        dict_cargadores = self.csv_to_dict( csv_cargadores )
        
        if dict_buses == 0 or dict_cargadores == 0:
            return 0, 0, 0

        buses = layer.getFeatures()  # Recibe las caracteristicas de la capa de buses.
        layer.startEditing()
        
        error = 0
        for bus in buses:
            point = bus.geometry().asPoint()  # Lee la geometria de la linea
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
                mensaje = str( "Favor introduzca el atributo PLANTEL en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            
            
            #########################
            #Atributos para trafo
            #########################
            
            #PHASEDESIG
            phasedesig = "ABC"
            fase = phaseOperations.renamePhase( phasedesig ).get('phaseCodeODSS')  # define código de OpenDSS
            numfase = phaseOperations.renamePhase( phasedesig ).get('phaseNumberTraf')
            
            
            
            #PRIMVOLT
            try:
                primvolt = bus['PRIMVOLT']
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo PRIMVOLT en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            #SECVOLT
            try:
                secvolt = bus['SECVOLT']
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo SECVOLT en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            voltages = trafoOperations.renameVoltage(int(primvolt), int(secvolt))
            loadvolt = str(voltages["LVCode"]["LL"])
            loadvoltLN = str(voltages["LVCode"]["LN"])
                
            if voltages["MVCode"]["LL"] == 0:
                voltoprLL = "UNKNOWN"
                voltoprLN = "UNKNOWN"
                aviso = str( 'No se encuentra el código de tensión ' + str(primvolt) )
                #QMessageBox.warning(None, 'Alerta transformador buses', aviso)
            else:
                voltoprLL = str(voltages["MVCode"]["LL"])
                voltoprLN = str(voltages["MVCode"]["LN"])
            
            #PRIMCONN
            try:
                primconn = bus['PRIMCONN']
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo PRIMCONN en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            #Se determina la tensión l-n del primario
            if primconn.lower() == "d":
                voltoprLN = voltoprLL
            elif primconn.lower() == "sp":
                voltoprLN = str( 2*float(voltoprLL) )
                voltoprLN = format(voltoprLN, '.3f' )
            elif primconn.lower() == "y":
                voltoprLN = float(voltoprLL)/sqrt(3)
                voltoprLN = format(voltoprLN, '.3f' )
            #si no es ninguno de estos casos se deja la tensión por defecto (según el cuadro del manual)
            
            #SECCONN
            try:
                secconn = bus['SECCONN']
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo SECCONN en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            #Se determina la tensión l-n del secundario
            if secconn.lower() == "d":
                loadvoltLN = loadvolt
            elif secconn.lower() == "sp":
                loadvoltLN = 2*float(loadvolt)
                loadvoltLN = format(loadvoltLN, '.3f' )
            elif secconn.lower() == "y":
                loadvoltLN = float(loadvolt)/sqrt(3)
                loadvoltLN = format(loadvoltLN, '.3f' )
            #si no es ninguno de estos casos se deja la tensión por defecto (según el cuadro del manual)
            
            #Preguntar Tavo (4 delta 4 hilos)
            
            
            #RATEDKVA
            try:
                ratedkva = bus['RATEDKVA']
                
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo RATEDKVA en la capa de buses y asegúrese que sea un número ")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            #TAPSETTING
            try:
                tapsetting = bus['TAPSETTING']
                tapsetting = format(float(tapsetting), '.3f')
            except KeyError:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo TAPSETTING en la capa de buses")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
                
            #Diccionario de trafos del bus
            dict_trafo = {"NPHAS": numfase, "MVCODE": primvolt, "LVCODE": secvolt, "TAPS": tapsetting, 'PHASE': fase,
                            'KVA': ratedkva, 'KVM': primvolt, 'KVL': secvolt, 'CONME': primconn, 'CONBA': secconn,
                            'LOADCONNS': '.1.2.3', 'LOADCONF': 'wye', 'LOADVOLT': loadvolt, 'LOADVOLTLN': loadvoltLN,
                            'GRUPO_MV': group, "VOLTMTLL": voltoprLL, "VOLTMTLN": voltoprLN}
            
            
            #########################
            #Atributos para carga BT
            #########################
            
            #Model: atributo opcional
            try:
                model = int(bus['MODEL'])
                if model >= 1 and model <= 8:
                    model = 1
                model = str( model )
            except:
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
                kw_month =  str( bus['KWHMONTH'] )
                kw_month_t = float( kw_month )
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo KWHMONTH en la capa de buses y verifique que sea un valor numérico")
                QMessageBox.critical(None,"QGIS2OpenDSS error buses", mensaje)
                return 0, 0, 0
            
            
                
            dict_load_bt = {'kWh': kw_month, 'kW': 1.0, 'CONNS': conns, 'CONF': conex, 'CURVASIG': '', 'class': bus['CLASS'], 
                            'NOMVOLT': nom_volt, 'MODEL': model}
            
            """
            #Se llama a la función encargada de determinar si el SERVICE tiene un tipo de cable correcto
            sucessfull = self.CableAnalysis( bus, point, grafoMT, conns, toler, tipo_analisis = "MT" ) 
            
            #Para mostrar el mensaje de errores en CableAnalysis sólo una vez
            if sucessfull == 0:
                error = 1
            """
            
            #Diccionarios buses y cargadores del plantel actual
            dict_buses_plant = dict_buses[ nombre_plantel ]
            dict_cargadores_plant = dict_cargadores[ nombre_plantel ]
            
            
            
            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            dssname = str( "plant_" + nombre_plantel.lower() ) 
            
            #Cambios en la capa
            idx_dss = bus.fieldNameIndex("DSSName")
            layer.changeAttributeValue( bus.id(), idx_dss, dssname)
            
            datos = {"INDEXDSS": indexDSS, 'ID': bus.id(), "LAYER": layer, "nodo": nodo, 'X1': x1, 'Y1': y1, 'GRUPO': group, 'NOMBRE PLANTEL': nombre_plantel, 'DSSName': dssname,
                     'DICT TRAFO': dict_trafo, 'DICT LOAD BT': dict_load_bt, 'DICT BUSES': dict_buses_plant, 'DICT CARG': dict_cargadores_plant
                     }
            kWhBuses.append(float(bus['KWHMONTH']))
            datosBuses.append(datos)
            grafoBuses.add_node(nodo)
            grafoBuses.nodes[nodo].update( datos )
            
        if error == 1:
            aviso = "Verifique el archivo log, debido a que hubo errores asociados al SERVICE y tipo de cable asociado al bus"
            QMessageBox.warning(None, "Error MT", aviso)
        layer.commitChanges()
        return datosBuses, grafoBuses, kWhBuses
    
    
    
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    def ReaderDataLoadMT(self, layer, datosCAR, grafoCAR, kWhMVload, toler, indexDSS, grafoMT):
        cargasMT = layer.getFeatures()  # Recibe las caracteristicas de la capa de cargas.
        error = 0
        for carga in cargasMT:
            point = carga.geometry().asPoint()  # Lee la geometria de la linea
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
                model = str( model )
            except:
                model = "1"
            
            #CONN: atributo opcional (indica si está conectada en delta o en estrella)
            try:
                conf = str( carga['CONN'] )
                if conf.lower() == "d" or conf.lower() == "delta":
                    conex = "delta"
                elif conf.lower() == "y" or conf.lower() == "estrella" or conf.lower() == "wye":
                    conex = "wye"
                else:
                    conex = ""
            except:
                conex = ""
                
            #PHASEDESIG
            try:
                phasedesig = carga['PHASEDESIG']
                data_phasedesig = phaseOperations.renamePhase( phasedesig )
                conns = data_phasedesig['phaseCodeODSS']
                cant_fases = data_phasedesig['phaseNumber']
                
                if conns == "NONE": #Caso en que se haya introducido un código no válido para PHASEDESIG
                    mensaje = str( "Introduzca un código válido para PHASEDESIG")
                    QMessageBox.critical(None,"QGIS2OpenDSS error MT", mensaje)
                    return 0, 0, 0
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo PHASEDESIG en la capa de cargas de MT")
                QMessageBox.critical(None,"QGIS2OpenDSS error MT", mensaje)
                return 0, 0, 0
            
            try:
                #NOMVOLT
                code_nomvolt = carga['NOMVOLT']
                code_nomvolt = int( code_nomvolt )
                conf_, nom_volt = self.GetNominalVoltMT(code_nomvolt, cant_fases)
                
                #Caso en que el tipo de conexion no se haya introducido
                if conex == "":
                    conex = conf_
                
                if conf == 0: #Caso en que se haya introducido un código no válido para NOMVOLT
                    mensaje = str( "Introduzca un código válido para NOMVOLT")
                    QMessageBox.critical(None,"QGIS2OpenDSS error MT", mensaje)
                    return 0, 0, 0
            
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo NOMVOLT en la capa de cargas de MT")
                QMessageBox.critical(None,"QGIS2OpenDSS error MT", mensaje)
                return 0, 0, 0
                
            #Factor de potencia
            try:
                pf_ = float( carga['PF'] )
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo PF en la capa de cargas de MT y verifique que sea un valor numérico")
                QMessageBox.critical(None,"QGIS2OpenDSS error MT", mensaje)
                return 0, 0, 0
            pf = str( pf_ )
            
            """
            #Se llama a la función encargada de determinar si el SERVICE tiene un tipo de cable correcto
            sucessfull = self.CableAnalysis( carga, point, grafoMT, conns, toler, tipo_analisis = "MT" ) 
            
            #Para mostrar el mensaje de errores en CableAnalysis sólo una vez
            if sucessfull == 0:
                error = 1
            """
            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            
            datos = {"INDEXDSS": indexDSS, 'ID': carga.id(), "LAYER": layer, "nodo": nodo, 'X1': x1,
                     'Y1': y1, 'kWh': carga['KWHMONTH'], 'GRUPO': group, 'kW': 1.0, 'CONNS': conns,
                     'CONF': conex, 'CURVASIG': '', 'class': carga['CLASS'], 'NOMVOLT': nom_volt, 'MODEL': model, 'CODE_NOMVOLT': code_nomvolt, 'PowerFactor':  pf,'N_FASES': cant_fases}
            kWhMVload.append(float(carga['KWHMONTH']))
            datosCAR.append(datos)
            grafoCAR.add_node(nodo)
            grafoCAR.nodes[nodo].update( datos )
            
        if error == 1:
            aviso = "Verifique el archivo log, debido a que hubo errores asociados al SERVICE y tipo de cable asociado a la carga de MT"
            QMessageBox.warning(None, "Error MT", aviso)
        return datosCAR, grafoCAR, kWhMVload

        
    
    """
    Esta función encuentra el primer último 0 en determinada columna
    Retorna un -1 si se encuentra en la última posición, un x + 1 caso contrario,
    donde x es el número de fila donde se encontró
    
    Parámetros:
    *column (columna de un dataframe, pero también puede ser un vector
    
    Valores retornados:
    *t (int): número de columna + 1 en la que fue encontrado el primer cero
    """
    
    def find_t(self, column):
        n_columnas = len(column)
        for j in reversed( range( n_columnas ) ): #se recorren al revés la columnas
            dato = column[j]
            if dato == 0 and j == n_columnas - 1:
                return -1 
            elif dato == 0:
                return j + 1 

   
   
    """
    #**************************************************************************
    #**************************************************************************
    ############################### WriteFileEV ###############################
    #**************************************************************************
    #**************************************************************************    
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
            path_csv = self.name_loadshapes.split("/") #path relativo del csv
            name_csv = path_csv[1] #Nombre del archivo csv
            folder_csv = path_csv[0] #Nombre de la carpeta donde está el csv
            
            name_loadshapes = folder_csv + "/" + self.circuitName + "_EVLoadshapes.dss" #Path relativo de los loadshapes dss
            filename_ev_dss = self.foldername + '/' + name_dss #Path absoluto
            
            filename_loadshapes = self.foldername + "/" + name_loadshapes
            print( filename_ev_dss, " ", filename_loadshapes, " ", name_csv )
            
            file_ev = open( filename_ev_dss, 'w' )
            file_loadshape = open( filename_loadshapes, 'w' )
            
            columns_evs_loadshapes = self.evs_loadshapes.columns            
            contador = -1 #Ya que se aumenta al principio, no al final
            
            for ve in grafoEV.nodes(data=True):
                contador += 1
                ve_ = ve[1]
                #Datos de BT
                x1 = str( ve_['X1'] )
                y1 = str( ve_['Y1'] )
                x1_ = int( float(x1)/toler )
                y1_ = int( float(y1)/toler )
                nodo1 = ( x1_, y1_ )
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
                daily_curve = str( "EV_" + str(contador) )
                cant_soc = len(soc)
                extra_sentence = "!Group=" + str( grupo )
                if len(soc) != 1: #Caso en que se tengan varios estados de carga iniciales
                    #Obtener t (para soc) (donde se da el último 0 en la columna)
                    name_column = columns_evs_loadshapes[ contador ]
                    columna = self.evs_loadshapes[ name_column ]
                    t = self.find_t( columna )
                    for i in range(1, cant_soc):
                        extra_sentence += " !stored=" + str(soc[i]) + " " 
                    extra_sentence += "!t=" + str(t) + " "
                else:
                    extra_sentence += " !t=-1"
                
                sentence = str( "new storage." + name_ev_dss )
                sentence += " bus1=" + bus + conns
                sentence += " phases=1 model=1"
                sentence += " kW=" + str(kW)
                sentence += " kV=" + str(kV)
                sentence += " pf=0.98"
                sentence += " kWrated=" + str(kW)
                sentence += " kWhrated=" + str(kWh)
                sentence += " %reserve=0"
                sentence += " %stored=" + str( soc[0] )
                sentence += " %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW daily=" + daily_curve
                sentence += " " + extra_sentence + "\n"
                sentence = str( sentence )
                file_ev.write( sentence )
                
                sentence_loadshape = "New Loadshape." + daily_curve  + " npts=96 mInterval=15 mult=(file="
                sentence_loadshape += name_csv + ", col=" + str(contador + 1) + ", header=no) useactual=no\n"
                sentence_loadshape = str( sentence_loadshape )
                file_loadshape.write( sentence_loadshape )
            
            #se cierran los archivos
            file_ev.close() 
            file_loadshape.close()
            
            #Escritura master
            self.output_filesQGIS2DSS.write('\nredirect ' +  name_loadshapes)
            self.output_filesQGIS2DSS.write('\nredirect ' + name_dss)
            
            return 1
        except:
            self.print_error()
            mensaje = str( "Hubo un error al escribir el archivo de VE.\nPara más información revise el código de error en la consola.")
            QMessageBox.critical(None,"QGIS2OpenDSS escritura VE", mensaje)
            return 0
        
        
    """
    #**************************************************************************
    #**************************************************************************
    ############################### WriteFileEV ###############################
    #**************************************************************************
    #**************************************************************************    
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
            file_bus = open( filename_bus_dss, 'w' )
            
            name_bus = self.circuitName + "_StorageBuses.dss"
            filename_bus_real = self.foldername + '/' + name_bus #Path absoluto
            file_bus_real = open( filename_bus_real, 'w' )
            
            #Extra loadshapes (temporal)
            name_csv = "csv_loadshapes_buses.csv"
            name_loadshape_dss = self.circuitName + "_LoadshapesBuses.dss"
            filename_loadshape_buses = self.foldername + '/' + name_loadshape_dss #Path absoluto
            file_loadshape_bus = open( filename_loadshape_buses, 'w' )
            sentence_loadshape = ""
            
            contador = -1
            
            for bus in grafoBuses.nodes(data=True):                
                bus_ = bus[1]
                #Datos de BT
                x1 = str( bus_['X1'] )
                y1 = str( bus_['Y1'] )
                nodo = bus[0]
                x1_ = int( float(x1)/toler )
                y1_ = int( float(y1)/toler )
                nodo1 = ( x1_, y1_ )
                
                #Datos generales
                datos_trafo = bus_['DICT TRAFO']
                datos_carga_bt = bus_['DICT LOAD BT']
                datos_buses = bus_['DICT BUSES']
                datos_cargador = bus_['DICT CARG']
                nombre_plantel = bus_['NOMBRE PLANTEL']
                group = str( bus_['GRUPO'] )
                dssname = str( bus_['DSSName'] )
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
                datos_nodo = {'bus': busLV, 'X': x1, 'Y': y1, "GRAFO": grafoBuses, "VOLTAGELN": str(kV_LowLN)}               
                busBT_List[nodo] = datos_nodo
                
                #Línea trafo
                trafo_line = 'new transformer.' + trafName + ' phases=3 windings=2 ' + noloadloss + " " + imag + ' buses=[' + busMV + '.1.2.3 '
                trafo_line += busLV + '.1.2.3]' + ' conns=[' + confMV + ' ' + confLV + ']' + ' kvs=[' + kV_MedLL + " " +  kV_LowLL + ']'
                trafo_line += ' kvas=[' + kVA + " " + kVA + '] ' + impedance + ' Taps=[' + tap + ', 1]' + normhkva + ' !GroupMV=' + group + "\n"
                line_monitor = "new monitor.Mon" + trafName + " Element=transformer." + trafName + " Terminal=1 Mode=1\n"
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
                    kw_carg = ""
                    try:
                        kw_carg = datos_cargador[dato]['KW']
                        kw_carg = " !kw_charg=" + str(kw_carg)
                    except:
                        if i == 0: #porque no necesariamente hay igual cantidad de cargadores que de buses
                            self.print_error()
                            mensaje = str( "Por favor añada el CSV de cargadores con los datos requeridos.\nPara más información revise el código de error en la consola.")
                            QMessageBox.critical(None,"QGIS2OpenDSS escritura buses", mensaje)
                            return 0
                        else:
                            pass
                    
                    name_bus_dss = dssname + "_" + str( i ) 
                    #kW = "1"
                    conns = ".1.2.3"
                    cantFases = "3"
                    
                    #temporal
                    kW = "75"
                    
                    try:
                        kWh = datos_buses[dato]['KWHBATTERY']
                        soc = datos_buses[dato]['SOCi']
                        h_llegada = datos_buses[dato]['HORA LLEGADA']
                        h_salida = datos_buses[dato]['HORA SALIDA']
                    except:
                        self.print_error()
                        mensaje = str( "Por favor añada el CSV de buses con los datos requeridos.\nPara más información revise el código de error en la consola.")
                        QMessageBox.critical(None,"QGIS2OpenDSS escritura buses", mensaje)
                        return 0

                        
                    #Nombre del daily curve
                    daily_curve = "plantel" + str( contador )
                    
                    extra_sentence = "daily=" + daily_curve + " !GroupMV=" + str( group ) + " !hora_llegada=" + str(h_llegada) + " !hora_salida=" + str(h_salida) + kw_carg
                   
                    """
                    if len(soc) != 1: #Caso en que se tengan varios estados de carga iniciales
                        #Obtener t (para soc) (donde se da el último 0 en la columna)
                        name_column = columns_evs_loadshapes[ contador ]
                        columna = self.evs_loadshapes[ name_column ]
                        t = self.find_t( columna )
                        for i in range(1, cant_soc):
                            extra_sentence += " !stored=" + str(soc[i]) + " " 
                        extra_sentence += "!t=" + str(t) + " "
                    else:
                        extra_sentence += " !t=-1"
                    """
                    
                    #Línea bus
                    bus_line += "new storage." + name_bus_dss
                    bus_line += " bus1=" + busLV + conns
                    bus_line += " phases=3 model=1"
                    bus_line += " kW=" + str(kW)
                    bus_line += " kV=" + str(kV_LowLL)
                    bus_line += " pf=0.98"
                    bus_line += " kWrated=" + str(kW)
                    bus_line += " kWhrated=" + str(kWh)
                    bus_line += " %reserve=0"
                    bus_line += " %stored=" + str( soc )
                    bus_line += " %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW"
                    bus_line += " " + extra_sentence + "\n"
                    monitor_line += "new monitor.mon_" + name_bus_dss + " element=storage." + name_bus_dss + " mode=1\n"
                    
                    #Temporal
                    sentence_loadshape += "New Loadshape." + daily_curve  + " npts=96 mInterval=15 mult=(file="
                    sentence_loadshape += name_csv + ", col=" + str(contador + 1) + ", header=no) useactual=no\n"
                
                #Carga BT
                kW = str(datos_carga_bt['kW'])
                conns = str( datos_carga_bt['CONNS'] )
                conf = str( datos_carga_bt['CONF'] )
                model = str( datos_carga_bt['MODEL'] )
                kWhmonth = str( datos_carga_bt['kWh'] )
                loadclass = str( datos_carga_bt['class'] )
                daily = str( datos_carga_bt['CURVASIG'] )
                pf = "0.9"
                loadName = "LV_" + dssname
                loadName = str( loadName )
                btload_line = str("new load." + loadName + " bus1=" + busLV + conns + " kV=" + str(kV_LowLL) + " model=" + model + " conn=" + conf)
                btload_line += " kW=" + kW + " pf=" + pf + " status=variable phases=" +  cantFases + " " + daily
                btload_line += " !kWh="  + kWhmonth + " class=" + loadclass + " !GroupMV=" + group + "\n"
                
                sentence = trafo_line + btload_line
                file_bus_real.write(bus_line + monitor_line)
                file_bus.write( sentence )
                
                #Temporal
                file_loadshape_bus.write( sentence_loadshape )
                
                
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
        except:
            self.print_error()
            mensaje = str( "Hubo un error al escribir el archivo de buses.\nPara más información revise el código de error en la consola.")
            QMessageBox.critical(None,"QGIS2OpenDSS escritura buses", mensaje)
            return 0
       
    
    
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
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
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    
    def VETypeStudy(self, studytype, evs_aux, grafoCAR, toler, percent_ve = 0):
        n_carros = 0
        
        #Tipo de estudio en que se proporciona la capa de VES
        if studytype == "layer":
            for ev in evs_aux:
                DSSName = str( "EV_" + self.circuitName + "_" + str(n_carros) )
                DSSName_List.append(DSSName)
                n_carros += 1 
            print( "n_carros = ", n_carros )
        
        #Tipo de estudio random
        if studytype == "random":
            for ev in evs_aux:
                DSSName = str( "EV_" + self.circuitName + "_" + str(n_carros) )
                class_ = ev['class']
                n_carros += 1
                if class_ == "R":
                    DSSName_List.append(DSSName) #sólo deben utilizarse cargas residenciales
                
            percent_ve = float( percent_ve / 100)
            n_carros = int( n_carros*percent_ve ) #cantidad de carros tomados en cuenta para el estudio con base en el porcentaje indicado por el usuario
            print( "n_carros = ", n_carros )
            if n_carros <= 0:
                mensaje = str( "Favor introduzca el atributo SERVICE en la capa de EV con el formato indicado en el manual")
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0
                
            DSSName_List = np.random.shuffle(DSSName_List)
            DSSName_List = DSSName_List[ : n_carros ] #se reajusta el vector aleatoriamente para que sólo seleccione la cantidad de VE seleccionada
        
        #Tipo de estudio por probabilidad
        if studytype == "prob":
            #Crear diccionario a partir del csv
            for ev in evs_aux:
                point = ev.geometry().asPoint()  # Lee la geometria de la linea
                x1 = point[0]
                y1 = point[1]
                x1 = int( x1/toler )
                y1 = int( y1/toler )
                nodo1 = ( x1, y1 )
                try:
                    inf_load_BT = grafoCAR.nodes[nodo1]
                    kwh = inf_load_BT['kWh']
                    class_ = inf_load_BT['class']
                except:
                    continue
                
                prob = 10 #asignar del csv (diccionario)
                n = 1000 #cantidad de iteraciones
                ve_enable = np.random.binomial(n, prob)
                
                if ve_enable == 1 and class_ == "R":
                    DSSName = str( "EV_" + self.circuitName + "_" + str(n_carros) )
                    DSSName_List.append(DSSName)
                n_carros += 1
    
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #***************** LECTURA DE CAPA DE CARROS ELÉCTRUCOS *******************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    #**************************************************************************
    def ReaderDataEV(self, layer, datosEV, grafoEV, kWhLVev, toler, indexDSS, grafoBTTotal, grafoCAR, grafoBT):
        error = 0
        evs = layer.getFeatures()  # Recibe las caracteristicas de la capa de evs.
        #Se necesita la cantidad de carros para obtener los datos aleatorios, en caso de requerirse
        n_carros = 0
        for ev in evs:
            n_carros += 1 
        print( "n_carros = ", n_carros )
        
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
        evs_loadshapes, evs_power, evs_initial_soc, evs_kwh = CreacionPerfilesEV( power_val, kwh_val, total_cars =  n_carros )
        self.evs_loadshapes = evs_loadshapes #se utiliza como parámetro en la escritura de VEs
        path_loadshapes = self.foldername + "/profiles"
            
        #Crea la carpeta donde se crearán los perfiles aleatorios, si esta no existe
        if not os.path.exists(path_loadshapes):
            os.makedirs(path_loadshapes)
        
        self.name_loadshapes = "profiles/EV_profiles.csv" #Parámetro utilizado por la función que escribe archivo dss de VEs para saber donde están guardados los perfiles de VEs
        name_loadshapes = str( self.foldername + "/" + self.name_loadshapes )
        evs_loadshapes.to_csv(name_loadshapes, header=False, index=False) 
        kw_random = list( evs_power.iloc[ 0 ].values )
        kwH_random = list( evs_kwh.iloc[ 0 ].values )
        
        contador = -1 #Porque se aumenta al inicio del for, no al final
        layer.startEditing()
        evs = layer.getFeatures()
        for ev in evs:
            contador = contador + 1            
            DSSName = str( "EV_" + self.circuitName + "_" + str(contador) )
            
            nodo = self.CoordPointProcees(ev, toler)
            
            #Model: atributo opcional
            try:
                model = int(ev['MODEL'])
                if model >= 1 and model <= 8:
                    model = 1
                model = str( model )
            except:
                model = "1"
                
            point = ev.geometry().asPoint()  # Lee la geometria de la linea
            x1 = point[0]
            y1 = point[1]
            x1_ = int( x1/toler )
            y1_ = int( y1/toler )
            nodo1 = ( x1_, y1_ )
            
            try:
                inf_load_BT = grafoCAR.nodes[nodo1]
                kWh = inf_load_BT['kWh']
                
            except:
                aviso = str( "**El EV " + DSSName + " no se encuentra en el grafo de baja tensión. Verifique que se encuentre conectado**" )
                aviso = str( aviso + "\n")
                self.archivo_errlog.write( aviso )
                
            try:
                group = inf_load_BT['LV_GROUP']
            except KeyError:
                group = 'N/A'
            
            #Lectura de datos de capa
            try:
                
                #CONNECTION
                connection = ev['SERVICE']
                connection = int( connection )
                connection = str( connection )
                
                if connection == "1":
                    conns = ".1"
                elif connection == "2":
                    conns = ".2"
                elif connection == "12":
                    conns = ".1.2"
                elif connection == "123":
                    conns = ".1.2.3"
                else:
                    mensaje = str( "Favor introduzca un código válido para el atributo SERVICE en la capa de EV")
                    QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                    return 0, 0, 0, 0
            except:
                self.print_error()
                mensaje = str( "Favor introduzca el atributo SERVICE en la capa de EV con el formato indicado en el manual")
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0
            #Fin de datos de capa
            
            #NOMVOLT
            code_nomvolt = inf_load_BT['CODE_NOMVOLT']
            code_nomvolt = int( code_nomvolt )
            conf, nom_volt = self.GetNominalVoltBT(code_nomvolt, connection)
            
            if conf == 0: #Caso en que se haya introducido un código no válido para NOMVOLT
                mensaje = str( "Introduzca un código válido para NOMVOLT en la capa de cargas BT")
                QMessageBox.critical(None,"QGIS2OpenDSS error BT", mensaje)
                return 0, 0, 0
                
            #CLASS
            class_ = inf_load_BT['class']
            #Fin lectura de datos de cargas BT
            
            
            #Se llama a la función encargada de determinar si SERVICE tiene un tipo de cable correcto asociado
            self.CableAnalysis( ev, point, grafoBT, conns, toler ) 
            
            #Lectura de datos de capa
            
            #kWH
            try:
                kwHrated = float( ev['KWHBATTERY'] )
                if kwHrated == None or kwHrated == "":
                    #Asignar datos aleatorios
                    kwHrated = float( kwH_random[ contador ] )
                
            except:
                #Asignar datos aleatorios
                kwHrated = float( kwH_random[ contador ] )
            
            #kW
            try:
                kw = ev['KW']
                if kw == None or kw == "":
                    #Asignar datos aleatorios
                    kw = float( kw_random[contador] )
            except:
                #Asignar datos aleatorios
                kw = float( kw_random[contador] )
            
            #Vector de estado de carga inicial (state of charge initial) . Es un dato aleatorio siempre
            n_filas_soc = len( evs_initial_soc )
            vect_soc = []
            for k in range( n_filas_soc ):
                data_soc = evs_initial_soc.iloc[ n_filas_soc - k - 1][ contador ] #Para que asigne primero el último valor
                if data_soc != None:
                    data_soc = float( data_soc )
                    if np.isnan(data_soc) == False: 
                        vect_soc.append( data_soc )
            vect_soc.sort(reverse = True) #Se ordena el vector descendentemente
                        
            #Cambios en la capa
            idx_dss = ev.fieldNameIndex("DSSName")
            layer.changeAttributeValue( ev.id(), idx_dss, DSSName)
            idx_grup = ev.fieldNameIndex("LV_GROUP")
            layer.changeAttributeValue( ev.id(), idx_grup, group)
                
            
            #Coordenadas
            x1 = point[0]
            y1 = point[1]
            
            
            datos = {"INDEXDSS": indexDSS, 'ID': ev.id(), "LAYER": layer, "nodo": nodo, 'X1': x1,
                     'Y1': y1, 'kWh': kwHrated, 'GRUPO': group, 'kW': kw, 'CONNS': conns,
                     'CONF': conf, 'CURVASIG': '', 'class': class_, 'NOMVOLT': nom_volt, 'MODEL': model, 'SOC_INIC': vect_soc, 'DSSName': DSSName}
            datosTotal = {"type": "EV"}
            datosEV.append(datos)
            grafoEV.add_node(nodo)
            grafoEV.nodes[nodo].update( datos )
            grafoBTTotal.add_node(nodo)
            grafoBTTotal.nodes[nodo].update( datosTotal )
        
        layer.commitChanges()
        return datosEV, grafoEV, grafoBTTotal

    #########################################################################################################
    #########################################################################################################
    def BusAsignationTraf(self, circuitName, grafo, busMT_List, busMTid, busBT_List, busBTid, tipo, grafoMT):
        graphNodes = list( grafo.nodes(data=True) )
        for NODE in graphNodes:
            
            dataList = NODE[1]
            nodo = NODE[0]
            BTbus = 'BUSLV' + circuitName + str(busBTid)
            grafo.node[nodo]["BUSBT"] = BTbus
            
            busBT_List[nodo] = {'bus': BTbus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafo, "VOLTAGELN": dataList["LOADVOLTLN"]}
            busBTid += 1
            if nodo in busMT_List:  # Verifica si el nodo del transformador ya está en los nodos creados en MT
                
                grafo.node[nodo]["BUSMT"] = busMT_List[nodo]["bus"]
                for secondNode in grafoMT[nodo]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                    dataLine = grafoMT[nodo][secondNode]
                    if phaseOperations.trafoPhaseMT(dataLine['PHASE'], dataList['PHASE']) == 0:

                        layer = grafo.node[nodo]["LAYER"]
                        indexPhase = auxiliary_functions.getAttributeIndex(self, layer, "PHASEDESIG")
                        indexPowerA = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEA")
                        indexPowerB = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEB")
                        indexPowerC = auxiliary_functions.getAttributeIndex(self, layer, "KVAPHASEC")
                        grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPhase,
                                                                       dataLine['PHASEDESIG'])
                        ############# Solo se realiza la autocorreción si la línea y el transformador tienen la misma cantidad de fases ######
                        ###CORRECIÓN DE FASE DE TRANSFORMADOR
                        PowerA = float(dataList["KVA_FA"])
                        PowerB = float(dataList["KVA_FB"])
                        PowerC = float(dataList["KVA_FC"])

                        if (dataList['PHASE'] == ".1.2" and dataLine['PHASE'] == ".1.3") or (
                                dataList['PHASE'] == ".1.3" and dataLine['PHASE'] == ".1.2") or (
                                dataList['PHASE'] == ".2" and dataLine['PHASE'] == ".3") or (
                                dataList['PHASE'] == ".3" and dataLine['PHASE'] == ".2"):
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerB, PowerC)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerC, PowerB)
                            grafo.node[nodo]["KVA_B"] = PowerC
                            grafo.node[nodo]["KVA_C"] = PowerB
                            aviso = QCoreApplication.translate('dialog',
                                                               u'Conexión entre fases distintas corregida en (') + str(
                                dataList["X1"]) + ',' + str(dataList["Y1"]) + QCoreApplication.translate('dialog',
                                                                                                         u'). Línea MT con fase ') + str(
                                dataLine['PHASE']) + QCoreApplication.translate('dialog',
                                                                                ' y transformador con fase ') + str(
                                dataList['PHASE'])
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Fase de Transformadores'), aviso)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.node[nodo]["PHASE"] = dataLine['PHASE']
                        elif (dataList['PHASE'] == ".1.2" and dataLine['PHASE'] == ".2.3") or (
                                dataList['PHASE'] == ".2.3" and dataLine['PHASE'] == ".1.2") or (
                                dataList['PHASE'] == ".1" and dataLine['PHASE'] == ".3") or (
                                dataList['PHASE'] == ".3" and dataLine['PHASE'] == ".1"):
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerA, PowerC)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerC, PowerA)
                            grafo.node[nodo]["KVA_A"] = PowerC
                            grafo.node[nodo]["KVA_C"] = PowerA
                            aviso = QCoreApplication.translate('dialog',
                                                               u'Conexión entre fases distintas corregida en (') + str(
                                dataList["X1"]) + ',' + str(dataList["Y1"]) + QCoreApplication.translate('dialog',
                                                                                                         u'). Línea MT con fase ') + str(
                                dataLine['PHASE']) + QCoreApplication.translate('dialog',
                                                                                ' y transformador con fase ') + str(
                                dataList['PHASE'])
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Fase de Transformadores'), aviso)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.node[nodo]["PHASE"] = dataLine['PHASE']
                        elif (dataList['PHASE'] == ".2.3" and dataLine['PHASE'] == ".1.3") or (
                                dataList['PHASE'] == ".1.3" and dataLine['PHASE'] == ".2.3") or (
                                dataList['PHASE'] == ".2" and dataLine['PHASE'] == ".1") or (
                                dataList['PHASE'] == ".1" and dataLine['PHASE'] == ".2"):
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerB, PowerA)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPowerA, PowerB)
                            grafo.node[nodo]["KVA_B"] = PowerA
                            grafo.node[nodo]["KVA_A"] = PowerB
                            aviso = QCoreApplication.translate('dialog',
                                                               u'Conexión entre fases distintas corregida en (') + str(
                                dataList["X1"]) + ',' + str(dataList["Y1"]) + QCoreApplication.translate('dialog',
                                                                                                         u'). Línea MT con fase ') + str(
                                dataLine['PHASE']) + QCoreApplication.translate('dialog',
                                                                                ' y transformador con fase ') + str(
                                dataList['PHASE'])
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Fase de Transformadores'), aviso)
                            grafo.node[nodo]["LAYER"].changeAttributeValue(grafo.node[nodo]["ID"], indexPhase,
                                                                           dataLine['PHASEDESIG'])
                            grafo.node[nodo]["PHASE"] = dataLine['PHASE']
                        else:
                            aviso = QCoreApplication.translate('dialog', u'Conexión entre fases distintas en (') + str(
                                dataList["X1"]) + ',' + str(dataList["Y1"]) + QCoreApplication.translate('dialog',
                                                                                                         u'). Línea MT con fase ') + str(
                                dataLine['PHASE']) + QCoreApplication.translate('dialog',
                                                                                ' y transformador con fase ') + str(
                                dataList['PHASE'])
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Fase de Transformadores'), aviso)


            else:
                busMTid += 1
                bus = 'BUSMV' + circuitName + str(busMTid)
                busMT_List[nodo] = {"NPHAS": dataList["NPHAS"], 'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"],
                                    "GRAFO": grafo, "VOLTAGELL": dataList["VOLTMTLL"],
                                    "VOLTAGELN": dataList["VOLTMTLN"], "PHASES": dataList["PHASE"]}
                grafo.node[nodo]["BUSMT"] = bus
                aviso = QCoreApplication.translate('dialog',
                                                   'Hay 1 transformador ') + tipo + QCoreApplication.translate('dialog',
                                                                                                               ' sin red primaria: (') + str(
                    dataList["X1"]) + ',' + str(dataList["Y1"]) + ')'
                QgsMessageLog.logMessage(aviso, QCoreApplication.translate('dialog', 'Alerta Transformadores'), Qgis.Warning) #QgsMessageLog.WARNING
        return grafo, busMT_List, busMTid, busBT_List, busBTid

    def BusAdapterLines(self, GRAFO, SOURCE, DATOS):
#        print("DATOS = ", DATOS)
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
            except:
                nodofrom = nodo1_old
                nodoto = nodo2_old
                X1Y1 = X1Y1_old
                X2Y2 = X2Y2_old
                connected = False
                EQUAL = True
        return nodofrom, nodoto, connected, EQUAL, X1Y1, X2Y2

    def IslandIdentification(self, grafoBTTotal, grafoBT, grafoACO, grafoCAR):  ######SI NO FUNCIONA: IMPORTAR LOS GRAFOS A LA FUNCION.
        # iDENTIFICA CUAL ES EL TRANSFORMADOR CONECTADO A CADA LINEA DE BAJA TENSION, ACOMETIDAS Y CARGAS.
        
        print ( os.path.dirname(os.path.abspath(__file__)) )
        
        dir_scripts = os.path.dirname(os.path.abspath(__file__))                       # La carpeta donde todas los archivos .py se encuentran
        dir_archivo = dir_scripts + "\Pruebas.txt"
              
        archivo = open(dir_archivo, 'w')
        

        connected_components = list(nx.connected_component_subgraphs(grafoBTTotal))
        
        # print( connected_components )

        for graph in connected_components:
            TrafoNode = "NULL"
            TRAFVOLTLL = "NULL"
            TRAFVOLTLN = "NULL"
            TRAFNPHASES = "NULL"
            
            for node in list(graph.nodes(data=True)):  # Identifica el nodo del transformador
                archivo.write( str( str(node) + "\n") )
                if len(node[1]) != 0 and node[1]['type'] == 'TRAF':
                    TrafoNode = node[0]
                    #print( "Nodo error: ", node )
                    TRAFVOLTLL = node[1]["LOADVOLT"]
                    TRAFVOLTLN = node[1]["LOADVOLTLN"]
                    TRAFNPHASES = node[1]["NPHAS"]
                    break
                    
            for edge in list(graph.edges(data=True)):     
                             
                datos = edge[2]
                nodo1 = edge[0]
                nodo2 = edge[1]
                
                
                if datos["type"] == "LBT":
                    #print( "LBT IslandIdentification" )
                    grafoBT[nodo1][nodo2]["TRAFNODE"] = TrafoNode
                    grafoBT[nodo1][nodo2]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoBT[nodo1][nodo2]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoBT[nodo1][nodo2]["TRAFNPHAS"] = TRAFNPHASES
                    #print( " grafoBT[nodo1][nodo2]['TRAFVOLTLL'] = ",  grafoBT[nodo1][nodo2]['TRAFVOLTLL'] )
                elif datos["type"] == "ACO":
                    #print( "ACO IslandIdentification" )
                    grafoACO[nodo1][nodo2]["TRAFNODE"] = TrafoNode
                    grafoACO[nodo1][nodo2]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoACO[nodo1][nodo2]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoACO[nodo1][nodo2]["TRAFNPHAS"] = TRAFNPHASES
                    #print( "Buscando TRAFNODE: grafoACO[nodo1][nodo2]", grafoACO[nodo1][nodo2] )

            for vertice in list( graph.nodes(data=True) ):  # Identifica el nodo del transformador
                if len(vertice[1]) != 0 and vertice[1]['type'] == 'LOAD':
                    #print( "LOAD IslandIdentification" )
                    datos = vertice[1]
                    nodo = vertice[0]
                    grafoCAR.node[nodo]["TRAFVOLTLN"] = TRAFVOLTLN
                    grafoCAR.node[nodo]["TRAFVOLTLL"] = TRAFVOLTLL
                    grafoCAR.node[nodo]["TRAFNPHAS"] = TRAFNPHASES
                    
        
        archivo.close()
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
        except:
            self.print_error()
            mensaje = "Seleccione una capa del tipo línea y repita la simulación"
            QMessageBox.critical(None,"QGIS2OpenDSS error", mensaje)
            return 0
        
    #####################################################
    
    def ChangeName(self, Dir):
        try:
            Dir = str( Dir )
            
            
            for filename in os.listdir( Dir ):
                nombre = ""
                            
                #Si no hay guión lo añade, y cambién ceros
                if filename.startswith("Curve") or filename.startswith("curve"):
                    
                    #Cambia la primera c a minúscula
                    nombre = list( filename )
                    if nombre[0] == 'C':
                        nombre[0] = "c"
                        
                    
                    #Busca el "_", si no lo encuentra añade un "_" y 00 antes de la letra
                    posicion_ = filename.find("_")
                            
                    if posicion_ == -1:
                        
                        
                        n = len( nombre )
                        inicio = nombre[ : n - 5 ]
                        letra = nombre[ n - 5 ]
                        final = str( "_00" + letra + ".txt" )
                        inicio.append( final )
                        nombre = str( "".join( inicio ) )
                        
                        
                    
                    #Cambia la extensión del archivo a .txt	
                    nombre = list( nombre )
                    n = len( nombre )
                    nombre[ n - 4 : ] = ".txt"
                        
                    nombre = "".join(nombre)
                    os.rename(filename, nombre)
        except:
            self.print_error()
			
    #####################################################
    #####################################################
    #####################################################
    #############  Ruta más larga, funciones  ###########
    #####################################################
    #####################################################
    #####################################################
    
    
    """
    Algoritmo que calcula la ruta más larga. Como este grafo representa una red eléctrica de distribución,
    entonces tiene una estructura particular, ya que sólo habrá un único camino para llegar a cualquier nodo. Por tanto,
    se utiliza el algoritmo de Dijkstra que ya está implementado en la librería Networkx.
    Básicamente lo que se hace es iterar por desde el nodo origen hasta todos los nodos para encontrar la ruta más larga.
    """	
    def RutaMasLargaReal(self, Grafo, nodo_origen, dep, err, dir_archivo):
        dep = 0
        graf = 0
        err = 0
        peso_runta_mas_larga = 0
        ruta_mas_larga = []
        archivo = open(dir_archivo, 'a')
        
        try:
            ruta_mas_larga = ""
            peso_ruta_mas_larga = 0#-float("inf")
            
            cantidad_nodos = len(Grafo)
            nodos = list(Grafo.nodes())
            #archivo.write(( "Lista de nodos:" + str(nodos) ))
                        
            #Aquí inicia el algoritmo en sí. Este primer ciclo sirve para tener como nodo origen todos los nodos existentes
            for nodo_destino in nodos: #range(1, cantidad_nodos, 1 ):
            
                
                #Este ciclo lo que va cambiando es el nodo destino.
                                
                try:
                    ruta = nx.dijkstra_path(Grafo, nodo_origen, nodo_destino)
                    peso_ruta = nx.dijkstra_path_length(Grafo, nodo_origen, nodo_destino)
                    
                    #Opciones de depuración, muestra cómo va analizando nodo por nodo
                    if dep == "1":
                        # fix_print_with_import
                        print("*********************************************************************")
                        # fix_print_with_import
                        print("Nodo origen: ", nodo_origen, " nodo destino: ", nodo_destino)
                        # fix_print_with_import
                        print("Ruta: ", ruta)
                        # fix_print_with_import
                        print("Peso ruta: ", peso_ruta)
                        
                    if peso_ruta > peso_ruta_mas_larga:
                        ruta_mas_larga = ruta
                        peso_ruta_mas_larga = peso_ruta
                    if dep == "1":
                        print("La ruta más larga, luego de analizar el nodo destino", nodo_destino, " es: ", ruta_mas_larga)
                    
                except:
                    if err == "1":
                        archivo.close()
                        self.print_error()
                    
                        
            if dep == "1":
                # fix_print_with_import
                print("\n\n********************************************************************************")
            
            
            if ruta_mas_larga == "":
                archivo.write( "Este grafo está vacío, por lo que no hay una ruta más larga.\n" )
                peso_runta_mas_larga = 0
                ruta_mas_larga = []
            else:
                archivo.write( str( "La ruta más larga desde el nodo origen " + str( nodo_origen) + " es: \n" + str(ruta_mas_larga) ) )
                           
            
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
                                    
        except:
            if err == "1":
                self.print_error()
                                        
        return ruta_mas_larga, peso_ruta_mas_larga

####################################################################################################################

    """
    Función que obtiene ciertos datos de un arista entre dos nodos dados
    No se implementa el uso de las excepciones, debido a que precisamente las excepciones son útiles en el main
    """
    def ObtenerDatosArista(self, Grafo, nodo1, nodo2, DatoBuscado):
        dato = 0
        dato = Grafo.get_edge_data(nodo1, nodo2, "weight")
        dato = dato.get("weight", 0)
        dato = dato.get(DatoBuscado, 0)
                     
        return dato
    
    
####################################################################################################################
    """
    Subestación
    Retorna tres grafos con un nuevo nodo añadido (el que representa a la subestación). La diferencia radica en la utilización que 
    se hace de cada nodo desde el main.
    También retorna el nodo en el que está ubicada la subestación, para ser utilizado como nodo origen en el cálculo de la ruta más larga.
    """
    
    def SubEst(self, layerSE_, grafoTotal, grafo_Subest, grafoDistancias, tolerancia, dir_archivo):
        subests_ = layerSE_.getFeatures() #Recibe las caracteristicas de la capa de subestación.
        indexDSS = auxiliary_functions.getAttributeIndex(self, layerSE_, "DSSName")
        
        for subest in subests_:
            point = subest.geometry().asPoint() #Lee la geometria de la linea
            node = self.CoordPointProcees(subest, tolerancia)
            
            archivo = open(dir_archivo, 'w')
            archivo.write( str( "La ubicación del nodo de la subestación, según la función que lee la capa de la subestación, es: " + str( node ) + "\n") )
            archivo.close()
            grafo_Subest.add_node(node, pos = node)
            grafoDistancias.add_node(node, pos = node)#Agrega la línea al grafo con todos los datos
            grafoTotal.add_node(node, pos = node)
            
        return grafoTotal, grafo_Subest, grafoDistancias, node
            
####################################################################################################################
    """
    Media tensión
    Se encarga de leer la capa de media tensión. Retorna el grafo de media tensión, el grafo total y el grafo de distancias.
    Estos dos últimos son utilizados desde el main como grafos que representan todo el circuito.
    El grafoTotal lo único que sirve es para hacer pruebas, y el grafo de distancias es el utilizado en el cálculo de la ruta más larga.
    """
    def MT(self, layerMT, GrafoTotal, GrafoMT_, GrafoDistancias, tolerancia, subterranea):
        
        indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT, "DSSName")
        lineasMT_ = layerMT.getFeatures()
        
        for lineaMT in lineasMT_:
            geom = lineaMT.geometry()
            line = self.MultiStringToMatrix( geom )
            if line == 0:
                return 0, 0, 0
            n = len(line) #Cantidad de vértices de la línea
            
            fase = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseCodeODSS')
            faseOrig= lineaMT['PHASEDESIG']
            cantFases = phaseOperations.renamePhase(lineaMT['PHASEDESIG']).get('phaseNumber')
            opervoltLN=lineOperations.renameVoltage(lineaMT['NOMVOLT']).get('LVCode')['LN']
            opervoltLL=lineOperations.renameVoltage(lineaMT['NOMVOLT']).get('LVCode')['LL']
            config = lineOperations.renameVoltage(lineaMT['NOMVOLT']).get('config') # wye or delta
            nodo1, nodo2 = self.CoordLineProcess(lineaMT, tolerancia)
            
            
            #Se agregan los nodos
            GrafoDistancias.add_node(nodo1, pos = nodo1)
            GrafoDistancias.add_node(nodo2, pos = nodo2)
            GrafoMT_.add_node(nodo1, pos = nodo1)
            GrafoMT_.add_node(nodo2, pos = nodo2)
            GrafoTotal.add_node(nodo1, pos = nodo1)
            GrafoTotal.add_node(nodo2, pos = nodo2)
            
            LineLength = lineaMT.geometry().length()
            
            tension = "mv"
            geometry_code = self.DeterminarGeometryCode( subterranea, lineaMT, tension )
            Impedancia = self.DeterminarImpedancia(geometry_code)
            if Impedancia == 0: #Caso en que no se haya encontrado un geometry code
                mensaje = str( "No se encontró el código " + str(geometry_code) + " en la biblioteca de cables.\n Favor agregar esta información para obtener la ruta más larga")
                QMessageBox.critical(None,"QGIS2OpenDSS error Ruta Más Larga", mensaje)
                return 0, 0, 0
            Impedancia = Impedancia*LineLength/1000
            if subterranea: #Determina si la línea es aérea o subterránea
                air_ugnd ='ugnd'
                datosLinea = {"IMPEDANCIA": Impedancia, "PHASEDESIG": faseOrig, "INDEXDSS": indexDSS, 'ID':lineaMT.id(), "LAYER": layerMT, "nodo1":nodo1, "nodo2":nodo2, "X1":line[0][0] , "Y1":line[0][1], "X2":line[n-1][0] , "Y2":line[n-1][1], 'NEUMAT':lineaMT['NEUTMAT'],'NEUSIZ':lineaMT['NEUTSIZ'],'PHAMAT':lineaMT['PHASEMAT'],'PHASIZ':lineaMT['PHASESIZ'],'NOMVOLT':lineaMT['INSULVOLT'],'PHASE':fase,'SHLEN':LineLength,'AIR_UGND':air_ugnd,'INSUL':lineaMT['INSULMAT'],'NPHAS':cantFases,'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN, "SHIELD":lineaMT["SHIELDING"] }
            else:
                air_ugnd ='air'
                datosLinea = {"IMPEDANCIA": Impedancia, "PHASEDESIG": faseOrig,"INDEXDSS": indexDSS, 'ID':lineaMT.id(),"LAYER": layerMT, "nodo1":nodo1, "nodo2":nodo2, "X1":line[0][0] , "Y1":line[0][1], "X2":line[n-1][0] , "Y2":line[n-1][1],'NEUMAT':lineaMT['NEUTMAT'],'NEUSIZ':lineaMT['NEUTSIZ'],'PHAMAT':lineaMT['PHASEMAT'],'PHASIZ':lineaMT['PHASESIZ'],'CCONF':lineaMT['LINEGEO'],'PHASE':fase,'SHLEN':LineLength,'AIR_UGND':air_ugnd,'NPHAS':cantFases,'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
            GrafoMT_.add_edge(nodo1, nodo2, weight = datosLinea)#Agrega la línea al grafo con todos los datos
            GrafoDistancias.add_edge(nodo1, nodo2, weight = LineLength)#Agrega la línea al grafo con todos los datos
            GrafoTotal.add_edge(nodo1, nodo2, weight = datosLinea)#Agrega la línea al grafo con todos los datos
            
            
        return GrafoTotal, GrafoMT_, GrafoDistancias

####################################################################################################################
    """
    Baja tensión
    Función que lee la capa de baja tensión y crea los grafos de Distancias (un grafo con sólo las distancias, el grafoTotal, y el grafo de baja tensión.
    Se llama desde el main para crear un grafo total de distancias, el grafo de baja tensión solamente, y el grafo total que lo único que sirve es para 
    realizar pruebas.
    """
    def BT(self, layer_BT, grafoTotal, grafoBT, grafoDistancias, tolerancia, subterranea):
        
        indexDSS = auxiliary_functions.getAttributeIndex(self, layer_BT, "DSSName")
        lineas = layer_BT.getFeatures() #Recibe las caracteristicas de la capa de lineas de baja tensión.
        
        for linea in lineas:
            geom = linea.geometry()
            line = self.MultiStringToMatrix(geom)
            if line == 0:
                return 0, 0, 0
            
            LineLength = linea.geometry().length()
            n = len(line) #Cantidad de vértices de la línea
            LVCode = linea['NOMVOLT']
            nodo1, nodo2 = self.CoordLineProcess(linea, tolerancia)
            
            
            #Se agregan los nodos
            grafoDistancias.add_node(nodo1, pos = nodo1)
            grafoDistancias.add_node(nodo2, pos = nodo2)
            grafoBT.add_node(nodo1, pos = nodo1)
            grafoBT.add_node(nodo2, pos = nodo2)
            grafoTotal.add_node(nodo1, pos = nodo1)
            grafoTotal.add_node(nodo2, pos = nodo2)
            
            cantFases = lineOperations.renameVoltage(linea['NOMVOLT']).get('cantFases') # 1 or 3 phases
            conns = lineOperations.renameVoltage(linea['NOMVOLT']).get('conns') # phaseCodeOpenDSS
            config = lineOperations.renameVoltage(linea['NOMVOLT']).get('config') # wye or delta
            
            tension = "lv"
            geometry_code = self.DeterminarGeometryCode(subterranea, linea, tension)
            Impedancia = self.DeterminarImpedancia(geometry_code)
            if Impedancia == 0: #Caso en que no se haya encontrado un geometry code, se retornarán ceros para saber que no se continuará
                mensaje = str( "No se encontró el código " + str(geometry_code) + " en la biblioteca de cables.\n Favor agregar esta información para obtener la ruta más larga")
                QMessageBox.critical(None,"QGIS2OpenDSS error Ruta Más Larga", mensaje)
                return 0, 0, 0
                
            Impedancia = Impedancia*LineLength/1000
            
            try:
                group=linea['LV_GROUP']
            except KeyError:
                group='N/A'
            if subterranea: #Determina si la línea es aérea o subterránea
                air_ugnd ='ugnd'
                datosLinea={"IMPEDANCIA": Impedancia, "LVCODE":LVCode , "INDEXDSS":indexDSS, "LAYER":layer_BT, "ID":linea.id(), "nodo1":nodo1, "nodo2":nodo2,'NEUMAT':linea['NEUTMAT'],'NEUSIZ':linea['NEUTSIZ'],'PHAMAT':linea['PHASEMAT'],'PHASIZ':linea['PHASESIZ'],'X1':line[0][0],'Y1':line[0][1],'X2':line[n-1][0],'Y2':line[n-1][1],'SHLEN':LineLength,'AIR_UGND':air_ugnd,'NPHAS':cantFases,'CONNS':conns,'CONF':config,'INSUL':linea['INSULMAT'],'GRUPO':group}#, 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
            
            else:
                air_ugnd ='air'
                datosLinea={"IMPEDANCIA": Impedancia, "LVCODE":LVCode , "INDEXDSS":indexDSS, "LAYER":layer_BT, "ID":linea.id(), "nodo1":nodo1, "nodo2":nodo2,'NEUMAT':linea['NEUTMAT'],'NEUSIZ':linea['NEUTSIZ'],'PHAMAT':linea['PHASEMAT'],'PHASIZ':linea['PHASESIZ'],'X1':line[0][0],'Y1':line[0][1],'X2':line[n-1][0],'Y2':line[n-1][1],'SHLEN':LineLength,'AIR_UGND':air_ugnd,'NPHAS':cantFases,'CONNS':conns,'CONF':config,'GRUPO':group, 'TIPO':linea['TYPE']}#, 'VOLTOPRLL':opervoltLL,'VOLTOPRLN':opervoltLN}
                
            grafoBT.add_edge(nodo1, nodo2, weight = datosLinea)#Agrega la línea al grafo con todos los datos
            grafoDistancias.add_edge(nodo1, nodo2, weight = LineLength)#Agrega la línea al grafo con todos los datos
            grafoTotal.add_edge(nodo1, nodo2, weight = datosLinea)#Agrega la línea al grafo con todos los datos
            
                
        return grafoTotal, grafoBT, grafoDistancias
        
     
#####################################################
    """Esta función lo que hace es determinar qué grafo tiene más nodos consecutivos en una ruta dada.
    Se le pasan como parámetros:
        -La posición a partir de la cual se quieren empezar a recorrer ambos grafos
        -Los dos grafos a recorrer
        -La ruta que se va a recorrer
        
    Retorna un 1 en caso de que el primer grafo tenga más nodos consecutivos en la ruta, un 2 si es el grafo 2, o un 0 si se llega al final del grafo y
    ambos tienen la misma cantidad de nodos consecutivos presentes.
    
        
    """
    def DeterminarGrafo(self, posicion_nodo_en_ruta, ruta, Grafo1, Grafo2):
        graf = 0
        
        n_nodos = len(ruta)
        
        #Caso en que sea el último nodo, entonces recorre de atrás para adelante
        if posicion_nodo_en_ruta == n_nodos - 1:
            for i in range(posicion_nodo_en_ruta, 1, -1):
                nodo = ruta[ i ]
                nodo_anterior = ruta[ i - 1]
                
                try:
                    Grafo1.get_node(nodo, nodo_anterior)
                    graf = 1
                except:
                    pass
                    
                
                try:
                    Grafo2.get_node(nodo, nodo_anterior)
                    
                    if graf == 0:
                        graf = 2
                    
                    elif graf == 1:
                        graf = 0
                    
                except:
                    pass
                        
            return graf
                
                
        #Caso en que no sea el último nodo        
        else:
            for i in range(posicion_nodo_en_ruta, n_nodos - 2):
                nodo = ruta[ i ]
                nodo_siguiente = ruta[ i + 1]
                                              
                try:
                    Grafo1.get_node(nodo, nodo_siguiente)
                    graf = 1
                except:
                    pass
                    
                
                try:
                    Grafo2.get_node(nodo, nodo_siguiente)
                    
                    if graf == 0:
                        graf = 2
                    
                    elif graf == 1:
                        graf = 0
                    
                except:
                    pass
                        
            return graf
        return graf

#####################################################
    """
    Esta función lee el archivo generado por OpenDSS y a partir de él determina la impedancia para un geometry code dado.
    Entonces lo que hace es recorrer el archivo Datos_LineConstants.txt hasta que encuentra la palabra buscada. Dependiendo de si es trifásico o no
    busca los componentes simétricos para determinar la impedancia, o en caso contrario determina la impedancia a partir de la primera posición de la matriz
    de resistencias y de la primera también de la de inductancias.
    Realiza la comparación con todas las palabras en minúscula, para que no existan errores.
    Además, escribe en Resultatos.txt para opciones de depuración
    """

    def DeterminarImpedancia(self, Codigo_buscado, dir_archivo = "" ):
        
       
        palabra = str("Geometry Code = " + Codigo_buscado + "\n")
        
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
            
        #archivoDepuracion.write(str( "El geometry code está en la línea " + str(resultado + 1) + "\n"))
        
                
        geometry_code = lines[resultado]
        contador = geometry_code.find("=")
        fases = geometry_code[ contador + 2 ]
        
               
        #Si es trifásico o monofásico se debe buscar la impedancia de forma distinta
        
        #Caso trifásico
        if fases == "3":
        
            valor_buscado = "-------------------Equiv Symmetrical Component --------------------\n"
            
            i = 1
            #Busca hasta 50 líneas más abajo de donde se econtró el geometru code la frase buscada (valor_buscado)
            for i in range(1, 50):
                if valor_buscado == lines[ resultado + i ]:
                    posicion_equiv = resultado + i
                    break
                    
            lin_imp = lines[ posicion_equiv + 4]
            contador = lin_imp.find("=")
            lin_imp = lin_imp[ contador + 2:]
            contador = lin_imp.find("(")
            
            impedancia = lin_imp[:contador - 1]
            contador = impedancia.find("j")
            R = Decimal(  impedancia[: contador - 2] )
            
            X = Decimal( impedancia[contador + 1 :] )
            
            impedancia = complex(R, X)
        
        #Caso monofásico o bifásico
        else:
            
            valor_buscado_r = "R MATRIX, ohms per km\n"
            valor_buscado_x = "jX MATRIX, ohms per km\n"
            i = 1
            
            #Busca hasta 50 líneas más abajo de donde se econtró el geometru code la frase buscada (valor_buscado_r y valor_buscado_x)
            for i in range(1, 50):
                if lines[ resultado + i ] == valor_buscado_r:
                    posicion_r = resultado + i
                if lines[ resultado + i ] == valor_buscado_x:
                    posicion_y = resultado + i
                    break
                    
            R = lines[ posicion_r + 1 ]
            contador = R.find(",")
            R = Decimal( R[: contador - 1] )
            
            X = lines[ posicion_y + 1 ]
            contador = X.find(",")
            X = Decimal( X[: contador - 1] )
            impedancia = complex(R, X)
        
        #archivoDepuracion.write(str("Impedancia (Ohm/km)= " + str(impedancia) + "\n"))
        #archivoDepuracion.write("*********************************************\n")
        return impedancia

#####################################################
    """    
    Esta función lo que hace es redondear un número complejo. Entonces para ello divide el número complejo en parte real e imaginaria,
    hace uso de la función "round", y vuelve a convertir el número a complejo.
    """    
    def RedondearComplejo(self, complejo, numero_decimales):
        try:
            Re = round( complejo.real, numero_decimales )
            Im = round( complejo.imag, numero_decimales )
            numero = complex( Re, Im)
        
        except:
            numero = complejo
                
        return numero

#####################################################
    """
    Función que determina el geometry code a partir de los datos que se le pasan. Lo más importante es que toma en cuenta si se trata de un geometry
    code de baja o media tensión.
    """
    def DeterminarGeometryCode(self, subterranea, linea, tension):
        GeometryCode = ""
        phase_size = linea['PHASESIZ']
        neut_size = linea['NEUTSIZ']
        phase_mat = linea['PHASEMAT']
        neut_mat = linea['NEUTMAT']
        cantFases = lineOperations.renameVoltage(linea['NOMVOLT']).get('cantFases') # 1 or 3 phases
        #3fmv4cu3/0aac_h
        #Baja tensión no necesita line_geo
        if tension == "lv":
            GeometryCode = cantFases +"f"+ tension + phase_size + phase_mat + neut_size +  neut_mat
        
        #MT
        else:
            if subterranea == False:
                line_geo = linea['LINEGEO']
                line_geo = str(line_geo)
                if  line_geo.lower() == "b" or line_geo.lower() == "m": #si tiene un m o b se debe ajustar el line_geo
                    line_geo = "h"
            else: #líneas subterráneas
                line_geo = "h"
            
            GeometryCode = cantFases +"f"+ tension + phase_size + phase_mat + neut_size +  neut_mat + "_" + line_geo
            
        GeometryCode = str(GeometryCode)
        
        return GeometryCode 
    
##############################################################################################################################        
##############################################################################################################################

    def RutaMasLargaMain(self, grafoDistancias, grafoMT_subt, grafoMT_aer, grafoBT_subt, grafoBT_aer, nodo_origen, dir_archivo):
        
        try:
            archivo = open(dir_archivo, 'w')
            archivo.write( str("") )
            archivo.close()
        
        except:
            QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Debe introducirse una ruta válida donde se mostrarán los resultados"))
            return
        
        try:
            
            tolerancia = 1
            error = 0
            dep = 0
            
            #Calculo de la ruta más larga       
            ruta, distancia = self.RutaMasLargaReal(grafoDistancias, nodo_origen, dep, error, dir_archivo)
            
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
            foldername = self.dlg.lineEdit_dirOutput.text() #Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
            archivo = open(dir_archivo, 'a')
            archivo.write( str("La cantidad de nodos en la ruta es de " + str( cantidad ) + "\n") )
                  
        
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
            
            #Se recorren todos los nodos de la ruta más larga. Se dividen por turnos, debido a que pueden haber nodos en distintas capas con idénticas ubicaciones
            while i < cantidad - 1:
                nodo_i = ruta[ i ]
                nodo_d = ruta[ i + 1 ]
                
                #Media tensión aérea
                if turno == 1 or turno == 0:
                         
                    try:
                        valor_m = self.ObtenerDatosArista( grafoMT_aer, nodo_i, nodo_d, long_)
                        distancia_mta += valor_m
                        
                        imp_ma = self.ObtenerDatosArista( grafoMT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_ma += imp_ma
                        
                        turno = 1
                        contador = 0
                        
                        #archivo.write("Media tensión \n")
                        #archivo.write( str( "Nodo origen = " + str( nodo_i ) + "\n" ) )
                        #archivo.write( str( "Nodo destino = " + str( nodo_d ) + "\n" ) )
                        
                                            
                        if valor_m != 0:
                            contador_dist_ma += 1
                                             
                        if imp_ma != 0:
                            contador_imp_ma += 1
                        
                            
                    except:
                        #archivo.write("Excepción en media tensión!\n")
                        turno = 0
                        
                
                #Media tensión subterránea
                if turno == 2 or turno == 0:
                         
                    try:
                        valor_m = self.ObtenerDatosArista( grafoMT_subt, nodo_i, nodo_d, long_)
                        distancia_mts += valor_m
                        
                        imp_ma = self.ObtenerDatosArista( grafoMT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_ms += imp_ma
                        
                        turno = 2
                        contador = 0
                        
                        #archivo.write("Media tensión \n")
                        #archivo.write( str( "Nodo origen = " + str( nodo_i ) + "\n" ) )
                        #archivo.write( str( "Nodo destino = " + str( nodo_d ) + "\n" ) )
                                            
                        if valor_m != 0:
                            contador_dist_ms += 1
                            
                        if imp_ma != 0:
                            contador_imp_ms += 1
                    except:
                        #archivo.write("Excepción en media tensión!\n")
                        turno = 0
                        
                    
                #Baja tensión aérea
                if turno == 3 or turno == 0:
                
                    try:
                        valor_b = self.ObtenerDatosArista( grafoBT_aer, nodo_i, nodo_d, long_)
                        distancia_bta += valor_b
                        imp_la = self.ObtenerDatosArista( grafoBT_aer, nodo_i, nodo_d, impedancia)
                        impedancia_la += imp_la
                        
                        turno = 3
                        contador = 0
                        
                        #archivo.write("Baja tensión \n")
                        #archivo.write( str( "Nodo origen = " + str( nodo_i ) + "\n" ) )
                        #archivo.write( str( "Nodo destino = " + str( nodo_d ) + "\n" ) )
                                     
                                          
                        if valor_b != 0:
                            contador_dist_la += 1
                                              
                        if imp_la != 0:
                            contador_imp_la += 1
                            
                    except:
                        turno = 0
                        
                
                #Baja tensión subterránea        
                if turno == 4 or turno == 0:
                
                    try:
                        valor_b = self.ObtenerDatosArista( grafoBT_subt, nodo_i, nodo_d, long_)
                        distancia_bts += valor_b
                        imp_la = self.ObtenerDatosArista( grafoBT_subt, nodo_i, nodo_d, impedancia)
                        impedancia_ls += imp_la
                        
                        turno = 4
                        contador = 0
                        
                        #archivo.write("Baja tensión \n")
                        #archivo.write( str( "Nodo origen = " + str( nodo_i ) + "\n" ) )
                        #archivo.write( str( "Nodo destino = " + str( nodo_d ) + "\n" ) )
                                     
                                          
                        if valor_b != 0:
                            contador_dist_ls += 1
                        if imp_la != 0:
                            contador_imp_ls += 1
                    except:
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
            
            archivo.write( str( "La distancia total es de: " + str( round( distancia_total/1000, 3) ) + " km, con un total de " + str( contador_dist_t ) +" nodos\n" ) )
            archivo.write( str( "La impedancia total es de: " + str( self.RedondearComplejo(impedancia_total, 4) ) + " Ohms, con un total de " + str( contador_imp_t ) + " nodos\n\n\n" ) )
            
            archivo.write( str( "La distancia en media tensión es de: " + str( round(distancia_mt/1000, 3) ) + " km, con un total de " + str( contador_dist_m ) + " nodos\n" ) )
            archivo.write( str( "La distancia en baja tensión es de: " + str( round(distancia_bt, 3) ) + " m, con un total de " + str( contador_dist_l ) + " nodos\n" ) )
            
            archivo.write( str( "La impedancia en media tensión es de: " + str( self.RedondearComplejo(impedancia_mt, 4) ) + " Ohms, con un total de " + str( contador_imp_m )+ " nodos\n" ) )
            archivo.write( str( "La impedancia en baja tensión es de: " + str( self.RedondearComplejo(impedancia_lt, 4) ) + " Ohms, con un total de " + str( contador_imp_l ) + " nodos\n\n\n" ) )
            
            archivo.write( str( "La distancia en media tensión aérea es de: " + str( round(distancia_mta/1000, 3) ) + " km\n" ) )
            archivo.write( str( "La distancia en media tensión subterránea es de: " + str( round(distancia_mts, 3) ) + " m\n" ) )
            
            archivo.write( str( "La impedancia en media tensión aérea es de: " + str( self.RedondearComplejo(impedancia_ma, 4) ) + " Ohms\n" ) )
            archivo.write( str( "La impedancia en media tensión subterránea es de: " + str( self.RedondearComplejo(impedancia_ms, 4) ) + " Ohms\n\n" ) )
            
            archivo.write( str( "La distancia en baja tensión aérea es de: " + str( round(distancia_bta, 3) ) + " m\n") )
            archivo.write( str( "La distancia en baja tensión subterránea es de: " + str( round(distancia_bts, 3) ) + " m\n" ) )
            
            archivo.write( str( "La impedancia en baja tensión aérea es de: " + str( self.RedondearComplejo(impedancia_la, 4) ) + " Ohms\n" ) )
            archivo.write( str( "La impedancia en baja tensión subterránea es de: " + str( self.RedondearComplejo(impedancia_ls, 4) ) + " Ohms\n" ) )
                  
              
            #archivo.write( str( "La ruta más larga es:\n" + str( ruta ) + "\n") )
                           
            tiempo_f = time.time()
            tiempo_total = ( tiempo_f - self.time )/60
            
            archivo.write( str( "El tiempo de ejecución es de " + str( tiempo_total ) + " minutos\n" ) )
            
            """
            
            nodos_MT= list(GrafoMT_.nodes()
            nodos_BT= list(GrafoBT.nodes())
            
            archivo.write( str( "Nodos media tensión: \n" + str( nodos_MT ) + "\n") )
            archivo.write( str( "Nodos baja tensión: \n" + str( nodos_BT ) + "\n") )
            
            """
            
            archivo.close()
            aviso = str( "Se exportaron los resultados de la ruta más larga en el siguiente archivo:\n" + dir_archivo )
            QMessageBox.information(None, QCoreApplication.translate('dialog', "Ruta más larga"), aviso)
            print("Final ruta más larga")
            return 1
        
        except:
            self.print_error()
            return 0
    #####################################################
    #####################################################
    #####################################################
    #######  Final de funciones de ruta más larga  ######
    #####################################################
    #####################################################
    #####################################################
    
    
    #Función que se encarga de imprimir los errores que han ocurrido
    def print_error(self):
        exc_info = sys.exc_info()
        print("\nError: ", exc_info )
        print("*************************  Información detallada del error ********************")
        for tb in traceback.format_tb(sys.exc_info()[2]):
            print(tb)

    """
    Función que se encarga de instalar una librería en la versión de python de QGIS
    -Parámetros de entrada:
    *librarie_name (string): nombre de la librería a instalar (tal como se le debe pasar a pip)
    
    -Valores retornados:
    *1 en caso de finalizar exitosamente
    *0 en caso de ocurrir algún error
    """
    
    def install_libraries(self, librarie_name):
        try:
            import shutil
            import subprocess
            from pathlib import Path
            #Se obtiene el path de QGIS
            directorio = str( os.path )
            fin_dir = directorio.find("\\apps")
            inic_dir = directorio.find("C:\\")
            path = directorio[ inic_dir : fin_dir - 1]
            #Se obtiene version de Python en QGIS
            info = sys.version_info
            verspy1 = str( info[0] )
            verspy2 = str( info[1] )
            carp_python = str( verspy1 + verspy2 )
            carp_python = str( "Python" + carp_python )
            
            #Se copia los archivos
            dir_origen = path + "\\bin\\"
            name_file_or = "python" + verspy1 + ".dll"
            archivo_origen = str( dir_origen + name_file_or )
            dir_destino = str( path + "\\apps\\" + carp_python)        
            name_dest = dir_destino +  name_file_or
            
            my_file = Path( name_dest )
            print("name_dest = ", my_file.exists() )
            if my_file.exists() == False:            
                #Copia python3.dll
                shutil.copy( archivo_origen, dir_destino )        
            
            #Copia python37.dll
            name_file_or = "python" + verspy1 + verspy2 + ".dll"
            archivo_origen = dir_origen + name_file_or
            name_dest = dir_destino +  name_file_or
            
            my_file = Path( name_dest )
            print("name_dest = ", my_file.exists() )
            
            if my_file.exists() == False:            
                #Copia python37.dll
                shutil.copy( archivo_origen, dir_destino )
            
            #Instalación de librerías
            
            #Actualización de pip
            subprocess.call('python.exe -m pip install –upgrade pip', cwd = dir_destino, shell = True )
            
            #Instalación libreria
            sentencia = str( "python.exe -m pip install " + librarie_name )
            subprocess.call(sentencia, cwd = dir_destino, shell = True )
            
            print("Instalación de librería ", librarie_name, " finalizada.")
            return 1
        
        except:
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
    def AssignLoadshapes( self, grafoCAR, folder_profile, foldername, circuitName, Buses = False ) :
        errorLoadShape = False
        try:
            # De las curvas disponibles, toma las existentes y las pone en un vector
            curv_disp = {"residential": [], "commercial": [], "industrial": []}
            for sector in ["residential", "commercial", "industrial"]:
                try:
                    os.chdir(folder_profile + "\\" + sector)  # Se ubica en la carpeta
                    directorio = str( folder_profile + "\\" + sector )
                    self.ChangeName( directorio )
                    
                    for file in glob.glob("*.txt") or glob.glob("*.dss"):
                        if file[:5].lower() == "curve":
                            energ = file[:(len(file) - 5)]
                            energ = energ.replace("curve", "")
                            energ = energ.replace("_", ".")
                            curv_disp[sector].append(float(energ))
                except FileNotFoundError: #no necesariamente existen todas las carpetas residencial, comercial e industrial
                    continue
                except:
                    self.print_error()
            graphNodes = list( grafoCAR.nodes(data=True) )
            self.progress.progressBar.setValue(53)
            # print grafoCAR.number_of_nodes()
            i = 1
            for NODE in graphNodes:
                dataList = NODE[1]
                
                nodo = NODE[0]
                if Buses == True:
                    class_ = dataList['DICT LOAD BT']["class"]
                else:
                    class_ = dataList["class"]
                class_t = str( class_ ).lower()
                
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
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Error en la creación de los loadshapes\nVerifique que se asigne una clase válida")                    
                    return grafoCAR, errorLoadShape
                
                if Buses == True:
                    kwh = dataList['DICT LOAD BT']["kWh"]
                else:
                    kwh = dataList["kWh"]
                
                energLoad = format(float( kwh ), '.2f')
                aux = list(abs(np.array(curv_disp[sector]) - np.array(len(curv_disp[sector]) * [ float(energLoad)])))  # Diferencia en kwh de la carga y las curvas existentes
                
                if len(aux) == 0: #caso en que no se haya encontrado el dato correspondiente a las curvas de cierto sector 
                    print( "Aux = 0, sector = ", sector, " energLoad = ", energLoad )
                    pass #revisar
                
                error = min(aux) / float(energLoad)
                enerLoadStr = str(energLoad)
                                                                        
                enerLoadStr = enerLoadStr.replace(".", "_")
                ClosCurvEnerg = float(curv_disp[sector][aux.index(min(aux))])
                                            
                ClosCurvEnergStr = str(format(ClosCurvEnerg, '.2f'))
                ClosCurvEnergStr = ClosCurvEnergStr.replace(".", "_")
                                            
                if error <= 0.02:  # Significa que la diferencia de energía con la curva más cercana es menor al 2%
                    if Buses == True:
                        grafoCAR.node[nodo]['DICT LOAD BT']['CURVASIG'] = 'daily=curve' + ClosCurvEnergStr + final_letter_loadshape
                        
                    else:
                        grafoCAR.node[nodo]['CURVASIG'] = 'daily=curve' + ClosCurvEnergStr + final_letter_loadshape

                else:  # A las curvas asignadas, si no existen, crea una curva artificial adaptada del valor más cercano existentes
                    if Buses == True:
                        grafoCAR.node[nodo]['DICT LOAD BT']['CURVASIG'] = 'daily=curve' + enerLoadStr + final_letter_loadshape
                    else:
                        grafoCAR.node[nodo]['CURVASIG'] = 'daily=curve' + enerLoadStr + final_letter_loadshape
                    os.chdir(folder_profile + "\\" + sector)
                    file_name = 'curve' + str(ClosCurvEnergStr) + final_letter_loadshape + '.txt'
                    
                    file_data = []
                    with open(file_name) as f:
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
                        k = float(ClosCurvEnerg) / (aux * 0.25 * 30)

                    file = open('curve' + enerLoadStr + final_letter_loadshape + '.txt', "w")  # _new
                    for j in range(96):
                        file.write(str(format(k * file_data_parse[j], '.2f')) + ' \n')
                    file.close()
                    curv_disp[sector].append(float(energLoad))

            # Create a file with all the loadshapes
            filename = circuitName + '_Loadshapes.dss'
            output_shpdss = open(foldername + '/' + filename, 'w')
            output_shpdss.write('!' + folder_profile + '\n')
            for sector in ["residential", "commercial", "industrial"]:
                try:
                    os.chdir(folder_profile + "\\" + sector)  # Se ubica en la carpeta
                    for file in glob.glob("*.txt"):
                        output_shpdss.write('New Loadshape.' + file.replace('.txt',
                                                                            '') + ' npts=96 minterval=15 mult=(file=' + folder_profile + '\\' + sector + '\\' + file + ') useactual=no \n')
                
                except FileNotFoundError: #no necesariamente existen todas las carpetas residencial, comercial e industrial
                    continue
                except:
                    self.print_error()
            return grafoCAR, errorLoadShape
        except:
            self.print_error()
            errorLoadShape = True
            if Buses == True:
                QMessageBox.critical(None, "QGIS2OpenDSS Error", "Error en la creación de los loadshapes de buses\nVerifique que existen los archivos de curvas en la carpeta indicada.\n*) NOTA: no se creará el archivo de cargas ni el de LoadShapes.")                    
            else:
                QMessageBox.critical(None, "QGIS2OpenDSS Error", "Error en la creación de los loadshapes de BT/MT\nVerifique que existen los archivos de curvas en la carpeta indicada.\n*) NOTA: no se creará el archivo de cargas ni el de LoadShapes.")                    
            return grafoCAR, errorLoadShape
    
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
    
    def csv_to_dict( self, csv_name ):
        import pandas as pd #se debe dejar aquí debido a que no es una librería que QGIS traiga por defecto
        dict_buses = {}
        
        try:
            dataframe = pd.read_csv( csv_name, index_col = 0 ) #se convierte el csv a un dataframe con la primera columna como índice
            
            for index, row in dataframe.iterrows():
                row_dict = row.to_dict() #se convierte la columna del dataframe a diccionario
                
                #Asignación de valores al diccionario
                try:
                    i = len( dict_buses[index] )
                    dict_buses[index][i] = row_dict
                except:
                    dict_buses[index] = {}
                    dict_buses[index][0] = row_dict
            return dict_buses
        
        
        except:
            self.print_error()
            return 0
        
    
    #========================================================================================================================================
    #========================================================================================================================================
    #========================================================================================================================================
    def run(self):
        #Instalación librerías
        try:
            from sklearn.mixture import GaussianMixture
        except:
            self.install_libraries("scikit-learn") #Instalación de librerías extra requeridas
            from sklearn.mixture import GaussianMixture
        
        """

        #Instalación librerías
        try:
            import dss
        except:
            self.install_libraries("dss_python") #Instalación de librerías extra requeridas
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
        # Run the dialog event loop
        result = self.dlg.exec_()
        circuitName = self.dlg.lineEdit_nameCircuit.text().upper()  # Recibe el nombre del circuito indicado en la ventana de dialogo.
        foldername = self.dlg.lineEdit_dirOutput.text()  # Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
        self.foldername = foldername     
        folder_profile = self.dlg.lineEdit_AC.text()
        self.folder_profile = folder_profile
        cargas1 = self.dlg.comboBox_CA1.currentIndex()
        cargas2 = self.dlg.comboBox_CA2.currentIndex()
        cargas3 = self.dlg.comboBox_CA3.currentIndex()
        cargas = cargas1 + cargas2 + cargas3

        MTL1 = self.dlg.comboBox_LMT1.currentIndex()
        MTL2 = self.dlg.comboBox_LMT2.currentIndex()
        MTL3 = self.dlg.comboBox_LMT3.currentIndex()
        MTL = MTL1 + MTL2 + MTL3

        BTL1 = self.dlg.comboBox_LBT1.currentIndex() + self.dlg.comboBox_ACO1.currentIndex()
        BTL2 = self.dlg.comboBox_LBT2.currentIndex() + self.dlg.comboBox_ACO2.currentIndex()
        BTL3 = self.dlg.comboBox_LBT3.currentIndex() + self.dlg.comboBox_ACO3.currentIndex()
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
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', u"La operación no se pudo completar") + "\n" + QCoreApplication.translate('dialog', "Debe crear un proyecto de QGIS"))
            return
        elif result and (
                not circuitName or not foldername):  # Se asegura de que el complemento se ejecute solo si se tiene completa la informacin necesaria
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar") + "\n" + QCoreApplication.translate(
                'dialog', "Debe indicar el nombre del circuito y la carpeta de destino"))
            return
        elif result and (self.dlg.comboBox_SE.currentIndex() == 0) and (MTL != 0):
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar") + "\n" + QCoreApplication.translate(
                'dialog', u"Debe seleccionar la capa de la subestación"))
            return

        elif result and (self.dlg.comboBox_SE.currentIndex() == 0) and TX == 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar") + "\n" + QCoreApplication.translate(
                'dialog', u"Al menos debe seleccionar la capa de la subestación o transformadores"))
            return
            
        
        elif result and (BTL != 0) and TX == 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar") + "\n" + QCoreApplication.translate(
                'dialog', "Debe seleccionar la capa de la transformadores"))
            return
        elif result and not folder_profile and cargas > 0:
            QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        u"La operación no se pudo completar") + "\n" + QCoreApplication.translate(
                'dialog', "Para modelar la carga debe ingresar la carpeta de perfiles"))
            return
        elif result and circuitName and foldername:
            
            #Se crea el directorio de errores si no existe
            dir_logs = foldername + "/error"
            if not os.path.exists( dir_logs ):
                os.makedirs( dir_logs )
            else:
                shutil.rmtree( dir_logs, ignore_errors=True )
            
            dir_archivo_log = str( dir_logs + "/error_EV" + circuitName + ".log" )
            #Borra archivo de errores
            archivo_errlog = open(dir_archivo_log, 'w')
            archivo_errlog.write( str("") )
            archivo_errlog.close()
            
            
            
            archivo_errlog = open(dir_archivo_log, 'a')
            self.archivo_errlog = archivo_errlog
            
            if self.dlg.check_rutamaslarga.isChecked():
                ###############################################################################################################
                ###############################################################################################################
                #########################################  RUTA MÁS LARGA  #####################################################
                ###############################################################################################################
                ###############################################################################################################
                
                #Se crea el archivo con los datos de impedancias
                LlamarOpenDSS()
                #Esto es sólo para limpiar el archivo donde se va a escribir
                try:
                    foldername = self.dlg.lineEdit_dirOutput.text() #Almacena el nombre de la carpeta de destino seleccionada en la ventana de diálogo.
                    name_file = str( "/ResultadosRutaMasLarga" + circuitName + ".txt" )
                    dir_archivo = foldername + name_file
                    print(dir_archivo)
                    archivo = open(dir_archivo, 'w')
                    archivo.write( str("") )
                    archivo.close()
                
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Debe introducirse una ruta válida donde se mostrarán los resultados"))
                    return
                        
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
                    NombreSE = self.dlg.comboBox_SE.currentText() #Recibe la capa de subestación seleccionada en la lista desplegable 
                    layerSE = QgsProject.instance().mapLayersByName(NombreSE)[0] #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    grafoTotal, grafo_Subest, grafoDistancias, nodo_origen = self.SubEst(layerSE, grafoTotal, grafo_Subest, grafoDistancias, tolerancia, dir_archivo)
                
                except:
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor inserte una capa para la subestación"))
                    return
                
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
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT1, grafoTotal, grafoMT_subt, grafoDistancias, tolerancia, subterraneaMT1)
                            
                        else:
                            subterraneaMT1 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT1, grafoTotal, grafoMT_aer, grafoDistancias, tolerancia, subterraneaMT1)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                            
                            
                    else:
                        QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Debe seleccionar al menos una capa de media tensión"))
                        return
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de media tensión tenga todos los datos necesarios"))
                    return
                
                
                #Lectura media tensión 2
                      
                try:
                    if len(NombreMT2)!=0:
                        layerMT2 = QgsProject.instance().mapLayersByName(NombreMT2)[0] #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                        if self.dlg.checkBox_LMT2.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaMT2 = True
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT2, grafoTotal, grafoMT_subt, grafoDistancias, tolerancia, subterraneaMT2)
                            
                        else:
                            subterraneaMT2 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT2, grafoTotal, grafoMT_aer, grafoDistancias, tolerancia, subterraneaMT2)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                            
                    
                
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de media tensión tenga todos los datos necesarios"))
                    return
                            
                
                #Lectura media tensión 3
                try:
                    if len(NombreMT3)!=0:
                        layerMT3 = QgsProject.instance().mapLayersByName(NombreMT3)[0] #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                        
                        if self.dlg.checkBox_LMT3.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaMT3 = True
                            grafoTotal, grafoMT_subt, grafoDistancias = self.MT(layerMT3, grafoTotal, grafoMT_subt, grafoDistancias, tolerancia, subterraneaMT3)
                        else:
                            subterraneaMT3 = False
                            grafoTotal, grafoMT_aer, grafoDistancias = self.MT(layerMT3, grafoTotal, grafoMT_aer, grafoDistancias, tolerancia, subterraneaMT3)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                                 
                
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de media tensión tenga todos los datos necesarios"))
                    return
                
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
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT1, grafoTotal, grafoBT_subt, grafoDistancias, tolerancia, subterraneaBT1)
                            
                        else:
                            subterraneaBT1 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT1, grafoTotal, grafoBT_aer, grafoDistancias, tolerancia, subterraneaBT1)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                        
                    else:
                        QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Debe seleccionar al menos una capa de baja tensión"))
                        
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de baja tensión tenga todos los datos necesarios"))
                    return
                    
                    
                #Baja tensión 2
                try:
                    if len(NombreBT2)!=0:
                        layerBT2 = QgsProject.instance().mapLayersByName(NombreBT2)[0]
                        indexDSS=auxiliary_functions.getAttributeIndex(self, layerBT2, "DSSName")
                        
                        if self.dlg.checkBox_LBT2.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaBT2 = True
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT2, grafoTotal, grafoBT_subt, grafoDistancias, tolerancia, subterraneaBT2)
                        else:
                            subterraneaBT2 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT2, grafoTotal, grafoBT_aer, grafoDistancias, tolerancia, subterraneaBT2)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de baja tensión tenga todos los datos necesarios"))
                    return
                
                #Baja tensión 3
                try:
                    if len(NombreBT3)!=0:
                        layerBT3 = QgsProject.instance().mapLayersByName(NombreBT3)[0]
                        indexDSS=auxiliary_functions.getAttributeIndex(self, layerBT3, "DSSName")
                        
                        if self.dlg.checkBox_LBT3.isChecked(): #Determina si la línea es aérea o subterránea
                            subterraneaBT3 = True
                            grafoTotal, grafoBT_subt, grafoDistancias = self.BT(layerBT3, grafoTotal, grafoBT_subt, grafoDistancias, tolerancia, subterraneaBT3)
                        else:
                            subterraneaBT3 = False
                            grafoTotal, grafoBT_aer, grafoDistancias = self.BT(layerBT3, grafoTotal, grafoBT_aer, grafoDistancias, tolerancia, subterraneaBT3)
                        
                        #Error si no se leyó un geometry code inexistente
                        if grafoTotal == 0: 
                            return
                except:
                    self.print_error()
                    QMessageBox.critical(None,"QGIS2OpenDSS Error",QCoreApplication.translate('dialog', u"Favor revise que su tabla de atributos de baja tensión tenga todos los datos necesarios"))
                    return
                    
                self.RutaMasLargaMain(grafoDistancias, grafoMT_subt, grafoMT_aer, grafoBT_subt, grafoBT_aer, nodo_origen, dir_archivo)
                ###############################################################################################################
                ###############################################################################################################
                #########################################  FIN RUTA MÁS LARGA  #####################################################
                ###############################################################################################################
                ###############################################################################################################
                    
            
            
            self.progress.show()
            self.progress.progressBar.setRange(0, 100)
            self.progress.progressBar.setValue(0)

            startpluginTime = timeit.default_timer()  # starts runTime counter

            Error = False  # Se inicializa esta variable. Cambia a True si ocurre un error crítico.
            # Time meters init
            startTime = time.time()
            toler = 0.1  # tolerancia para los grafos en metros
            grafoBTTotal = nx.Graph()
            """1-Se inicia contador de barras MT y BT"""
            busnumMT = 1  # inicializa contador de barras de MT
            busnumBT = 1  # inicializa contador de barras de BT
            # 2.1-Crea lista con las coordenadas de Inicio, Final y Fase de las lineas de media tension.
            
            
            selectedLayerMT1 = self.dlg.comboBox_LMT1.currentText()  # Índice de layer_list con lineas MT seleccionada en la lista desplegable
            selectedLayerMT2 = self.dlg.comboBox_LMT2.currentText()  # Índice de layer_list con lineas MT seleccionada en la lista desplegable
            selectedLayerMT3 = self.dlg.comboBox_LMT3.currentText()  # Índice de layer_list con lineas MT seleccionada en la lista desplegable
            
            
            busMT_List = {}  # Lista de Bus MT
            busMTid = 1
            busBT_List = {}  # Lista de Bus BT

            ###Crea lista con datos de subestación
            
            #selectedLayerSE = 'subest_Santos'
            
            selectedLayerSE = self.dlg.comboBox_SE.currentText()  # Recibe la capa de subestación seleccionada en la lista desplegable
            
            
            
            datosSE = []
            grafoSubt = nx.Graph()
            grafoMT = nx.Graph()
            startTimeSub = time.time()
            SEactive = False
            try:  ## Lectura de datos de subestación
                if len(selectedLayerSE) != 0:
                    layerSE = QgsProject.instance().mapLayersByName(selectedLayerSE)[0] #Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    
                    if self.dlg.Model.isChecked():
                        mode = "MODEL"
                    elif self.dlg.checkBox_AutoTraf.isChecked():
                        mode = "AUTO"
                    elif self.dlg.noModel.isChecked():
                        mode = "NOMODEL"

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
                                datos = {"INDEXDSS": indexDSS, "LAYER": layerSE, "TAP": setTap, "ID": subest.id(),
                                         'X1': point[0], 'Y1': point[1], 'VOLTAJEALT': subest['HIGHVOLT'],
                                         'VOLTAJEMED': subest['MEDVOLT'], 'VOLTAJEMEDLN': VOLTAJEMEDLN,
                                         'VOLTAJEBAJ': subest['LOWVOLT'], 'CONEXIONAL': CONEXIONAL,
                                         'CONEXIONME': CONEXIONME, 'CONEXIONBA': CONEXIONBA,
                                         'KVA_ALTA': subest['KVAHIGH'], 'KVA_MEDIA': subest['KVAMED'],
                                         'KVA_BAJA': subest['KVALOW'], 'PHASEDESIG': '.1.2.3',
                                         'WINDINGS': subest['WINDINGS'], 'TAPS': TAPS, 'MINTAP': MINTAP,
                                         'MAXTAP': MAXTAP, 'XHL': subest['XHL'], 'XHT': subest['XHT'],
                                         'XLT': subest['XLT']}
                            else:
                                datos = {"INDEXDSS": indexDSS, "LAYER": layerSE, "TAP": setTap, "ID": subest.id(),
                                         'X1': point[0], 'Y1': point[1], 'VOLTAJEALT': subest['HIGHVOLT'],
                                         'VOLTAJEMED': subest['MEDVOLT'], 'VOLTAJEMEDLN': VOLTAJEMEDLN,
                                         'VOLTAJEBAJ': '', 'CONEXIONAL': CONEXIONAL, 'CONEXIONME': CONEXIONME,
                                         'CONEXIONBA': '', 'KVA_ALTA': subest['KVAHIGH'], 'KVA_MEDIA': subest['KVAMED'],
                                         'KVA_BAJA': '', 'PHASEDESIG': '.1.2.3', 'WINDINGS': subest['WINDINGS'],
                                         'TAPS': TAPS, 'MINTAP': MINTAP, 'MAXTAP': MAXTAP, 'XHL': subest['XHL'],
                                         'XHT': subest['XHT'], 'XLT': subest['XLT']}
                            datosSE.append(datos)
                        if mode == "AUTO":
                            if int(subest['TAPS']) > 0:
                                TAPS = subest['TAPS']
                                TAP_MAX_MIN = str(subest['TAPMAX/MI'])
                                TAP_MAX_MIN = TAP_MAX_MIN.split('/')
                                MINTAP = TAP_MAX_MIN[1]
                                MAXTAP = TAP_MAX_MIN[0]
                            setTap = str(subest['TAPSETTING'])
                            datos = {"INDEXDSS": indexDSS, "LAYER": layerSE, "TAP": setTap, "ID": subest.id(),
                                     'X1': point[0], 'Y1': point[1], 'VOLTAJEALT': subest['HIGHVOLT'],
                                     "VOLTAJEMEDLN": float( subest['MEDVOLT'] ) / sqrt(3), 'VOLTAJEMED': subest['MEDVOLT'],
                                     'KVA_ALTA': subest['KVAHIGH'], 'KVA_MEDIA': subest['KVAMED'], 'TAPS': TAPS,
                                     'MINTAP': MINTAP, 'MAXTAP': MAXTAP, 'XHL': subest['XHL']}

                        if mode == "NOMODEL":
                            datos = {"INDEXDSS": indexDSS, "LAYER": layerSE, "ID": subest.id(), 'X1': point[0],
                                     'Y1': point[1], 'VOLTAJEMED': subest['MEDVOLT'], 'VOLTAJEMEDLN': VOLTAJEMEDLN }
                        SubsNode = node
                        grafoSubt.add_node(node)
                        grafoSubt.nodes[node].update( datos )
                        
                        grafoMT.add_node(node)
                        grafoMT.nodes[node].update( datos )

                    if grafoSubt.number_of_nodes() == 0:
                        SEactive = False
                    else:
                        SEactive = True
                        for NODE in list( grafoSubt.nodes(data=True) ):
                            dataList = NODE[1]
                            
                            nodo = NODE[0]
                            bus = 'BUSMV' + circuitName + str(busMTid)
                            
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"],
                                                "GRAFO": grafoSubt, "VOLTAGELN": dataList["VOLTAJEMEDLN"], "NPHAS": "3"}
                            grafoSubt.node[nodo]["BUSMT"] = bus
                            busMTid += 1

            except KeyError:
                self.print_error()
                QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                            "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate(
                    'dialog', "de la tabla de atributos de subestación"))
                SEactive = False
                Error = True

            endTimeSub = time.time()

            try:  ###Lectura y Conectividad de media tensión

                datosLMT = []
                if len(selectedLayerMT1) != 0:
                    
                    layerMT1 = QgsProject.instance().mapLayersByName(selectedLayerMT1)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    if self.dlg.checkBox_LMT1.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT1, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT1, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    if grafoMT == 0:
                        self.progress.close()
                        return

                if len(selectedLayerMT2) != 0:
                    layerMT2 = QgsProject.instance().mapLayersByName(selectedLayerMT2)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    if self.dlg.checkBox_LMT2.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT2, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT2, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    if grafoMT == 0:
                        self.progress.close()
                        return

                if len(selectedLayerMT3) != 0:
                    layerMT3 = QgsProject.instance().mapLayersByName(selectedLayerMT3)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    if self.dlg.checkBox_LMT3.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerMT3, "DSSName")
                    grafoMT, datosLMT = self.ReaderDataLMT(layerMT3, grafoMT, datosLMT, toler, subterranea, indexDSS)
                    if grafoMT == 0:
                        self.progress.close()
                        return
                aviso_bus = False
                aviso_fases = False
                if grafoMT.number_of_edges() == 0:

                    LMTactive = False
                else:  # Se identifica la conectividad de las líneas de Media Tensión.
                    startTimeMT = time.time()
                    LMTactive = True
                    nodesMT = grafoMT.nodes()  # guarda todos los nodos del grafoMT en una lista
                    ################## Asignación de Bus a líneas MT
                    
                    dir_archivo_MTlog = str( dir_logs + "/error_MT" + circuitName + ".log" )
                    #Borra archivo de errores
                    archivo_errlogMT = open(dir_archivo_MTlog, 'w')
                    
            
                    
                    for node in nodesMT:  # node es el nodo en nombramiento
                        i = 1
                        #print( " --------------------------------------------------------------------- ")
                        #print( grafoMT[ node ] )
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
                                    aviso = "Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (" + str(busMT_List[node]['X']) + ", " + str(busMT_List[node]['Y']) + ")"
                                    aviso_bus = True
                                    archivo_errlogMT.write( aviso )
                                    
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
                                                                       u'Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (') + str(
                                        busMT_List[node]['X']) + ', ' + str(busMT_List[node]['Y']) + ')'
                                        
                                    aviso_bus = True
                                    archivo_errlogMT.write( aviso )
                                    
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
                                                                       u'Existe una línea de MT con bus1 igual a bus2 dada su cercanía en (') + str(
                                        busMT_List[node]['X']) + ', ' + str(busMT_List[node]['Y']) + ')'
                                    aviso_bus = True
                                    archivo_errlogMT.write( aviso )
                                    
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
                                    if (node == grafoMT[node][secondNode]['nodo1']) and ( node == grafoMT[node][otherNodes[j]]['nodo2']):
                                        NodoLineaCerc = otherNodes[j]
                                        NodoLinealejan = secondNode
                                        if (phaseOperations.linePhaseMT(grafoMT[node][NodoLineaCerc]['PHASE'],
                                                                        grafoMT[node][NodoLinealejan]['PHASE']) == 0):
                                            aviso = QCoreApplication.translate('dialog',
                                                                               u'Conexión de fases distintas en (') + str(
                                                busMT_List[node]['X']) + ',' + str(
                                                busMT_List[node]['Y']) + QCoreApplication.translate('dialog',
                                                                                                    u'). Línea MT ') + str(
                                                grafoMT[node][secondNode]['NPHAS']) + QCoreApplication.translate(
                                                'dialog', 'F con fase ') + str(
                                                grafoMT[node][secondNode]['PHASE']) + QCoreApplication.translate(
                                                'dialog', ' y MT ') + str(
                                                grafoMT[node][otherNodes[j]]['NPHAS']) + QCoreApplication.translate(
                                                'dialog', 'F con fase ') + str(grafoMT[node][otherNodes[j]]['PHASE'])
                                            aviso_fases = True
                                            archivo_errlogMT.write( aviso )
                                            
                                            
                                    elif (node == grafoMT[node][secondNode]['nodo2']) and (node == grafoMT[node][otherNodes[j]]['nodo1']):
                                        NodoLineaCerc = secondNode
                                        NodoLinealejan = otherNodes[j]
                                        if (phaseOperations.linePhaseMT(grafoMT[node][NodoLineaCerc]['PHASE'],
                                                                        grafoMT[node][NodoLinealejan]['PHASE']) == 0):
                                            aviso = QCoreApplication.translate('dialog',
                                                                               u'Conexión de fases distintas en (') + str(
                                                busMT_List[node]['X']) + ',' + str(
                                                busMT_List[node]['Y']) + QCoreApplication.translate('dialog',
                                                                                                    u'). Línea MT ') + str(
                                                grafoMT[node][secondNode]['NPHAS']) + QCoreApplication.translate(
                                                'dialog', 'F con fase ') + str(
                                                grafoMT[node][secondNode]['PHASE']) + QCoreApplication.translate(
                                                'dialog', ' y MT ') + str(
                                                grafoMT[node][otherNodes[j]]['NPHAS']) + QCoreApplication.translate(
                                                'dialog', 'F con fase ') + str(grafoMT[node][otherNodes[j]]['PHASE'])
                                            aviso_fases = True
                                            archivo_errlogMT.write( aviso )

                    i += 1
                if aviso_bus == True:
                    aviso = "Favor revise el archivo log de MT, ya que habían líneas de MT con bus1 = bus2"
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Líneas primarias'), aviso)
                    archivo_errlogMT.close()
                if aviso_fases == True:
                    aviso = "Favor revise el archivo log de MT, ya que habían conexiones de fases distintas en MT"
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Líneas primarias'), aviso)
                    archivo_errlogMT.close()
                    
                endTimeMT = time.time()
            except KeyError:  # Si los nombres de las columnas no son correctos, el programa genera un aviso y no se ejecuta
                self.print_error()
                QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                            "Verifique los nombres de las columnas de las tablas de atributos de líneas de media tensión"))
                LMTactive = False
                Error = True

            self.progress.progressBar.setValue(10)
            ################################################################## Transformadores Grafos
            
            selectedLayerTR1 = self.dlg.comboBox_TR1.currentText()  # Recibe la capa con transformadores seleccionada en la lista desplegable
            selectedLayerTR2 = self.dlg.comboBox_TR2.currentText()  # Recibe la capa con transformadores seleccionada en la lista desplegable
            selectedLayerTR3 = self.dlg.comboBox_TR3.currentText()  # Recibe la capa con transformadores seleccionada en la lista desplegable
            
            
            

            datosT1F = []
            datosT2F = []
            datosT3F_Multi = []
            datosT3F_Single = []
            Graph_T1F = nx.Graph()
            Graph_T2F = nx.Graph()
            Graph_T3F_multi = nx.Graph()
            Graph_T3F_single = nx.Graph()
            busBTid = 1

            try:  ## Lectura y CONECTIVIDAD TRANSFORMADORES
                
                #### Lectura de datos y creación de grafos de transformadores
                if len(selectedLayerTR1) != 0:
                    layerT1 = QgsProject.instance().mapLayersByName(selectedLayerTR1)[0]
                    layerT1.startEditing()
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT1, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                        layerT1, toler, datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                        Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal)

                if len(selectedLayerTR2) != 0:
                    layerT2 = QgsProject.instance().mapLayersByName(selectedLayerTR2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT2, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                        layerT2, toler, datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                        Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal)
                    layerT2.startEditing()
                if len(selectedLayerTR3) != 0:
                    layerT3 = QgsProject.instance().mapLayersByName(selectedLayerTR3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerT3, "DSSName")
                    datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi, Graph_T3F_single, Graph_T2F, Graph_T1F, grafoBTTotal = self.ReaderDataTrafos(
                        layerT3, toler, datosT3F_Multi, datosT3F_Single, datosT2F, datosT1F, Graph_T3F_multi,
                        Graph_T3F_single, Graph_T2F, Graph_T1F, indexDSS, grafoBTTotal)
                    layerT3.startEditing()
                if (len(datosT1F) == 0 and len(datosT2F) == 0 and len(datosT3F_Multi) == 0 and len(
                        datosT3F_Single) == 0):
                    LTRactive = False
                else:  ##### Asignación de bus a transformadores
                    startTimeTraf = time.time()
                    LTRactive = True
                    if Graph_T1F.number_of_nodes() > 0:  # Verifica que existen transformadores monofásicos
                        Graph_T1F, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T1F, busMT_List, busMTid, busBT_List, busBTid, u"monofásico", grafoMT)
                    if Graph_T3F_multi.number_of_nodes() > 0:  # Verifica que existen transformadores trifásicos de 3 unidades monofásicas
                        Graph_T3F_multi, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T3F_multi, busMT_List, busMTid, busBT_List, busBTid, u"trifásico", grafoMT)
                    if Graph_T3F_single.number_of_nodes() > 0:  # Verifica que existen transformadores trifásicos de 1 unidad
                        Graph_T3F_single, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T3F_single, busMT_List, busMTid, busBT_List, busBTid, u"trifásico", grafoMT)
                    if Graph_T2F.number_of_nodes() > 0:  # Verifica que existen transformadores bifásicos
                        Graph_T2F, busMT_List, busMTid, busBT_List, busBTid = self.BusAsignationTraf(circuitName, Graph_T2F, busMT_List, busMTid, busBT_List, busBTid, u"bifásico", grafoMT)

            except KeyError:
                self.print_error()
                QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate('dialog', "de las tablas de atributos de transformadores"))
                    
                LTRactive = False
                Error = True
            endTimeTraf = time.time()
            self.progress.progressBar.setValue(20)
            ############################################## Baja tensión
            
            selectedLayerBT1 = self.dlg.comboBox_LBT1.currentText()  # Índice de layer_list con lineas BT seleccionada en la lista desplegable
            selectedLayerBT2 = self.dlg.comboBox_LBT2.currentText()  # Índice de layer_list con lineas BT seleccionada en la lista desplegable
            selectedLayerBT3 = self.dlg.comboBox_LBT3.currentText()  # Índice de layer_list con lineas BT seleccionada en la lista desplegable
            
            
            
            datosLBT = []  # datosLBT guarda informacion de ubicacion y fase de las lineas BT3
            grafoBT = nx.Graph()
            LBTactive = False
           ###  Lectura de datos y creación de grafo de LINEAS BAJA TENSION
                
            if len(selectedLayerBT1) != 0:
                try:
                    layerBT1 = QgsProject.instance().mapLayersByName(selectedLayerBT1)[0]
                    LBTactive = True
                    if self.dlg.checkBox_LBT1.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT1, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT1, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                
                except:
                    self.print_error()        
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 1 de líneas de baja tensión"))
                    
                    LBTactive = False
                    Error = True
                                                                                


            if len(selectedLayerBT2) != 0:
                try:
                    layerBT2 = QgsProject.instance().mapLayersByName(selectedLayerBT2)[0]
                    LBTactive = True
                    if self.dlg.checkBox_LBT2.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT2, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT2, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                    
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 2 de líneas de baja tensión"))
                    
                    LBTactive = False
                    Error = True
                                                                         
            if len(selectedLayerBT3) != 0:
                try:
                    layerBT3 = QgsProject.instance().mapLayersByName(selectedLayerBT3)[0]
                    LBTactive = True
                    if self.dlg.checkBox_LBT3.isChecked():  # Determina si la línea es aérea o subterránea
                        subterranea = True
                    else:
                        subterranea = False
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBT3, "DSSName")
                    datosLBT, grafoBT, grafoBTTotal = self.ReaderDataLBT(layerBT3, datosLBT, grafoBT, grafoBTTotal, toler, subterranea, indexDSS)
                
                except:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog',
                                                                                        "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate(
                'dialog', "de las tablas de atributos 3 de líneas de baja tensión"))
                    LBTactive = False
                    Error = True

                
                
            endTimeBT = time.time()

            self.progress.progressBar.setValue(30)
            # 2.3-Crea lista con las coordenadas de Inicio, Final y Fase de las lineas de media tension.
                  
            selectedLayerACO1 = self.dlg.comboBox_ACO1.currentText()  # Índice de layer_list con acometidas en la lista desplegable
            selectedLayerACO2 = self.dlg.comboBox_ACO2.currentText()  # Índice de layer_list con acometidas en la lista desplegable
            selectedLayerACO3 = self.dlg.comboBox_ACO3.currentText()  # Índice de layer_list con acometidas en la lista desplegable
            
            
            
            datosACO = []  # datosACO guarda informacion de ubicacion de acometidas
            grafoACO = nx.Graph()
            ACOactive = False
            ### Lectura de datos y construcción de grafo de acometidas
                
            if len(selectedLayerACO1) != 0:
                try:
                    layerACO1 = QgsProject.instance().mapLayersByName(selectedLayerACO1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO1, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO1, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return
                    ACOactive = True
                except:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate( 'dialog', "de la tabla 1 de atributos de acometidas"))
                    ACOactive = False
                    Error = True
                
                
            if len(selectedLayerACO2) != 0:
                try:
                    layerACO2 = QgsProject.instance().mapLayersByName(selectedLayerACO2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO2, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO2, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return
                    ACOactive = True
                except:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate( 'dialog', "de la tabla 2 de atributos de acometidas"))
                    ACOactive = False
                    Error = True

            if len(selectedLayerACO3) != 0:
                try:
                    layerACO3 = QgsProject.instance().mapLayersByName(selectedLayerACO3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerACO3, "DSSName")
                    datosACO, grafoACO, grafoBTTotal = self.ReaderDataAcom(layerACO3, datosACO, grafoACO, grafoBTTotal, toler, indexDSS, grafoBT)
                    if datosACO == 0:
                        self.progress.close()
                        return
                    ACOactive = True
                except:
                    self.print_error()
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", QCoreApplication.translate('dialog', "Verifique los nombres de las columnas") + "\n" + QCoreApplication.translate( 'dialog', "de la tabla 3 de atributos de acometidas"))
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
            
            if len(selectedLayerCA_MT1) != 0:
                try:
                    layerCAMV1 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV1, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV1, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT )
                    if datosMVCAR == 0:
                        self.progress.close()
                        return
                    CARmvactive = True
                
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de cargas de media tensión")
                    Error = True
                    
            if len(selectedLayerCA_MT2) != 0:
                try:
                    layerCAMV2 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV2, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV2, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT )
                    if datosMVCAR == 0:
                        self.progress.close()
                        return
                    CARmvactive = True
                
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de cargas de media tensión")
                    Error = True
                    
            if len(selectedLayerCA_MT3) != 0:
                try:
                    layerCAMV3 = QgsProject.instance().mapLayersByName(selectedLayerCA_MT3)[0]
                    
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCAMV3, "DSSName")
                    datosMVCAR, grafoCAR_mv, kWhMVload = self.ReaderDataLoadMT(layerCAMV3, datosMVCAR, grafoCAR_mv, kWhMVload, toler, indexDSS, grafoMT )
                    if datosMVCAR == 0:
                        self.progress.close()
                        return
                    CARmvactive = True
                
                except:
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
            
            if len(selectedLayerCA1) != 0:
                try:
                    layerCA1 = QgsProject.instance().mapLayersByName(selectedLayerCA1)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA1, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA1, datosCAR, grafoCAR, kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT)
                    if datosCAR == 0:
                        self.progress.close()
                        return
                    CARactive = True
                
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCA2) != 0:
                try:
                    layerCA2 = QgsProject.instance().mapLayersByName(selectedLayerCA2)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA2, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA2, datosCAR, grafoCAR, kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT)
                    if datosCAR == 0:
                        self.progress.close()
                        return
                    CARactive = True
                    
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
                
            if len(selectedLayerCA3) != 0:
                try:
                    layerCA3 = QgsProject.instance().mapLayersByName(selectedLayerCA3)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerCA3, "DSSName")
                    datosCAR, grafoCAR, kWhLVload, grafoBTTotal = self.ReaderDataLoadBT(layerCA3, datosCAR, grafoCAR,  kWhLVload, toler, indexDSS, grafoBTTotal, grafoBT)
                    if datosCAR == 0:
                        self.progress.close()
                        return
                    CARactive = True
                
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique los nombres de las columnas\nde la tabla de atributos de cargas de baja tensión")
                    Error = True
            
            ### Lectura de datos y construcción de grafo de Buses eléctricos
            selectedLayerBuses = self.dlg.comboBox_plantelbuses.currentText()
                        
            datosBuses = []
            kWhBuses = []
            grafoBuses = nx.Graph()
            Buses_active = False
            Error_Buses = False
            
            if len(selectedLayerBuses) != 0:
                try:
                    dir_csvbuses = self.dlg.lineEdit_csvbus.text()
                    dir_csvcharg = self.dlg.lineEdit_dircsvcarg.text()
                    
                    if dir_csvbuses == "" or dir_csvcharg == "":
                        QMessageBox.critical(None, "QGIS2OpenDSS Error", "Favor indique el nombre de los csv para realizar la simulación de buses eléctricos")
                        self.progress.close()
                        return
                        
                    
                    layerBuses = QgsProject.instance().mapLayersByName(selectedLayerBuses)[0]
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerBuses, "DSSName")
                    datosBuses, grafoBuses, kWhBuses = self.ReaderDataBuses(layerBuses, datosBuses, grafoBuses, kWhBuses, toler, indexDSS, grafoMT, dir_csvbuses, dir_csvcharg)
                    
                    print("Final buses")
                    if datosBuses == 0:
                        self.progress.close()
                        return
                    Buses_active = True
                
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que todos los atributos requeridos se encuentren\n en la tabla de atributos de buses")
                    Error_Buses = True
                    
            
            
            self.progress.progressBar.setValue(30)
            grafoBT, grafoACO, grafoCAR = self.IslandIdentification(grafoBTTotal, grafoBT, grafoACO, grafoCAR)
            self.progress.progressBar.setValue(33)
            
            #####################################################################################################
            #####################################################################################################
            #######################################  VEHÍCULOS ELÉCTRICOS  ######################################
            #####################################################################################################
            #####################################################################################################
            name_layer_evs = self.dlg.comboBox_EV.currentText() #Recibe el nombre de la capa de vehículos eléctricos seleccionada
            if name_layer_evs != "" and name_layer_evs != None and CARactive == True: #Verifica que se haya seleccionado alguna capa en el combobox de vehículos eléctricos, y que se seleccione una capa de cargas
                try:
                    #Declaración de variables necesarias para EV
                    datosEV = []
                    kWhLVev = []
                    grafoEV = nx.Graph()
                    
                    layerEV = QgsProject.instance().mapLayersByName(name_layer_evs)[0] #Se selecciona la capa de vehículos eléctricos
                    datosEV, grafoEV, grafoBTTotal = self.ReaderDataEV( layerEV, datosEV, grafoEV, kWhLVev, toler, indexDSS, grafoBTTotal, grafoCAR, grafoBT ) #Lectura de capa de EV
                    
                    if datosEV == 0: #caso en que haya un error leyendo la capa de VE
                        self.print_error()    
                        QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que la capa de vehículos eléctricos tenga todos los datos solicitados")
                        self.progress.close()
                        return
                    print( "VEHÍCULOS ELÉCTRICOS FINAL" )
                except:
                    self.print_error()    
                    QMessageBox.critical(None, "QGIS2OpenDSS Error", "Verifique que la capa de vehículos eléctricos tenga todos los datos solicitados")
                    Error = True
            
            
            self.progress.progressBar.setValue(38)
            dir_archivo = dir_scripts + "\Pruebas.txt"
                  
            #archivo = open(dir_archivo, 'a')
            #archivo.write("\n\nData line error final\n")
            
        

            if len(datosLBT) == 0:  ## CONECTIVIDAD LINEAS DE BAJA TENSION
                LBTactive = False
            else:
                startTimeBT = time.time()
                LBTactive = True
                nodesBT = grafoBT.nodes()  # guarda todos los nodos del grafoBT en una lista
                ################## Asignación de Bus a líneas BT
                
                aviso_busBT = False
                dir_archivo_BTlog = str( dir_logs + "/error_BT" + circuitName + ".log" )
                archivo_errBTlog = open(dir_archivo_BTlog, 'w')
                
                for node in nodesBT:  # node es el nodo en nombramiento
                    try:
                        if node in busBT_List:  # Verifica si el nodo existe (significa que está conectada a un transformador)
                            bus = busBT_List[node]["bus"]
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBTid += 1
                        for secondNode in grafoBT[node]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                            dataLine = grafoBT[node][secondNode]  # info de la linea
                            #archivo.write( str( str(dataLine) + "\n" ) )
                            #print( "Data Line error final", dataLine )
                            if dataLine['nodo1'] == dataLine['nodo2']:  # Verifica si la línea empieza y termina en el mismo punto
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoBT
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoBT, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine['TRAFVOLTLN']}  #
                                aviso = QCoreApplication.translate('dialog', u'Existe una línea de BT con bus1 igual a bus2 dada su cercanía en (') + str( busBT_List[node]['X']) + ', ' + str(busBT_List[node]['Y']) + ')'
                                archivo_errBTlog.write( aviso )
                                aviso_busBT = True
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoBT, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine['TRAFVOLTLN']}  #
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoBT
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X2'], 'Y': dataLine['Y2'], "GRAFO": grafoBT, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"]}

                    except:
                        self.print_error()
                    
                if aviso_busBT == True:
                    aviso = "Favor revise el archivo log de BT, ya que habían líneas con bus1 = bus2 en BT"
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Errores BT'), aviso)
                    archivo_errBTlog.close()
            
            contador_err = 0            
            if len(datosACO) == 0:  ## CONECTIVIDAD DE ACOMETIDAS
                ACOactive = False
            else:
                
                startTimeAco = time.time()
                dir_archivo_ACOlog = str( dir_logs + "/error_ACO" + circuitName + ".log" )
                archivo_errACOlog = open(dir_archivo_ACOlog, 'w')
                aviso_busAco = False
                ACOactive = True
                nodesACO = grafoACO.nodes()  # guarda todos los nodos del grafoACO en una lista
                ################## Asignación de Bus a líneas Acometidas
                for node in nodesACO:  # node es el nodo en nombramiento
                    try:
                        if node in busBT_List:  # Verifica si el nodo existe (significa que está conectada a un transformador o línea de baja tensión)
                            bus = busBT_List[node]["bus"]
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBTid += 1
                        for secondNode in grafoACO[ node ]:  # itera sobre las lineas que contienen el nodo. Data es el otro nodo de la linea
                            dataLine = grafoACO[node][secondNode]  # info de la linea
                            #print( "Data Line 1902", dataLine )
                            if dataLine['nodo1'] == dataLine['nodo2']:  # Verifica si la línea empieza y termina en el mismo punto
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoACO
                                dataLine['bus2'] = bus  # Agrega el bus1 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoACO, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"]}  #
                                aviso = QCoreApplication.translate('dialog', u'Existe una línea de acometidas con bus1 igual a bus2 dada su cercanía en (') + str( busBT_List[node]['X']) + ', ' + str(busBT_List[node]['Y']) + ')'
                                archivo_errACOlog.write( aviso )
                                aviso_busAco = True
                            elif node == dataLine['nodo1']:
                                dataLine['bus1'] = bus  # Agrega el bus1 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X1'], 'Y': dataLine['Y1'], "GRAFO": grafoACO, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"]}  #
                            elif node == dataLine['nodo2']:
                                dataLine['bus2'] = bus  # Agrega el bus2 al grafoACO
                                busBT_List[node] = {'bus': bus, 'X': dataLine['X2'], 'Y': dataLine['Y2'], "GRAFO": grafoACO, "VOLTAGELL": dataLine["TRAFVOLTLL"], "VOLTAGELN": dataLine["TRAFVOLTLN"]}
                        endTimeAco = time.time()
                    except:
                        self.print_error()
                        if contador_err == 0:
                            contador_err = 1
                
                if aviso_busAco == True:
                    aviso = "Favor revise el archivo log de acometidas, ya que habían líneas con bus1 = bus2 en BT"
                    QMessageBox.warning(None, QCoreApplication.translate('dialog', u'Errores acometidas'), aviso)
                    archivo_errACOlog.close()
                    
                        
                        
            self.progress.progressBar.setValue(40)
            
            #nuevo
            buses_active = False
            if len(datosBuses) != 0:  ### CONECTIVIDAD DE PLANTELES DE BUSES
                buses_active = True
                
                #Datos de la subestación (necesarios sólo si la carga no tiene un bus asociado)
                datos_subest = list( grafoSubt.nodes(data=True) )
                dato = datos_subest[0][1]
                tension_ll = dato['VOLTAJEMED']
                tension_ln = dato['VOLTAJEMEDLN']               
                
                startTimeLoad = time.time()
                graphNodes = list( grafoBuses.nodes(data=True) )
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        
                        if nodo in busMT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de MT
                            grafoBuses.node[nodo]["BUS"] = busMT_List[nodo]["bus"]
                            grafoBuses.node[nodo]["VOLTAGELL"] = busMT_List[nodo]["VOLTAGELL"]
                            grafoBuses.node[nodo]["VOLTAGELN"] = busMT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSMV_plant_buses_' + circuitName + str(busMTid)
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoBuses,
                                                "VOLTAGELN": "0.12"}
                            grafoBuses.node[nodo]["BUS"] = bus
                            grafoBuses.node[nodo]["VOLTAGELL"] = tension_ll #revisar con Tavo
                            grafoBuses.node[nodo]["VOLTAGELN"] = tension_ln #revisar con Tavo
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (') + str( dataList["X1"]) + ',' + str(dataList["Y1"]) + ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busMTid += 1
                        endTimeLoad = time.time()
                    except:
                        self.print_error()
            
            #nuevo
            CARmv_active = False
            if len(datosMVCAR) != 0:  ### CONECTIVIDAD DE CARGAS MT
                #Datos de la subestación (necesarios sólo si la carga no tiene un bus asociado)
                datos_subest = list( grafoSubt.nodes(data=True) )
                dato = datos_subest[0][1]
                tension_ll = dato['VOLTAJEMED']
                tension_ln = dato['VOLTAJEMEDLN']
                                
                CARmv_active = True
                startTimeLoad = time.time()
                graphNodes = list( grafoCAR_mv.nodes(data=True) )
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busMT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de MT
                            grafoCAR_mv.node[nodo]["BUS"] = busMT_List[nodo]["bus"]
                            grafoCAR_mv.node[nodo]["VOLTAGELL"] = busMT_List[nodo]["VOLTAGELL"]
                            grafoCAR_mv.node[nodo]["VOLTAGELN"] = busMT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSMV' + circuitName + str(busMTid)
                            busMT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCAR_mv,
                                                "VOLTAGELN": "0.12"}
                            grafoCAR_mv.node[nodo]["BUS"] = bus
                            #Asigna los datos de la subestacion si no existe ub bus asociado
                            grafoCAR_mv.node[nodo]["VOLTAGELL"] = str( tension_ll ) #revisar con Tavo
                            grafoCAR_mv.node[nodo]["VOLTAGELN"] = str( tension_ln ) #revisar con Tavo
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (') + str( dataList["X1"]) + ',' + str(dataList["Y1"]) + ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busMTid += 1
                        endTimeLoad = time.time()
                    except:
                        self.print_error()

            if len(datosCAR) == 0:  ### CONECTIVIDAD DE CARGAS BT
                CARactive = False
            else:
                                        
                CARactive = True
                startTimeLoad = time.time()
                graphNodes = list( grafoCAR.nodes(data=True) )
                for NODE in graphNodes:
                    try:
                        dataList = NODE[1]
                        nodo = NODE[0]
                        if nodo in busBT_List:  # Verifica si el nodo de la carga ya está en los nodos creados de BT
                            grafoCAR.node[nodo]["BUS"] = busBT_List[nodo]["bus"]
                            grafoCAR.node[nodo]["VOLTAGELL"] = busBT_List[nodo]["VOLTAGELL"]
                            grafoCAR.node[nodo]["VOLTAGELN"] = busBT_List[nodo]["VOLTAGELN"]
                        else:
                            bus = 'BUSLV' + circuitName + str(busBTid)
                            busBT_List[nodo] = {'bus': bus, 'X': dataList["X1"], 'Y': dataList["Y1"], "GRAFO": grafoCAR,
                                                "VOLTAGELN": "0.12"}
                            grafoCAR.node[nodo]["BUS"] = bus
                            grafoCAR.node[nodo]["VOLTAGELL"] = "0.24"
                            grafoCAR.node[nodo]["VOLTAGELN"] = "0.12"
                            aviso = QCoreApplication.translate('dialog', 'Hay 1 carga desconectada: (') + str( dataList["X1"]) + ',' + str(dataList["Y1"]) + ')'
                            #QMessageBox.warning(None, QCoreApplication.translate('dialog', 'Alerta Cargas'), aviso)
                            busBTid += 1
                        endTimeLoad = time.time()
                    except:
                        self.print_error()
                        
    
            self.progress.progressBar.setValue(50)
            
            #######################################################  LOADSHAPE BUSES
            startTimeLoadShape = time.time()
            errorLoadShapeBuses = False
            if (Buses_active == True and folder_profile != ""):  # revisa si hay cargas
                #Asigna los loadshapes de Buses
                grafoBuses, errorLoadShapeBuses = self.AssignLoadshapes( grafoBuses, folder_profile, foldername, circuitName, Buses = True )
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(55)
            startTimeWriting = time.time()
            
            #######################################################  LOADSHAPE CARGAS MT
            startTimeLoadShape = time.time()
            errorLoadShapeMV = False
            if (CARmvactive == True and folder_profile != ""):  # revisa si hay cargas
                #Asigna los loadshapes de MT
                grafoCAR_mv, errorLoadShapeMV = self.AssignLoadshapes( grafoCAR_mv, folder_profile, foldername, circuitName, Buses = False )
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(55)
            startTimeWriting = time.time()

            #######################################################  LOADSHAPE CARGAS BT #revisar para agregar cargas MT
            startTimeLoadShape = time.time()
            errorLoadShape = False
            if (CARactive == True and folder_profile != ""):  # revisa si hay cargas
                #Asigna los loadshapes de BT
                grafoCAR, errorLoadShape = self.AssignLoadshapes( grafoCAR, folder_profile, foldername, circuitName, Buses = False )
            endTimeLoadSahpe = time.time()
            ##########################   Loadshapesx
            self.progress.progressBar.setValue(55)
            startTimeWriting = time.time()

            ###LECTURA Y CONECTIVIDAD DE GD LV
            selectedLayerGD = self.dlg.comboBox_GD_lv.currentText()

            if len(selectedLayerGD) != 0:
                try:
                    layerGD = QgsProject.instance().mapLayersByName(selectedLayerGD)[0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    indexDSS = auxiliary_functions.getAttributeIndex(self, layerGD, "DSSName")
                    grafoGD = nx.Graph()
                    grafoGD, busBTid, busBT_List = self.ReaderDataGD(toler, layerGD, grafoGD, indexDSS, Graph_T3F_multi,   Graph_T3F_single, Graph_T2F, Graph_T1F, grafoCAR,  circuitName, busBTid, busBT_List, busMT_List)
                except:
                    self.print_error()    
                    

            ####9-Crea archivos de salida en la carpeta seleccionada
            """SALIDAS PARA OPENDSS"""
            output_filesQGIS2DSS = open(foldername + '/' + circuitName + '_OutputQGIS2OpenDSS.dss', 'w')  # genera el archivo con la lista de archivos creados
            nombres_capas = "" #variable que almacenará los datos requeridos para escribir txCapacities y lineCurrents
            self.output_filesQGIS2DSS = output_filesQGIS2DSS
            # Líneas de media tensión y monitores de líneas de media tensión.
            output_filesQGIS2DSS.write('\nredirect Bibliotecas/bibliotecas.dss')
            
            ###################################  Escritura LMT

            if LMTactive == True:
                try:
                    filename = circuitName + '_LinesMV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_lmtdss = open(foldername + '/' + filename, 'w')
                    filenameMon = circuitName + '_Monitors.dss'
                    # output_filesQGIS2DSS.write('\nredirect '+filenameMon)
                    output_monitorsdss = open(foldername + '/' + filenameMon, 'w')
                    nombres_capas += "\n!Layers LinesMV: "
    
                    if len(selectedLayerMT1) != 0:
                        nombres_capas += str( selectedLayerMT1 ) + ","
                        layerMT1.startEditing()  # Activa modo edición
                    if len(selectedLayerMT2) != 0:
                        nombres_capas += str( selectedLayerMT2 ) + ","
                        layerMT2.startEditing()  # Activa modo edición
                    if len(selectedLayerMT3) != 0:
                        nombres_capas += str( selectedLayerMT3 )
                        layerMT3.startEditing()  # Activa modo edición
                    
                    n = 0
                    for linea in grafoMT.edges(data=True):
                        DATOS = linea[2]
                        air_or_ugnd = linea[2]['AIR_UGND']
                        configFase = linea[2]['PHASE']  # Recibe la fase del bus en formato ODSS
                        cantFases = linea[2]['NPHAS']  # Recibe la cantidad de fases de la linea
                        opervoltLN = linea[2]['VOLTOPRLN']  # ,' !1ph_basekV=',opervolt
                        if air_or_ugnd == 'air':
                            equipment = str(cantFases) + 'FMV' + str(linea[2]['PHASIZ']) + str(linea[2]['PHAMAT']) + str(linea[2]['NEUSIZ']) + str(linea[2]['NEUMAT']) + '_' + str(linea[2]['CCONF'])  # Recibe la informacion del tipo de conductor, cantidad y aislamiento
                        else:
                            equipment = str(cantFases) + 'FMV' + str(linea[2]['PHASIZ']) + str(linea[2]['PHAMAT']) + '_' + str(linea[2]['NOMVOLT']) + str(linea[2]['INSUL'])  # Recibe la informacion del tipo de conductor, cantidad y aislamiento
    
                        busfrom = linea[2]['bus1']
                        busto = linea[2]['bus2']
                        if float(linea[2]['SHLEN']) == 0:
                            linea[2]['SHLEN'] = 0.0001
                        sh_len = "{0:.4f}".format(linea[2]['SHLEN'])  # Recibe la longitud de la linea
                        if (busfrom == 'BUSMV' + circuitName + str(1)) or (busto == 'BUSMV' + circuitName + str(1)):
                            lineName = "MV" + str(cantFases) + 'P' + circuitName + str(n)
                        else:
                            lineName = "MV" + str(cantFases) + 'P' + circuitName + str(n)
                        n += 1
                        line = '%s%s%s%s%s%s%s%s%s%s%s%s%s %s%s\n' % (
                        'new line.', lineName, ' bus1=', busfrom, configFase, ' bus2=', busto, configFase, ' geometry=', equipment, ' length=', sh_len, ' units=m', ' !1ph_basekV=',
                        opervoltLN)  # Se usan las variables anteriores en formar el string de salida
                        output_lmtdss.write(line)  # Escribe el string de salida en el archivo
                        element = 'line.' + lineName
                        line = '%s%s%s%s%s %s%s\n' % (
                        'new monitor.Mon', lineName, ' Element=', element, ' Terminal=1 Mode=0', ' !1ph_basekV=',
                        opervoltLN)
                        output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                        linea[2]["LAYER"].changeAttributeValue(linea[2]["ID"], linea[2]["INDEXDSS"], lineName)
                    output_lmtdss.close()  # Cierra el archivo de salida
                    output_monitorsdss.close()  # Cierra el archivo de salida
                    if len(selectedLayerMT1) != 0:
                        layerMT1.commitChanges()  # Guarda
                    if len(selectedLayerMT2) != 0:
                        layerMT2.commitChanges()  # Guarda
                    if len(selectedLayerMT3) != 0:
                        layerMT3.commitChanges()  # Guarda
                        
                except:
                    self.print_error()
                        
                        
                #######################################

            ##Buses de media tensión con coordenadas
            if len(busMT_List) > 0:
                try:
                    filename = circuitName + '_BusListMV.csv'
                    filename2 = circuitName + '_MV_BaseKV_LN.dss'
                    # output_filesQGIS2DSS.write('\nBusCoords '+filename)
                    # output_filesQGIS2DSS.write('\nredirect '+filename2)
                    output_buslistmt = open(foldername + '/' + filename, 'w')
                    output_baseKV_DSS = open(foldername + '/' + filename2, 'w')
                    layerMTBusName = "Bus_MT_Layer"
                    attrs_MtBusLayer = ['BUS', 'BASEKV_LN', 'PHASES']
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
                        line = '%s,%s,%s\n' % (busMT_List[bus]['bus'], busMT_List[bus]['X'], busMT_List[bus]['Y'])  # Se usan las variables anteriores en formar el string de salida
                        output_buslistmt.write(line)  # Escribe el string de salida en el archivo
                        lineBase = "%s%s%s%s\n" % ("SetKVBase bus=", busName, " kvln=", MTvoltage)
                        output_baseKV_DSS.write(lineBase)
                        feat = QgsFeature(pr.fields())
                        feat.setGeometry( QgsGeometry.fromPointXY(QgsPointXY(float(busMT_List[bus]['X']), float(busMT_List[bus]['Y']))))
                        feat["BUS"] = busName
                        feat["BASEKV_LN"] = MTvoltage
                        feat["PHASES"] = Nphases
                        pr.addFeatures([feat])
                    output_buslistmt.close()  # Cierra el archivo de salida
                    output_baseKV_DSS.close()
                    layer.updateExtents()
                    layer.commitChanges()
                
                except:
                    self.print_error()
    
            self.progress.progressBar.setValue(60)
            ###Se genera la salida de subestación para OpenDSS
            if len(selectedLayerSE) != 0:
                try:
                    
                    if mode == "MODEL":
                        filename = circuitName + '_Substation.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                        output_sedss = open(foldername + '/' + filename, 'w')
                        layerSE.startEditing()  # Activa modo edición
                        # if (ndatosSE!=0): # revisa si hay subestaciones
                        output_sedss.write("!UNIT\n")
                        for NODE in list( grafoSubt.nodes(data=True) ):
                            dataList = NODE[1]
                            nodo = NODE[0]
                            cantFases = '3'
                            fases = ' phases=' + str(cantFases)
                            dev = ' windings=' + str(dataList['WINDINGS'])
                            busHV = 'Sourcebus'
                            busMV = str(dataList['BUSMT'])
    
                            busLV = ''
                            normhkva = " normhkva=" + str(dataList['KVA_ALTA'])
                            kVA = ' kVAs=[' + str(float(dataList['KVA_ALTA'])) + ' ' + str(float(dataList['KVA_MEDIA']))
                            kV = 'kVs=[' + str(dataList['VOLTAJEALT']) + ' ' + str(dataList['VOLTAJEMED'])
                            react = ' xhl=' + str(dataList['XHL'])
                            con = ' conns=[' + dataList['CONEXIONAL'] + ' ' + dataList['CONEXIONME']
                            if dataList['WINDINGS'] > 2:
                                kVA = kVA + ' ' + str(float(dataList['KVA_BAJA']))
                                kV = kV + ' ' + str(dataList['VOLTAJEBAJ'])
                                busLV = ' BUSMV_TerSub.1.2.3'
                                react = react + ' xht=' + str(dataList['XHT']) + ' xlt=' + str(dataList['XLT'])
                                con = con + ' ' + dataList['CONEXIONBA']
                            kVA = kVA + ']'
                            kV = kV + ']'
                            buses = ' buses=[Sourcebus.1.2.3 ' + busMV + '.1.2.3' + busLV + ']'
                            con = con + ']'
                            taps = ''
                            if dataList['TAPS'] != '':
                                taps = 'wdg=1 numtaps=' + str(dataList['TAPS']) + ' tap=' + str(dataList['TAP']) + ' maxtap=' + str(dataList['MAXTAP']) + ' mintap=' + str(dataList['MINTAP'])
                            line = '%s %s %s %s %s %s %s %s %s %s%s\n' % ( 'new transformer.HVMV', buses, fases, dev, con, kV, kVA, react, ' %loadloss=0 %noloadloss=0 ', taps, normhkva)
                            output_sedss.write(line)
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], "HVMV")
                        output_sedss.close()
                        layerSE.commitChanges()  # Guarda
                    if mode == "AUTO":
                        filename = circuitName + '_Substation.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                        output_sedss = open(foldername + '/' + filename, 'w')
                        layerSE.startEditing()  # Activa modo edición
                        # if (ndatosSE!=0): # revisa si hay subestaciones
                        for NODE in list( grafoSubt.nodes(data=True) ):
                            dataList = NODE[1]
                            nodo = NODE[0]
                            cantFases = '1'
                            fases = ' phases=' + str(cantFases)
                            dev = ' windings=2'
                            busHV = 'Sourcebus'
                            busMV = str(dataList['BUSMT'])
                            Vautohigh = float(dataList['VOLTAJEALT']) / sqrt(3)
                            Vautolow = float(dataList['VOLTAJEMED']) / sqrt(3)
                            Zauto = float(dataList['XHL'])
                            kVAauto = float(dataList['KVA_ALTA']) / 3
    
                            Vtraf1 = Vautohigh
                            Vtraf2 = Vautohigh - Vautolow
                            nt = Vtraf2 / Vtraf1
                            Ztraf = (1 - nt) * Zauto / nt
                            kVAtraf = nt * kVAauto / (1 - nt)
    
                            normhkva = " normhkva=" + str(kVAtraf)
                            kVA = ' kVAs=[' + str(kVAtraf) + ' ' + str(kVAtraf) + "]"
                            kV = ' kVs=[' + str(Vtraf1) + ' ' + str(Vtraf2) + "]"
                            react = ' xhl=' + str(Ztraf)
    
                            busesTx1 = ' buses=[bridge.1.0 ' + 'bridge.1.4]'
                            busesTx2 = ' buses=[bridge.2.0 ' + 'bridge.2.5]'
                            busesTx3 = ' buses=[bridge.3.0 ' + 'bridge.3.6]'
    
                            taps = ''
                            if dataList['TAPS'] != '':
                                taps = 'wdg=1 numtaps=' + str(dataList['TAPS']) + ' tap=1.00' + ' maxtap=' + str(
                                    dataList['MAXTAP']) + ' mintap=' + str(dataList['MINTAP'])
                            line1 = '%s%s%s%s%s%s%s%s%s%s\n' % (
                            'new transformer.HVMV_auto1', busesTx1, fases, dev, kV, kVA, normhkva, react,
                            ' %loadloss=0 %noloadloss=0 ', taps)
                            line2 = '%s%s%s%s%s%s%s%s%s%s\n' % (
                            'new transformer.HVMV_auto2', busesTx2, fases, dev, kV, kVA, normhkva, react,
                            ' %loadloss=0 %noloadloss=0 ', taps)
                            line3 = '%s%s%s%s%s%s%s%s%s%s\n' % (
                            'new transformer.HVMV_auto3', busesTx3, fases, dev, kV, kVA, normhkva, react,
                            ' %loadloss=0 %noloadloss=0 ', taps)
                            jumper1 = "new line.jumper1 bus1=SourceBus.1.2.3 bus2=bridge.1.2.3 R=0 X=0.00001 Normamps=7000\n"
                            jumper2 = "new line.jumper2 bus1=bridge.4.5.6 bus2=" + busMV + ".1.2.3 R=0 X=0.00001 Normamps=7000\n"
    
                            output_sedss.write("!AUTO\n")
                            output_sedss.write(jumper1)
                            output_sedss.write(jumper2)
                            output_sedss.write(line1)
                            output_sedss.write(line2)
                            output_sedss.write(line3)
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], "HVMV_auto")
                        output_sedss.close()
                        layerSE.commitChanges()  # Guarda
                    if mode == "NOMODEL":
                        filename = circuitName + '_Substation.dss'
                        output_sedss = open(foldername + '/' + filename, 'w')
                        for NODE in list( grafoSubt.nodes(data=True) ):
                            dataList = NODE[1]
                            nodo = NODE[0]
    
                            bus = dataList['BUSMT']
    
                            line = bus + " , " + str(dataList["VOLTAJEMED"])
                            output_sedss.write("!NOMODEL\n")
                            output_sedss.write(line)
                        output_sedss.close()
                except:
                    self.print_error()
                self.progress.progressBar.setValue(65)
            
            ##################################################
            ##   ESCRITURA DE TRANSFORMADORES
            ##################################################
            # Se genera la salida de transformadores y monitores de transformadores para OpenDSS
            if (LTRactive == True):
                try:
                    filename = circuitName + '_Transformers.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_trdss = open(foldername + '/' + filename, 'w')
                    filenameMon = circuitName + '_Monitors.dss'
                    #output_filesQGIS2DSS.write('\nredirect ' + filenameMon)
                    output_monitorsdss = open(foldername + '/' + filenameMon, 'a')
                    
                    nombres_capas += "\n!Layers Transformers: "
    
                    if len(selectedLayerTR1) != 0:
                        nombres_capas += str( selectedLayerTR1 ) + ","
                        layerT1.startEditing()  # Activa modo edición
                    if len(selectedLayerTR2) != 0:
                        nombres_capas += str( selectedLayerTR2 ) + ","
                        layerT2.startEditing()  # Activa modo edición
                    if len(selectedLayerTR3) != 0:
                        nombres_capas += str( selectedLayerTR3 )
                        layerT3.startEditing()  # Activa modo edición
    
                    if (Graph_T1F.number_of_nodes() != 0):  # revisa si hay trafos monofásicos
                        output_trdss.write('//Transformadores Monofasicos\n')  # Escribe el string de salida en el archivo
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
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            phaseMV = dataList['PHASE'] + ".0"
                            tap = dataList['TAPS']
    
                            grupo_trafo_lv = str( dataList['GRUPO_LV'] )
                            grupo_trafo_mv = str( dataList['GRUPO_MV'] )
    
                            reactance = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN,
                                                                            int(float(kVA))).get('X')
                            resistance = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN,
                                                                             int(float(kVA))).get('R')
                            noloadloss = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN,
                                                                             int(float(kVA))).get('Pnoload')
                            imag = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, int(float(kVA))).get(
                                'Im')
                            trafName = circuitName + cantFases + 'P_' + str(n + 1)
                            line = 'new transformer.' + trafName + ' phases=1 windings=3 ' + reactance + ' ' + resistance + " " + noloadloss + " "
                            line += imag + ' Buses=[' + busMV + phaseMV + ' ' + busLV + '.1.0 ' + busLV + '.0.2]' + ' kvs=[' + kV_MedLN + " "
                            line += kV_LowLN + " " + kV_LowLN + ']' + ' kVAs=[' + kVA + " " + kVA + " " + kVA + '] conns=[wye wye wye] Taps=[' + tap + ', 1, 1]'
                            line += normhkva +  " !GroupMV=" + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                            output_trdss.write(line + "\n")
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                            element = 'transformer.' + trafName
                            line = 'new monitor.Mon' + trafName + ' Element=' + element + ' Terminal=1 Mode=1' + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                            output_monitorsdss.write(line + "\n")  # Escribe el string de salida en el archivo
    
                            n += 1
                    if (Graph_T3F_single.number_of_nodes() != 0):  # revisa si hay trafos trifásicos Single
                        output_trdss.write(
                            '\n//Transformadores Trifasicos Simples\n')  # Escribe el string de salida en el archivo
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
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            tap = dataList['TAPS']
                            if (dataList['CONME'] == 'Y'):
                                confMV = 'wye'
                            else:
                                confMV = 'delta'
                            if (dataList['CONBA'] == 'Y'):
                                confLV = 'wye'
                            else:
                                confLV = 'delta'
                            phaseMV = dataList['PHASE']
                            grupo_trafo_lv = str( dataList['GRUPO_LV'] )
                            grupo_trafo_mv = str( dataList['GRUPO_MV'] )
                            impedance = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get('Z')
                            noloadloss = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get(
                                'Pnoload')
                            imag = trafoOperations.impedanceSingleUnit(cantFases, kV_MedLL, kV_LowLN, kVA).get('Im')
                            trafName = circuitName + cantFases + 'P_' + str(n + 1)
                            line = 'new transformer.' + trafName + ' phases=3 windings=2 ' + noloadloss + " " + imag + ' buses=[' + busMV + '.1.2.3 '
                            line += busLV + '.1.2.3]' + ' conns=[' + confMV + ' ' + confLV + ']' + ' kvs=[' + kV_MedLL + " " +  kV_LowLL + ']'
                            line += ' kvas=[' + kVA + " " + kVA + '] ' + impedance + ' Taps=[' + tap + ', 1, 1]' + normhkva + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                            output_trdss.write(line + "\n")
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], trafName)
                            element = 'transformer.' + trafName
                            line = 'new monitor.Mon' + trafName + ' Element=' + element + ' Terminal=1 Mode=1'
                            output_monitorsdss.write(line + "\n")  # Escribe el string de salida en el archivo
                            n += 1
                    if (Graph_T3F_multi.number_of_nodes() != 0):  # revisa si hay trafos trifásicos Multi
                        output_trdss.write(
                            '\n//Transformadores Trifasicos de tres unidades Monofasicas\n')  # Escribe el string de salida en el archivo
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
    
                            grupo_trafo_lv = str( dataList['GRUPO_LV'] )
                            grupo_trafo_mv = str( dataList['GRUPO_MV'] )
    
                            impedanceA = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impA')['Za']
                            impedanceB = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impB')['Zb']
                            resistanceB = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impB')['Rb']
                            reactanceB = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impB')['Xb']
                            impedanceC = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impC')['Zc']
                            imagA = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impA')['ImA']
                            imagB = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impB')['ImB']
                            imagC = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impC')['ImC']
                            noloadlossA = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impA')['PnoloadA']
                            noloadlossB = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impB')['PnoloadB']
                            noloadlossC = \
                            trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, kVA_C, phaseMV).get(
                                'impC')['PnoloadC']
    
                            if (dataList['CONBA'] == '4D'):  # si el transformador es delta 4 hilos en baja tensión
                                line_A = 'new transformer.' + circuitName + '3U3P_1_' + str(n + 1) + ' phases=1 windings=2 ' +  imagA
                                line_A += ' buses=[' + busMV + '.1.0 ' + busLV + '.3.1' + ']' + " " + noloadlossA + " " + impedanceA + ' kvs=[' + kV_MedLN + " " + kV_LowLL + ']'
                                line_A += ' kvas=[' + kVA_A + " " + kVA_A + ']' + ' conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_A + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                line_B = 'new transformer.' + circuitName + '3U3P_2_' + str(n + 1) + ' phases=1 windings=3 ' + imagB
                                line_B += ' buses=[' + busMV + '.2.0 ' + busLV + '.1.0 ' + busLV + '.0.2]' + " " + noloadlossB + " " + reactanceB + " "
                                line_B += resistanceB + ' kvs=[' + kV_MedLN + " " + kV_LowLN + " " + kV_LowLN + ']' + ' kvas=[' + kVA_B + " " + kVA_B + " " + kVA_B + ']'
                                line_B += ' conns=[wye wye wye] Taps=[' + tap + ', 1, 1]' + normhkva_B + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                line_C = 'new transformer.' + circuitName + '3U3P_3_' + str(n + 1) + ' phases=1 windings=2 ' + imagC
                                line_C += ' buses=[' + busMV + '.3.0 ' + busLV + '.2.3]' + " " + noloadlossC + " " + impedanceC + ' kvs=[' + kV_MedLN + " " + kV_LowLL + ']'
                                line_C += ' kvas=[' + kVA_C + " " + kVA_C + '] conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_C + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                output_trdss.write(line_A + "\n")
                                output_trdss.write(line_B + "\n")
                                output_trdss.write(line_C + "\n")
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"],
                                                                       circuitName + '3U3P_' + str(n + 1))
    
                                element = 'transformer.' + circuitName + '3U3P_1_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_1_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                                element = 'transformer.' + circuitName + '3U3P_2_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_2_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                                element = 'transformer.' + circuitName + '3U3P_3_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_3_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                            elif (dataList['CONBA'] == 'Y'):
                                line_A = 'new transformer.' + circuitName + '3U3P_1_' + str(n + 1) + ' phases=1 windings=2 ' + imagA
                                line_A += ' buses=[' + busMV + '.1.0' + " " + busLV + '.3.1]' + " " + noloadlossA + " " + impedanceA + ' kvs=[' + kV_MedLN + " " + kV_LowLL + ']'
                                line_A += ' kvas=[' + kVA_A + " " + kVA_A + '] conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_A + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
    
                                line_B = 'new transformer.' + circuitName + '3U3P_2_' + str(n + 1) + ' phases=1 windings=2 ' + imagB
                                line_B += ' buses=[' + busMV + '.2.0 ' + busLV + '.1.2]' + " " + noloadlossB + " " + impedanceB + ' kvs=[' + kV_MedLN + " " + kV_LowLL + '] kvas=[' + kVA_B + " " + kVA_B + ']'
                                line_B += ' conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_B + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
    
                                line_C = 'new transformer.' + circuitName + '3U3P_3_' + str(n + 1) + ' phases=1 windings=2 ' + imagC
                                line_C += ' buses=[' +  busMV + '.3.0 ' + busLV + '.2.3]' + " " + noloadlossC + " " + impedanceC + ' kvs=[' + kV_MedLN + " " + kV_LowLL + ']'
                                line_C += ' kvas=[' + kVA_C + " " +  kVA_C + '] conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_C + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                output_trdss.write(line_A + "\n")  # res
                                output_trdss.write(line_B + "\n")
                                output_trdss.write(line_C + "\n")
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"],
                                                                       circuitName + '3U3P_' + str(n + 1))
    
                                element = 'transformer.' + circuitName + '3U3P_1_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_1_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                                element = 'transformer.' + circuitName + '3U3P_2_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_2_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                                element = 'transformer.' + circuitName + '3U3P_3_' + str(n + 1)
                                line = '%s%s%s%s%s%s%s\n' % (
                                'new monitor.Mon', circuitName, '3U3P_3_', str(n + 1), ' Element=', element,
                                ' Terminal=1 Mode=1')
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                            n += 1
                    if (Graph_T2F.number_of_nodes() != 0):  # revisa si hay trafos bifásicos
                        output_trdss.write(
                            '\n//Transformadores bifásicos (Conexiones especiales de dos transformadores)\n')  # Escribe el string de salida en el archivo
                        n = 0
                        for TRAFO2F in Graph_T2F.nodes(data=True):
                            dataList = TRAFO2F[1]
                            nodo = TRAFO2F[0]
                            busMV = str(dataList['BUSMT'])
                            busLV = str(dataList['BUSBT'])
                            kV_MedLL = dataList['VOLTMTLL']
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
                            grupo_trafo_lv = str( dataList['GRUPO_LV'] )
                            grupo_trafo_mv = str( dataList['GRUPO_MV'] )
                            
                            if (dataList['CONBA'] == '4D'):  # si el transformador es delta 4 hilos en baja tensión
                                if phase == '.1.2':  # Las variables conexME y conexBA se utilizan para escribir a qué nodos de la barra se conecta la estrella abierta
                                    if float(dataList['KVA_FA']) >= float(dataList['KVA_FB']):
                                        buff_kVA_A = kVA_A
                                        buff_kVA_B = kVA_B
                                        kVA_A = buff_kVA_B
                                        kVA_B = buff_kVA_A
    
                                    conexME_trafoA = '.1.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.3.1'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.2.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.1.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.2'  # Conexión en barra de baja tensión del transformador B
                                    impedanceA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impA')[
                                        'Za']  # Se obtienen las impedancias según las fases del banco en las que hay transformadores
                                    reactanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impB')['Rb']
                                    resistanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impB')['Xb']
                                    imagA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impA')['ImA']
                                    imagB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impB')['ImB']
                                    noloadlossA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impA')['PnoloadA']
                                    noloadlossB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_B, 0, phase).get(
                                        'impB')['PnoloadB']
                                    kVA_trafoA = kVA_A  # Potencia del transformador A
                                    kVA_trafoB = kVA_B  # Potencia del transformador B
                                if phase == '.1.3':
                                    if float(dataList['KVA_FA']) >= float(dataList['KVA_FC']):
                                        buff_kVA_A = kVA_A
                                        buff_kVA_C = kVA_C
                                        kVA_A = buff_kVA_C
                                        kVA_C = buff_kVA_A
    
                                    conexME_trafoA = '.1.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.2.1'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.3.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.1.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.3'  # Conexión en barra de baja tensión del transformador B
                                    impedanceA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impA')['Za']
                                    reactanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impB')['Rb']
                                    resistanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impB')['Xb']
                                    imagA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impA')['ImA']
                                    imagB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impB')['ImB']
                                    noloadlossA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impA')['PnoloadA']
                                    noloadlossB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_A, kVA_C, 0, phase).get(
                                        'impB')['PnoloadB']
                                    kVA_trafoA = kVA_A
                                    kVA_trafoB = kVA_C
                                if phase == '.2.3':
                                    if float(dataList['KVA_FB']) >= float(dataList['KVA_FC']):
                                        buff_kVA_B = kVA_B
                                        buff_kVA_C = kVA_C
                                        kVA_B = buff_kVA_C
                                        kVA_C = buff_kVA_B
                                    conexME_trafoA = '.2.0'  # Conexión en barra de media tensión del transformador A
                                    conexBA_trafoA = '.1.2'  # Conexión en barra de baja tensión del transformador A
                                    conexME_trafoB = '.3.0'  # Conexión en barra de media tensión del transformador B
                                    conexBA1_trafoB = '.2.0'  # Conexión en barra de baja tensión del transformador B
                                    conexBA2_trafoB = '.0.3'  # Conexión en barra de baja tensión del transformador B
                                    impedanceA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impA')['Za']
                                    reactanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impB')['Rb']
                                    resistanceB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impB')['Xb']
                                    imagA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impA')['ImA']
                                    imagB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impB')['ImB']
                                    noloadlossA = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impA')['PnoloadA']
                                    noloadlossB = \
                                    trafoOperations.impedanceMultiUnit(kV_MedLL, kV_LowLN, kVA_B, kVA_C, 0, phase).get(
                                        'impB')['PnoloadB']
                                    kVA_trafoA = kVA_B
                                    kVA_trafoB = kVA_C
                                normhkva_A = " normhkva=" + kVA_trafoA
                                normhkva_B = " normhkva=" + kVA_trafoB
                                line_A = 'new transformer.' + circuitName + '2U3P_1_' + str(n + 1) + ' phases=1 windings=2 ' + imagA + " "
                                line_A += impedanceA + " " + noloadlossA + ' buses=[' + busMV + conexME_trafoA + ' ' + busLV + conexBA_trafoA + '] '
                                line_A += ' kvs=[' + kV_MedLN + " " + kV_LowLL + '] kvas=[' + kVA_trafoA + " " + kVA_trafoA + ']'
                                line_A += ' conns=[wye wye] Taps=[' + tap + ', 1]' + normhkva_A + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                line_B = 'new transformer.' + circuitName + '2U3P_2_' + str(n + 1) + ' phases=1 windings=3 ' + imagB + " "
                                line_B += reactanceB + resistanceB + " " + noloadlossB + ' buses=[' + busMV + conexME_trafoB + ' ' + busLV
                                line_B += conexBA1_trafoB + ' ' + busLV + conexBA2_trafoB + '] kvs=[' + kV_MedLN + " " + kV_LowLN + " " + kV_LowLN + ']'
                                line_B += ' kvas=[' + kVA_trafoB + " " + kVA_trafoB + " " + kVA_trafoB + '] conns=[wye wye wye] Taps=[' + tap + ', 1, 1]'
                                line_B +=  normhkva_B + ' !GroupMV=' + grupo_trafo_mv + ' !GroupLV=' + grupo_trafo_lv
                                output_trdss.write(line_A + "\n")
                                output_trdss.write(line_B + "\n")
                                dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"],
                                                                       circuitName + '2U3P_' + str(n + 1))
                                element = 'transformer.' + circuitName + '2U3P_1_' + str(n + 1)
                                line = 'new monitor.Mon' + circuitName + '2U3P_1_' + str(n + 1) + ' Element=' + element +' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                                element = 'transformer.' + circuitName + '2U3P_2_' + str(n + 1)
                                line = 'new monitor.Mon' + circuitName + '2U3P_2_' + str(n + 1) + ' Element=' + element + ' Terminal=1 Mode=1\n'
                                output_monitorsdss.write(line)  # Escribe el string de salida en el archivo
                            n += 1
                    output_trdss.close()
                    output_monitorsdss.close()
                    if len(selectedLayerTR1) != 0:
                        layerT1.commitChanges()  # Activa modo edición
                    if len(selectedLayerTR2) != 0:
                        layerT2.commitChanges()  # Activa modo edición
                    if len(selectedLayerTR3) != 0:
                        layerT3.commitChanges()  # Activa modo edición
                
                except:
                    self.print_error()
    
                    
            #####################################
            ##   FIN ESCRITURA DE TRANSFORMADORES
            #####################################
            self.progress.progressBar.setValue(70)
            # Se genera la salida de transformadores y monitores de transformadores para OpenDSS
            
            ##################################
            ### ESCRITURA DE BAJA TENSIÓN ####
            ##################################
            if (LBTactive == True):
                try:
                    
                    filename = circuitName + '_LinesLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    nombres_capas += "\n!Layers LinesLV: "
                    output_lbtdss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerBT1) != 0:
                        nombres_capas += str( selectedLayerBT1 ) + ","
                        layerBT1.startEditing()  # Activa modo edición
                    if len(selectedLayerBT2) != 0:
                        nombres_capas += str( selectedLayerBT2 ) + ","
                        layerBT2.startEditing()  # Activa modo edición
                    if len(selectedLayerBT3) != 0:
                        nombres_capas += str( selectedLayerBT3 )
                        layerBT3.startEditing()  # Activa modo edición
                    n = 1
                    for line in grafoBT.edges(data=True):
                        dataLine = line[2]
                        TrafNode = dataLine["TRAFNODE"]
                        busfrom = line[2]['bus1']
                        busto = line[2]['bus2']
                        # conns=dataLine['CONNS']
    
                        if dataLine[
                            'TRAFNPHAS'] == "NULL":  # Si la linea no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                            cantFases = dataLine['NPHAS']
                            desc = "Disconnected"
                            busBT_List[line[0]]['VOLTAGELN'] = "0.12"
                            busBT_List[line[1]]['VOLTAGELN'] = "0.12"
                        else:
                            cantFases = dataLine['TRAFNPHAS']
                            desc = ""
                        if (dataLine['AIR_UGND'] == 'air'):
                            if (cantFases == '1'):
                                equipment = 'Geometry=1FLV' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + str(
                                    dataLine['NEUSIZ']) + str(dataLine['NEUMAT'])
                                if dataLine['TIPO'] == 'TPX':
                                    equipment = 'Linecode=TRPX' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT'])
                                conns = ".1.2"
                            elif (cantFases == '3' or cantFases == '2'):
                                equipment = 'Geometry=3FLV' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + str(
                                    dataLine['NEUSIZ']) + str(dataLine['NEUMAT'])
                                if dataLine['TIPO'] == 'QPX':
                                    equipment = 'Linecode=QDPX' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT'])
                                conns = ".1.2.3"
                            else:
                                equipment = 'NONE'
                        else:
                            if (cantFases == '1'):
                                equipment = 'LineCode=1FLV' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + str(
                                    dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + '_' + str(dataLine['INSUL'])
                                conns = ".1.2"
                            elif (cantFases == '3' or cantFases == '2'):
                                equipment = 'LineCode=3FLV' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + str(
                                    dataLine['PHASIZ']) + str(dataLine['PHAMAT']) + '_' + str(dataLine['INSUL'])
                                conns = ".1.2.3"
                            else:
                                equipment = 'NONE'
                                conns = "NONE"
                        if float(dataLine['SHLEN']) == 0:
                            dataLine['SHLEN'] = 0.0001
                        sh_len = "{0:.4f}".format(dataLine['SHLEN'])  # Recibe la longitud de la linea
                        opervoltLN = dataLine["TRAFVOLTLN"]
                        grupo_aco = dataLine['GRUPO']
                        lineName = "LV" + cantFases + 'F' + circuitName + str(n)
                        line = '%s%s %s%s%s %s%s%s %s%s%s %s %s%s %s%s  %s\n' % (
                        'new line.', lineName, 'bus1=', busfrom, conns, 'bus2=', busto, conns, equipment, ' length=',
                        sh_len, 'units=m', ' !1ph_basekV=', opervoltLN, ' Group=', grupo_aco, desc)
                        output_lbtdss.write(line)  # Escribe el string de salida en el archivo
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"], dataLine["INDEXDSS"], lineName)
                        ######## modificación del shape
                        n += 1
                    output_lbtdss.close()  # Cierra el archivo de salida
                    if len(selectedLayerBT1) != 0:
                        layerBT1.commitChanges()  # Guarda
                    if len(selectedLayerBT2) != 0:
                        layerBT2.commitChanges()  # Guarda
                    if len(selectedLayerBT3) != 0:
                        layerBT3.commitChanges()  # Guarda
    
                except:
                    self.print_error()

            ##############################
            ### FIN ESCRITURA BAJA TENSIÓN
            ##############################
            self.progress.progressBar.setValue(80)
            ##############################
            ###  ESCRITURA ACOMETIDAS
            ##############################
            if (ACOactive == True):
                try:
                    
                
                    filename = circuitName + '_ServicesLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_acodss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerACO1) != 0:
                        layerACO1.startEditing()  # Activa modo edición
                    if len(selectedLayerACO2) != 0:
                        layerACO2.startEditing()  # Activa modo edición
                    if len(selectedLayerACO3) != 0:
                        layerACO3.startEditing()  # Activa modo edición
    
                    n = 1
                    for line in grafoACO.edges(data=True):
                        dataLine = line[2]
                        TrafNode = dataLine["TRAFNODE"]
                        busfrom = line[2]['bus1']
                        busto = line[2]['bus2']
                        # conns=dataLine['CONNS']
                        if dataLine[
                            'TRAFNPHAS'] == "NULL":  # Si la linea no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                            cantFases = dataLine['NPHAS']
                            desc = "Disconnected"
                            busBT_List[line[0]]['VOLTAGELN'] = "0.12"
                            busBT_List[line[1]]['VOLTAGELN'] = "0.12"
                        else:
                            cantFases = dataLine['TRAFNPHAS']
                            desc = ""
                        if (cantFases == '1'):
                            equipment = 'TRPX' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT'])
                            conns = ".1.2"
                        elif (cantFases == '3' or cantFases == '2'):
                            equipment = 'QDPX' + str(dataLine['PHASIZ']) + str(dataLine['PHAMAT'])
                            conns = ".1.2.3"
                        else:
                            equipment = 'NONE'
                            conns = "NONE"
                        if float(dataLine['SHLEN']) == 0:
                            dataLine['SHLEN'] = 0.0001
                        sh_len = "{0:.4f}".format(dataLine['SHLEN'])  # Recibe la longitud de la linea
                        opervoltLN = dataLine['TRAFVOLTLN']  # ,' !1ph_basekV=',opervoltLN
                        grupo_aco = dataLine['GRUPO']
                        lineName = "SRV" + cantFases + 'F' + circuitName + str(n)
                        text = '%s%s %s%s%s %s%s%s %s%s%s%s %s %s%s %s%s  %s\n' % (
                        'new line.', lineName, 'bus1=', busfrom, conns, 'bus2=', busto, conns, ' linecode=', equipment,
                        ' length=', sh_len, 'units=m', ' !1ph_basekV=', opervoltLN, ' Group=', grupo_aco, desc)
                        output_acodss.write(text)  # Escribe el string de salida en el archivo
                        dataLine["LAYER"].changeAttributeValue(dataLine["ID"], dataLine["INDEXDSS"], lineName)
                        n += 1
                    output_acodss.close()  # Cierra el archivo de salida
                    if len(selectedLayerACO1) != 0:
                        layerACO1.commitChanges()  # Guarda
                    if len(selectedLayerACO2) != 0:
                        layerACO2.commitChanges()  # Guarda
                    if len(selectedLayerACO3) != 0:
                        layerACO3.commitChanges()  # Guarda
                
                except:
                    self.print_error()
                    
            ##############################
            ###   FIN ESCRITURA ACOMETIDAS
            ##############################
            self.progress.progressBar.setValue(90)
            
            
            #nuevo
            #################
            ### Escritura Cargas MT
            #################
            write_loadshape_MT = False
            if (CARmvactive == True) and not errorLoadShape:
                try:
                    if folder_profile != "":
                        filename = circuitName + '_Loadshapes.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                    print("Escribiendo MV en salida azul")
                    filename = circuitName + '_LoadsMV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    #output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_cadss_mv = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerCA_MT1) != 0:
                        layerCAMV1.startEditing()  # Guarda
                    if len(selectedLayerCA_MT2) != 0:
                        layerCAMV2.startEditing()  # Guarda
                    if len(selectedLayerCA_MT3) != 0:
                        layerCAMV3.startEditing()  # Guarda
                    n = 0
                    self.progress.progressBar.setValue(90.5)
                    
                    for carga in grafoCAR_mv.nodes(data=True):
                                                                        
                        dataList = carga[1]
                        nodo = carga[0]
                        bus = str(dataList['BUS'])
                        kW = str(dataList['kW'])
                        conns = str( dataList['CONNS'] )
                        conf = str( dataList['CONF'] )
                        model = str( dataList['MODEL'] )
                        kWhmonth = str( dataList['kWh'] )
                        loadclass = str( dataList['class'] )
                        daily = str( dataList['CURVASIG'] )
                        Grupo = str( dataList['GRUPO'] )
                        kV = str( dataList['NOMVOLT'] )
                        cantFases = str( dataList['N_FASES'] )
                        pf = str( dataList['PowerFactor'] )
                        loadName = "MV_" + cantFases + 'F' + circuitName + str(n + 1)
                        loadName = str( loadName )
                        line = str("new load." + loadName + " bus1=" + bus + conns + " kV=" + kV + " model=" + model + " conn=" + conf)
                        line = str( line + " kW=" + kW + " pf=" +pf + " status=variable phases=" +  cantFases + " " + daily )
                        line = str( line + " !kWh="  + kWhmonth + " class=" + loadclass + " !Group=" + Grupo +  desc + "\n" )
                        output_cadss_mv.write(line)
                        dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], loadName)
                        n += 1
                    
                    output_cadss_mv.close()
                    if len(selectedLayerCA_MT1) != 0:
                        layerCAMV1.commitChanges()  # Guarda
                    if len(selectedLayerCA_MT2) != 0:
                        layerCAMV2.commitChanges()  # Guarda
                    if len(selectedLayerCA_MT3) != 0:
                        layerCAMV3.commitChanges()  # Guarda
                    
                    write_loadshape_MT = True
    
                except:
                    self.print_error()

            #################
            ### Escritura Cargas BT
            #################
            if (CARactive == True) and not errorLoadShape:
                try:
                    if folder_profile != "" and write_loadshape_MT == False: #write_loadshape_MT se utiliza para que no se escriba de nuevo el archivo de Loadshapes si ya fue escrito en las cargas de MT
                        filename = circuitName + '_Loadshapes.dss'
                        output_filesQGIS2DSS.write('\nredirect ' + filename)
                        
                    filename = circuitName + '_LoadsLV.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_cadss = open(foldername + '/' + filename, 'w')
                    if len(selectedLayerCA1) != 0:
                        layerCA1.startEditing()  # Guarda
                    if len(selectedLayerCA2) != 0:
                        layerCA2.startEditing()  # Guarda
                    if len(selectedLayerCA3) != 0:
                        layerCA3.startEditing()  # Guarda
                    n = 0
                    self.progress.progressBar.setValue(90.5)
                    for carga in grafoCAR.nodes(data=True):                                                
                        dataList = carga[1]
                        nodo = carga[0]
                        bus = str(dataList['BUS'])
                        kW = str(dataList['kW'])
                        conns = str( dataList['CONNS'] )
                        conf = str( dataList['CONF'] )
                        model = str( dataList['MODEL'] )
                        kWhmonth = str( dataList['kWh'] )
                        loadclass = str( dataList['class'] )
                        daily = str( dataList['CURVASIG'] )
                        Grupo = str( dataList['GRUPO'] )
                        kV = str( dataList['NOMVOLT'] )
                        
                        if dataList['TRAFNPHAS'] == "NULL":  # Si la carga no tiene algun transformador conectado se le asigna la cant de fases que dice en el shape
                            cantFases = "1"
                            desc = "disconnected"
                        else:
                            cantFases = dataList['TRAFNPHAS']
                            desc = ""
                        
                        if loadclass == "R":
                            kvar = str(0.1)
                        else:
                            kvar = str(0.3)
    
                        loadName = cantFases + 'F' + circuitName + str(n + 1)
                        loadName = str( loadName )
                        line = str("new load." + loadName + " bus1=" + bus + conns + " kV=" + kV + " model=" + model + " conn=" + conf)
                        line = str( line + " kW=" + kW + " kvar=" +kvar + " status=variable phases=" +  cantFases + " " + daily )
                        line = str( line + " !kWh="  + kWhmonth + " class=" + loadclass + " !Group=" + Grupo +  desc + "\n" )
                        output_cadss.write(line)
                        dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], loadName)
                        n += 1
                    output_cadss.close()
                    if len(selectedLayerCA1) != 0:
                        layerCA1.commitChanges()  # Guarda
                    if len(selectedLayerCA2) != 0:
                        layerCA2.commitChanges()  # Guarda
                    if len(selectedLayerCA3) != 0:
                        layerCA3.commitChanges()  # Guarda
    
                except:
                    self.print_error()
            ########################
            ### Fin escritura Cargas
            ########################

            ########################
            ### Escritura de GD
            ########################
            
            
            if len(selectedLayerGD) != 0:
                try:
                    filename = circuitName + '_Loadshapes.dss'
                    output_shpdss = open(foldername + '/' + filename, 'a+')
                    layerGD = QgsProject.instance().mapLayersByName(selectedLayerGD)[
                        0]  # Se selecciona la capa de la base de datos "layers" según el índice de layer_list
                    self.progress.progressBar.setValue(92)
                    filename = circuitName + '_DG.dss'
                    output_filesQGIS2DSS.write('\nredirect ' + filename)
                    output_GDdss = open(foldername + '/' + filename, 'w')
                    layerGD.startEditing()  # Guarda
                    if errorLoadShape or cargas == 0:
                        filenameloads = circuitName + '_Loadshapes.dss'
                        output_shpdss = open(foldername + '/' + filenameloads, 'w')
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
                                output_shpdss.write(
                                    'New XYCurve.MyPvsT npts=4 xarray=[.001 25 75 100] yarray=[1.2 1.0 0.8 0.6]\n')
                                output_shpdss.write(
                                    'New XYCurve.MyEff npts=4 xarray=[.1 .2 .4 1.0] yarray=[.86 .9 .93 .97]\n')
    
                            if CURVE1 not in shapewritten:
                                shapewritten[CURVE1] = 0
                                name1 = CURVE1.replace('.txt', '')
                                name1 = name1.replace('.dss', '')
                                output_shpdss.write(
                                    'New Loadshape.' + name1 + ' npts=96 minterval=15 csvfile=' + folder_profile + '\\DG\\' + CURVE1 + '\n')
                            if CURVE2 not in shapewritten:
                                shapewritten[CURVE2] = 0
                                name2 = CURVE2.replace('.txt', '')
                                name2 = name2.replace('.dss', '')
                                output_shpdss.write(
                                    'New Tshape.' + name2 + ' npts=96 minterval=15 csvfile=' + folder_profile + '\\DG\\' + CURVE2 + '\n')
                            pvname = "PV" + NPHAS + "F" + circuitName + str(n)
                            pvSentence = "New PVSystem." + pvname + " bus1=" + bus + conn + " kV=" + kV + " phases=" + NPHAS + " kVA=" + kVA + " PF=1 conn=" + conf + " irrad=0.90 Pmpp=" + kVA + " temperature=25 effcurve=Myeff P-TCurve=MyPvsT Daily=" + name1 + " TDaily=" + name2 + " %cutin=0.01 %cutout=0.01 enabled=yes \n"
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], pvname)
                            output_GDdss.write(pvSentence)
                            n += 1
                        else:
                            if (CURVE1, CURVE2) not in shapewritten:
                                shapewritten[(CURVE1, CURVE2)] = "curveDG" + str(i)
                                output_shpdss.write("New Loadshape.curveDG" + str(
                                    i) + " npts=96 minterval=15 mult=(file=" + folder_profile + '\\DG\\' + CURVE1 + ") Qmult=(file=" + folder_profile + '\\DG\\' + CURVE2 + ") useactual=no \n")
                                i += 1
                            GDName = "DG" + NPHAS + "F" + circuitName + str(n)
                            DGsentence = "New Generator." + GDName + " Bus1=" + bus + conn + " phases=" + NPHAS + " conn=" + conf + " kVA=" + kVA + " kV=" + kV + " kW=1 kVAR=1 Model=1 daily=" + \
                                         shapewritten[(CURVE1, CURVE2)] + " status=variable\n"
                            dataList["LAYER"].changeAttributeValue(dataList["ID"], dataList["INDEXDSS"], GDName)
                            output_GDdss.write(DGsentence)
                            n += 1
                    output_GDdss.close()
                    layerGD.commitChanges()
                
                
                except:
                    self.print_error()
            try:
                if (cargas > 0 and not errorLoadShape) or selectedLayerGD != 0:
                    pass
                    #output_shpdss.close()
            except:
                pass
                    
                    
            ########################
            ### Fin de escritura de GD
            ########################
            
            ########################
            ### Escritura de plantel de buses
            ########################
            writeBuses = False
            if selectedLayerBuses != "" and selectedLayerBuses != None and CARactive == True:
                busBT_List = self.WriteFileBuses(toler, grafoBuses, busBT_List)
                if busBT_List == 0: #caso en que hubo errores escribiendo los buses
                    return
                writeBuses = True
                
            
            #Se escribe el archivo de monitores
            if LTRactive == True or writeBuses == True:
                output_filesQGIS2DSS.write('\nredirect ' + filenameMon)
            
            
            ########################
            ### Escritura de VE
            ########################
            if name_layer_evs != "" and name_layer_evs != None and Buses_active == True:
                self.WriteFileEV(toler, grafoCAR, grafoEV)
            ########################
            ### Fin de escritura de VE
            ########################

            self.progress.progressBar.setValue(94)
            if len(busBT_List) > 0:  ###Buses de baja tensión con coordenadas
                try:
                    filename = circuitName + '_BusListLV.csv'
                    filename2 = circuitName + '_LV_KVBaseLN.dss'
                    # output_filesQGIS2DSS.write('\nBusCoords '+filename)
                    # output_filesQGIS2DSS.write('\nredirect '+filename2)
                    output_buslistbt = open(foldername + '/' + filename, 'w')
                    output_baseKV_DSS = open(foldername + '/' + filename2, 'w')
                    layerBTBusName = "Bus_BT_Layer"
                    attrs_BtBusLayer = ["BUS", "BASEKV_LN"]
                    layer = auxiliary_functions.newShape(layerBTBusName, attrs_BtBusLayer, "POINT", projPath)
                    layer.startEditing()
                    pr = layer.dataProvider()
                    index = layer.dataProvider().fieldNameIndex('BUS')
                    for bus in list(busBT_List.keys()):
                        busName = busBT_List[bus]['bus']
                        BTvoltage = busBT_List[bus]['VOLTAGELN']
                        line = '%s,%s,%s\n' % (busBT_List[bus]['bus'], busBT_List[bus]['X'], busBT_List[bus][
                            'Y'])  # Se usan las variables anteriores en formar el string de salida
                        output_buslistbt.write(line)  # Escribe el string de salida en el archivo
                        lineBase = "%s%s%s%s\n" % ("SetKVBase bus=", busName, " kvln=", BTvoltage)
                        output_baseKV_DSS.write(lineBase)
                        feat = QgsFeature(pr.fields())
                        feat.setGeometry(
                            QgsGeometry.fromPointXY(QgsPointXY(float(busBT_List[bus]['X']), float(busBT_List[bus]['Y']))))
                        feat["BUS"] = busName
                        feat["BASEKV_LN"] = BTvoltage
                        pr.addFeatures([feat])
                    output_buslistbt.close()  # Cierra el archivo de salida
                    output_baseKV_DSS.close()  # Cierra el archivo de salida
                    layer.updateExtents()
                    layer.commitChanges()
                
                except:
                    self.print_error()
                    
            
            output_filesQGIS2DSS.write( nombres_capas ) #se escriben los nombres de las capas de líneas MV, LV, y trafos
            output_filesQGIS2DSS.close()  # cierra el archivo con la lista de archivos creados
            
            starRevLinesDisc = time.time()
            self.progress.progressBar.setValue(97)
            ############  Revisión de líneas desconectas
            connected_components_MT = list(
                nx.connected_component_subgraphs(grafoMT))  # Determina cuales son los componentes conectados
            connected_components_BT = list(nx.connected_component_subgraphs(grafoBTTotal))
            connected_components_ACO = list(nx.connected_component_subgraphs(grafoACO))

            for i, graph in enumerate(connected_components_MT):
                if graph.number_of_edges() == 1:
                    for edge in list(graph.edges(data=True)):
                        aviso = QCoreApplication.translate('dialog',
                                                           u"Hay 1 segmento de línea MT  desconectado en: (") + str(
                            edge[2]['X1']) + ',' + str(edge[2]['Y1']) + ') ; (' + str(edge[2]['X2']) + ',' + str(
                            edge[2]['Y2']) + ')'
                        QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Líneas primarias"), aviso)

            for i, graph in enumerate(connected_components_BT):
                if graph.number_of_edges() == 1:
                    for edge in list(graph.edges(data=True)):
                        if (not Graph_T1F.has_node(edge[0])) and (not Graph_T2F.has_node(edge[0])) and (
                        not Graph_T3F_multi.has_node(edge[0])) and (not Graph_T3F_single.has_node(edge[0])) and (
                        not Graph_T1F.has_node(edge[1])) and (not Graph_T2F.has_node(edge[1])) and (
                        not Graph_T3F_multi.has_node(edge[1])) and (not Graph_T3F_single.has_node(edge[1])):
                            aviso = QCoreApplication.translate('dialog',
                                                               u"Hay 1 segmento de línea BT  desconectado en: (") + str(
                                edge[2]['X1']) + ',' + str(edge[2]['Y1']) + ') ; (' + str(edge[2]['X2']) + ',' + str(
                                edge[2]['Y2']) + ')'
                            QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Líneas secundarias"), aviso)
            endRevLinesDisc = time.time()

            if Error == True:
                self.iface.messageBar().pushCritical("QGIS2OpenDSS", QCoreApplication.translate('dialog',
                                                                                               u'Sucedió un error grave y  no fue posible completar la operación'))  # Aviso de finalizado en barra de QGIS
            else:

                self.iface.messageBar().pushInfo("QGIS2OpenDSS", QCoreApplication.translate('dialog',
                                                                                               'Finalizado. Los archivos fueron creados correctamente en') + ":\n" + foldername)  # Aviso de finalizado en barra de QGIS
            self.progress.close()
            finalTime = time.time()

            ##Copia las bibliotecas a la carpeta de salida
            try:
                bibPath = foldername + '/Bibliotecas'
                # self.mkdir_p(bibPath)
                pluginPath = str(os.path.dirname(os.path.abspath(inspect.stack()[0][1])))
                shutil.copytree(pluginPath + '/Bibliotecas', bibPath)
            except:
                #self.print_error()
                aviso = QCoreApplication.translate('dialog',
                                               u"No se exportó la biblioteca de conductores porque ya existe en la carpeta de salida.")
                
                print( aviso )
                #QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Biblioteca de cables"), aviso)

            ################# TIEMPOS
            if LMTactive == True:
                """
                aviso = QCoreApplication.translate('dialog',
                                                   u"Tiempo de conectividad de líneas de media tensión: ") + str(
                    -startTimeMT + endTimeMT) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if LBTactive == True:
                aviso = QCoreApplication.translate('dialog',
                                                   u"Tiempo de conectividad de líneas de baja tensión: ") + str(
                    -startTimeBT + endTimeBT) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if LTRactive == True:
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de transformadores: ") + str(
                    -startTimeTraf + endTimeTraf) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if (ACOactive == True):
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de líneas de acometidas: ") + str(
                    -startTimeAco + endTimeAco) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if SEactive == True:
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de la subestación: ") + str(
                    -startTimeSub + endTimeSub) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            if (CARactive == True):
                aviso = QCoreApplication.translate('dialog', u"Tiempo de conectividad de las cargas: ") + str(
                    -startTimeLoad + endTimeLoad) + QCoreApplication.translate('dialog', ' segundos')
                QMessageBox.warning(None, QCoreApplication.translate('dialog', u"Tiempo de ejecución"), aviso)
            """
            # print "El tiempo es: " + str(finalTime-startTime)
            # print "El tiempo de escritura fue "+ str(finalTime-startTimeWriting)
            
            try:
                archivo_errBTlog.close()
            except:
                pass
            
            try:
                archivo_errlog.close()
            except:
                pass
            
            try:
                archivo_errlogMT.close()
            except:
                pass
