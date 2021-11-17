# -*- encoding: utf-8 -*-
"""
Created on Thu Mar 12 10:55:01 2020

@author: Orlando Pereira
"""
import numpy as np
import datetime
import os
import sys
import traceback
from PyQt5.QtWidgets import QMessageBox

#from qgis.PyQt import QtGui, uic
from PyQt5 import QtWidgets

"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
Función Diferencia_factores

Parámetros de entrada:
*matriz_bus_cargador
*cargadores

Valores de salida:
*sorted_list
*sorted_vals

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""

def Diferencia_factores(matriz_bus_cargador, cargadores):

    sorted_vals = sorted(cargadores.iloc[:,1].astype(int), reverse=True)
    
    sorted_list = {}
    for val in sorted_vals:
        ordered_list= []
        for idx in range(len(matriz_bus_cargador.index)):   
            if matriz_bus_cargador.iloc[idx, 0] == val :
                ordered_list.append((idx, matriz_bus_cargador.index[idx], matriz_bus_cargador.iloc[idx, 5], matriz_bus_cargador.iloc[idx, 0],  matriz_bus_cargador.iloc[idx, 3]))
        
        sorted_tuple = sorted(ordered_list, key=lambda tup: tup[2])
        sorted_list[val] = [list(elem) for elem in sorted_tuple]
    
    return sorted_list, sorted_vals

"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
Función Asignacion_cargador_it

Parámetros de entrada:
*buses, sorted_list
*sorted_vals
*dict_disponibilidad
*matriz_bus_cargador
*trafo_kVA
*periodos_totales

Valores de salida:
No retorna nada

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""

def Asignacion_cargador_it(buses, sorted_list, sorted_vals,
                           dict_disponibilidad, matriz_bus_cargador,
                           trafo_kVA, periodos_totales, cargadores,
                           res_hour):
    import pandas as pd
    tiempos_cargador = pd.DataFrame(0,
                                    index=cargadores.iloc[:,1].astype(int),
                                    columns=['max', 'total'])
    
    for carg in range(len(cargadores)):
        tiempos_cargador.loc[int(cargadores.iloc[carg,1]), 'max'] = periodos_totales * cargadores.iloc[carg,3]
    
    check_buses = pd.DataFrame(0, index=list(range(0, len(buses)-1)), columns=['check'])
    
    for val in sorted_vals:
        
        for carg in range(len(cargadores)):
            if cargadores.iloc[carg,1].astype(int) == val:
                n_carg =  cargadores.iloc[carg,3].astype(int) #Número de cargadores
                
        
        for num in range(n_carg):
            p_val = [x for x in range(0,periodos_totales)] #lista de periodos disponibles para tal cargador
            for elem in range(len(sorted_list[val])):
                bus_id = int(sorted_list[val][elem][1].split('_')[1])
                if check_buses.loc[bus_id -1,'check'] == 0:
                    fin_t = int(dict_disponibilidad[bus_id][1])
                    bus_idx = int(sorted_list[val][elem][0])
                    assgn_carg = int(sorted_list[val][elem][3])
                    period_carg = int(sorted_list[val][elem][4])
                    ini_t = int(dict_disponibilidad[bus_id][0])
                    fin_carga = int(matriz_bus_cargador.iloc[bus_idx,3])
                    
                    if len(p_val) >= period_carg: 
                        flag = True  #marcador 
                        while (((ini_t not in p_val) or (fin_carga-1 not in p_val))) and (fin_carga <= fin_t):
                            flag = False
                            ini_t += 1
                            fin_carga += 1
                            if (ini_t in p_val) and (fin_carga-1 in p_val):
                                flag = True
                        ##################################################
                        #si el periodo de disponibilidad DEL BUS está dentro del rango de disponibIlidad del CARGADOR
                        if flag == True and (matriz_bus_cargador.iloc[:,9+ini_t:9+fin_carga].sum(axis=0) < trafo_kVA - assgn_carg).all():
                            #borrar datos del arreglo de disponibilidad
                            for x in range(ini_t, fin_carga):
                                try:
                                    p_val.remove(x)
                                except:
                                    pass
                            
                            #asigna los valores en la matriz_bus_cargador
                            matriz_bus_cargador.iloc[bus_idx,9+ini_t:9+fin_carga] = assgn_carg
                            check_buses.loc[bus_id-1,'check'] = 1
                                            
                        # ACTUALIZAR EL VECTOR DE TIEMPOS
                            tiempos_cargador.loc[int(sorted_list[val][elem][3]),'total'] += sorted_list[val][elem][4]
    
    if (check_buses.loc[:,'check'].sum(axis=0)) == len(buses)-1:
        try:
            #print('SÍ HAY SOLUCIÓN')
            #Eliminar las columnas innecesarias
            matriz_final = matriz_bus_cargador.drop(matriz_bus_cargador.columns[1:9], axis=1)
            #Eliminar las filas innecesarias
            del_idx = []
            
            for idx in range(len(matriz_bus_cargador.index)):
                if matriz_bus_cargador.iloc[idx, 9:periodos_totales+9].sum(axis=0) == 0:
                    del_idx.append(idx)
                    
            for label in del_idx:
                matriz_final2 = matriz_final.drop(matriz_final.index[del_idx])
                
            matriz_final2['kWh_cargar'] = list(buses.iloc[1:,5])
            matriz_final2['kWh_cargado'] = 0
            
            for idx2 in range(len(matriz_final2.index)):
                matriz_final2.iloc[idx2,periodos_totales+2] = matriz_final2.iloc[idx2,1:periodos_totales+1].sum(axis=0) * res_hour 
                
                matriz_final2 = matriz_final2.fillna(0)           
            #QMessageBox.information(None, "Exitoso", "Operación exitosa. Favor verique la solución en " + csv_name)
            return matriz_final2
        except:
            print_error()
            #QMessageBox.warning(None, "Exitoso", "Hubo un error al escribir la solución. Favor intente de nuevo.")
            return -1
        
    else:
        #QMessageBox.information(None, "Exitoso", "Operación exitosa, sin embargo con los datos provistos no fue posible encontrar una solución.")
        print('NO HAY SOLUCIÓN')
        return 0
       

"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
Función read_excel_file

Parámetros de entrada:
*excel_name (str): dirección del excel a abrir
*Trafo_kVA (int): kVA del transformador del plantel
*plantel_name (str): nombre del plantel al que corresponden estos datos
*foldername_out (str): directorio de salida para el csv
Valores de salida:
*0, 0 si finaliza exitosamente
Caso contrario:
*lista_cargadores
*lista_cap_kWh

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////            
"""

def read_excel_file(excel_name, plantel_name, Trafo_kVA, foldername_out):
    try:
        import pandas as pd
        Trafo_kVA = int( Trafo_kVA )
        real_path = os.path.dirname( excel_name )
        dfs = pd.read_excel(excel_name, sheet_name='Detallada', header=None)
        
        buses = dfs.iloc[:,0:6] #dataframe de solo buses
        cargadores = dfs.iloc[:,6:] #dataframe de solo cargadores
        
        to_del_b = []
        for idx in range(len(buses.index)-1,1,-1): 
            if np.isnan(buses.iloc[idx,1]) == True:
                to_del_b.append(buses.index[idx])
                
        to_del_c = []
        for idx in range(len(cargadores.index)-1,1,-1): 
            if np.isnan(cargadores.iloc[idx,1]) == True:
                to_del_c.append(cargadores.index[idx])
        for idx in range(len(cargadores.index)-1,1,-1):
            if cargadores.iloc[idx,3] == 0:
                to_del_c.append(cargadores.index[idx])
                
        
        buses = buses.drop(to_del_b) #buses purgados
        cargadores = cargadores.drop(to_del_c) #cargadores purgados
        ##################
        
        min_hora_llegada= min(buses.iloc[1:len(buses),2])
        max_hora_salida = max(buses.iloc[1:len(buses),3])
        
        if min_hora_llegada.hour > max_hora_salida.hour:
            d_1 = datetime.datetime(2020,1,1,min_hora_llegada.hour,min_hora_llegada.minute)
            d_2 = datetime.datetime(2020,1,2,max_hora_salida.hour,max_hora_salida.minute)
        
        else:
            d_1 = datetime.datetime(2020,1,1,max_hora_salida.hour,max_hora_salida.minute)
            d_2 = datetime.datetime(2020,1,1,min_hora_llegada.hour,min_hora_llegada.minute)
            
        timedelta = (d_2 -d_1)
        time_hours = timedelta.seconds/3600 #horas de disponibilidad máximas
        
        res_min = 15
        res_hour = res_min/60
        res_seconds = res_min*60
        
        periodos_totales = int(time_hours/res_hour) #Número de periodos a obtener
        
        #Disponibilidad de periodos por bus
        disponibilidad_bus = []
        dict_disponibilidad = {}
        
        for bus in range(len(buses)-1):
            hora_ini = buses.iloc[bus+1,2]
            hora_fin = buses.iloc[bus+1,3]
            
            if hora_ini > hora_fin:
                date_ini = datetime.datetime(2020,1,1,hora_ini.hour,hora_ini.minute)
                date_fin = datetime.datetime(2020,1,2,hora_fin.hour,hora_fin.minute)
            else:
                date_ini = datetime.datetime(2020,1,1,hora_ini.hour,hora_ini.minute)
                date_fin = datetime.datetime(2020,1,1,hora_fin.hour,hora_fin.minute)  
            
            disp_periodos = int(np.floor((date_fin - date_ini).seconds/3600/res_hour))
            
            p_1 = np.floor((date_ini - d_1).seconds/3600/res_hour)
            p_2 = np.floor((date_fin - d_1).seconds/3600/res_hour)
            
            dict_disponibilidad[int(bus)+1]= [int(p_1),int(p_2)-1]
            
            for carg in range(len(cargadores)):
                disponibilidad_bus.append(disp_periodos)
                
        lista_horas = [datetime.time((d_1 + datetime.timedelta(seconds = res_seconds*t)).hour,(d_1 + datetime.timedelta(seconds = res_seconds*t)).minute ) for t in range(periodos_totales)]
        
        lista_idx = []
        lista_cargadores = []
        lista_kWh = []
        
        for bus in range(len(buses)-1):
            for carg in range(len(cargadores)):
                lista_idx.append('bus_'+str(bus+1)+'_'+str(cargadores.iloc[carg,1].astype(int)))
                lista_cargadores.append(cargadores.iloc[carg,1])
                lista_kWh.append(buses.iloc[bus+1,5])
        
        matriz_bus_cargador = pd.DataFrame(index = lista_idx, columns = ['Cargadores', 'kWh_cargar','Tiempo',
                                                                        'Periodos', 'Disponibilidad', 'Factor_Tiempo',
                                                                        'Prioridad_Local', 'Prioridad_Global',
                                                                        'Uso_Cargador'] + lista_horas)
        matriz_bus_cargador.loc[:,'Cargadores'] = lista_cargadores
        matriz_bus_cargador.loc[:,'kWh_cargar'] = lista_kWh
        matriz_bus_cargador.loc[:,'Tiempo'] = np.divide(lista_kWh, lista_cargadores)
        matriz_bus_cargador.loc[:,'Periodos'] = np.ceil(np.divide(lista_kWh, lista_cargadores)/res_hour)
        matriz_bus_cargador.loc[:,'Disponibilidad'] = disponibilidad_bus
        matriz_bus_cargador.loc[:,'Factor_Tiempo'] = np.divide(disponibilidad_bus,np.ceil(np.divide(lista_kWh, lista_cargadores)/res_hour))
        
        #Distribución de buses
        
        
        for bus in range(len(buses)-1):
            for carg in range(len(cargadores)):
                
                if matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 3] > matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 4]:
                    matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 8:] = 0
        
        # Prioridad local
                    
        for bus in range(len(buses)-1):
            counter = 1
            
            for carg in range(len(cargadores)):
                
                if matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 8] != 0:
                    matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 6] = counter
                    counter+=1
                else:
                    matriz_bus_cargador.iloc[len(cargadores) * bus + carg, 6] = 0
        
        sorted_list, sorted_vals = Diferencia_factores(matriz_bus_cargador, cargadores)
        matriz_final = Asignacion_cargador_it(buses, sorted_list, sorted_vals, dict_disponibilidad, matriz_bus_cargador, Trafo_kVA, periodos_totales, cargadores, res_hour)
        lista_cargadores, lista_cap_kWh, loadshapes_path = loadshape_buses(matriz_final, buses, foldername_out, res_seconds, d_1, plantel_name)
        
        if lista_cargadores == -1:
            #QMessageBox.warning(None, "Error", "Ocurrió un error al crear el csv con los loadshapes de buses eléctricos, por lo que la simulación no será exitosa.")
            return 0, 0, 0
        
        return lista_cargadores, lista_cap_kWh, loadshapes_path
        
    except FileNotFoundError:
        folder = os.path.dirname(excel_name)
        mensaje = "Favor verifique que todos los archivos de Excel de información de buses fueron proporcionados."
        mensaje += "\nDeben estar ubicados en: " + folder + "\nDebe ser un archivo xlsx con el siguiente formato: data_buses_nombredelplantel.xlsx"
        mensaje += "\nDonde nombredelplantel debe ser exactamente el mismo que se indica en la tabla de atributos del plantel en el atributo PLANTEL"
        QMessageBox.critical(None, "Error lectura de archivos buses eléctricos", mensaje)
        print_error()
        return 0, 0, 0
    except ImportError:
        print_error()
        QMessageBox.critical(None, "Error", "Ocurrió un error al importar ciertas librerías. Por favor instale PyQt5 y xlrd.\nPara más detalles verifique el error en la consola")
        return 0, 0, 0
    except:
        print_error()
        QMessageBox.critical(None, "Error", "Ocurrió un error al leer el archivo " + excel_name + ". Por favor verifique que tenga la hoja con nombre 'Detallada' e intente nuevamente.\nPara más detalles verifique el error en la consola")
        return 0, 0, 0
        

"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

Función print_error
Función encargada de imprimir en consola los detalles de un error cuando se 
presente alguno. No tiene parámetros de entrada ni de salida.
        
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""

def print_error():
    exc_info = sys.exc_info()
    print("\nError: ", exc_info)
    print("*************************  Información detallada del error ********************")
    for tb in traceback.format_tb(sys.exc_info()[2]):
        print(tb)
        
#%% CREACIÓN DE LOADSHAPES
"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

Función loadshape_buses
Se encarga de crear el csv con los loadshapes de buses

Parámetros de entrada:
*matriz_final
*buses
*foldername_out (str): directorio de salida para el csv
*res_seconds
*d_1
*plantel_name (str): nombre del plantel

Valores retornados:
*lista_cargadores
*lista_cap_kWh
        
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""

def loadshape_buses(matriz_final, buses, foldername_out, res_seconds, d_1, plantel_name):
    try:
        import pandas as pd
        d_base = datetime.datetime(2020,1,1,0,0)
        p_base = 96
        
        lista_horas = [datetime.time((d_base + datetime.timedelta(seconds = res_seconds*t)).hour,(d_1 + datetime.timedelta(seconds = res_seconds*t)).minute ) for t in range(p_base)]
        lshape_df = pd.DataFrame(0, index = lista_horas, columns = matriz_final.index)
        
        for t in lista_horas:
            if t in matriz_final.columns:
                lshape_df.loc[t,:] = matriz_final.loc[:,t]
        
        #LOADSHAPES
        lshape_df = lshape_df.where(lshape_df < 1, -1)
        loadshapes_path = foldername_out + "/LoadshapesBuses_" + plantel_name + ".csv"
        lshape_df.to_csv(loadshapes_path, index = False, header = None)
        
        #LISTA CARGADORES Y CAPACIDADES CORRESPONDIENTES (ESTÁN EN ORDEN COMO SE DAN EN EL EXCEL)
        lista_cargadores = list(matriz_final.loc[:,'Cargadores'].astype(int))
        lista_cap_kWh = list(buses.iloc[1:,1])
        
        loadshape = "LoadshapesBuses_" + plantel_name + ".csv"
        
        return lista_cargadores, lista_cap_kWh, loadshape
    except:
        print_error()
        mensaje = "Hubo un error desconocido en la lectura de los loadshapes de buses eléctricos. Favor intente de nuevo."
        QMessageBox.critical(None, "Error lectura de archivos buses eléctricos", mensaje)
        return -1, -1, -1
        
"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

Función excel_to_dict
Se encarga de convertir los datos a diccionarios
Parámetros de entrada:
*path (str): dirección del Excel a leer
*plantel_name (str): nombre del plantel
*Trafo_kVA (int): valor de los kVA del trafo.
*foldername_out (str): directorio de salida para el csv
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""

def excel_to_dict(path, plantel_name, Trafo_kVA, foldername_out):
    try:
        import pandas as pd
        lista_cargadores, lista_cap_kWh, loadshapes_path = read_excel_file(path, plantel_name, Trafo_kVA, foldername_out)
        if lista_cargadores == 0:
            return 0
        pd_ = pd.read_excel(path, sheet_name="Detallada")
        columnas = list(pd_.columns)
        i = 0
        dict_buses = {}
        for columna in columnas:
            if columna == "Porcentaje típico del estado de carga al final del día":
                for k in range(i+1, len(columnas) ):
                    pd_ = pd_.drop(columns=columnas[k], errors='raise')
                break
            i += 1
        
        pd_ = pd_.drop(columns='Hora en que las unidades llegan al plantel de buses', errors='ignore')
        pd_ = pd_.drop(columns='Hora en que las unidades salen del plantel de buses', errors='ignore')
        pd_ = pd_.dropna(how='any', axis=0)
        
        i = 0
        for fila in pd_.iterrows():
            valor = fila[1]
            soci = valor.loc['Porcentaje típico del estado de carga al final del día']
            kwh = valor.loc['Capacidad nominal en kWh de batería']
            dict_buses[i] = {'KWHBATTERY': kwh, 'SOCi': soci, 'KW': lista_cargadores[i], 'CSV_NAME': loadshapes_path}
            i += 1
        
        return dict_buses
    
    except:
        print_error()
        return 0
                    

	

"""
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

Función principal
Se encarga de abrir el cuadro de diálogo para seleccionar el archivo de
Excel y llamar a la función read_excel_file
        
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
"""
        
if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv) 
    name = "C:/Users/bjza0/Documents/Proyectos_actuales/ves_baterias_digitalizacion/Optimizacion_buses/data_buses_Guadalupe.xlsx"
    plantel_name = "Guadalupe"
    out = "C:/Users/bjza0/Documents/Proyectos_actuales/ves_baterias_digitalizacion/Optimizacion_buses/AEBs/"
    trafo_kva = 1000
    excel_to_dict( name, plantel_name, trafo_kva, out )
    
    
    
    """
    app = QtWidgets.QApplication(sys.argv) 
    opt = GUI_Optimizacion()
    opt.dlg.show()
    result = opt.dlg.exec_()
    path = opt.dlg.lineEdit_ExcelFile.text()
    kva = opt.dlg.kVA.value()
    plantel_name = "guada"
    
    #path, _ = QFileDialog.getOpenFileName( None, "Seleccione archivo con los datos de buses", "", "Archivos de Excel (*.xlsx *.xlsx *.xlsm *.xltx *.xltm)" ) 
    if path == "":
        QMessageBox.critical(None, "Error", "Favor introduzca un directorio válido")
        
    #Se ejecuta si se presionó ok
    if result:        
        if kva == 0:
            QMessageBox.warning(None, "Advertencia", "No asignó el valor de kVA del trafo, se hará con un valor de 1000 kVA")
            read_excel_file(path, plantel_name)
        else:
            read_excel_file(path, plantel_name, kva)
            
    """
        
    
    
###############################################################################            
