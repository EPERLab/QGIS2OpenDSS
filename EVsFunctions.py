# -*- coding: utf-8 -*-
#%% PAQUETES
import sys
import traceback
import numpy as np
from . import Random_Events
#import Random_Events
from PyQt5.QtWidgets import QMessageBox

"""
Esta función cambia el valor del SOC del VE cuando se da una carga a medianoche
Parámetros de entrada:
*DSScircuit (): circuito eléctrico simulado
*diccionario_EVs (dict): diccionario de DSS_Names, t y SOC_f
*t (int)

Valores retornados:
DSScircuit (): circuito eléctrico simulado actualizado con la SOC de los VEs actualizada
"""

def update_storage(DSScircuit, diccionario_EVs, t):    
    if len(diccionario_EVs[int(t)]) > 0:
        for ev in diccionario_EVs[t]:            
            DSScircuit.setActiveElement('storage.'+ev)
            DSScircuit.ActiveElement.Properties('%stored').val = diccionario_EVs[t][ev]    
    return(DSScircuit)


"""
Esta función se encarga de crear un diccionario de los t y DSSNames de los VE. Este t representa el momento en que 
se empieza a cargar el VE después de medianoche. En caso de no suceder, el t es -1.
Para lograr obtener este diccionario se debe leer un dss con la información del VE. Para ello se parte del hecho de que las líneas de dicho archivo tendrán el siguiente formato:
new storage.ve_1FFLX35 bus1=BUSLVFLX384.1.2 phases=1 model=1 kW=3.6 kV=0.208 pf=0.98 kWrated=3.6 kWhrated=24.0 %reserve=0 %stored=15.0 %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW daily=EV_6 !t=-1
new storage.ve_1FFLX102 bus1=BUSLVFLX398.1.2 phases=1 model=1 kW=7.2 kV=0.208 pf=0.98 kWrated=7.2 kWhrated=18.0 %reserve=0 %stored=34.0 %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW daily=EV_18 !stored=18.0 !t=94

Parámetros de entrada:
*path_file (string): dirección del archivo dss que se quiere leer
*vector_soc_t (dict): diccionario al que se agregará la información referente a los VE presentes en el archivo dss

Valores retornados:
*vector_soc_t (dict): diccionario actualizado, agregando los datos de los VE presentes en el dss.
"""
def CreateList_SOC_t( path_file, vector_soc_t ):
    try:
        with open( path_file ) as f:
            lineList = f.readlines()		
        for line in lineList:
			#Búsqueda de t
            in_t = line.find("!t=")
            in_t += len("!t=")
            fin_t = line.find(" ", in_t)
            if fin_t == -1: #Caso en que no haya encontrado un espacio después del t buscará un cambio de líne
                fin_t = line.find("\n",in_t)
                if fin_t == -1: #Caso en que no haya encontrado un cambio de línea ni un espacio significa que es el elemento final
                    fin_t = in_t + 2
			
            t = line[in_t:fin_t]
            t = int( t )
            
            if t != -1: #si la carga empieza antes de medianoche
        
                #Búsqueda del dssname
                in_dss = line.find("storage.")
                in_dss += len("storage.")
                fin_dss = line.find(" ", in_dss)
                dss_name = line[in_dss:fin_dss]
                dss_name = str( dss_name )
                
                #Búsqueda del soc_f (el que es después de medianoche)
                in_soc_f = line.find("!stored=")
                in_soc_f += len("!stored=")
                fin_soc_f = line.find(" ", in_soc_f)
                soc_f = line[in_soc_f:fin_soc_f]                
                vector_soc_t[t][dss_name] = soc_f
                
        return vector_soc_t
			
    except:
        exc_info = sys.exc_info()
        print("\nError: ", exc_info )
        print("*************************  Información detallada del error ********************")
        for tb in traceback.format_tb(sys.exc_info()[2]):
            print(tb)
        mensaje = str( "Hubo un error al cambiar el orden de los SOC de VE.\nPara más información revise el código de error en la consola.")
        QMessageBox.critical(None,"QGIS2OpenDSS escritura VE", mensaje)
		
        return 0

    
#%%
"""
########################################################################
########################################################################
######################  CreacionPerfilesEV  ############################
########################################################################
########################################################################

Función encargada de obtener ciertos valores con base en las datos suministrados en 
las encuestas. Utiliza una función de probabilidad para obtener los datos de forma 
"aleatoria".

#Parámetros de entrada:
*sample_wd
*sample_we
*sample_gnal
*power_val
*kwh_val
*total_cars (int): cantidad de carros para los que se obtiene un valor.

#Valores retornados:
*evs_loadshapes (dataframe): representa...
*evs_power (dataframe): representa los kW aleatorios de los VEs. Contiene una cantidad de total_cars elementos
*evs_initial_soc (dataframe): dataframe que representa el estado de carga inicial de los VEs. Contiene una cantidad de ... elementos
*evs_kwh (dataframe): representa los kW aleatorios de los VEs. Contiene una cantidad de total_cars elementos
"""
def CreacionPerfilesEV( power_val, kwh_val, total_cars = 100 ):
    import pandas as pd
    
    """
    general_dict = calculo_GMM(sample_gnal, power_val, kwh_val)
    weekday_dict = calculo_GMM(sample_wd, power_val, kwh_val)
    weekend_dict = calculo_GMM(sample_we, power_val, kwh_val)
    """
    
    events_ev_disp = {}
    events_ev_disp['weekday'] = [1]
    events_ev_disp['weekend'] = [1]
    
    random_results, _, _ = Random_Events.Random_Events( total_cars, events_ev_disp, power_val, kwh_val, 10, 3 )
    #random_results, _, _ = Random_Events.Random_Events( total_cars, events_ev_disp, weekday_dict, weekend_dict, general_dict, power_val, kwh_val, 10, 3 )
    
    #Crear un pandas para luego pasar a un txt
    
    evs_loadshapes = pd.DataFrame(0, index=[x for x in range(0,96)], columns=['curve_'+str(x) for x in range(1,total_cars+1)])
    evs_initial_soc = pd.DataFrame(np.nan, index=[x for x in range(0,2)], columns=['soc_ev_'+str(x) for x in range(1,total_cars+1)])
    evs_power = pd.DataFrame(0, index=[x for x in range(0,1)], columns=['pow_ev_'+str(x) for x in range(1,total_cars+1)])
    evs_kwh = pd.DataFrame(0, index=[x for x in range(0,1)], columns=['kwh_ev_'+str(x) for x in range(1,total_cars+1)])
    
    for ev in random_results:
        if len(random_results[ev]['av_start']) > 0:
            for i in range(len(random_results[ev]['av_start'])):
                ini = random_results[ev]['av_start'][i]
                fin = ini + random_results[ev]['duration'][i]
                
                for j in range(ini, fin+1):
                    evs_loadshapes['curve_'+str(ev)].loc[j] = -1
                    
    for ev in random_results:
        if len(random_results[ev]['SOC']) > 0:
            for i in range(len(random_results[ev]['SOC'])):
                soc_val = random_results[ev]['SOC'][i]
                evs_initial_soc['soc_ev_'+str(ev)].loc[i] = soc_val
    
    for ev in random_results:
        if len(random_results[ev]['charge_demand']) > 0:
            pow_val = random_results[ev]['charge_demand'][0]
            evs_power['pow_ev_'+str(ev)].loc[0] = pow_val
    
    for ev in random_results:
        if len(random_results[ev]['battery_cap']) > 0:
            kWh_val = random_results[ev]['battery_cap'][0]
            evs_kwh['kwh_ev_'+str(ev)].loc[0] = kWh_val
    
    return evs_loadshapes, evs_power, evs_initial_soc, evs_kwh

def AnalizarEncuestas(var = True):
    import os
    import pandas as pd
    path = os.path.dirname(os.path.abspath(__file__))
    dir_file = str( path + "/data.csv" )
    print( "dir_file EVSfuncion = ", dir_file )
    new_data = pd.read_csv( dir_file )
    
    #Diferenciar los datos por "entre semana" y en "fines de semana"
    
    #PRIMERO, REALIZAR UN CÓDIGO AUTOMÁTICO PARA LAS POTENCIAS
    
    power_list = list(new_data['Potencia'])
    power_val = list(set(power_list))
    power_val.remove(power_val[0])
    
    power_code = [x+1 for x in range(len(power_val))] #Código para cada valor de potencia
    
    kwh_list = list(new_data['Tamano_Bateria'])
    kwh_val = list(set(kwh_list))
    kwh_val.remove(kwh_val[0])
    
    kwh_code = [x+1 for x in range(len(kwh_val))] #Código para cada valor de potencia
    
    #GENERAL CASE
    sample_gnal = {}
    sample_gnal['charge_demand'] = []
    sample_gnal['battery_cap'] = []
    
    #ENTRE SEMANA
    sample_wd = {}
    sample_wd['time'] = []
    sample_wd['duration'] = []
    sample_wd['SoC'] = []
    
    #FIN DE SEMANA
    sample_we = {}
    sample_we['time'] = []
    sample_we['duration'] = []
    sample_we['SoC'] = []
    
    for element in list(new_data.index):
        if np.isnan(new_data.loc[element]['Tamano_Bateria']) == False:
            for val in range(len(kwh_val)):
                if new_data.loc[element]['Tamano_Bateria'] == kwh_val[val]:
                    sample_gnal['battery_cap'].append(val+1)
        
        if np.isnan(new_data.loc[element]['Potencia']) == False:
            for val in range(len(power_val)):
                if new_data.loc[element]['Potencia'] == power_val[val]:
                    sample_gnal['charge_demand'].append(val+1)
        
        if new_data.loc[element]['Dia_Conexion'] == 1:
            sample_wd['time'].append(new_data.loc[element]['Hora_conexion'])
            sample_wd['duration'].append(new_data.loc[element]['duracion'])
            sample_wd['SoC'].append(new_data.loc[element]['SoC_ini'])
                
        else:
            sample_we['time'].append(new_data.loc[element]['Hora_conexion'])
            sample_we['duration'].append(new_data.loc[element]['duracion'])
            sample_we['SoC'].append(new_data.loc[element]['SoC_ini'])
    
    return sample_wd, sample_we, sample_gnal, power_val, kwh_val

                
#%%            
#PRUEBA
#for ev in random_results:        
#    np.savetxt(r'ev_curve'+str(ev)+'.txt', evs_loadshapes['curve_'+str(ev)].values, fmt='%d')

if __name__ == '__main__':
    sample_wd, sample_we, sample_gnal, power_val, kwh_val = AnalizarEncuestas()
    evs_loadshapes, evs_power, evs_initial_soc, evs_kwh = CreacionPerfilesEV(power_val, kwh_val)
    print( evs_loadshapes )
    
