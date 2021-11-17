# -*- coding: utf-8 -*-
"""
Created on Sat Aug  3 18:01:11 2019

@author: ogpg0_000
"""

#%% PAQUETES

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture
import scipy

#%% LECTURA DEL DOCUMENTO

old_data = pd.read_csv('ev_cr_data.csv')


#%% PROCESANDO LOS DATOS

new_data = pd.DataFrame(index = old_data.index, columns= (list(old_data.columns) +['duracion'])).drop(columns = ['Timestamp', 'Nombre'])

#Tipo de vehículo
new_data['Tipo'] = old_data['Tipo'].copy()

#Capacidad de la batería

for element in list(old_data.index):
    if (old_data.loc[element]['Tamano_Bateria'][0:3].isdigit()) == True:
        new_data.loc[element]['Tamano_Bateria'] = int(old_data.loc[element]['Tamano_Bateria'][0:3])
    else:
        if (old_data.loc[element]['Tamano_Bateria'][0:2].isdigit()) == True:
            new_data.loc[element]['Tamano_Bateria'] = int(old_data.loc[element]['Tamano_Bateria'][0:2])
        
        elif (old_data.loc[element]['Tamano_Bateria'][0:1].isdigit()) == True:
            new_data.loc[element]['Tamano_Bateria'] = int(old_data.loc[element]['Tamano_Bateria'][0:1])
        
# Potencia

for element in list(old_data.index):
    if (old_data.loc[element]['Potencia'][0].isdigit()) == True:
        new_data.loc[element]['Potencia'] = float(old_data.loc[element]['Potencia'][:-3])
        
# Día de conexión

wd = ['Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes']
we = ['Sábado', 'Domingo']

for element in list(old_data.index):
    if old_data.loc[element]['Dia_Conexion'] in wd:
        new_data.loc[element]['Dia_Conexion'] = 1
    else:
        new_data.loc[element]['Dia_Conexion'] = 2
        
# Hora de conexión

for element in list(old_data.index):
    hora = old_data.loc[element]['Hora_conexion'][:2]
    minutos = old_data.loc[element]['Hora_conexion'][-2:]
    
    # determinar el número de iteración correspondiente a la hora
    it_num = int(hora)*4 + int(int(minutos)/15)
    new_data.loc[element]['Hora_conexion'] = it_num

# Hora de desconexión

for element in list(old_data.index):
    hora = old_data.loc[element]['Hora_desconexion'][:2]
    minutos = old_data.loc[element]['Hora_desconexion'][-2:]
    
    # determinar el número de iteración correspondiente a la hora
    it_num = int(hora)*4 + int(int(minutos)/15)
    new_data.loc[element]['Hora_desconexion'] = it_num

# Duración de la carga

for element in list(old_data.index):
    #Conexión
    hora_con = old_data.loc[element]['Hora_conexion'][:2]
    hora_con = int(hora_con)
    min_con = old_data.loc[element]['Hora_conexion'][-2:]
    min_con = int(min_con)
    #Desconexión
    hora_des = old_data.loc[element]['Hora_desconexion'][:2]
    hora_des = int(hora_des)
    min_des = old_data.loc[element]['Hora_desconexion'][-2:]
    min_des = int(min_des)
    
    if hora_des < hora_con:
        min_dur = (24 - hora_con + hora_des)*60 + (60-min_con)%60 + min_des
    
    elif hora_des > hora_con:
        min_dur = (hora_des-hora_con)*60 + (60-min_con)%60 + min_des
    
    elif hora_des == hora_con:
        min_dur = min_des - min_con
    
    new_data.loc[element]['duracion'] = min_dur

new_data['SoC_ini'] = old_data['SoC_ini'].copy()
new_data['SoC_final'] = old_data['SoC_final'].copy()

#%% MODELOS GAUSSIANOS

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
    

#Cálculo de las funciones de densidad

def calculo_GMM(data, power_val, kwh_val):
    num_power_vals = len(power_val)
    num_kwh_vals = len(kwh_val)
    final_dict = {} #diccionario con toda la información total

    i=0
    
    for data_type in data:
        final_dict[data_type] = {} #división de la división por el tipo de información (hora conexion, duracion, SOC)
        i = i+1 #Número de figura o de tipo de dato analizado
        X=np.expand_dims(data[data_type],1)
        
        n_components = np.arange(1, 4)
        
#        if data_type != 'charge_demand':
#            n_components = np.arange(1, 4)
#        else:
#            n_components = np.arange(1, num_power_vals-1)
        
        models = [GaussianMixture(n).fit(X) for n in n_components]
        #chi_2[rep][data_type] = []
        
        if data_type == 'time': 
            gmm_x = np.linspace(0,95,96)
            bins = range(0,97)
            
        elif data_type == 'duration':
            gmm_x = np.linspace(min(data[data_type]),max(data[data_type]),max(data[data_type])-min(data[data_type])+1)
            bins = range(min(data[data_type]), max(data[data_type])+2)
        
        elif data_type == 'SoC':
            gmm_x = np.linspace(0,100,101)
            bins = range(0,102)
        
        elif data_type == 'charge_demand':
            gmm_x = np.linspace(1,num_power_vals,num_power_vals)
            bins = range(1,num_power_vals+2)
        
        elif data_type == 'battery_cap':
            gmm_x = np.linspace(1,num_kwh_vals,num_kwh_vals)
            bins = range(1,num_kwh_vals+2)
            
        
        final_dict[data_type]['gmm'] = {}
        
        final_dict[data_type]['gmm']['valores'] = {}
        final_dict[data_type]['gmm']['valores'] = [np.exp(models[n].score_samples(gmm_x.reshape(-1,1))) for n in range(len(models))]
        
        final_dict[data_type]['gmm']['parametros'] = {}
        final_dict[data_type]['gmm']['parametros']['weight'] = [models[n].weights_ for n in range(len(models))]
        final_dict[data_type]['gmm']['parametros']['mean'] = [models[n].means_ for n in range(len(models))]
        final_dict[data_type]['gmm']['parametros']['covariance'] = [models[n].covariances_ for n in range(len(models))]
        
        # HISTOGRAM MAKE UP
        
        final_dict[data_type]['histogram'] = {}
        
        plt.figure(i)
        final_dict[data_type]['histogram'] = plt.hist(data[data_type], bins, density= True)
        
        for n in range(len(final_dict[data_type]['gmm']['valores'])):
            plt.plot(gmm_x, final_dict[data_type]['gmm']['valores'][n], color="crimson", lw=1, label="GMM"+str(n+1))
            plt.legend()
        
        #Chisquare
        
        final_dict[data_type]['chi_2'] = {}
        final_dict[data_type]['chi_2'] = [scipy.stats.chisquare(final_dict[data_type]['histogram'][0],final_dict[data_type]['gmm']['valores'][n])[0] for n in range(len(final_dict[data_type]['gmm']['valores']))]

    
    return(final_dict)

general_dict = calculo_GMM(sample_gnal, power_val, kwh_val)
weekday_dict = calculo_GMM(sample_wd, power_val, kwh_val)
weekend_dict = calculo_GMM(sample_we, power_val, kwh_val)

#%%
events_ev_disp = {}
events_ev_disp['weekday'] = [1]
events_ev_disp['weekend'] = [1]

import Random_Events
total_cars = 20
random_results, _, _ = Random_Events.Random_Events(total_cars, events_ev_disp, weekday_dict, weekend_dict, general_dict, power_val, kwh_val, 10, 3)


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
            
#%%            
#PRUEBA
#for ev in random_results:        
#    np.savetxt(r'ev_curve'+str(ev)+'.txt', evs_loadshapes['curve_'+str(ev)].values, fmt='%d')             
