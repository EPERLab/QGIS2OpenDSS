# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 20:02:10 2019

@author: ogpg0_000
"""

import math
import numpy as np
import random
import csv
import calendar
import time
import datetime
import random
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib import patches
import os

#%%
def Prob_Make_up(y):
    while (float(format(sum(y),'.8f')) > 1.00000001) or (float(format(sum(y),'.8f')) < 0.99999999):
        if sum(y) < 1:
            ajust = (1-sum(y))/len(y)
            y = y + ajust*np.ones(len(y))
        else:
            ajust = (sum(y)-1)/len(y)
            y = y - ajust*np.ones(len(y))
    return y

def car_selection_day_1(total_cars, av_cars_day_1): #RANDOM SELECTION OF CARS WITH THE RESPECTIVE AV PER DAY
        import pandas as pd
        av_cars_sel = random.sample(set(range(total_cars)),av_cars_day_1) #select randomlya number of cars depending on the probability 
        av_cars_list = total_cars*[0] # 0 is not available
        if len(av_cars_sel) > 0:
            for i in range(len(av_cars_sel)):
                av_cars_list[av_cars_sel[i]] = 1 # 1 is available
        
        return(av_cars_list)
        

#This function convert a csv into a numpy vector
def csv_numpy( csv_name, nombre_columna ):
	import pandas as pd
	dataframe = pd.read_csv( csv_name )
	dato_real = dataframe[ nombre_columna ]
	my_data = dato_real.to_numpy()
	return my_data

def Random_Events(total_cars, events_ev_disp, power_val, kwh_val, fixed_day_prob, fixed_day):
	
    dir_path = os.path.dirname(os.path.realpath(__file__))
    print( "dir_path = ", dir_path )
    
    
    dato1 = csv_numpy( dir_path + "/Probabilidades/dato1.csv", "Valor")
    dato2 = csv_numpy( dir_path + "/Probabilidades/dato2.csv", "Valor")
    dato3 = csv_numpy( dir_path + "/Probabilidades/dato3.csv", "Valor")
    dato4 = csv_numpy( dir_path + "/Probabilidades/dato4.csv", "Valor")
    dato5 = csv_numpy( dir_path + "/Probabilidades/dato5.csv", "Valor")
    dato6 = csv_numpy( dir_path + "/Probabilidades/dato6.csv", "Valor")
    dato7 = csv_numpy( dir_path + "/Probabilidades/dato7.csv", "Valor")
    dato8 = csv_numpy( dir_path + "/Probabilidades/dato8.csv", "Valor")
    dato9 = csv_numpy( dir_path + "/Probabilidades/dato9.csv", "Valor")
    dato9 = dato9[0]
    dato11 = csv_numpy( dir_path + "/Probabilidades/dato11.csv", "Valor")
    dato11 = dato11[0]
    
    """
    Original
    dato1 = weekday_dict['time']['gmm']['valores'][2]
    dato2 = weekend_dict['time']['gmm']['valores'][1]
    dato3 = weekday_dict['SoC']['gmm']['valores'][2]
    dato4 = weekend_dict['SoC']['gmm']['valores'][2]
    dato5 = weekday_dict['duration']['gmm']['valores'][1]
    dato6 = weekend_dict['duration']['gmm']['valores'][2]
    dato7 = general_dict['charge_demand']['histogram'][0]
    dato8 = general_dict['battery_cap']['histogram'][0]
    dato9 = weekday_dict['duration']['histogram'][1][0]
    dato11 = weekend_dict['duration']['histogram'][1][0]
    """
    
    #Availability start time probabilities
    wd_prob_av_start_1 = Prob_Make_up( dato1 )

    we_prob_av_start_1 = Prob_Make_up( dato2 )

    
    #Availability initial SOC probabilities
    wd_prob_SOC_1 = Prob_Make_up( dato3 )
    we_prob_SOC_1 = Prob_Make_up( dato4 )
    
    #Availability duration probabilities
    wd_prob_duration_1 = Prob_Make_up( dato5 )
    we_prob_duration_1 = Prob_Make_up( dato6 )
    
    #Availability duration probabilities
    w_prob_power_1 = Prob_Make_up( dato7 ) #POR MIENTRAS SE CONSIGUEN LOS DATOS
    w_prob_kwh_1 = Prob_Make_up( dato8 ) #POR MIENTRAS SE CONSIGUEN LOS DATOS

    
########################################################################################################################    
    
    #Here we go:
    prev_av_random_events = {}
    
    # day of the week: 0 if it is monday and 6 if it is sunday
    if fixed_day == False:
        day_1 = np.random.choice(range(0,7))
    else:
        day_1 = fixed_day
    
    # PROBABILITY DEFINITION PER DAY
    prob_day_1 = fixed_day_prob
    
    if day_1 < 5:
        av_cars_day_1 = int(np.round(total_cars*prob_day_1*10/100))
        av_car_list_day_1 = car_selection_day_1(total_cars,av_cars_day_1) # Random selection of cars

    else:
        av_cars_day_1 = int(np.round(total_cars*prob_day_1*10/100))
        av_car_list_day_1 = car_selection_day_1(total_cars,av_cars_day_1) # Random selection of cars

    for i in range(1,total_cars+1):
        prev_av_random_events[i] = {'reps_per_day':[],'av_start':[],'SOC':[],'duration':[], 'charge_demand':[], 'battery_cap':[]}
        day_before_const = total_cars*[0] # 
    
    #Number of availability events per day    
    
    for i in range(total_cars):
        ######################################################  day 1 ###################################################### 
        if av_car_list_day_1[i] == 1:
            if day_1 < 5:
                prev_av_random_events[i+1]['reps_per_day'] = 1
                prev_av_random_events[i+1]['duration'].append(int(np.random.choice(range( dato9, dato9 + len(wd_prob_duration_1)), p = wd_prob_duration_1)/15))
                prev_av_random_events[i+1]['av_start'].append(np.random.choice(range(0,96), p = wd_prob_av_start_1))    
                prev_av_random_events[i+1]['SOC'].append(np.random.choice(range(0,101), p = wd_prob_SOC_1))
                prev_av_random_events[i+1]['charge_demand'].append(np.random.choice(power_val, p = w_prob_power_1))
                prev_av_random_events[i+1]['battery_cap'].append(np.random.choice(kwh_val, p = w_prob_kwh_1))
                day_before_const[i] = (prev_av_random_events[i+1]['duration'][0] + prev_av_random_events[i+1]['av_start'][0])-96 
                    
            ##################################################### WEEKEND #######################################################
            else:
                prev_av_random_events[i+1]['reps_per_day'] = 1
                prev_av_random_events[i+1]['duration'].append(int(np.random.choice(range( dato11, dato11 + len(we_prob_duration_1)), p = we_prob_duration_1)/15))
                prev_av_random_events[i+1]['av_start'].append(np.random.choice(range(0,96), p = we_prob_av_start_1))    
                prev_av_random_events[i+1]['SOC'].append(np.random.choice(range(0,101), p = we_prob_SOC_1))
                prev_av_random_events[i+1]['charge_demand'].append(np.random.choice(power_val, p = w_prob_power_1))
                prev_av_random_events[i+1]['battery_cap'].append(np.random.choice(kwh_val, p = w_prob_kwh_1))
                day_before_const[i] = (prev_av_random_events[i+1]['duration'][0] + prev_av_random_events[i+1]['av_start'][0])-96
                
                
                       
        else:
            prev_av_random_events[i+1]['reps_per_day'] = 0
            
            ####################################   FIN DEL CICLO DEL DÍA 1 ####################################
        
    def After_day_Events(prev_av_random_events,day_before_const):    
        import pandas as pd
        av_random_events = {}
        
        for i in range(1,len(day_before_const)+1):
            av_random_events[i] = {'reps_per_day': [], 'av_start':[],'SOC':[],'duration':[], 'charge_demand':[]}
            
        def SOC_at_24_calc(bef_av_start, SOC_ini, power):  #SOC value calculation at 00:00
            av_dur_bef_24 = 96 - bef_av_start
            units = (power/4)*av_dur_bef_24 #Con base a una simulación cada 15 minutos
            if SOC_ini+units > 100:
                SOC_at_24 = 100
            else:
                SOC_at_24 = SOC_ini+units
            
            return(SOC_at_24)
        
        for i in range(len(day_before_const)):
            
            av_random_events[i+1]['reps_per_day'] = prev_av_random_events[i+1]['reps_per_day']
            av_random_events[i+1]['charge_demand'] = prev_av_random_events[i+1]['charge_demand']
            av_random_events[i+1]['battery_cap'] = prev_av_random_events[i+1]['battery_cap']
            
            if day_before_const[i] > 0:
                
                #CASE 1
                if day_before_const[i] < prev_av_random_events[i+1]['av_start'][0]: #if the two events don´t coincide
                    
                    av_random_events[i+1]['av_start'].append(0)
                    av_random_events[i+1]['duration'].append(day_before_const[i])
                    
                    SOC_at_24 = SOC_at_24_calc(prev_av_random_events[i+1]['av_start'][-1], prev_av_random_events[i+1]['SOC'][-1], prev_av_random_events[i+1]['charge_demand'][-1])
                    
                    av_random_events[i+1]['SOC'].append(SOC_at_24)
                    
                    for j in range(len(prev_av_random_events[i+1]['av_start'])):
                        
                        av_random_events[i+1]['av_start'].append(prev_av_random_events[i+1]['av_start'][j])
                        av_random_events[i+1]['SOC'].append(prev_av_random_events[i+1]['SOC'][j])
                        
                        if j == len(prev_av_random_events[i+1]['av_start'])-1: #If it's the last in the list
                            av_random_events[i+1]['duration'].append(95-prev_av_random_events[i+1]['av_start'][j])
                        
                        else:
                            av_random_events[i+1]['duration'].append(int(prev_av_random_events[i+1]['duration'][j]/15))
                        
                      
                #CASE 2
                elif day_before_const[i] > prev_av_random_events[i+1]['av_start'][0]: #if the two events coincide
                    
                    av_random_events[i+1]['av_start'].append(0)
                    av_random_events[i+1]['duration'].append(prev_av_random_events[i+1]['av_start'][0]-1) #1 minute before
                    
                    SOC_at_24 = SOC_at_24_calc(prev_av_random_events[i+1]['av_start'][-1], prev_av_random_events[i+1]['SOC'][-1], prev_av_random_events[i+1]['charge_demand'][-1])
        
                    av_random_events[i+1]['SOC'].append(SOC_at_24)
                    
                    for j in range(len(prev_av_random_events[i+1]['av_start'])):
                        av_random_events[i+1]['av_start'].append(prev_av_random_events[i+1]['av_start'][j])
                        av_random_events[i+1]['SOC'].append(prev_av_random_events[i+1]['SOC'][j])
                        
                        if j == len(prev_av_random_events[i+1]['av_start'])-1: #If it's the last in the list
                            av_random_events[i+1]['duration'].append(95-prev_av_random_events[i+1]['av_start'][j])
                        
                        else:
                            av_random_events[i+1]['duration'].append(int(prev_av_random_events[i+1]['duration'][j])/15)
            
            else: 
                av_random_events[i+1] = prev_av_random_events[i+1]
        
        return av_random_events
    
    av_random_events = After_day_Events(prev_av_random_events,day_before_const)
    #return(av_random_events, day_before_const, av_car_list_day_1, av_car_list_day_2, av_cars_day_1, av_cars_day_2)
    return(av_random_events, prob_day_1, day_1)
        
