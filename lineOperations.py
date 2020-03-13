# -*- coding: utf-8 -*-
from builtins import str
from qgis.core import QgsProject, QgsMessageLog
def renameVoltage(LVCodein): #Para monofasicos, tension L-N, para trifasicos tension L-L
    LVCodein = int(LVCodein)
    if LVCodein == 20:
        LVCode = {'LN':0.12,'LL':0.208}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    
    elif LVCodein == 30:
        LVCode = {'LN':0.12,'LL':0.24}
        conns='.1.2'
        cantFases='1'
        config='wye'   #split Phase
    
    elif LVCodein == 35:
        LVCode = {'LN':0.254,'LL':0.44}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    
    elif LVCodein == 40:
        LVCode = {'LN':0.240,'LL':0.48}
        conns='.1.2.3'
        cantFases='3'
        config='delta' 		# Esto es 4D
    
    elif LVCodein == 50:
        LVCode = {'LN':0.277,'LL':0.48}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    
    elif LVCodein == 60:
        LVCode = {'LN':0.48,'LL':0.48} #Conexión delta 3 conductores
        conns='.1.2.3'
        cantFases='3'
        config='delta' 		
   
    elif LVCodein == 70:
        LVCode = {'LN':0.24,'LL':0.416}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
   
    elif LVCodein == 80:
        LVCode = {'LN': 2.4,'LL': 2.4}
        conns='.1.2.3'
        cantFases='3'
        config='delta'
   
    elif LVCodein == 110:
        LVCode = {'LN': 4.16,'LL': 4.16}
        conns='.1.2.3'
        cantFases='3'
        config='delta'
       
    elif LVCodein == 120:
        LVCode = {'LN': 2.4,'LL': 4.16}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    
    elif LVCodein == 150:
        LVCode = {'LN': 7.20,'LL':7.20}
        conns='.1.2.3'
        cantFases='3'
        config='delta'
    
    elif LVCodein == 160:
        LVCode = {'LN': 4.16,'LL': 7.20}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    
    elif LVCodein == 210:
        LVCode = {'LN': 7.22,'LL': 12.5}
        conns='.1.2.3'
        cantFases='3'
        config='wye'    
        
    elif LVCodein == 230:
        LVCode= {'LN':7.62,'LL':13.2}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
        
    elif LVCodein == 260:
        LVCode = {'LN': 13.8, 'LL': 13.8}
        conns='.1.2.3'
        cantFases='3'
        config='delta'
        
    elif LVCodein == 270:
        LVCode = {'LN':7.9674,'LL':13.8}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    elif LVCodein == 340:
        LVCode= {'LN':14.38,'LL':24.9}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    elif LVCodein == 380:
        LVCode = {'LN':19.9186,'LL':34.5}
        conns='.1.2.3'
        cantFases='3'
        config='wye'
    else:
        LVCode = {'LN':'NONE','LL':'NONE'}
        conns='NONE'
        cantFases='NONE'
        config='NONE'
        aviso= u'La tensión de operación '+str(LVCodein)+' es desconocida.'
        QgsMessageLog.logMessage(aviso,u'Tensiones')
		
    voltageCodes={'LVCode':LVCode,'conns':conns,'cantFases':cantFases,'config':config}
	
    return voltageCodes

