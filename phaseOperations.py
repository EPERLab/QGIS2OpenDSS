# -*- coding: utf-8 -*-
#!/usr/bin/python
#phaseOperations realiza diferentes operaciones utilizando la informacion de las fases de los elementos. ?
#El modulo renamePhase reescribe la codificacion que tengan las fases de lineas o transformadores usando las letras A, B y C o .1,.2,.3 para ODSS.
#El modulo linePhase se utiliza para revisar la coherencia entre fases de lineas de MT y BT.
#El modulo trafoPhase se utiliza para revisar la coherencia entre fases de los transformadores con los buses de MT y BT a los que se han asignado.


from builtins import str
def renamePhase(phaseCode):
    phaseCode = str(phaseCode).upper()
    if phaseCode == "7" or phaseCode == 'ABC' or phaseCode == 'RST':
        phaseCode = 'ABC'
        phaseCodeODSS = '.1.2.3'
        phaseNumber = '3'
        phaseNumberTraf = '3'
    elif phaseCode == "6" or phaseCode == 'AB' or phaseCode == 'RS':
        phaseCode = 'AB'
        phaseCodeODSS = '.1.2'
        phaseNumber = '2'
        phaseNumberTraf = '3'
    elif phaseCode == "5" or phaseCode == 'AC' or phaseCode == 'RT':
        phaseCode = 'AC'
        phaseCodeODSS = '.1.3'
        phaseNumber = '2'
        phaseNumberTraf = '3'
    elif phaseCode == "4" or phaseCode == 'A' or phaseCode == 'R':
        phaseCode = 'A'
        phaseCodeODSS = '.1'
        phaseNumber = '1'
        phaseNumberTraf = '1'
    elif phaseCode == "3" or phaseCode == 'BC' or phaseCode == 'ST':
        phaseCode = 'BC'
        phaseCodeODSS = '.2.3'
        phaseNumber = '2'
        phaseNumberTraf = '3'
    elif phaseCode == "2" or phaseCode == 'B' or phaseCode == 'S':
        phaseCode = 'B'
        phaseCodeODSS = '.2'
        phaseNumber = '1'
        phaseNumberTraf = '1'
    elif phaseCode == "1" or phaseCode == 'C' or phaseCode == 'T':
        phaseCode = 'C'
        phaseCodeODSS = '.3'
        phaseNumber = '1'
        phaseNumberTraf = '1'
    elif phaseCode == "0":
        phaseCode = 'NONE'
        phaseCodeODSS='NONE'
        phaseNumber = 'NONE'
        phaseNumberTraf = 'NONE'
    else:
        phaseCode = 'NONE'
        phaseCodeODSS='NONE'
        phaseNumber = 'NONE'
        phaseNumberTraf = 'NONE'
    phaseCodes = {'phaseCode': phaseCode, 'phaseCodeODSS': phaseCodeODSS,
				  'phaseNumber': phaseNumber, 'phaseNumberTraf': phaseNumberTraf}
    return phaseCodes

def linePhaseMT(ph1,ph2): #Verifica coherencia de fase entre líneas MT
    g=0
    #Línea trifasica, admite conexion a trifasico, monofasico, bifasico
    if ph1=='.1.2.3':
        if ph2=='.1.2.3' or ph2=='.1.2' or ph2=='.1.3' or ph2=='.2.3' or ph2=='.1'  or ph2=='.2'  or ph2=='.3':
            g=1
    #Línea bifasica AB, admite conexion a bifasico AB,  monofásico A y monofásico B
    if ph1=='.1.2':
        if ph2=='.1.2' or ph2=='.1' or ph2=='.2':
            g=1
    #Elemento bifasico AC, admite conexion a bifasico AC, monofásico A y monofásico C
    if ph1=='.1.3':
        if ph2=='.1.3' or ph2=='.1' or ph2=='.3':
            g=1
    #Elemento bifasico BC, admite conexion a bifasico BC, monofásico B y monofásico C
    if ph1=='.2.3':
        if  ph2=='.2.3' or ph2=='.2' or ph2=='.3':
            g=1
    #Elemento monofasico A, admite conexion a monofasico A
    if ph1=='.1':
        if ph2=='.1':
            g=1
    #Elemento monofasico B, admite conexion a monofasico B
    if ph1=='.2':
        if ph2=='.2':
            g=1
    #Elemento monofasico C, admite conexion a monofasico C
    if ph1=='.3':
        if ph2=='.3':
            g=1
    return g

def trafoPhaseBT(ph1,ph2): #Verifica coherencia de fase entre líneas de baja tensión - Arg1:Fase Transformador; Arg2: Fase LíneaBT
    g=0
    #Transformador trifásico o conexión especial trifásica (Dos transformadores), admite conexión sólo con línea BT trifásica.
    if ((ph1=='ABC') or (ph1=='AB') or (ph1=='AC') or (ph1=='BC')):
        if ph2=='ABC':
            g=1
    #Transformador monofásico A, sólo admite conexión con líneaBT monofásica A
    if ph1=='A':
        if ph2=='A':
            g=1
    #Transformador monofásico B, sólo admite conexión con líneaBT monofásica B
    if ph1=='B':
        if ph2=='B':
            g=1
    #Transformador monofásico C, sólo admite conexión con líneaBT monofásica C
    if ph1=='C':
        if ph2=='C':
            g=1
    return g
    
def trafoPhaseMT(ph1,ph2): #1 y 2 se asignan según la dirección del flujo (LMT>Trafo>LBT)
    g=0
    #Elemento trifasico, admite conexion a trifasico, monofasico, bifasico
    if ph1=='.1.2.3':
        if ph2=='.1.2.3' or ph2=='.1.2' or ph2=='.1.3' or ph2=='.2.3' or ph2=='.1' or ph2=='.2' or ph2=='.3':
            g=1
    #Elemento bifasico .1.2, admite derivación .1.2, .1 y .2
    if ph1=='.1.2':
        if ph2=='.1.2' or ph2=='.1' or ph2=='.2':
            g=1
    #Elemento bifasico .1.3, admite derivación .1.3, .1 y .3
    if ph1=='.1.3':
        if ph2=='.1.3' or ph2=='.1' or ph2=='.3':
            g=1
    #Elemento bifasico .2.3, admite admite derivación .2.3, .2 y .3
    if ph1=='.2.3':
        if ph2=='.2.3' or ph2=='.2' or ph2=='.3':
            g=1
    #Elemento monofasico .1, derivación .1 
    if ph1=='.1':
        if ph2=='.1':
            g=1
    #Elemento monofasico .2, derivación .2 
    if ph1=='.2':
        if ph2=='.2':
            g=1
    #Elemento monofasico .3, derivación .3 
    if ph1=='.3':
        if ph2=='.3':
            g=1
    return g
