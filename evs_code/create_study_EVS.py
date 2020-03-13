import sys
import traceback
import os
import numpy as np
from .EVsFunctions import CreacionPerfilesEV, AnalizarEncuestas, CreateList_SOC_t #funciones necesarias para VEs
from PyQt5.QtWidgets import QMessageBox

"""
Función que se encarga de determinar la probabilidad de cierto consumo según la
lista pasada como parámetro. Se redondea el consumo a la decena más cercana, debido a que
las probabilidades están dadas de esta forma (cada 10 kWH)
Parámetros de entrada:
*list_consum_prob (list): lista que tiene en la columna 0 los kWH y en la columna 1 la probabilidad
asociada a dicho consumo.
*kWH (int): consumo al que se le debe obtener la probabilidad asociada 

Valores retornados:
*prob (float): probabilidad asociada a los kWH pasados como parámetros.
Tendrá un valor de 0 en caso de no encontrar el kWH en la lista.
"""

def GetProbability( list_consum_prob, kWH ):
    prob = 0
    
    #Si la demanda es mayor a 1500 kWh no existen probabilidades asociadas, por lo que se asigna la misma que si tuviera 1500 kWh
    if kWH > 1500:
        kWH = 1500
		 
    #Se redondea el kWH (a la decena más cercana)
    kWH = kWH/10
    kWH_decimalpart = kWH
    kWH_decimalpart = kWH % 1
    kWH_decimalpart = round( kWH_decimalpart )
    kWH = int( int(kWH)*10 + kWH_decimalpart*10 )
    
    for line in list_consum_prob:
        try:
            kwh_temp = line[0]
            kwh_temp = float( kwh_temp )
            kwh_temp = int( kwh_temp )
            if kwh_temp == kWH:
                prob = float( line[1] )
                return prob
        except:
            pass #Debido a que la primera línea de la lista podría tener letras
    return prob
 
"""
 Esta función es llamada desde la función CreateEVDss, y se encarga de crear la lista de DSSNames
 que corresponden a las EVs que posteriormente se encuentran en la simulación. La lista se crea de
 distinta forma dependiendo del tipo de estudio que se a realizarse. Si es random, se crea una 
 lista y con la función np.suffle se elige aleatoriamente a los primeros n_carros a ser incluidos en el estudio.
 Si es por consumo, se crea una lista ordenada descendentemente por kWH de cada casa y elige las primeras
 n_carros casas.
 Finalmente, si es por probabilidad, se elige si un vehículo en x casa estará en la simulación mediante la función
 np.binomial, la cual con la probabilidad dada, retorna un 0 o 1, lo cual permite elegir si la carga asociada estará
 en la simulación o no.
 Parámetros:
 *lineList: corresponde a la lista de líneas del archivo de cargas de LV
 *study_type (string): tipo de estudio. Tiene tres posibles valores: "random" para estudio aleatorio,
"cosum" para un estudio donde se eligen las casas de mayor consumo, y "prob" para un estudio por probabilidad
*percent_evs (int): porcentaje de vehículos eléctricos (con respecto a la cantidad de casas existentes) que se van a incluir en la
simulación. Debe ser un número entre 0 y 100
*list_prob (list): parámetro opcional en el caso de estudios random y de consumo, pero en el caso de estudio por probabilidad
se debe pasar como parámetro.
*name_dss_ev (str): nombre del dss con VE creado por el azul

Valores retornados:
*DSSName_List (list): corresponde a la lista de DSSNames que estarán en la simulación a realizarse
*n_carros (int): cantidad de vehículos eléctricos que habrá en el estudio
*En caso de haber algún error retorna 0, 0
"""
def GetDSSNameList( lineList, study_type, percent_evs, name_dss_ev, list_prob = "" ):
    try:
        #Select lines
        lim_iteraciones = 100
        bandera = True
        num_iteraciones = 0
        max_carros_enc = 0
        DSSName_List_max = []
        
        #Se llama a esta función para saber si hay cierta cantidad de VEs creada por plugin azul
        cantidad_evs_layer, buses_list_layer = buses_list_ev_layer( name_dss_ev )
        
        if study_type == "consumo":
            matriz_name_kwh = []
        
        while bandera == True:
            num_iteraciones += 1
            n_carros = 0
            num_casas = 0
            DSSName_List = []
            
            for line in lineList:
                #El tipo de carga debe ser residencial
                in_class = line.find("class=")
                in_class += len("class=")
                fin_class = line.find(" ", in_class)
                class_ = line[in_class:fin_class]
                if class_ != "R":
                    continue
                
                #Sólo se debe aumentar si la carga no es residencial
                num_casas += 1
                
                #Busca el DSSName para agregarlo a la lista
                in_dssname = line.find("load.") #sintax find: str.find(sub[, start[, end]] )
                in_dssname += len( "load." )
                fin_dssname = line.find(" ", in_dssname)
                dssname = line[in_dssname:fin_dssname]
                
                in_bus = line.find("bus1=") 
                in_bus += len("bus1=") 
                fin_bus = line.find(".", in_bus)
                bus = line[in_bus:fin_bus]
                
                #Si el bus ya está en la lista de buses donde hay un VE no se debe considerar
                if bus in buses_list_layer:
                    continue
                
                #Caso por consumo
                if study_type == "consumo":
                    in_kwh = line.find("kWh=")
                    in_kwh += len("kWh=")
                    fin_kwh = line.find(" ", in_kwh)
                    kwh = line[in_kwh:fin_kwh]
                    vector = [ dssname, kwh ]
                    matriz_name_kwh.append(vector)
                    n_carros += 1
                
                #Caso por probabilidades
                if study_type == "prob":
                    #kWH para asignar probabilidades del diccionario
                    kWh_in = line.find("kWh=")
                    kWh_in += len("kWh=")
                    kWh_fin = line.find(" ", kWh_in)
                    kWh = float( line[kWh_in:kWh_fin] )
                    kWh = round( kWh )
                    prob = GetProbability( list_prob, kWh ) #asignar del csv (diccionario)
                    
                    n = 1 #cantidad de iteraciones
                    ve_enable = np.random.binomial(n, prob)
                    if ve_enable == 1:
                        DSSName_List.append( dssname )
                        n_carros += 1
                #Caso aleatorio
                if study_type == "random":
                    DSSName_List.append( dssname )
                    n_carros += 1
                
            #Se verifica que realmente haya que añadir VEs
            percent_ve = float( percent_evs / 100)
            cantidad_total = int( num_casas*percent_ve )
            cantidad_ve_deseados =  cantidad_total - cantidad_evs_layer #cantidad de carros tomados en cuenta para el estudio con base en el porcentaje indicado por el usuario
            #print("EVs layer = ", cantidad_evs_layer, " , cantidad_ve_rojo = ", cantidad_ve_deseados, ", cantidad deseaada = ", cantidad_ve_deseados, ", cantidad total = ", cantidad_total )
            
            if cantidad_ve_deseados <= 0:
                mensaje = "La capa de vehículos eléctricos ya aportaba una cantidad igual o superior o igual al porcentaje deseado de penetración, por lo que no se añadirán VEs"
                QMessageBox.warning(None,"QGISrunOpenDSS mensaje EV", mensaje)
                return [], 0
            elif n_carros < cantidad_ve_deseados:
                mensaje = "No se ha encontrado la cantidad deseada de VE. El máximo fueron " + str( n_carros ) + ".\nSe simulará con esta cantidad"
                QMessageBox.warning(None,"QGISrunOpenDSS mensaje EV", mensaje)
            
            
            #Se debe analizar si se alcanzó la cantidad de VEs adecuada para un estudio probabilístico
            if study_type == "prob":
                if max_carros_enc < n_carros: #Se actualiza el número máximo de carros alcanzado
                    max_carros_enc = n_carros
                    DSSName_List_max = DSSName_List
                    bandera = True #se debe continuar iterando
                
                if n_carros >= cantidad_deseada_ve: #Caso en que se haya alcanzado la cantidad deseada de EV
                    print( "Total iteraciones = ", num_iteraciones )
                    DSSName_List = DSSName_List[ : cantidad_ve_deseados ]
                    return DSSName_List, cantidad_ve_deseados
                    
                if num_iteraciones == lim_iteraciones: #Caso en que se haya alcanzado el límite de operaciones sin alcanzar la cantidad deseada
                    n_carros = max_carros_enc
                    DSSName_List = DSSName_List_max
                    mensaje = "No se ha encontrado la cantidad deseada de VE. El máximo fueron " + str( n_carros ) + ".\nSe simulará con esta cantidad"
                    QMessageBox.warning(None,"QGISrunOpenDSS mensaje EV", mensaje)
                    return DSSName_List, n_carros
            else:
                bandera = False #si el estudio no es probabilístico no se debe seguir iterando
                
            if n_carros <= 0:
                mensaje = str( "Ha ocurrido un error en VE, ya que no se encontró una cantidad superior a cero VE")
                QMessageBox.critical(None,"QGISrunOpenDSS error EV", mensaje)
                return 0, 0
            
            if study_type == "random":
                np.random.shuffle(DSSName_List) #se ordena el vector de forma aleatoria
                
            elif study_type == "consumo":
                matriz_name_kwh = sorted(matriz_name_kwh, key=lambda x: x[1], reverse=True) #Se ordena la matriz de forma descendente
                for x in matriz_name_kwh: DSSName_List.append( x[0] )
                
            DSSName_List = DSSName_List[ : cantidad_ve_deseados ] #se reajusta el vector aleatoriamente para que sólo seleccione la cantidad de VE seleccionada
        
        print( "n_carros = ", cantidad_ve_deseados )
        return DSSName_List, cantidad_ve_deseados
    except:
        exc_info = sys.exc_info()
        print("\nError: ", exc_info )
        print("*************************  Información detallada del error ********************")
        for tb in traceback.format_tb(sys.exc_info()[2]):
            print(tb)
        return 0, 0
    
"""
Esta función encuentra el primer último 0 en determinada columna
Retorna un -1 si se encuentra en la última posición, un x + 1 caso contrario,
donde x es el número de fila donde se encontró

Parámetros:
*column (columna de un dataframe, pero también puede ser un vector

Valores retornados:
*t (int): número de columna + 1 en la que fue encontrado el primer cero
"""

def find_t(column):
    n_columnas = len(column)
    for j in reversed( range( n_columnas ) ): #se recorren al revés la columnas
        dato = column[j]
        if dato == 0 and j == n_columnas - 1:
            return -1 
        elif dato == 0:
            return j + 1 

"""
Esta función se encarga de obtener los datos referentes al dss de EVs creada por el plugin QGIS2OpenDSS (azul)
Se usa para no repetir los VEs en caso de existir una cantidad de VEs ya modelada.
Esto lo hace buscando el bus asociado al VE para no volver a conectar otro VE a ese bus.

-Parámetros de entrada:
*name_dss_ev: nombre del dss donde se modelan los VEs.

-Valores retornados:
*number_evs (int): cantidad de VEs modelados por el plugin azul
*layer_buses_list (list): nombre de los buses que tienen un VE asociado
"""

def buses_list_ev_layer( name_dss_ev ):
    number_evs = 0
    layer_buses_list = []
	
    #Caso en que no haya un archivo de evs creado por el azul
    if name_dss_ev == "":
        return number_evs, layer_buses_list
    
    with open(name_dss_ev, "r") as f:
        lines = f.readlines()
    for line in lines:
        #Verifica que se trate de un VE
        in_ev = line.find("new storage") #sintax find: str.find(sub[, start[, end]] )
        
        #Si no se trata de un VE continúa
        if in_ev == -1:
            continue
        
        in_bus = line.find("bus1=") 
        in_bus += len("bus1=") 
        
        if in_bus !=-1:
            fin_bus = line.find(".", in_bus)
            bus = line[in_bus:fin_bus]
            layer_buses_list.append( bus )
            number_evs += 1
    
    return number_evs, layer_buses_list
            
		
		
		
		
		

"""
Se basa en el siguiente formato de definir una carga para obtener los datos:
new load.3FFLX1 bus1=BUSLVFLX1188.1.2.3 kV=0.208 model=1 conn=wye kW=1.0 kvar=0.1 status=variable phases=3 daily=curve313_00R !kWh=312.57 class= R !Group=N/A

Esta es la función principal cuando se hace una simulación por EV sin una capa de los mismos.
Se encarga de llamar a otra función que obtiene la lista de DSSName y si el valor del DSSName actual presente
en la capa de cargas de BT se encuentra en esta lista entonces se agrega el EV al nuevo dss correspondiente a EV.

Parámetros:
*file_path (string): dirección del archivo de cargas. Es un dss
*study_type (string): tipo de estudio. Tiene tres posibles valores: "random" para estudio aleatorio,
"consum" para un estudio donde se eligen las casas de mayor consumo, y "prob" para un estudio por probabilidad
*percent_evs (int): porcentaje de vehículos eléctricos (con respecto a la cantidad de casas existentes) que se van a incluir en la 
simulación. Debe ser un número entre 0 y 100.
*circuitName (string): nombre del circuito simulado
*name_output_azul (string): dirección del archivo de salida (dss) del plugin QGIS2OpenDSS
*name_dss_ev (str): nombre del dss con VE creado por el plugin azul
*list_prob (list): parámetro opcional en el caso de estudios random y de consumo, pero en el caso de estudio por probabilidad
se debe pasar como parámetro.

Valores retornados:
*0 en caso de haber errores
*1 si concluyó exitosamente
"""
def CreateEVDss( file_path, study_type, percent_evs, circuitName, name_output_azul, vector_soc_t, name_dss_ev, list_prob = "" ):
    try:
        with open( file_path ) as f:
            lineList = f.readlines()
        
        DSSName_List, n_carros = GetDSSNameList(lineList, study_type, percent_evs, name_dss_ev, list_prob)
        if DSSName_List == 0: #caso en que haya habido un error en GetDSSNameList
            return 0     
            
        if DSSName_List == []: #caso en que no haya que crear el dss de VEs
            return vector_soc_t     
        #Se obtienen datos aleatorios con base en las encuestas
        sample_wd, sample_we, sample_gnal, power_val, kwh_val = AnalizarEncuestas()
        evs_loadshapes, evs_power, evs_initial_soc, evs_kwh = CreacionPerfilesEV( power_val, kwh_val, total_cars =  n_carros )
        columns_evs_loadshapes = evs_loadshapes.columns
        idx_dss = file_path.rfind("\\")
        path_dss = file_path[ : idx_dss ]
        path_profiles = path_dss + "/profiles"
        #Crea la carpeta donde se crearán los perfiles aleatorios, si esta no existe
        if not os.path.exists(path_profiles):
            os.makedirs(path_profiles)
        
        name_csv = "EV" + study_type + str(percent_evs) + "_profiles.csv"
        dir_csv = path_profiles + "/" + name_csv
        
        #Save the loadshapes to csv
        evs_loadshapes.to_csv(dir_csv, header=False, index=False)
        
        name_loadshapes = circuitName + "_EV" + study_type + str(percent_evs) +"Loadshapes.dss" #Path relativo de los loadshapes dss
        name_dss = circuitName + "_EV" + study_type + str(percent_evs) + ".dss"
        
        
        filename_ev_dss = path_dss + '/' + name_dss #Path absoluto
        filename_loadshapes = path_profiles + "/" + name_loadshapes
        print( "filename_ev_dss = ", filename_ev_dss )
        print( "filename_loadshapes = ", filename_loadshapes )
         
        kw_random = list( evs_power.iloc[ 0 ].values )
        kwH_random = list( evs_kwh.iloc[ 0 ].values )
        
        file_ev = open( filename_ev_dss, 'w' )
        file_loadshape = open( filename_loadshapes, 'w' )
        
        contador = -1 #Ya que se aumenta al principio, no al final
        for line in lineList:
            in_dssname = line.find("load.") #sintax find: str.find(sub[, start[, end]] )
            in_dssname += len("load.")
            fin_dssname = line.find(" ", in_dssname)
            dssname = line[in_dssname:fin_dssname]
            
            if dssname not in DSSName_List:
                continue #si el dssname no está en la lista no se agrega
            
            contador += 1
            
            in_group = line.find("!Group=") 
            in_group += len("!Group=") 
            fin_group = line.find(" ", in_group)
            if fin_group == 0:
                fin_group = line.find("\n", in_group)
            group = line[in_group:fin_group]
            
            
            in_bus = line.find("bus1=") 
            in_bus += len("bus1=") 
            fin_bus = line.find(".", in_bus)
            bus = line[in_bus:fin_bus]
            
            conns_fin = line.find(" ", fin_bus)
            conns = line[fin_bus:conns_fin]
            
            kV_in = line.find("kV=")
            kV_in += len("kV=")
            kV_fin = line.find(" ", kV_in)
            kV = line[kV_in:kV_fin]
            
            
            kWh_in = line.find("kWh=")
            kWh_in += len("kWh=")
            kWh_fin = line.find(" ", kWh_in)
            kWh = line[kWh_in:kWh_fin]
           
            #Asignar datos aleatorios            
            kwHrated = float( kwH_random[ contador ] )
            kW = float( kw_random[contador] )
            
            #SOC
            #Obtener t (donde se da el último 0 en la columna)
            name_column = columns_evs_loadshapes[ contador ]
            columna = evs_loadshapes[ name_column ]
            t = find_t( columna )
            
            #Se organiza el vector de SoC
            n_filas_soc = len( evs_initial_soc )
            vect_soc = []
            extra_sentence = "!Group=" + str( group )
            for k in range( n_filas_soc ):
                data_soc = evs_initial_soc.iloc[ n_filas_soc - k - 1][ contador ] #Para que asigne primero el último valor
                if data_soc != None and np.isnan(data_soc) == False: #verifica que el valor no sea 
                    data_soc = float( data_soc )
                    vect_soc.append( data_soc )
                vect_soc.sort(reverse = True) #Se ordena el vector descendentemente
                    
            cant_soc = len( vect_soc )
            if cant_soc != 1: #Caso en que se tengan varios estados de carga iniciales
                #Obtener t (para soc) (donde se da el último 0 en la columna)
                name_column = columns_evs_loadshapes[ contador ]
                columna = evs_loadshapes[ name_column ]
                t = find_t( columna )
                for i in range(1, cant_soc):
                    extra_sentence += " !stored=" + str(vect_soc[i]) + " " 
                extra_sentence += "!t=" + str(t) + " "
            else:
                extra_sentence += " !t=-1"
                        
            soc_i = vect_soc[0]
            
            #Nombre del daily curve
            daily_curve = "EV" + study_type + str(percent_evs) + "_" + str(contador)
           
            sentence = ""
            sentence = "new storage.ve_" + dssname
            sentence += " bus1=" + bus + conns
            sentence += " phases=1 model=1"
            sentence += " kW=" + str(kW)
            sentence += " kV=" + str(kV)
            sentence += " pf=0.98"
            sentence += " kWrated=" + str(kW)
            sentence += " kWhrated=" + str(kwHrated)
            sentence += " %reserve=0"
            sentence += " %stored=" + str( soc_i )
            sentence += " %EffCharge=100 %IdlingkW=0 enabled=y dispmode=FOLLOW daily=" + daily_curve
            sentence += " " + extra_sentence + "\n"
            sentence = str( sentence )
                        
            sentence_loadshape = "New Loadshape." + daily_curve  + " npts=96 mInterval=15 mult=(file="
            sentence_loadshape += name_csv + ", col=" + str(contador + 1) + ", header=no) useactual=no\n"
            sentence_loadshape = str( sentence_loadshape )
            file_ev.write( sentence )
            file_loadshape.write( sentence_loadshape )
        
        #se cierran los archivos
        file_ev.close() 
        file_loadshape.close()
        
        #Agrega datos de simulaciones a archivo de salida del azul
        with open(name_output_azul, "a") as myfile:
            myfile.write( "\nredirect profiles/" + name_loadshapes + "\n" )
            myfile.write( "redirect " + name_dss + "\n" )
        
        vector_soc_t = CreateList_SOC_t( filename_ev_dss, vector_soc_t ) #Se crea lista de SOC_t
        
        return vector_soc_t
    except:
        exc_info = sys.exc_info()
        print("\nError: ", exc_info )
        print("*************************  Información detallada del error ********************")
        for tb in traceback.format_tb(sys.exc_info()[2]):
            print(tb)
        mensaje = str( "Hubo un error al escribir el archivo de VE.\nPara más información revise el código de error en la consola.")
        QMessageBox.critical(None,"QGISrunOpenDSS escritura VE", mensaje)
        
        return 0
            
if __name__ == "__main__":
    percent_evs = 50
    file_path = "C:\\Users\\bjza0\\Documents\\Proyectos_actuales\\ves_baterias_digitalizacion\\FLX\\evs_code\\FLX_LoadsLV.dss"
    study_type = "consumo"
    circuitName = "FLX"
    name_output_azul = "FLX_OutputQGIS2OpenDSS"
    CreateEVDss( file_path, study_type, percent_evs, circuitName, name_output_azul, list_prob = "" )
