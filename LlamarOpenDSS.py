# -*- coding: utf-8 -*-

import sys
import traceback
import os                                                                      # Importar librería para buscar carpetas y ubicaciones en windows
from decimal import Decimal


def SetUpCOMInterface():
    import comtypes.client
    DSSobj = comtypes.client.CreateObject("OpenDSSEngine.dss")		
    DSSstart = DSSobj.Start(0)
    DSStext = DSSobj.Text
    DSScircuit = DSSobj.ActiveCircuit
    
    return DSSobj, DSSstart, DSStext, DSScircuit

def LlamarOpenDSS(file_path = ""):
	
	if file_path == "":
		dir_scripts =  os.path.dirname(os.path.abspath(__file__))
		file_path = dir_scripts +'\Bibliotecas\Config_Lineas.dss'
	print( file_path )

# Information de entrada
	print("\n")
	# Inicialización de OpenDSS
	[DSSobj, DSSstart, DSStext, DSScircuit] = SetUpCOMInterface()
	
	# Creación de circuito y compilación de archivo master
	# OJO que el master ha sido retocado - revisemos	
	DSStext.Command = 'clear'
	DSStext.Command = 'set defaultbasefrequency=60'
	DSStext.Command = 'new circuit.Datos'
	
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\WireDataCU.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\WireDataACSR.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\WireDataAAC.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\WireDataAAAC.dss'
	
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_Alum_EPR.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_Alum_PVC.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_Alum_XLPE.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_CU_EPR.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_CU_PVC.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\CNData_CU_XLPE.dss'
	
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\ServiceUGSC1FOpenDSS_CU_PVC.txt' 
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\ServiceUGSC1FOpenDSS_CU_XLP.dss'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\ServiceUGSC3FOpenDSS_CU_PVC.txt'
	DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\ServiceUGSC3FOpenDSS_CU_XLP.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\triplexneutroAAAC.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\triplexneutroCU.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\triplexneutroAAC.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\triplexneutroACSR.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\quadruplexneutroAAAC.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\quadruplexneutroAAC.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\quadruplexneutroACSR.dss'
	#DSStext.Command = 'redirect ' + dir_scripts + '\Bibliotecas\quadruplexneutroCU.dss'
	
	DSStext.Command = 'redirect ' + file_path
	DSStext.Command = 'set datapath = ' + dir_scripts
	DSStext.Command = 'show lineconstants freq=60 units=km'
	DSStext.Command = 'solve'

def DeterminarImpedancia(Codigo_buscado):	
	palabra = str("Geometry Code = " + Codigo_buscado + "\n")
	# fix_print_with_import
	print("Buscando: ", palabra)
	archivo = open('Datos_LineConstants.txt', 'r')
	lines = archivo.readlines()
	
	
	#Se busca en qué línea del archivo está la palabra buscada.
	contador = 0
	resultado = 0
	
	for line in lines:
			
		if line == palabra:
			resultado = contador
			# fix_print_with_import
			print("Está en la línea ", resultado)
			break
		contador = contador + 1
	
	if resultado == 0:
		return 0
	
	geometry_code = lines[resultado]
	contador = geometry_code.find("=")
	fases = geometry_code[ contador + 2 ]
	
	# fix_print_with_import
	print("Es un sistema de ", fases, " fases.")
	
	#Si es trifásico o monofásico se debe buscar la impedancia de forma distinta
	if fases == "3":
		print("Trifásico")
		valor_buscado = "-------------------Equiv Symmetrical Component --------------------\n"
		
		i = 1
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
    
	else:
		print("Monofásico o bifásico")
		valor_buscado_r = "R MATRIX, ohms per km\n"
		valor_buscado_x = "jX MATRIX, ohms per km\n"
		i = 1
		
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
		
	return impedancia

if __name__ == "__main__":
	
	impedancia_total = 0
	
	#try:
	LlamarOpenDSS()
	            
	print( "Se ha creado con éxito el archivo Datos_LineConstants.txt")
	
	#except:
	#	print("Ha ocurrido un error, favor corregir el siguiente error antes de continuar")
	#	for tb in traceback.format_tb(sys.exc_info()[2]):
	#		print(tb)
	
	
	Codigo_buscado = "3fmv4cu3/0aac_h"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	"""
	Codigo_buscado = "2fmv477aac3/0aac_h"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "1fmv477aac3/0aac_h"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	
	
	print("/////////////////////////////////")
	Codigo_buscado = "1flv6aac6aac"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	
	Codigo_buscado = "2flv6aac6aac"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "3flv6aac6aac"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	print("/////////////////////////////////")
	
	Codigo_buscado = "3fmv1/0aaac2aaac_b"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "2fmv1/0aaac2aaac_b"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "1fmv1/0aaac2aaac_b"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	print("/////////////////////////////////")
	
	Codigo_buscado = "3fmv4aaac4aaac_m"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "2fmv4aaac4aaac_m"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "1fmv4aaac4aaac_m"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	Codigo_buscado = "xxxx"
	impedancia = DeterminarImpedancia(Codigo_buscado)
	impedancia_total += impedancia
	print("La impedancia en Ohm/m, es de: ", impedancia, "\n")
	
	print("*************************************************************")
	print("La impedancia total, en Ohm/m, es de: ", impedancia_total)
	print("*************************************************************")
	
	
"""
