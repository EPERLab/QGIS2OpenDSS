from numpy import genfromtxt
import pandas as pd

def csv_numpy( csv_name, nombre_columna ):

	dataframe = pd.read_csv( csv_name )
	dato_real = dataframe[ nombre_columna ]
	my_data = dato_real.to_numpy()
	return my_data

if __name__ == '__main__':
	csv_name = "wd_prob_av_start_1.csv"
	nombre_columna = "Probabilidad"
	
	my_data = csv_numpy( csv_name, nombre_columna )
	print( my_data )
