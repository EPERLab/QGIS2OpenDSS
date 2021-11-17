"""
# ################################################################
# ################################################################
Función que se encarga de seleccionar un día donde estén la mayor
cantidad de columnas completas para ser utilizadas en un load_shape
Inicialmente los IDs de los medidores están en los nombres de
las columnas, y la columna 'Fecha' es donde se encuentra tanto la
fecha como la hora, separadas por un espacio.

Valores de entrada:
id_ (int): número de fila actual de la tabla de atributos
*carga
*layer (): capa donde está la info de AMIS
*cols_str (str list): lista con los nombres de columnas del dataframe
con la información de ids por circuito
*idx_ami (int): número de columna del atributo 'AMI'
*idx_id (int): número de columna del atributo 'ID'
*name_attribute (str): nombre del atributo donde está el identificador
del AMI
*self.df_data (dataframe con la información de los loadshapes por
medidor
*self.folder_profile (str): directorio donde 

Valores retornados:
*total_data (dataframe): dataframe con todos los datos
*df_fin (dataframe): dataframe con el día seleccionado y los IDs
seleccionados.

# ################################################################
# ################################################################
"""

def assign_ids(self, id_, carga, layer, cols_str, idx_ami, idx_id,
			   name_attribute='ID'):
	try:
		id_carga = str(carga[name_attribute])
		dtype_ = cols_str.dtype
		
		if id_carga not in cols_str:
			return False, None
		if dtype != np.dtype('O'):
			try: id_carga = int(id_carga)
			except Exception: pass
		
		col = self.df_data[id_carga]
		filename_ = self.folder_profile + "/amis"
		filename_ += "/" + str(id_carga) + ".txt"
		col.to_csv(filename_, header=True, index=False)
		layer.changeAttributeValue(id_, idx_ami, 'Yes')
		layer.changeAttributeValue(id_, idx_id, str(id_carga))
		return True, id_carga
	except Exception:
		return False, None

"""
# ################################################################
# ################################################################
Función que se encarga de seleccionar un día donde estén la mayor
cantidad de columnas completas para ser utilizadas en un load_shape
Inicialmente los IDs de los medidores están en los nombres de
las columnas, y la columna 'Fecha' es donde se encuentra tanto la
fecha como la hora, separadas por un espacio.

Valores de entrada:
*path (string): directorio donde se encuentran el archivo


Valores retornados:
*total_data (dataframe): dataframe con todos los datos
*df_fin (dataframe): dataframe con el día seleccionado y los IDs
seleccionados.

# ################################################################
# ################################################################
"""

def convert_files(path):
    vect_dataf = []
    files = []

    type_files = ['*.csv', '*.xlsx', '*.xls',
                  '*.xlsm', '*.xltx', '*.xltm']

    for ext in type_files:
        search_exp = path + "/" + ext
        match = glob.glob(search_exp)
        print(match)
        files.extend(match)

    for filename in files:
        if "~" in filename:
            continue
        print("filename = ", filename)
        name, file_extension = os.path.splitext(filename)
        # Lectura de datos
        if file_extension == ".csv":
            df_data = pd.read_csv(filename, header=0, index_col=None)
        else:
            df_data = pd.read_excel(filename, header=0, index_col=None,
                                    sheet_name=0)
        vect_dataf.append(df_data)
        
    total_data = pd.concat(vect_dataf)
    columns_well = ['Fecha']
    columns = total_data.columns
    col_adapt =  set(columns) - set(columns_well)
    col_adapt = list(col_adapt)
    print("Recorridos todos")

    total_data = pd.melt(total_data, id_vars=['Fecha'], value_vars=col_adapt)
    total_data.rename(columns={"Fecha": "Fecha_Tiempo"}, inplace=True)
    total_data[['FECHA','TIEMPO']] = total_data.Fecha_Tiempo.str.split(" ", expand=True)
    total_data = total_data[['TIEMPO', 'FECHA', 'value', 'variable']]
    cols = ['TIEMPO', 'FECHA', 'variable']
    mean2 = total_data.groupby(cols).filter(lambda g: g.isnull().sum().sum() < 1)
    n_med = len(list(mean2.variable.unique()))
    fecha_df = mean2.groupby(['FECHA', 'variable']).filter(lambda g: len(g) == 96)
    if fecha_df.empty is False:
        fecha_fin = fecha_df.loc[0, 'FECHA']
        sel_fin = fecha_df[fecha_df['FECHA'] == fecha_fin]
        df_fin = sel_fin.pivot(index='TIEMPO', columns='variable',
                               values='value').reset_index(drop=True)
        filename_ = path + "/Datos.csv"
        df_fin.to_csv(filename_, sep=";", header=True, index=True, decimal=",")
    else:
        df_fin = pd.DataFrame()
        
    
    """
    total_data = total_data[['TIEMPO', 'value', 'variable']]
    mean = total_data.groupby(['TIEMPO', 'variable']).mean()
    mean = mean.dropna(axis=0, how='any')
    cantidad = mean.groupby(['variable']).size()

    #total_data_correct = total_data.pivot(columns='FASE', values=col_adapt)
    """
    
    return total_data, df_fin


if __name__ == "__main__":
    path = "C:/Users/bjza0/Documents/Circuitos/SB-AMI"
    total_data = convert_files(path)
    print("Final")

	
	
	
