
def puntero(num_lineas,ind_ficticio):
    b=ind_ficticio-num_lineas
    if (b>=0):
        return b,2
    else:
        return ind_ficticio,1
