from builtins import range
def busList(busAsigLine,busList):
    #busList =[]
    for n in range(len(busAsigLine)):
        newbus1=[busAsigLine[n]['BUS1'],busAsigLine[n]['X1'],busAsigLine[n]['Y1']] 
        newbus2=[busAsigLine[n]['BUS2'],busAsigLine[n]['X2'],busAsigLine[n]['Y2']] 		
        if (newbus1 not in busList):
            busList.append(newbus1)
        if (newbus2 not in busList):
            busList.append(newbus2)		
    return busList

def busListTrafo(busAsigTrafo,busList):
    #busList =[]
    for n in range(len(busAsigTrafo)):
        newbus1=[busAsigTrafo[n]['BUSBT'],busAsigTrafo[n]['X1'],busAsigTrafo[n]['Y1']] 	
        if (newbus1 not in busList):
            busList.append(newbus1)	
    return busList 