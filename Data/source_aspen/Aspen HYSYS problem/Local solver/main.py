import os
import win32com.client as win32
import numpy as np
import time

''' Connecting to the Aspen Hysys App just one time during optimization'''      
print(' # Connecting to the Aspen Hysys App ... ')
hyapp    = win32.Dispatch('HYSYS.Application')			   # Connecting to the Application
hyCase   = hyapp.ActiveDocument                          # Access to active document
hysolver = hyCase.Solver

LB=np.array([0.3000,0.7500,1.8750,4.6750,0.8590,2.5970,2.5410,3.9110])
UB=np.array([5.7000,14.2500,35.6250,88.8250,16.3210,49.3430,48.2790,74.3090])
x0 = (LB+UB)/2

def hy_distinguish(hysolver):
	i=4
	P=np.linspace(0,0,num=1001)
	k=0
	timeover=0
	while k<2:
		i+=1
		solveboolean = 1-hysolver.issolving
		P[i] = solveboolean
		k = P[i-2] + P[i-1] + P[i]
		time.sleep(1)
		if P[1000]==1:
			timeover=1
			break
	return timeover	

def hy_Object(hyCase, hysolver, variable):
	error=0
	error_type = np.array([0,0,0])
	butane = 100 - (variable[4] + variable[5] + variable[6] + variable[7])
	if variable[0]<=variable[1] and variable[1]<=variable[2] and variable[2]<=variable[3] and variable[4] >= 0:
		hyCase.Flowsheet.operations.Item('optimization').Cell('C2').cellvalue = variable[0]*100   #LP multiply 100 for making unit as bar
		hyCase.Flowsheet.operations.Item('optimization').Cell('C3').cellvalue = variable[1]*100   #MP1
		hyCase.Flowsheet.operations.Item('optimization').Cell('C4').cellvalue = variable[2]*100   #MP2
		hyCase.Flowsheet.operations.Item('optimization').Cell('C5').cellvalue = variable[3]*100   #HP
		hyCase.Flowsheet.operations.Item('optimization').Cell('C6').cellvalue = variable[4]   	  #Nitrogen
		hyCase.Flowsheet.operations.Item('optimization').Cell('C7').cellvalue = variable[5]       #Methane
		hyCase.Flowsheet.operations.Item('optimization').Cell('C8').cellvalue = variable[6]       #Ethane
		hyCase.Flowsheet.operations.Item('optimization').Cell('C9').cellvalue = variable[7]       #propane
		hyCase.Flowsheet.operations.Item('optimization').Cell('C10').cellvalue = butane    
		timeover=hy_distinguish(hysolver)
		# --there are infeasible signs?
		try:
			V1=hyCase.Flowsheet.operations.Item('optimization').Cell('I2').cellvalue
			V2=hyCase.Flowsheet.operations.Item('optimization').Cell('I3').cellvalue
			V3=hyCase.Flowsheet.operations.Item('optimization').Cell('I4').cellvalue
			V4=hyCase.Flowsheet.operations.Item('optimization').Cell('I5').cellvalue
			V5=hyCase.Flowsheet.operations.Item('optimization').Cell('I6').cellvalue
		except:
			hyCase.Flowsheet.operations.Item('optimization').Cell('C11').cellvalue = 3
			timeover=hy_distinguish(hysolver)
			error_type[1]=1
			if np.dot(error_type,error_type) !=0:
				error=1
				OBJ = np.random.random_sample()*10**30
				return OBJ
			else:
				pass
		
		if abs(V4-V5)>0.1:
			error_type[1]=1
			error =1
			OBJ = np.random.random_sample()*10**30
			return OBJ

		MTD_HX1 = hyCase.Flowsheet.operations.Item('optimization').Cell('C14').cellvalue
		MTD_HX2 = hyCase.Flowsheet.operations.Item('optimization').Cell('C16').cellvalue

		if MTD_HX1 <=2.85 or MTD_HX2<=2.85:
			hyCase.Flowsheet.operations.Item('optimization').Cell('C11').cellvalue = 3
			timeover=hy_distinguish(hysolver)
			if MTD_HX1 <=2.85 or MTD_HX2<=2.85:
				error_type[0]=1
				if np.dot(error_type,error_type) !=0:
					error=1
					OBJ = np.random.random_sample()*10**30
					return OBJ
		
		VF1 = hyCase.Flowsheet.operations.Item('optimization').Cell('I8').cellvalue
		VF2 = hyCase.Flowsheet.operations.Item('optimization').Cell('I9').cellvalue
		VF3 = hyCase.Flowsheet.operations.Item('optimization').Cell('I10').cellvalue
		VF4 = hyCase.Flowsheet.operations.Item('optimization').Cell('I11').cellvalue
		VF5 = hyCase.Flowsheet.operations.Item('optimization').Cell('I12').cellvalue

		if VF1!=1 or VF2!=1 or VF3!=1 or VF4!=1 or VF5 !=1:
			error_type[2]=1
			if np.dot(error_type,error_type) !=0:
				error =1
				OBJ = np.random.random_sample()*10**30
				return OBJ

        # -- Transfer output variables (HYSYS --> python)
		OBJ  = hyCase.Flowsheet.operations.Item('optimization').Cell('G10').cellvalue # E Total Energy Consumption / LNG Production(Ton per day)
		return OBJ
	else:
		error=1
		OBJ = np.random.random_sample()*10**30
		return OBJ

# test run for center point
'''once you connect Aspen Hysys, just use hy_Object function for optimization'''
f_x0 = hy_Object(hyCase, hysolver, x0)
print('function output of x0 is: ',f_x0)
