import os
import numpy as np
import sys

def Variable_Data(compileFile):
    """Initially in Princeton lib. One step above, there's the 'problem data' folder which contains 
    data about the attributes used"""
    
    path = os.getcwd()
    folder_path = os.path.dirname(path)        # Location of folder containing ProblemData and Princeton library is 1 above
    # os.chdir(folder_path)
    probdata_path = os.path.join(folder_path, 'problemdata')
    os.chdir(probdata_path)
    print(os.getcwd())
    data_file = compileFile + '.problem.data'
    with open(data_file, 'r') as f:
    # with open('3pk.problem.data', 'r') as f:
        for i, line in enumerate(f):
            if i == 0:
                numofVar = int(line)
            if i == 1:
                low = line.split()
            if i == 2:
                up =line.split()    
            if i == 3:
                SP =line.split()   # Starting point 

    Upper = np.zeros(numofVar)
    Lower = np.zeros(numofVar)
    StartPt = np.zeros(numofVar)
    for i in range(numofVar):
        Upper[i] = float(up[i])
        Lower[i] = float(low[i])
        StartPt[i] = float(SP[i])

    os.chdir(path)      # Going back to original path with C files
    return numofVar, Upper, Lower, StartPt
    
def create_input(inputFile, points):
    # inputFile is always "input.in" when we look at the C-code.
    infile = open(inputFile, 'w')
    for i in points:
        infile.write(str(i)+"\n")
    infile.close()

def output_value():
    with open('output.out', 'r') as op:
        for line in op:
            y = float(line)
    return y

def fn_call(X, compileFile, compileFlag=0):                         # X is a (1,m) vector
    X = X.flatten()                     # Convert X to (m,) for using in input.in

    create_input("input.in", X)         
    print("Compiling")
    if compileFlag == 1:
        os.system('gcc '+compileFile+'.c -o '+compileFile)
    
    os.system('.\\'+compileFile)
    
    # os.system('python callexe.py ' + compileFile + ' ' + "input.in")
        
    y_data = output_value()
    return y_data

class Trust_Region_method():
    def __init__(self, compileFile, numofVar, Upper, Lower, n_points, Scaling_factor = None, Up_bounds = None, Low_bounds = None, StartPt = None, method = 'SOBOL', sf = 0.5):
        self.compileFile = compileFile      # "3pk" or "aircrftb" or ...
        self.numofVar = numofVar
        print('# var', self.numofVar)
        self.Upper = Upper                  # From problem data
        self.Lower = Lower                  # From problem data
        self.StartPt = StartPt              # From problem data
        if Scaling_factor is not None:          # In 1st iteration, scaling factor is set to 1 for all
            self.Scaling_factor = Scaling_factor
        else: self.Scaling_factor = np.ones(self.numofVar)
        self.n_points = n_points
        self.method = method
        self.sf = sf
        self.Up_bounds = Up_bounds
        self.Low_bounds = Low_bounds

    def Space_generator(self):      # Called to decrease the sampling region by the Scaling_factor
        # self.Scaling_factor *= self.sf
        delta = (self.Upper - self.Lower) / 2 * self.Scaling_factor
        new_Upper = self.StartPt + delta
        new_Lower = self.StartPt - delta
        self.Up_bounds = np.minimum(self.Upper, new_Upper)
        self.Low_bounds = np.maximum(self.Lower, new_Lower)
        # print(new_Upper,Low)
        # return Up, Low, Scaling_factor

    def Sampling_data(self):
        """Function to create data based on required sampling technique"""

        path = os.getcwd()                      # C files location: source_princetonlibgloballib
        one_up = os.path.dirname(path)          # Location of all other folders
        # os.chdir(one_up)
        pts_path = os.path.join(one_up, 'Data_points_generator')    # Inside the Data_points_generator folder
        sys.path.insert(0, pts_path)    # Adding pts_path location to look for .py files to be imported

        from data_points import data_points_gen # Importing required files from data_points folder
        pts = data_points_gen(self.n_points, self.numofVar, self.method)    # The pts created by data_points_gen lie in range [0, 1]
        points = np.zeros((self.n_points, self.numofVar))              # Initializing points array
        y_data = np.zeros(self.n_points)              # Initializing points array
        
        if self.StartPt is not None:     # If is called when we already have a starting optimal point
            self.Space_generator()
            print('Scaling_factor', self.Scaling_factor)
        
        else:                            # Else called in absence of a starting optimal point i.e. start of First iteration
            self.Up_bounds = self.Upper
            self.Low_bounds = self.Lower
            self.Scaling_factor = np.ones(len(self.Low_bounds)) # Create ones for the number of axes

        print(self.Low_bounds)
        print(self.Up_bounds)
        print('Scaling_factor', self.Scaling_factor)

        print('pts', pts)

        for i in range(self.n_points):
            
            points[i] = (self.Up_bounds- self.Low_bounds) * pts[i] + self.Low_bounds
            
            # y_data[i] = fn_call(points[i], self.compileFile) 

        # print('y_data',y_data)    
        os.chdir(path)                          # Returning back to original python file's location

        return points

compileFile = "ex8_1_1"     
numofVar, Upper, Lower, StartPt = Variable_Data(compileFile)
# print(numofVar, Upper, Lower, StartPt)

x0 = np.reshape(StartPt, (1,numofVar))
print(x0)
# create_input("input.in", x0.flatten())
# os.system('gcc '+compileFile+'.c -o '+compileFile)
# os.system('.\\'+compileFile)
y0_ = fn_call(x0, compileFile)
print(y0_)
