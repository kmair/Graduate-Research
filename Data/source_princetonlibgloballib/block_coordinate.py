#%%
import numpy as np
import alamo_train
import os

def op_writer(filename, fnCalls, y):
    file_ = filename + ".txt"
    with open('opfile.txt', 'a') as write_file:
        write_file.write("%s,%s\n" %(fnCalls,y))

import matplotlib.pyplot as plt
import matplotlib.animation as animation

def animate(i):
    
    with open('opfile.txt','r') as data:
        xs = []
        ys = []
        for line in data:
            # print(line)
            x,y = line.split(',')
            xs.append(float(x))
            ys.append(float(y))

    plt.xlabel('Number of Function Calls')
    plt.ylabel('Objective value')
    plt.title('Live graph with matplotlib')	
	
# fn_writer(10)
    
# ani = animation.FuncAnimation(fig, animate, interval=1000) 
# plt.show()

class BCD():
    def __init__(self, x_sp, n_attr, data, Numpoints, fnCalls = 0, Present_iter = 0, n_blocks = None, Epochs = 1, var_order = None, max_epochs = 10, y_old = None):    
        self.x_sp = x_sp            # Starting point
        
        self.n_attr = n_attr        # n_attr = attrinbutes selected collectively to be minimized
        self.data = data            # data = about the present function's N_var,Upper, Lower, Startpt etc.
        self.Numpoints = Numpoints  # Number of points to sample for fitting a surrogate model
        self.fnCalls = fnCalls
        self.Present_iter = Present_iter
        self.n_blocks = n_blocks
        self.Epochs = Epochs

        print('EPOCHS', self.Epochs)
        if var_order == None:
            self.var_order = self.axis_order()
        else: self.var_order = var_order          # Order of var in an iteration
        # print(self.var_order)
        self.max_epochs = max_epochs             
        self.y_old = y_old
        
    def axis_order(self):              # Checks if order array is created or not
        if self.Epochs == 1:
            self.xl_writer("Headings")
        self.x_sp = np.reshape(self.x_sp, (1, len(self.x_sp)))  # Say there are 11 var and 
        _, m = np.shape(self.x_sp)
        import math
        self.n_blocks = math.ceil(m / self.n_attr)    # ceil(11/2) = 6
        single_arr = np.arange(1, self.n_blocks)      # [1,2,3,4,5]  would be the iteration orders with all n_attr
        
        order = np.repeat(single_arr, self.n_attr)     
        # [1,1,2,2,3,3,4,4,5,5] : For n_attr (here 2), that many variables iterated over in 1st, ..., 5th iteration
        
        n_attr_last = int( m - self.n_attr * (self.n_blocks-1) )  # n_attr in last block = 11 - 2 * (6-1)
        last_arr = np.ones(n_attr_last) * self.n_blocks    # [1] * 6 and appending it below
        order = np.append(order, last_arr)

        # Random ordering
        np.random.shuffle(order)          # [1,6,1,2,3,4,5,2,3,4,5] 
        # self.order = np.sort(self.order)         
        # print(m , self.n_attr , n_blocks)
        self.var_order = {} 
        for j in range(len(order)):
            
            x_j = order[j]          # x_j = 1, 2, 4, ... 

            if x_j in self.var_order:       # i.e., if this iteration has already been passed in var_order dictionary
                # print(x_j)
                self.var_order[int(x_j)].append(j)  

            else:                           
                self.var_order[int(x_j)] = list()   # else, we create a list for that iteration axis
                self.var_order[int(x_j)].append(j)  # Then we append all the axes to be selected for that iteration
        
        return self.var_order
    
    def modelData(self, up, low, scalingFactor, x_block, Numpoints, Evalpoint = None):
        '''
        Numpoints : The # of points to generate data from. It gives us flexibility to choose how many
        Evalpoints : If self.Numpoints was 5 & didn't gave satisfactory results, then on increasing Numpoints to 10, we'll not re-calculate points 0-5
        '''

        x_new = np.repeat(self.x_sp, Numpoints, axis=0)

        '''The block_data obj contains variable data for specific axes only'''
        # Below block data can be used to be passed as an argument to create tighter boundaries for data collection
        block_data = Sampling.Trust_Region_method(self.data.compileFile, len(up), up, low, Scaling_factor = scalingFactor, n_points = Numpoints, method = 'SOBOL', StartPt =x_block)
        '''Data fitting will be carried on the below block_pts'''
        
        block_pts = block_data.Sampling_data()      
        print('shape', block_pts.shape) 

        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]    # The variable being modified
            x_new[:,x_var] = block_pts[:,k]                 # Set specific axis with points collected from block_pts

        y_data = list()
        
        x_eval = x_new[Evalpoint:] if Evalpoint is not None else x_new

        for x in x_eval:
                
            if self.fnCalls > 100:                # <<<<<<<<<<<<<<<<<<<----------------------- Lting the number of iterations
                print('Max evals complete!')
                return

            compileFlag = 1 if self.fnCalls == 0 else 0              
            
            ycalc = Sampling.fn_call(x, self.data.compileFile, compileFlag)
            y_data.append(ycalc)
            self.fnCalls += 1
            print('# of compilations done = ', self.fnCalls)
        
        return block_pts, y_data

    def nextSP(self, up, low, scalingFactor, x_block, Numpoints, Evalpoint=None, err_tol = 0.1):
        block_pts, y_data = self.modelData(up, low, scalingFactor, x_block, Numpoints, Evalpoint)
        
        '''Minimizing the data to get best point along selected axis'''
        
        minimizer = alamo_train.Minimizing(block_pts, y_data, up, low)
        minimizing_model = minimizer.alamo_fn()
        x_results = minimizer.Model_min(minimizing_model)
        print('x_results', x_results)
        
        '''Now, we have to assign new x_sp and scaling factor'''
        "1. x_sp"
        # First, let's check if the new point returned is actually better
        x_temp = self.x_sp[0]
        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]     # The variable being modified
            x_temp[x_var] = x_results.x[k]
        
        y_actual = Sampling.fn_call(x_temp, self.data.compileFile)
        self.fnCalls += 1
        
        if y_actual >= x_results.fun * (1 + err_tol):
            Evalpoint = Numpoints
            newNumpoints = Numpoints * 2
            print('Entered if loop')
            return self.nextSP(up, low, scalingFactor, x_block, newNumpoints, Evalpoint)

        else:
            self.x_sp = np.array([x_temp])                    # UPDATED the starting point
        print('new_sp', self.x_sp)

        print('# of compilations = ', self.fnCalls)
        print(y_actual, x_results.fun)
        op_writer(self.data.compileFile, self.fnCalls, y_actual)
        
        return y_actual

    def Call_BCD(self):
        self.Present_iter += 1
        print('Iteration no.', self.Present_iter)

        # x-attributes in this iteration would be sampled
        '''First iteration: range[0 to n_attr], Second iteration: range[n_attr to 2*n_attr], .. 
        Last iteration: range[(n_iter-1) * n_attr to end]'''
        list_of_axes = self.var_order[self.Present_iter]        
        # Collecting the list of Upper and Lower limits and Starting point only along axes in the block
        x_block = list()        
        up = list()         # Sets the upper bound during alamo training
        low = list()
        scalingFactor = list()
        for k in range(len(list_of_axes)):
            # print('Axis data of', list_of_axes[k])
            x_block.append(self.x_sp[0][list_of_axes[k]])   # x_sp[0] because x_sp has a shape of (1, n_attr) 
            up.append(self.data.Upper[list_of_axes[k]])
            low.append(self.data.Lower[list_of_axes[k]])
            scalingFactor.append(self.data.Scaling_factor[list_of_axes[k]])

        up = np.array(up)
        low = np.array(low)
        scalingFactor = np.array(scalingFactor)
        x_block = np.array([x_block])      # Part of x_sp with concerned axes having a shape of (1,n_attr)
        print('x_block', x_block)

        y_actual = self.nextSP(up, low, scalingFactor, x_block, self.Numpoints)
        
        "2. scaling factor"
        scaling_fact = self.scaling(scalingFactor, y_actual, self.y_old, 1e-2)      # scaling_fact is the complete scaling factor before completion of the alamo train

        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]     # The variable being modified
            self.data.Scaling_factor[x_var] = scaling_fact[k]
        
        print("Scaling factor")
        print(self.data.Scaling_factor)

        ######## update y_old to y_actual only after updating the Scaling_factor
        if self.y_old == None or self.y_old > y_actual:           # Update iff y improves
            self.y_old = y_actual

        self.xl_writer(action = "Data")

        return self.Recursion()

    def Recursion(self):
        if self.Present_iter < self.n_blocks:
            # os.system("python live_animation.py")
            self.Call_BCD()

        elif self.Epochs < self.max_epochs:
            
            self.Epochs += 1
            print("        ______                       ______    ___        ___       ____          ")
            print("|\   | |          \          /      |         |   \      /   \     /       |     |")
            print("| \  | |____       \        /       |___      |___/     /     \   /        |_____|")
            print("|  \ | |            \  /\  /        |         |         \     /   \        |     |")
            print("|   \| |______       \/  \/         |______   |          \___/     \____   |     |")

            new_BCD = BCD(self.x_sp.flatten(), self.n_attr, self.data, fnCalls = self.fnCalls, Epochs = self.Epochs,
                            Numpoints = self.Numpoints, max_epochs = self.max_epochs, y_old = self.y_old)
            new_BCD.Recursion()

        else:
            print('All epochs trained')

    def scaling(self, scalingFactor, y_new, y_old, tol):   # tol = tolerance to stop
        
        if self.y_old == None:
            scaling_fact = scalingFactor * self.data.sf
        
        elif y_new < y_old * (1 - tol):
            scaling_fact = scalingFactor * self.data.sf

        elif y_new > y_old * (1 - tol):                     #
            scaling_fact = scalingFactor * self.data.sf
            
        else:
            print("Going into errors")
            scaling_fact = scalingFactor

        return scaling_fact

    @staticmethod           # Method to get back sheet by name
    def get_sheet_by_name(book, name):
        """Get a sheet by name from xlwt.Workbook, a strangely missing method.
        Returns None if no sheet with the given name is present.
        """
        # Note, we have to use exceptions for flow control because the
        # xlwt API is broken and gives us no other choice.
        import itertools

        try:
            for idx in itertools.count():
                sheet = book.get_sheet(idx)
                if sheet.name == name:
                    return sheet
        except IndexError:
            return None

    def xl_writer(self, action):
                
        import xlrd
        import xlwt
        import xlutils 
        from xlutils.copy import copy

        path = os.getcwd()                      # C files location: source_princetonlibgloballib
        xl_file = self.data.compileFile + '.xls' 
        sh_name = 'attr' + str(self.n_attr) + '_pts' + str(self.Numpoints)
        os.chdir(os.path.join(os.getcwd(), "Excel files"))
        fname = os.path.join(os.getcwd(), xl_file)

        if action == "Headings":
            
            print(fname)
            if os.path.exists(fname) is False:      # Creating a new xl file if it doesn't exist yet
                xl_workbook = xlwt.Workbook()
                wb = xl_workbook
                ws = wb.add_sheet(sh_name)        # New xl and sheet

            else:
                xl_workbook = xlrd.open_workbook(fname) # , on_demand = True
                wb = copy(xl_workbook)
                ws = wb.add_sheet(sh_name)        # New xl and sheet                 

            style = xlwt.easyxf('font: name Times New Roman, bold on') #, num_format_str='#,##0.00', color-index red)
            # wb = xlwt.Workbook()
            ws.write(0,0, 'Epochs', style)
            ws.write(0,1, 'Iterations', style)
            ws.write(0,2, 'Number of Compilations', style)
            ws.write(0,3, 'Y', style)

            for i in range(self.data.numofVar):
                X_var = 'X' + str(i+1)
                ws.write(0, 4 + i, X_var, style)
            
            wb.save(xl_file)

        if action == "Data":
            import xlutils
            from xlutils.copy import copy
            import itertools

            fname = os.path.join(os.path.dirname((os.path.abspath(__file__))), xl_file)
            xl_workbook = xlrd.open_workbook(fname) # , on_demand = True
            
            wb = copy(xl_workbook)
            w1 = BCD.get_sheet_by_name(wb, sh_name)

            nrow = len(w1._Worksheet__rows)

            w1.write(nrow, 0, self.Epochs)
            w1.write(nrow, 1, self.Present_iter)
            w1.write(nrow, 2, self.fnCalls)
            w1.write(nrow, 3, self.y_old)
            for i in range(self.data.numofVar):
                X_var = self.x_sp.flatten()[i]
                w1.write(nrow, 4 + i, X_var)

        wb.save(xl_file)

        
        os.chdir(path)

import Sampling

compileFile = "box3"
numofVar, Upper, Lower, StartPt = Sampling.Variable_Data(compileFile)
data = Sampling.Trust_Region_method(compileFile, numofVar, Upper, Lower, n_points = 1, method = 'SOBOL', StartPt =StartPt) # Scaling_factor = 1,
# Scaling_factor = [1,1, ...numofVar times]
blockCoordDescent = BCD(data.StartPt, 2, data, Numpoints = 10, max_epochs = 20)
blockCoordDescent.Call_BCD()
