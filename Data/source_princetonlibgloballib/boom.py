# Boosted Momentum algorithm

import numpy as np
import alamo_train
import os

def op_writer(filename, fnCalls, z):
    file_ = filename + ".txt"
    with open('opfile.txt', 'a') as write_file:
        write_file.write("%s,%s\n" %(fnCalls,z))

import matplotlib.pyplot as plt
import matplotlib.animation as animation

def final():
    print('Check the excel files')

def animate(i):
    
    with open('opfile.txt','r') as data:
        xs = []
        zs = []
        for line in data:
            # print(line)
            x, z = line.split(',')
            xs.append(float(x))
            zs.append(float(z))

    plt.xlabel('Number of Function Calls')
    plt.ylabel('Objective value')
    plt.title('Live graph with matplotlib')	
	
class BCD():
    def __init__(self, xt, n_attr, data, Numpoints, xt_1 = None, fnCalls = 0, Present_iter = 0, n_blocks = None, 
                 Epochs = 1, var_order = None, max_epochs = 10, z_old = None):    
        self.xt = xt                    # Starting point. The value of x at 't' epoch
        # xt starts equal to 'xt_1' but gets updated during the epoch
        self.xt_1 = xt                  # Previous point. The value of x at 't-1'
        self.Lambda = np.zeros_like(xt) # Difference between xt and xt_1

        self.n_attr = n_attr            # n_attr = attrinbutes selected collectively to be minimized
        self.data = data                # data = about the present function's N_var,Upper, Lower, Startpt etc.
        self.Numpoints = Numpoints      # Number of points to sample for fitting a surrogate model
        self.fnCalls = fnCalls
        self.Present_iter = Present_iter
        self.n_blocks = n_blocks        # Integer value of number of blocks
        self.Epochs = Epochs
        self.Beta = 0.9

        print('EPOCHS', self.Epochs)
        if var_order == None:
            self.var_order = self.axis_order()
        else: self.var_order = var_order          # Order of var in an iteration
        # print(self.var_order)
        self.max_epochs = max_epochs             
        self.z_old = z_old
        
    def axis_order(self):              # Checks if order array is created or not
        if self.Epochs == 1:
            self.xl_writer("Headings")

        self.xt = np.reshape(self.xt, (1, len(self.xt)))  # Say there are 11 var 
        _, m = np.shape(self.xt)
        import math
        self.n_blocks = math.ceil(m / self.n_attr)    # ceil(11/2) = 6
        single_arr = np.arange(1, self.n_blocks)      # [1,2,3,4,5]  would be the iteration orders with all n_attr
        
        order = np.repeat(single_arr, self.n_attr)     
        # [1,1,2,2,3,3,4,4,5,5] : For n_attr (here 2), that many variables iterated over in 1st, ..., 5th iteration
        
        n_attr_last = int( m - self.n_attr * (self.n_blocks-1) )  # n_attr in last block = 11 - 2 * (6-1)
        last_arr = np.ones(n_attr_last) * self.n_blocks    # [1] * 6 and appending it below
        order = np.append(order, last_arr)

        # Random ordering
        # This we'll change after 1st iteration
        np.random.shuffle(order)          # [1,6,1,2,3,4,5,2,3,4,5] 

        self.var_order = {} 
        for j in range(len(order)):
            
            x_j = order[j]          # x_j = 1, 2, 4, ... 

            if x_j in self.var_order:       # i.e., if this iteration has already been passed in var_order dictionary
                self.var_order[int(x_j)].append(j)  

            else:                           
                self.var_order[int(x_j)] = list()   # else, we create a list for that iteration axis
                self.var_order[int(x_j)].append(j)  # Then we append all the axes to be selected for that iteration
        # {1: [2,4], 2:[3,1], ....}
        return self.var_order
    
    def modelData(self, up, low, scalingFactor, x_block, Numpoints, Evalpoint = None):
        '''
        Numpoints : The # of points to generate data from. It gives us flexibility to choose how many
        Evalpoints : If self.Numpoints was 5 & didn't gave satisfactory results, then on increasing Numpoints to 10, we'll not re-calculate points 0-5
        '''
        x_new = np.repeat(self.xt, Numpoints, axis=0)

        '''The block_data obj contains variable data for specific axes only'''
        # Below block data can be used to be passed as an argument to create tighter boundaries for data collection
        block_data = Sampling.Trust_Region_method(self.data.compileFile, len(up), up, low, Scaling_factor = scalingFactor, n_points = Numpoints, method = 'SOBOL', StartPt = x_block)
        '''Data fitting will be carried on the below block_pts'''
        
        block_pts = block_data.Sampling_data()      
        print('shape', block_pts.shape) 

        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]        # The variable being modified
            x_new[:,x_var] = block_pts[:,k]                     # Set specific axis with points collected from block_pts

        z_data = list()
        
        if Evalpoint is not None:           # When reusing old data and we want to evaluate points from new points only 
            x_eval = x_new[Evalpoint:] 
            block_pts = block_pts[Evalpoint:]
        else:
            x_eval = x_new

        for x in x_eval:
                
            if self.fnCalls > 100:                # Lting the number of iterations
                return# msg

            compileFlag = 1 if self.fnCalls == 0 else 0              
            
            zcalc = Sampling.fn_call(x, self.data.compileFile, compileFlag)
            z_data.append(zcalc)
            self.fnCalls += 1
            print('# of compilations done = ', self.fnCalls)
        
        print(block_pts, z_data)
        return block_pts, z_data

    def nextSP(self, up, low, scalingFactor, x_block, Numpoints, Evalpoint=None, old_data=None, err_tol = 0.1):
        if old_data != None:        # Called when initial data coldn't provide a good fit
            x_block_data, z_block_data = old_data
            try:
                block_pts, z_data = self.modelData(up, low, scalingFactor, x_block, Numpoints, Evalpoint)
            except:
                print('Max evals complete!')
                return

            block_pts = np.append(x_block_data, block_pts, axis=0)
            z_data = np.append(z_block_data, np.array(z_data), axis=0)      # z_data is a list

        elif old_data == None:      # Fitting set of pts for the 1st time
            block_pts, z_data = self.modelData(up, low, scalingFactor, x_block, Numpoints)  # Evalpoint=None for this condition
        
        '''Minimizing the data to get best point along selected axis'''
        
        minimizer = alamo_train.Minimizing(block_pts, z_data, up, low)
        minimizing_model = minimizer.alamo_fn()
        x_results = minimizer.Model_min(minimizing_model)
        print('x_results', x_results)
        
        '''Now, we have to assign new xt and scaling factor'''
        "1. xt"
        # First, let's check if the new point returned is actually better
        x_temp = self.xt.flatten()       # To convert 2D array to 1D
        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]     # The variable being modified
            x_temp[x_var] = x_results.x[k]
        
        z_actual = Sampling.fn_call(x_temp, self.data.compileFile)
        self.fnCalls += 1
        
        # Not a good fit for the BB fun
        if z_actual >= x_results.fun * (1 + err_tol):
            print('Entered if loop')
            '''To avoid wasting the data collected till now, let's pass this as input 
            to fit in this function back'''
            x_pred = np.array([x_results.x])       #.shape, block_pts.shape)
            x_block_data = np.append(block_pts, x_pred, axis=0)
            z_block_data = np.append(z_data, z_actual)

            old_data = (x_block_data, z_block_data)
            Evalpoint = Numpoints
            newNumpoints = Numpoints * 2
            return self.nextSP(up, low, scalingFactor, x_block, newNumpoints, Evalpoint, old_data=old_data)

        # Predicted results are a good enough fit
        else:
            self.xt = np.array([x_temp])                    # UPDATED the starting point
        print('new_sp', self.xt)

        print('# of compilations = ', self.fnCalls)
        print(z_actual, x_results.fun)
        op_writer(self.data.compileFile, self.fnCalls, z_actual)
        
        return z_actual

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
            x_block.append(self.xt[0][list_of_axes[k]])   # xt[0] because xt has a shape of (1, n_attr) 
            up.append(self.data.Upper[list_of_axes[k]])
            low.append(self.data.Lower[list_of_axes[k]])
            scalingFactor.append(self.data.Scaling_factor[list_of_axes[k]])

        up = np.array(up)
        low = np.array(low)
        scalingFactor = np.array(scalingFactor)
        x_block = np.array([x_block])      # Part of xt with concerned axes having a shape of (1,n_attr)
        print('x_block', x_block)
        try:
            z_actual = self.nextSP(up, low, scalingFactor, x_block, self.Numpoints)
            
            "2. scaling factor"
            scaling_fact = self.scaling(scalingFactor, z_actual, self.z_old, 1e-2)      # scaling_fact is the complete scaling factor before completion of the alamo train

            for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
                x_var = self.var_order[self.Present_iter][k]     # The variable being modified
                self.data.Scaling_factor[x_var] = scaling_fact[k]
            
            print("Scaling factor")
            print(self.data.Scaling_factor)

            ######## update z_old to z_actual only after updating the Scaling_factor
            if self.z_old == None or self.z_old > z_actual:           # Update iff z improves
                self.z_old = z_actual

            self.xl_writer(action = "Data")

            return self.Recursion()

        except:
            return final()
        
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

            # CHANGES:
            # This loop entered for first time when self.Epochs = 2.
            self.Lambda = self.xt - self.xt_1
            
            self.xt_1 = self.xt     # setting xt to xt_1 as next epoch starts

            new_BCD = BCD(self.xt.flatten(), self.n_attr, self.data, fnCalls = self.fnCalls, Epochs = self.Epochs,
                            Numpoints = self.Numpoints, max_epochs = self.max_epochs, z_old = self.z_old)
            new_BCD.Recursion()

        else:
            print('All epochs trained')

    def scaling(self, scalingFactor, z_new, z_old, tol):   # tol = tolerance to stop
        
        if self.z_old == None:
            scaling_fact = scalingFactor * self.data.sf
        
        elif z_new < z_old * (1 - tol):
            scaling_fact = scalingFactor * self.data.sf

        elif z_new > z_old * (1 - tol):                     #
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

    def xl_writer(self, action, to_write = False):

        if to_write:                
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
                ws.write(0,3, 'Z', style)

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
                w1.write(nrow, 3, self.z_old)
                for i in range(self.data.numofVar):
                    X_var = self.xt.flatten()[i]
                    w1.write(nrow, 4 + i, X_var)

            wb.save(xl_file)

            os.chdir(path)

        else:
            print('No excel output')

import Sampling

compileFile = "box3"
numofVar, Upper, Lower, StartPt = Sampling.Variable_Data(compileFile)
data = Sampling.Trust_Region_method(compileFile, numofVar, Upper, Lower, n_points = 1, method = 'SOBOL', StartPt = StartPt) # Scaling_factor = 1,
# Scaling_factor = [1,1, ...numofVar times]
blockCoordDescent = BCD(data.StartPt, 2, data, Numpoints = 15, max_epochs = 1)
blockCoordDescent.Call_BCD()

