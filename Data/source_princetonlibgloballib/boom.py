# Boosted Momentum algorithm
import numpy as np
import alamo_train
import os
HOME_PATH = os.getcwd()
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
    def __init__(self, xt, n_attr, data, Numpoints, xt_1 = None, fnCalls = 0, Present_iter = 0, n_blocks = None, var_order = None, 
                 Epochs = 1, max_epochs = 10, z_old = None, Lambda = None, Beta = 0.9, history = dict(), patience = 3, improvement = 1e-3,
                 SP_dict = None, max_calls = 2500):    

        self.xt = xt                    # Starting point. The value of x at 't' epoch
        # xt starts equal to 'xt_1' but gets updated during the epoch
        
        self.n_attr = n_attr if n_attr < len(self.xt) else len(self.xt)
        # n_attr = attrinbutes selected collectively to be minimized
        self.data = data                # data = about the present function's N_var,Upper, Lower, Startpt etc.
        self.Numpoints = Numpoints      # Number of points to sample for fitting a surrogate model
        self.fnCalls = fnCalls
        self.Present_iter = Present_iter
        self.n_blocks = n_blocks        # Integer value of number of blocks
        self.Epochs = Epochs
        
        self.xt_1 = xt_1 if self.Epochs != 1 else xt       # Previous point. The value of x at 't-1'
        self.Lambda = Lambda if self.Epochs != 1 else np.zeros_like(xt) # Difference between xt and xt_1
        self.Beta = Beta                    # Beta = 0.9 selected based on papers
        self.history = history              # collects the z value after the end of epoch to track improvements
        self.patience = patience            # patience for staying on a block for iteration 
        self.improvement = improvement
        self.max_calls = max_calls

        print('EPOCHS', self.Epochs)
        if var_order == None:
            self.var_order = self.axis_order()
        else: self.var_order = var_order          # Order of var in an iteration
        # print(self.var_order)
        self.max_epochs = max_epochs             
        self.z_old = z_old if z_old != None else Sampling.fn_call(self.xt_1, self.data.compileFile) 
        self.SP_dict = SP_dict
        
    def axis_order(self):              # Checks if order array is created or not
        if self.Epochs == 1:
            self.xl_writer("Headings")
        print('self.xt', self.xt)
        self.xt = np.reshape(self.xt, (1, len(self.xt)))  # Say there are 11 var 
        _, m = np.shape(self.xt)
        self.n_blocks = np.ceil(m / self.n_attr)    # ceil(11/2) = 6
        
        # For 1st epoch only when we don't have Lambda
        order = np.arange(m) + 1
        order = order / self.n_attr
        order = np.ceil(order)
        # Random ordering
        # This we'll change after 1st iteration
        np.random.shuffle(order)          # [1,6,1,2,3,4,5,2,3,4,5] 

        if self.Epochs > 1:
            # goes from flat variables (with biggest steps) to variables with high slope
            axis_imp = np.argsort(-np.abs(self.Lambda)) + 1  # added '-' to sort in reverse order
            # axis_imp = [5,2,4,1,3]                
            order = np.ceil(axis_imp / self.n_attr)

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
    
    def modelData(self, up, low, scalingFactor, y_block, Numpoints, Evalpoint = None):
        '''
        Numpoints : The # of points to generate data from. It gives us flexibility to choose how many
        Evalpoints : If self.Numpoints was 5 & didn't gave satisfactory results, then on increasing Numpoints to 10, 
                     we'll not re-calculate points 0-5
        '''
        x_new = np.repeat(self.xt, Numpoints, axis=0)

        '''The block_data obj contains variable data for specific axes only'''
        # Below block data can be used to be passed as an argument to create tighter boundaries for data collection
        block_data = Sampling.Trust_Region_method(self.data.compileFile, len(up), up, low, Scaling_factor = scalingFactor,
                                                  n_points = Numpoints, method = 'SOBOL', StartPt = y_block)
        '''Data fitting will be carried on the below block_pts'''
        
        block_pts = block_data.Sampling_data()      
        # print('shape', block_pts.shape) 

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
                
            if self.fnCalls > self.max_calls:                # Lting the number of iterations ####################### To CHANGE 100
                return

            compileFlag = 1 if self.fnCalls == 0 else 0              
            
            zcalc = Sampling.fn_call(x, self.data.compileFile, compileFlag)
            z_data.append(zcalc)
            self.fnCalls += 1
        
        return block_pts, z_data

    def nextSP(self, up, low, scalingFactor, y_block, Numpoints, Evalpoint=None, old_data=None, err_tol = 0.2, patience = None):
        if patience is None:        # Occurs in the first iteration
            patience = self.patience 

        patience -= 1               # Decereasing the patience level
        
        if old_data != None:        # Called when initial data coldn't provide a good fit
            print('Numpoints', Numpoints )
            y_block_data, z_block_data = old_data
            try:
                block_pts, z_data = self.modelData(up, low, scalingFactor, y_block, Numpoints, Evalpoint)
            except:
                print('Max evals complete!')
                return

            block_pts = np.append(y_block_data, block_pts, axis=0)
            z_data = np.append(z_block_data, np.array(z_data), axis=0)      # z_data is a list

        elif old_data == None:      # Fitting set of pts for the 1st time
            block_pts, z_data = self.modelData(up, low, scalingFactor, y_block, Numpoints)  # Evalpoint=None for this condition
        
        '''Minimizing the data to get best point along selected axis'''
        
        minimizer = alamo_train.Minimizing(block_pts, z_data, up, low)
        minimizing_model = minimizer.alamo_fn()
        x_results = minimizer.Model_min(minimizing_model)
        # print('x_results', x_results)
        
        '''Now, we have to assign new xt and scaling factor'''
        "1. xt"
        # First, let's check if the new point returned is actually better
        x_temp = self.xt.flatten()       # To convert 2D array to 1D
        for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
            x_var = self.var_order[self.Present_iter][k]     # The variable being modified
            x_temp[x_var] = x_results.x[k]
        
        z_actual = Sampling.fn_call(x_temp, self.data.compileFile)
        self.fnCalls += 1
        
        print('Comparison of z (fitting, data):', z_actual, min(z_data))
        # Not a good fit for the BB fun so fit a fun with greater number of points
        '''Can add a patience parameter here'''
        if min(z_data) > self.z_old and z_actual > self.z_old:  # z_actual >= x_results.fun * (1 + err_tol)
            print('Entered if loop')
            # print('Patience', patience)
        
            if patience < 0:
                '''Get out of recursively revolving around the block stuck in a local optima and return the same z value
                for this iteration
                '''
                return self.z_old
            
            else:
                pass
            '''To avoid wasting the data collected till now, let's pass this as input 
            to fit in this function back'''
            x_pred = np.array([x_results.x])       #.shape, block_pts.shape)
            y_block_data = np.append(block_pts, x_pred, axis=0)
            z_block_data = np.append(z_data, z_actual)

            old_data = (y_block_data, z_block_data)
            Evalpoint = Numpoints
            newNumpoints = min(Numpoints * 2, Numpoints + 40)

            scaling_fact = self.scaling(scalingFactor, z_actual, self.z_old, 1e-2)      # scaling_fact is the complete scaling factor before completion of the alamo train
            
            # The below is to update the scaling_fact for our data also
            for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
                x_var = self.var_order[self.Present_iter][k]     # The variable being modified
                self.data.Scaling_factor[x_var] = scaling_fact[k]

            # Will use the updated scaling factor (will always be >1 as it's to get out of local optima)
            return self.nextSP(up, low, scaling_fact, y_block, newNumpoints, Evalpoint, old_data=old_data, patience = patience)

        # Predicted results are a good enough fit
        elif z_actual < min(z_data):
            print('Predicted results are a good enough fit')
            self.xt = np.array([x_temp])                    # UPDATED the starting point

            print('# of compilations = ', self.fnCalls)
            print(z_actual, x_results.fun)
            op_writer(self.data.compileFile, self.fnCalls, z_actual)
            return z_actual
        # Data points have a better point than Predicted results 
        elif z_actual > min(z_data):
            print('Data points have a better point than Predicted results')
            xbest = block_pts[ np.argmin(z_data) ]

            for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
                x_var = self.var_order[self.Present_iter][k]     # The variable being modified
                x_temp[x_var] = xbest[k]

            self.xt = np.array([x_temp])                    # UPDATED the starting point

            print('# of compilations = ', self.fnCalls)
            op_writer(self.data.compileFile, self.fnCalls, min(z_data))
            return min(z_data)

        else:
            print('Unpredicted behaviour')
        return z_actual

    def Call_BCD(self):
        self.Present_iter += 1
        
        print('Iteration no.', self.Present_iter)

        stop_search = False

        if self.Present_iter == self.n_blocks:      # Tracking history before iteration on last coordinate block
            stop_search = self.history_tracker()
        
        if stop_search:
            print('Going into newPoint fn')
            print('recurin fn', self.SP_dict)
            return newPoint(self.max_calls, self.n_attr, self.data, self.Numpoints, self.fnCalls, self.max_epochs,
            SP_dict= self.SP_dict)
        
        else:
            # pass
            # x-attributes in this iteration would be sampled
            '''First iteration: range[0 to n_attr], Second iteration: range[n_attr to 2*n_attr], .. 
            Last iteration: range[(n_iter-1) * n_attr to end]'''
            list_of_axes = self.var_order[self.Present_iter]        
            # Collecting the list of Upper and Lower limits and Starting point only along axes in the block
            y_block = list()        
            up = list()         # Sets the upper bound during alamo training
            low = list()
            scalingFactor = list()
            for k in range(len(list_of_axes)):
                yk = self.xt[0][list_of_axes[k]] + self.Beta * self.Lambda[list_of_axes[k]]
                # print(self.Epochs, 'Lambda',self.Lambda[list_of_axes[k]])
                # xt[0] because xt has a shape of (1, n_attr)
                y_block.append(yk)    
                up.append(self.data.Upper[list_of_axes[k]])
                low.append(self.data.Lower[list_of_axes[k]])
                scalingFactor.append(self.data.Scaling_factor[list_of_axes[k]])
            
            print(self.xt, '-->', y_block)
            print(self.Lambda)
                
            up = np.array(up)
            low = np.array(low)
            scalingFactor = np.array(scalingFactor)
            y_block = np.array([y_block])      # Part of xt with concerned axes having a shape of (1,n_attr)

            try:        #    <----------------------------------------------------- TO CHANGE -----------------------------------------------------
            # if 2>1:
                z_actual = self.nextSP(up, low, scalingFactor, y_block, self.Numpoints)
                
                "2. scaling factor"
                scaling_fact = self.scaling(scalingFactor, z_actual, self.z_old, 1e-2)      # scaling_fact is the complete scaling factor before completion of the alamo train

                for k in range(len(self.var_order[self.Present_iter])): # Selecting the var_order of the specific iteration
                    x_var = self.var_order[self.Present_iter][k]     # The variable being modified
                    self.data.Scaling_factor[x_var] = scaling_fact[k]
                
                ######## update z_old to z_actual only after updating the Scaling_factor
                if self.z_old > z_actual:           # Update iff z improves              self.z_old == None or
                    self.z_old = z_actual
                    print('z_old', self.z_old)

                self.xl_writer(action = "Data")

                return self.Recursion()

            except ValueError:
                print('ValueError')
                return final()
                    
    def Recursion(self):
        if self.Present_iter < self.n_blocks:
            # os.system("python live_animation.py")
            self.Call_BCD()

        elif self.Epochs < self.max_epochs:
            
            self.Epochs += 1
            print("        ______                      ______    ___     ___      ____          ")
            print("|\   | |         \          /      |         |   \   /   \    /       |     |")
            print("| \  | |____      \        /       |___      |___/  /     \  /        |_____|")
            print("|  \ | |           \  /\  /        |         |      \     /  \        |     |")
            print("|   \| |______      \/  \/         |______   |       \___/    \____   |     |", '\n')

            # CHANGES:
            # This loop entered for first time when self.Epochs = 2.
            print(self.xt, self.xt_1)
            self.Lambda = self.xt - self.xt_1
            print(self.Lambda)
            self.xt_1 = self.xt     # setting xt to xt_1 as next epoch starts

            new_BCD = BCD(self.xt.flatten(), self.n_attr, self.data, xt_1 = self.xt_1.flatten(), fnCalls = self.fnCalls, Epochs = self.Epochs,
                            Numpoints = self.Numpoints, max_epochs = self.max_epochs, z_old = self.z_old, Lambda = self.Lambda.flatten(), 
                            history=self.history, patience= self.patience, SP_dict= self.SP_dict)
            new_BCD.Recursion()

        else:
            print('All epochs trained')

    def history_tracker(self):
        self.history[self.Epochs] = self.z_old
        print('History', self.history)

        'Comparing the progress after 2 epochs at least and when the improvement is significant enough'
        if self.Epochs >=2 and self.history[self.Epochs] - self.history[self.Epochs - 1] >= -np.abs(self.history[self.Epochs] * self.improvement):
            print("-------------> Reached a local minima <-------------")
            return True     # Time to stop searching!

        else:            
            return False    # Significant improvement in values. Keep on iterating and it's not the time to stop searching.

    def scaling(self, scalingFactor, z_new, z_old, tol):   # tol = tolerance to stop
        # scalingFactor: Only for the variables being changed in the block
        print(z_new, z_old)
        
        ratio = z_new / z_old if np.abs(z_old) > 1e-3 else z_new / 1e-3 
        if z_new < z_old:         # * (1 - tol)
            print('Improvement', z_new , z_old)

        elif z_new >= z_old:      # * (1 - tol)
            print('Degradation', z_new , z_old)
        
        if ratio < 0:
            # self.data.sf is a constant <1 taken 

            if z_new < 0:   # This means z has decreased from positive to negative
                scaling_fact = scalingFactor * self.data.sf

            elif z_new > 0: # This means z has increased from positive to negative
                scaling_fact = scalingFactor / self.data.sf

        elif ratio >= 0:

            if z_new >= 0:   
                print('Enters 1st')
                scaling_fact = scalingFactor * ( ratio ** (1 / self.data.sf) )

            elif z_new < 0: 
                print('Enters 2nd')
                scaling_fact = scalingFactor * ( ratio ** (self.data.sf) )

        # if z_new < z_old:         # * (1 - tol)
        #     print('Improvement', z_new , z_old)
        #     scaling_fact = scalingFactor * self.data.sf

        # elif z_new >= z_old:      # * (1 - tol)
        #     print('Degradation', z_new , z_old)
        #     scaling_fact = scalingFactor * self.data.sf
            
        else:
            print("Going into errors")
            print('z_new', z_new, 'z_old', z_old)
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

    def xl_writer(self, action, to_write = True):

        if to_write:                
            import xlrd
            import xlwt
            import xlutils 
            from xlutils.copy import copy

            path = os.getcwd()                      # C files location: source_princetonlibgloballib
            xl_file = self.data.compileFile + '.xls' 
            sh_name = 'attr' + str(self.n_attr) + '_pts' + str(self.Numpoints) + '_beta' + str(self.Beta)

            # os.chdir(os.path.join(os.getcwd(), "Excel files"))
            os.chdir(os.path.join(HOME_PATH, "Excel files"))

            fname = os.path.join(os.getcwd(), xl_file)

            if action == "Headings":
                
                if os.path.exists(fname) is False:      # Creating a new xl file if it doesn't exist yet
                    xl_workbook = xlwt.Workbook()
                    wb = xl_workbook
                    ws = wb.add_sheet(sh_name)        # New xl and sheet

                else:
                    xl_workbook = xlrd.open_workbook(fname) 
                    wb = copy(xl_workbook)
                    
                    try:
                        ws = wb.add_sheet(sh_name)        # New xl and sheet  
                    except:
                        ws = wb.get_sheet(sh_name)

                style = xlwt.easyxf('font: name Times New Roman, bold on') #, num_format_str='#,##0.00', color-index red)
                # wb = xlwt.Workbook()
                nrow = len(ws._Worksheet__rows)

                ws.write(nrow,0, 'Epochs', style)
                ws.write(nrow,1, 'Iterations', style)
                ws.write(nrow,2, 'Number of Compilations', style)
                ws.write(nrow,3, 'Z', style)

                for i in range(self.data.numofVar):
                    X_var = 'X' + str(i+1)
                    ws.write(nrow, 4 + i, X_var, style)
                
                wb.save(xl_file)

            if action == "Data":
                import xlutils
                from xlutils.copy import copy
                import itertools

                fname = os.path.join(os.path.dirname( (os.path.abspath(__file__)) ), xl_file)
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

def newPoint(max_calls, n_attr, data = None, Numpoints = 20, fnCalls= 0, max_epochs = 20, SP_dict = None, numberSP = 5):
    '''
    f_calls: No. of fn calls remaining
    n_points: Number of starting points for the future
    '''

    if SP_dict == None:     # In first iteration, when we start with the already provided starting point
        data = Sampling.Trust_Region_method(compileFile, numofVar, Upper, Lower, numberSP, method = 'LATIN', StartPt = StartPt) # Scaling_factor = 1,
        # Sampling.fn_call()
        init_pts = data.Sampling_data()

        f_calls = len(init_pts)     # No. of calls to collect find the starting points 
        z = np.array([Sampling.fn_call(X, data.compileFile) for X in init_pts])
        SP_dict = {zsp: xsp for zsp, xsp in zip(z, init_pts)}
        blockCoordDescent = BCD(data.StartPt, n_attr, data, Numpoints = Numpoints, fnCalls= f_calls, max_epochs = max_epochs, SP_dict= SP_dict, max_calls= max_calls)
        blockCoordDescent.Call_BCD()

    # Greedy search: selecting new starting point which has least z (We can incorporate a probabilistic metric for selection)
    print('SP_dict', SP_dict)
    
    try:
        z_min = min(SP_dict.keys())
        x_min = SP_dict[z_min]
        # Having selected the x and z values, we delete this value in the dictionary so that we don't start off at same point again
        del SP_dict[z_min]
        print('-----data------------', data)
        blockCoordDescent = BCD(x_min, n_attr, data, Numpoints = Numpoints, fnCalls= fnCalls, max_epochs = max_epochs, SP_dict= SP_dict, max_calls= max_calls)
        blockCoordDescent.Call_BCD()

    except ValueError:      # This except catches the error when SP_dict is empty i.e., we have iterated over all starting points.
        print('Trained for all the starting points.')

if __name__ == "__main__":
    pass
max_calls = 3000
compileFile = "denschnb"
numofVar, Upper, Lower, StartPt = Sampling.Variable_Data(compileFile)


newPoint(max_calls, 1,Numpoints = 23)


# TARGET: http://thales.cheme.cmu.edu/dfo/comparison/comp.html 