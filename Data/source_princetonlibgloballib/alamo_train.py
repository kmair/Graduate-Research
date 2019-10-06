#%%
import numpy as np
import alamopy
# help(alamopy.doalamo.alamo)

#%%
# N = 2000
# m = 3
# xdata = np.random.rand(N,m)

# def y_(x):         # (x1 - 1)^2 + (x2 - 1)^2 + ...
#     m = len(x)
#     Root= np.arange(m)
#     s= 0
#     for j in range(m):
#         s += (x[j]-Root[j]/m)**2
#     # print(s)
#     return s

# ydat = []

# for j in range(N):
#     ydat.append(y_(xdata[j]))
    
# ydata = np.array(ydat)
# ydata

def Data_seggregate(xdata, ydata, trainfrac):   # seggregates data into test and train
    N = len(ydata)          # Not useful for ALAMO as it doesn't do any validation etc.
    num_train = int(trainfrac * N)
    x_train = xdata[:num_train, :]
    x_val  = xdata[num_train:, :]
    y_train = ydata[:num_train]
    y_val  = ydata[num_train:]
    return x_train, y_train, x_val, y_val

class Minimizing():
    def __init__(self, xdata, ydata, Upper, Lower, trainfrac=0.8, powers = (1,2)):
        self.xdata = xdata
        self.ydata = ydata
        self.trainfrac = trainfrac
        self.Upper = Upper
        self.Lower = Lower
        self.powers = powers        # Default model to fit upon

    def alamo_fn(self):
        # x_train, y_train, x_val, y_val = Data_seggregate(self.xdata, self.ydata, self.trainfrac)
        x_train, y_train = self.xdata, self.ydata

        '''
        Used savetrace=True because in its absence, running camel6 for instance surprisingly 
        gave trace.txt not found error after ~900+ epochs
        '''
        # print('Training model with', len(y_train))
        
        # For the present version of alamo, we get errors if points are 1 dim and passed directly
        N, m = x_train.shape
        
        # So, if m = 1, we'll have to flatten the x_train to (N,)
        # if m > 1 x_train can be passed as a 2D array of (N, m)
        if m == 1: x_train = x_train.flatten()  

        model = alamopy.doalamo.alamo(xdata = x_train, zdata = y_train, monomialpower = (1,2,3), expfcns=1, savetrace = True, 
                                      xmin = tuple(self.Lower), xmax = tuple(self.Upper))  # , xval = x_val, zval = y_val
        return model

    def Model_min(self, model):
        self.Lower = np.reshape(self.Lower, (-1,1))
        self.Upper = np.reshape(self.Upper, (-1,1))
        bnds = np.hstack((self.Lower, self.Upper))
        # print('MODEL', model['model'])
        # print('xdata', self.xdata)
        # print('ydata', self.ydata)
        # print('up', self.Upper)
        # print('low', self.Lower)

        from scipy.optimize import minimize
        ind = np.argmin(self.ydata)
        x0 = self.xdata[ind]
        # print(bnds.shape)
        min_result = minimize(model['f(model)'], x0, method='L-BFGS-B', tol=1e-6, bounds = bnds) # L-BFGS-B
        
        return min_result   # use min_result.y, min_result.x etc....
#%%
# xmin = np.ones(m)*0.2
# xmax = np.ones(m)

# xmin = np.reshape(xmin, (-1,1))
# xmax = np.reshape(xmax, (-1,1))
# print(xmin.shape)
# bnds = np.hstack((xmin,xmax))
# print('bnds',bnds)

# model = Minimizing(xdata, ydata, xmax, xmin)
# print(model)
# res = model.alamo_fn()
# print(res['f(model)'])
# ans = model.Model_min(res)
# print(ans)


#%%
