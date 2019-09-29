import numpy as np
import os
import sys
import scipy.optimize
import scipy.interpolate

def fn(X, name):
    if name == 'Rosen':
        return  scipy.optimize.rosen(X)

    elif  name == 'beale':
        x1, x2 = X    # Because first dim in N
        f = (1.5-x1+x1*x2)**2 + (2.25-x1+x1*x2**2)**2 + \
            (2.625-x1+x1*x2**3)**2
        return f

X = np.random.rand(2,100) * 5 - 2       # m, N

Z = fn(X, 'beale')      # takes X = m, N; N: no. of samples

rbf = scipy.interpolate.Rbf(*X, Z)

print(fn([1,1], 'beale') - rbf(1,1))

import Sampling
compileFile = "box3"
N = 2       # No. of var
start = np.array([0.5,0.2])
upper, lower = np.ones(2), np.zeros(2)

#% Group 1
delta = 0.01            # 0.1: Large, 0.01: Medium, 0.001: Smalls
diff = upper-lower

h1 = delta * min(diff)

mu, sigma = 0, 1 # mean and standard deviation
s = np.random.normal(mu, sigma, 500 * N)

perturbations = h1 * s.reshape(-1, N)
local_pts = perturbations + start

#% Group 4
data = Sampling.Trust_Region_method(compileFile, N, upper, lower, n_points = 500, method = 'SOBOL', StartPt = start) # Scaling_factor = 1,
global_pts = data.Sampling_data()

######################
# Scoring

# Criteria 1
def VR(pts):

    s = rbf(*pts.T)

    min_s, max_s = min(s) , max(s) 

    return (s - min_s) / (max_s - min_s)

VR_l = VR(local_pts)
VR_g = VR(global_pts)
print('VR_l', VR_l)
# print(VR_g)
def VD(pts, x):
    '''
    
    Parameters
    -------------
    pts: (N, m) shaped
    x:   (m ,k) shaped and is hence transposed
    '''
    
    delta = [min( np.sum( (pt - x.T)**2, axis= 1) ) for pt in pts]
    del_max, del_min = max(delta), min(delta)
    
    return (del_max - delta) / (del_max - del_min)

vd_l = VD(local_pts, X)
# print(vd_l)

# Criteria 2