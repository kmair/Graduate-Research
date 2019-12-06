import pandas as pd 
import sys
import os
import numpy as np

filename= sys.argv[1]
n_pts = sys.argv[2]
n_attr =  sys.argv[3]

HOME_PATH = os.getcwd()
# LIBRARY = 'source_princetonlibgloballib'
LIBRARY = 'source_convexmodels'
# LIBRARY = 'source_nonsmooth'

datapath = os.path.join(HOME_PATH, 'all_files')   # Codes/all_files
with open(os.path.join(datapath, filename + '.problem.data'), 'r') as file:
    lines = file.readlines()
    N = int(lines[0])
    # up = np.zeros(N)
    lower = lines[1].split()
    upper = lines[2].split()
    up = [float(val) for val in upper]
    low = [float(val) for val in lower]

range_diff = np.array(up) - np.array(low)

with open(os.path.join(datapath, filename + '.sol'), 'r') as file:
    line = file.read()
    solution = np.array([float(x) for x in line.split()])
# print(solution)

opt_df = pd.read_csv(os.path.join(datapath, 'optimal_results.csv'))

# Getting the reported optimal solution for given filename
opt_Z = opt_df[opt_df.problem == filename].solution
prob_smoothness = opt_df[opt_df.problem == filename].smoothness
prob_cvxity = opt_df[opt_df.problem == filename].convexity
prob_smoothness = np.array(prob_smoothness)[0]
prob_cvxity = np.array(prob_cvxity)[0]

opt_Z = np.array(opt_Z)[0]

folder = os.path.join(os.path.dirname(os.getcwd()), LIBRARY)

'''The xl_folder contains all the files'''
xl_folder = os.path.join(folder, 'Excel files')

os.chdir(xl_folder)

df = pd.read_excel(filename + '.xls', index_col= None, sheet_name='attr'+n_attr+'_pts'+n_pts+'_beta0.9')  # doctest: +SKIP

# A fn to write the best output based on Z value

# def pred_vs_optimal(opt_Z, pred_Z):

#     # Absolute error
#     abs_err = pred_Z - opt_Z 

#     # Relative error
#     # calculate Relative error iff |opt_Z| > 1e-3
#     if np.abs(opt_Z) > 1e-3:
#         if opt_Z > 0:
#             '''opt_Z = 5 & pred_Z = 10
#                then rel_err = (5 - 10) / 5 * -1 = 1'''
#             rel_err = - (opt_Z - pred_Z) / opt_Z
        
#         if opt_Z < 0:
#             '''opt_Z = -5 & pred_Z = 5
#                then rel_err = (-5 - (5)) / -5 = 2'''
#             rel_err = (opt_Z - pred_Z) / opt_Z

#     else:
#         rel_err = '-'

#     return (abs_err, rel_err)

'''Finding best solution for just the last solution at each new starting point'''
# try:
#     indices = df[df.Epochs == 'Epochs'].index
#     indices -= 1
#     dist = list()
#     Z_out = list()

#     for index in indices:
#         xs = np.array([ df.iloc[index]['X'+str(i+1)] for i in range(N) ])
#         manhat_dist = np.abs(xs-solution) / range_diff
#         avg_manhat_dist = sum(manhat_dist/N)
#         dist.append(avg_manhat_dist)
#         # print('manhat_dist',manhat_dist)

#         Z = df.iloc[index]['Z']
#         Z_out.append(Z)

#     closest = min(dist)
#     pred_Z = min(Z_out)

# # Below condition happens if the iteration only happens on the 1st starting point
# except:
#     x = np.array([ df.iloc[-1]['X'+str(i+1)] for i in range(N) ])
#     manhat_dist = np.abs(x-solution) / range_diff
#     avg_manhat_dist = sum(manhat_dist/N)
#     closest = avg_manhat_dist
#     pred_Z = df.iloc[-1]['Z']

# '''Call the pred_vs_optimal fn here'''
# abs_err, rel_err = pred_vs_optimal(opt_Z, pred_Z)


'''
Finding the convergence for least number of iterations. Explanation of convergence criteria.
If either Manhat dist < 0.1% or 
If relative err exists then when rel error < 0.5% and if this doesn't exist (opt_z -> 0), then when absolute error <= 1 
'''

def pred_vs_optimal2(X, Z, MD = 0.0001, RE = 0.005, AE = 1):
    # 1st check Distance criteria
    manhat_dist = np.abs(X-solution) / range_diff
    avg_manhat_dist = sum(manhat_dist)/N

    # Absolute error
    abs_err = Z - opt_Z 

    # Relative error
    # calculate Relative error iff |opt_Z| > 1e-3
    if np.abs(opt_Z) > 1e-3:
        '''opt_Z = 5 & Z = 10
            then rel_err = (5 - 10) / 5 * -1 = 1, but if
           opt_Z = -5 & Z = 5
            then rel_err = (-5 - (5)) / -5 = 2'''
        rel_err = np.abs( (opt_Z - Z) / opt_Z )

        if rel_err <= 0.005 or avg_manhat_dist <= MD:

            return ['YES', avg_manhat_dist, rel_err, abs_err]

    # Otherwise just check for abs error criteria

    else:
        rel_err = '-'
        if abs_err <= AE or avg_manhat_dist <= MD:
            return ['YES', avg_manhat_dist, rel_err, abs_err]
        
    return ['NO', avg_manhat_dist, rel_err, abs_err]

req_df = df[df.Epochs != 'Epochs']
indices = req_df.index
# print([df.iloc[indice].Z for indice in indices])

def check_convergence():
    min_avg_manhat_dist = np.inf
    min_abs_err = np.inf
    min_Z = np.inf

    for index in indices:
        X = np.array([ df.iloc[index]['X'+str(i+1)] for i in range(N) ])
        Z = np.array([ df.iloc[index]['Z'] ])[0]
        output = pred_vs_optimal2(X, Z)

        # Convergence within the iterations
        if output[0] == 'NO':
            if output[1] < min_avg_manhat_dist:
                min_avg_manhat_dist = output[1]

            if output[3] < min_abs_err:
                min_rel_err = output[2]
                min_abs_err = output[3]
                min_Z = Z

        else:
            return [Z] + output[1:] + [index]

    # If solution never converged then comes here & returns best values in 2500 iterations
    return [min_Z, min_avg_manhat_dist, min_rel_err, min_abs_err, 2500]

output = check_convergence()
   
min_Z, min_avg_manhat_dist, min_rel_err, min_abs_err, n_eval = output

# Write to file!
res_path = os.path.join(HOME_PATH, 'Results')
os.chdir(res_path)

fname = 'results' + prob_cvxity + prob_smoothness + '.csv'

with open(fname, 'a') as writer:
    writer.write(filename + ',' + str(N) + ',' + str(min_avg_manhat_dist) + ',' + str(opt_Z) + ',' + str(min_Z) + 
                 ',' + str(min_abs_err) + ',' + str(min_rel_err) + ',' + str(n_eval) + '\n')


