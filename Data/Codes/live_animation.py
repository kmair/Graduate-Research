#%%
import numpy as np
# xdata = np.arange(10)
# import sys

# val1 = sys.argv[1]      # Temp
# val2 = sys.argv[2]      # Temp
# # with open("temp.txt", "w") as opfile:
# #     for x in xdata:
# #         opfile.write("%s\n" %(x))

# import matplotlib.pyplot as plt
# import matplotlib.animation as animation

# fig = plt.figure()
# #creating a subplot 
# ax1 = fig.add_subplot(1,1,1)

# def fn_writer(n):
#     if n >0:
#         x = n
#         y = np.random.randn()
#         with open("temp.txt", "a") as write_file:
#             write_file.write("%s,%s\n" %(x,y))
#         return fn_writer(n-1)
#     else:
#         print('Else loop')
#         # print('Ran this file:')
#         # print('python live_animation.py ' + str(val1))
#         print(float(val1) + float(val2))
#         return

# def animate(i):
    
#     with open('temp.txt','r') as data:
#         xs = []
#         ys = []
#         for line in data:
#             # print(line)
#             x,y = line.split(',')
#             xs.append(float(x))
#             ys.append(float(y))

#             # print(ys, xs)
#     ax1.clear()
#     ax1.plot(xs, ys)

#     plt.xlabel('Date')
#     plt.ylabel('Price')
#     plt.title('Live graph with matplotlib')	
	
# fn_writer(10)
    
# ani = animation.FuncAnimation(fig, animate, interval=1000) 
# plt.show(block=False)
# plt.pause(1)
# plt.close(fig)

# ValueError: tnc: invalid return value from minimized function.
xdata = np.array([[-149.    ],
 [-112.5   ],
 [ -93.75  ],
 [-131.25  ],
 [-121.875 ],
 [ -84.375 ],
 [-103.125 ],
 [-140.625 ],
 [-135.9375],
 [ -98.4375]])

# ydata = np.array([306400569.7266908, 1.9424263952414376e+130, 1.9424263952414376e+130, 1.94242636729329e+130, 1.942426395239067e+130, 1.9424263952414376e+130, 1.9424263952414376e+130, 1.9420969028576306e+130, 1.9424233605939804e+130, 1.9424263952414376e+130]) / 1e+120
ydata = np.array([-150.,-112.5,-93.75,-131.25,-121.875,-84.375,-103.125,-140.625,-135.9375, -98.4375])
up = np.array([[150.]])
low = np.array([[-150.]])

import alamo_train

minimizer = alamo_train.Minimizing(xdata, ydata, up, low)
minimizing_model = minimizer.alamo_fn()

#%%
fn = minimizing_model['f(model)']
print(fn)
#%%
print(minimizing_model)
x_results = minimizer.Model_min(minimizing_model)
print('x_results', x_results)