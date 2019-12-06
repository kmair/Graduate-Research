import sys
import os
from latin_rand import add

def data_points_gen(I, M, method):
    # print(method)
    if method == 'SOBOL':
        import sobol_call
        R = sobol_call.main(I, M)

    elif method == 'HAMMERSLEY':
        import hammersley
        R = hammersley.main(I, M)
        print(R)

    elif method == 'RANDOM':
        import randlc
        R = randlc.main(I, M)

    elif method == 'LATIN':
        import latin_hypercube
        R = latin_hypercube.main(I, M)

    else:
        print("This sequencing method doesn't exist")
  
    return R

# To plot the points by different methods - for 2 dim only

# import matplotlib.pyplot as plt
# I = int(sys.argv[1])
# M = int(sys.argv[2])
# method = sys.argv[3]

# pts = data_points_gen(I, M, method)


# fig = plt.figure()
# ax = fig.add_subplot(111)

# ax.plot(pts[:,0], pts[:,1], 'ok')
# # ax.spines['bottom'].set_visible(False)
# # plt.xaxis.set_ticks([])
# ax.set_xticklabels([])
# ax.set_yticklabels([])
# ax.set_xticks([])
# ax.set_yticks([])
# plt.title(method)
# fig.savefig(method + '.png')
# plt.show()
