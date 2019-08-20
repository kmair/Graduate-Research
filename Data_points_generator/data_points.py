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
