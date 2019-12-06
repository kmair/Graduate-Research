# Temporary file to make file names

# l = []
# Collecting all the filenames
# with open('File_names.txt') as f:
#     lines = f.readlines()
#     for line in lines:
#         l.append(line[:-5])

# print(l)
# with open('File_names_out.txt', 'w+') as writer:

#     for line in l:
#         writer.write(line)
#         writer.write('\n')

# Sort in ascending order

num_var = dict()

file = '3pk'
def numOfVar(file):
    with open(file + '.problem.data') as f:
        lines = f.readlines()
        var = lines[0]


    return var

def file_iterate(filename):
    with open(filename) as filenames:
        lines = filenames.readlines()
        # print(lines)
        for file in lines:
            var = numOfVar(file.rstrip())
            var = var.rstrip()
            num_var[file.rstrip()] = int(var)

file_iterate('File_names_out.txt')

# Sorting var in ascending order
import operator

sorted_x = sorted(num_var.items(), key= lambda x: x[1])  #operator.itemgetter(1))

import collections
sorted_dict = collections.OrderedDict(sorted_x)

# print(sorted_dict)
with open('Data_sorted.txt', 'w') as writer:

    for tup in sorted_dict:
    # print(tup, num_var[tup])
        writer.write(tup + ' ' + str(num_var[tup]) + '\n')  #  