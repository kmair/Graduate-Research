var_detail = {}

with open('index selection.txt') as data:
    lines = data.readlines()
    for line in lines:
        n, numVars, numPts = line.split(' - ')
        n = int(n)
        numPts = numPts.rstrip()
        numPts = int(numPts)
        # print(v)
        Vars = numVars.split(',')
        # print(Vars)
        # print(type(Vars))
        # var_detail = {var: n for var in Vars}
        for var in Vars:
            if var in var_detail.keys():
                # print(var)
                lst = var_detail.values()
                print(lst)
                print(type(lst) )
                lst.append((n, numPts))
            else:
                var_detail[var] = [(n, numPts)]


print(var_detail)