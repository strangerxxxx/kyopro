def center_of_gravity(l):
    n = len(l)
    axis = len(l[0])
    res = [0] * axis
    for p in l:
        for i, q in enumerate(p):
            res[i] += q
    return [x / n for x in res]
