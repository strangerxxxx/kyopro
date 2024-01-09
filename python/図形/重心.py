def center_of_gravity(l):
    n = len(l)
    axis = len(l[0])
    res = [0] * axis
    for p in l:
        for i, q in enumerate(p):
            res[i] += q
    return [x / n for x in res]


def move_gravity(l):
    g = center_of_gravity(l)
    for i in enumerate(g):
        for j, k in enumerate(i):
            l[j] -= k
