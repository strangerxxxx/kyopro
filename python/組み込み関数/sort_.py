def bucket_sort(a, i_min: int = None, i_max: int = None):
    if i_min is None:
        i_min = min(a)
    if i_max is None:
        i_max = max(a)
    l = [0] * (i_max - i_min + 1)
    for i in a:
        l[i - i_min] += 1
    res = [None] * len(a)
    i = 0
    for j, k in enumerate(l):
        for _ in range(k):
            res[i] = j + i_min
            i += 1
    return res
