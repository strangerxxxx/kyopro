def convex_hull(ps: list, psIsSorted=False):
    # 凸包、反時計回り
    def cross3(a, b, c):
        return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])

    if not psIsSorted:
        ps.sort()
    res = []
    for p in ps:
        # while len(res) > 1 and cross3(res[-1], res[-2], p) >= 0:  # 一直線上で高々2点にする場合は ">=" にする
        while len(res) > 1 and cross3(res[-1], res[-2], p) > 0:
            res.pop()
        res.append(p)
    t = len(res)
    for i in range(len(ps) - 2, -1, -1):
        p = ps[i]
        while len(res) > t and cross3(res[-1], res[-2], p) > 0:
            res.pop()
        res.append(p)
    return res


def polygon_disconpose(ps):
    """凸包(反時計回り)の分解
    左辺、右辺、下側、上側を返す"""

    def _slice(l, r):
        if l % n < r % n:
            return ps[l:r]
        return ps[l:] + ps[:r]

    n = len(ps)
    l = min(x[0] for x in ps)
    r = max(x[0] for x in ps)
    index = -n
    if ps[0][0] == l:
        llindex = 0
        while ps[llindex - 1][0] == l:
            llindex -= 1
    else:
        while ps[index][0] > l:
            index += 1
        llindex = index
    while ps[index + 1][0] == l:
        index += 1
    lrindex = index
    while ps[index][0] < r:
        index += 1
    rlindex = index
    while ps[index + 1][0] == r:
        index += 1
    rrindex = index
    return (
        _slice(llindex, lrindex + 1),
        _slice(rlindex, rrindex + 1),
        _slice(lrindex, rlindex + 1),
        _slice(rrindex, llindex + 1),
    )


def rotating_calipers(ps, psIsSorted=False):
    # 凸多角形の直径
    def distance(a: int, b: int) -> float:
        # 2点間のユークリッド距離(普通の距離)
        d = 0
        for i, j in zip(a, b):
            d += (i - j) ** 2
        return d**0.5

    def cross(a, b, c, d):
        return (b[0] - a[0]) * (d[1] - c[1]) - (b[1] - a[1]) * (d[0] - c[0])

    qs = convex_hull(ps, psIsSorted)
    n = len(qs)
    if n == 2:
        return distance(qs[0], qs[1])
    i = j = 0
    for k in range(n):
        if qs[k] < qs[i]:
            i = k
        if qs[j] < qs[k]:
            j = k
    res = 0
    si = i
    sj = j
    while i != sj or j != si:
        res = max(res, distance(qs[i], qs[j]))
        if cross(qs[i], qs[i - n + 1], qs[j], qs[j - n + 1]) < 0:
            i = (i + 1) % n
        else:
            j = (j + 1) % n
    return res
