def two_point_linear(p, q):
    # 点p,点qを通る直線y=ax+b
    if p[0] == q[0]:
        return 0, p[0]
    a = (q[1] - p[1]) / (q[0] - p[0])
    return a, p[1] - p[0] * a


def two_point_linear2(p, q):
    # 点p,点qを通る直線ax+by+c=0
    a = q[1] - p[1]
    b = p[0] - q[0]
    c = p[1] * q[0] - p[0] * q[1]
    import math
    import functools

    g = functools.reduce(math.gcd, (a, b, c))
    if a < 0 or (a == 0 and b < 0):
        g *= -1
    return a // g, b // g, c // g


def vertical_bisector(p, q):
    # 点p,点qの垂直二等分線ax+by+c=0
    a = (q[0] - p[0]) * 2
    b = (q[1] - p[1]) * 2
    c = p[0] ** 2 - q[0] ** 2 + p[1] ** 2 - q[1] ** 2
    import math
    import functools

    g = functools.reduce(math.gcd, (a, b, c))
    if a < 0 or (a == 0 and b < 0):
        g *= -1
    return a // g, b // g, c // g


def point_line_distance(p, q, a, b, c):
    # 点(p,q)と直線ax+by+c=0の距離
    return abs(a * p + q * b + c) / (a**2 + b**2) ** 0.5


def calc_cross_point(a, b, c, d):
    # 四角形の対角線の交点
    denom = (c[0] - a[0]) * (d[1] - b[1]) - (c[1] - a[1]) * (d[0] - b[0])

    # 直線が平行な場合
    if not denom:
        return None

    ac = (b[0] - a[0], b[1] - a[1])
    r = ((d[1] - b[1]) * ac[0] - (d[0] - b[0]) * ac[1]) / denom

    distance = ((c[0] - a[0]) * r, (c[1] - a[1]) * r)
    return a[0] + distance[0], a[1] + distance[1]
