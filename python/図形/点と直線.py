def two_point_linear(p, q):
    # 点p,点qを通る直線y=ax+b
    if p[1] == q[1]:
        return 0, p[1]
    a = (q[0] - p[0]) / (q[1] - p[1])
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
    return abs(a * p + q * b + c) / (a ** 2 + b ** 2) ** 0.5
