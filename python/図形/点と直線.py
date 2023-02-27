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
    return abs(a * p + q * b + c) / (a**2 + b**2) ** 0.5


def calc_cross_point(pointA, pointB, pointC, pointD):
    # 四角形の対角線の交点
    denominator = (pointC[0] - pointA[0]) * (pointD[1] - pointB[1]) - (
        pointC[1] - pointA[1]
    ) * (pointD[0] - pointB[0])

    # 直線が平行な場合
    if not denominator:
        return None

    vectorAC = ((pointB[0] - pointA[0]), (pointB[1] - pointA[1]))
    r = (
        (pointD[1] - pointB[1]) * vectorAC[0] - (pointD[0] - pointB[0]) * vectorAC[1]
    ) / denominator

    distance = ((pointC[0] - pointA[0]) * r, (pointC[1] - pointA[1]) * r)
    return pointA[0] + distance[0], pointA[1] + distance[1]
