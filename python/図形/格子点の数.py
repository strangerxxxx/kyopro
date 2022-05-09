def lattice_points_line(p, q):
    # 線分が通る格子点の数(両端を含む)
    import math
    return math.gcd(abs(p[0] - q[0]),
                    abs(p[1] - q[1])) + 1


def lattice_points_polygon(p):
    # 凸包が通る格子点の数(p[0]==p[-1]とする)
    import math
    res = 0
    for i in range(len(p) - 1):
        res += math.gcd(abs(p[i][0] - p[i + 1][0]),
                        abs(p[i][1] - p[i + 1][1]))
    return res
