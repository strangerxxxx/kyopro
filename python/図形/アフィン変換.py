def main():
    def rotate_clockwise(p, mod=None):
        f = ((0, 1, 0), (-1, 0, 0), (0, 0, 1))
        return mat_mul(f, p, mod)

    def rotate_counterclockwise(p, mod=None):
        f = ((0, -1, 0), (1, 0, 0), (0, 0, 1))
        return mat_mul(f, p, mod)

    def rotate_180(p, mod=None):
        f = ((-1, 0, 0), (0, -1, 0), (0, 0, 1))
        return mat_mul(f, p, mod)

    def rotate(p, degree, mod=None):
        from math import sin, cos
        f = ((cos(degree), -sin(degree), 0),
             (sin(degree), cos(degree),  0),
             (0,           0,            1))
        return mat_mul(f, p, mod)

    def inversion_x(p, a, mod=None):
        # 直線x=aに対する反転
        f = ((-1, 0, 2 * a), (0, 1, 0), (0, 0, 1))
        return mat_mul(f, p, mod)

    def inversion_y(p, b, mod=None):
        # 直線y=bに対する反転
        f = ((1, 0, 0), (0, -1, 2 * b), (0, 0, 1))
        return mat_mul(f, p, mod)

    def translation(p, a, b, mod=None):
        # (a,b)だけ平行移動
        f = ((1, 0, 0), (0, 1, 0), (a, b, 1))
        return mat_mul(f, p, mod)

    def skew_x(p, degree, mod=None):
        # x軸を移動して平行四辺形化
        from math import tan
        f = ((1,           0, 0),
             (tan(degree), 1, 0),
             (0,           0, 1))
        return mat_mul(f, p, mod)

    def skew_y(p, degree, mod=None):
        # y軸を移動して平行四辺形化
        from math import tan
        f = ((1, tan(degree), 0),
             (0, 1,           0),
             (0, 0,           1))
        return mat_mul(f, p, mod)

    x, y = (1, 2)
    p = ((x,), (y,), (1,))
    ans = inversion_x(p, 3)


def mat_mul(a, b, mod=10 ** 9 + 7):
    c = [[0] * len(b[0]) for _ in range(len(a))]
    for i in range(len(a)):
        for j in range(len(b[0])):
            for k in range(len(b)):
                c[i][j] += a[i][k] * b[k][j]
            if not mod is None:
                c[i][j] %= mod
    return c
