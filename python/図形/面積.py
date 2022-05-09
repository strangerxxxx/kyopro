def triangle_area(a, b, c=(0, 0)):
    return abs(((a[0] - c[0]) * (b[1] - c[1])
                - (b[0] - c[0]) * (a[1] - c[1]))) / 2


def polygon_area(p):
    return abs(sum(p[i][0] * p[i - 1][1]
                   - p[i][1] * p[i - 1][0] for i in range(len(p)))) / 2
