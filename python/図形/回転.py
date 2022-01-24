def RotatePoint(coordinate, degree, radian: bool = False, center=(0, 0)):
    # 回転
    import math
    xi, yi = coordinate[0] - center[0], coordinate[1] - center[1]
    di = degree if radian else math.radians(degree)
    xj = xi * math.cos(di) - yi * math.sin(di)
    yj = xi * math.sin(di) + yi * math.cos(di)
    return xj + center[0], yj + center[1]


def RotateManhattan(x: int, y: int):
    return x - y, x + y
