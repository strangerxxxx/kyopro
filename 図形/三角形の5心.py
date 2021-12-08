
def distance(pa, pb):
    return sum([(i - j) ** 2 for i, j in zip(pa, pb)]) ** 0.5


def midpoint(pa, pb):
    return [(x + y) / 2 for x, y in zip(pa, pb)]


def side_length(pa, pb, pc):
    # 3辺の長さ
    return distance(pc, pb), distance(pa, pc), distance(pa, pb)


def area(a, b, c):
    # 面積
    s = (a + b + c) / 2
    return (s * (s - a) * (s - b) * (s - c)) ** 0.5


def sin_a(a, b, c):
    # sin∠BAC
    s = area(a, b, c)
    return max(min(s * 2 / b / c, 1), -1)


def cos_a(a, b, c):
    # cos∠BAC
    return max(min((b ** 2 + c ** 2 - a ** 2) / (2 * b * c), 1), -1)


def sin_o(pa, pb):
    # sin∠AOB
    a, b, c = side_length([0] * len(pa), pa, pb)
    return sin_a(a, b, c)


def cos_o(pa, pb):
    # cos∠AOB
    a, b, c = side_length([0] * len(pa), pa, pb)
    return cos_a(a, b, c)


def sin_t(pa, pb, pc):
    # 3つの角のsin
    a, b, c = side_length(pa, pb, pc)
    s = area(a, b, c)
    return s * 2 / b / c, s * 2 / c / a, s * 2 / a / b


def cos_t(pa, pb, pc):
    # 3つの角のcos
    a, b, c = side_length(pa, pb, pc)
    return cos_a(a, b, c), cos_a(b, c, a), cos_a(c, a, b)


def barycentric_coordinate(pa, pb, pc, ga, gb, gc, ignore=False):
    # △BCP:△CAP:△ABP=ga:gb:gcであるときの点Pの座標
    try:
        g = ga + gb + gc
        return [(a * ga + b * gb + c * gc) / g for a, b, c in zip(pa, pb, pc)]
    except:
        if ignore:
            return []
        else:
            raise ValueError('points on a straight line')


def incenter(pa, pb, pc, ignore=False):
    # 内心
    a, b, c = side_length(pa, pb, pc)
    return barycentric_coordinate(pa, pb, pc, a, b, c, ignore)


def circumcenter(pa, pb, pc, ignore=False):
    # 外心
    a, b, c = side_length(pa, pb, pc)
    try:
        return barycentric_coordinate(pa, pb, pc,
                                      sin_a(a, b, c) * cos_a(a, b, c),
                                      sin_a(b, c, a) * cos_a(b, c, a),
                                      sin_a(c, a, b) * cos_a(c, a, b), ignore)

    except:
        if ignore:
            return []
        else:
            raise ValueError('points on a straight line')


def centergravity(pa, pb, pc, ignore=False):
    # 重心
    return barycentric_coordinate(pa, pb, pc, 1, 1, 1, ignore)


def orthocenter(pa, pb, pc, ignore=False):
    # 垂心
    a, b, c = side_length(pa, pb, pc)
    try:
        return barycentric_coordinate(pa, pb, pc,
                                      sin_a(a, b, c) / cos_a(a, b, c),
                                      sin_a(b, c, a) / cos_a(b, c, a),
                                      sin_a(c, a, b) / cos_a(c, a, b), ignore)
    except:
        if ignore:
            return []
        else:
            raise ValueError('points on a straight line')


def excenter(pa, pb, pc, ignore=False):
    # 傍心
    a, b, c = side_length(pa, pb, pc)
    return (barycentric_coordinate(pa, pb, pc, -a, b, c, ignore),
            barycentric_coordinate(pa, pb, pc, a, -b, c, ignore),
            barycentric_coordinate(pa, pb, pc, a, b, -c, ignore))


# x = (12, 18), (390, 18), (102, 138)
x = (154, 26), (158, 72), (168, 187)
print('内心', incenter(*x, ignore=True))
print('外心', circumcenter(*x, ignore=True))
print('重心', centergravity(*x, ignore=True))
print('垂心', orthocenter(*x, ignore=True))
print('傍心', excenter(*x, ignore=True))
pa, pb, pc = x
a, b, c = side_length(pa, pb, pc)
print(cos_a(a, b, c))
print(cos_a(b, c, a))
print(cos_a(c, a, b))
