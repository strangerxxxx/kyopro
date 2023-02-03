def angle(a, b, c=None):
    # ∠acbの角度
    import math
    if c is None:
        va, vb = a, b
    else:
        va = [x - y for x, y in zip(a, c)]
        vb = [x - y for x, y in zip(b, c)]
    d = sum(x ** 2 for x in va)
    d *= sum(x ** 2 for x in vb)
    d **= 0.5
    return math.acos(sum(x * y for x, y in zip(va, vb)) / d)


def is_shape_angle(a, b, c=None):
    # ∠acbが鋭角(=1)、直角(=0)、鈍角(=-1)
    def sign(x: int) -> int:
        return (x > 0) - (x < 0)
    if c is None:
        va, vb = a, b
    else:
        va = [x - y for x, y in zip(a, c)]
        vb = [x - y for x, y in zip(b, c)]
    return sign(sum(x * y for x, y in zip(va, vb)))
