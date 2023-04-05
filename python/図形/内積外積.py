def inner_product(a, b):
    # 内積
    return sum(x * y for x, y in zip(a, b))


def cross_product(a, b):
    # 外積
    x = a[2] if len(a) == 3 else 0
    y = b[2] if len(b) == 3 else 0
    return a[1] * y - x * b[1], x * b[1] - a[0] * y, a[0] * b[1] - b[0] * a[1]


def line_side(p, a, b):
    # ベクトルABに対して点Pが直線上(=0)、左(=1),右(=-1)か判定
    x = (b[0] - a[0]) * (p[1] - a[1]) - (b[1] - a[1]) * (p[0] - a[0])
    return (x > 0) - (x < 0)
