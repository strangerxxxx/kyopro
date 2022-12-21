def inner_product(a, b):
    # 内積
    return sum(x * y for x, y in zip(a, b))


def cross_product(a, b):
    # 外積
    x = a[2] if len(a) == 3 else 0
    y = b[2] if len(b) == 3 else 0
    return a[1] * y - x * b[1], x * b[1] - a[0] * y, a[0] * b[1] - b[0] * a[1]
