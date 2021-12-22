def floor_log(n: int, a: int = 2) -> int:
    i = 1
    res = 0
    while i < n:
        i *= a
        res += 1
    return res


def int_log2(n: int) -> int:
    if n <= 0:
        raise ValueError
    return n.bit_length() - 1


def float_log2(n: float) -> int:
    import math
    return math.frexp(n)[1] - 1


def log2(n: float) -> float:
    import math
    return math.log2(n)
