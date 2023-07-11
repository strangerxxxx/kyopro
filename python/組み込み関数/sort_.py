def bucket_sort(a, i_min: int = None, i_max: int = None):
    if i_min is None:
        i_min = min(a)
    if i_max is None:
        i_max = max(a)
    l = [0] * (i_max - i_min + 1)
    for i in a:
        l[i - i_min] += 1
    res = [None] * len(a)
    i = 0
    for j, k in enumerate(l):
        for _ in range(k):
            res[i] = j + i_min
            i += 1
    return res


class HilbertOrder:
    def __init__(self, n: int) -> None:
        # n : x, yの最大値
        self.maxn = 1 << (n - 1).bit_length()

    def __call__(self, x: int, y: int):
        d = 0
        s = self.maxn >> 1
        while s:
            rx = (x & s) > 0
            ry = (y & s) > 0
            d += s**2 * ((rx * 3) ^ ry)
            if ry:
                s >>= 1
                continue
            if rx:
                x = self.maxn - 1 - x
                y = self.maxn - 1 - y
            x, y = y, x
            s >>= 1
        return d
