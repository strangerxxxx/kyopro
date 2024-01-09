def inversion_num(l, maxl=None):
    # assert all(x >= 0 for x in l)
    bit = BIT(max(l) + 1) if maxl is None else BIT(maxl)
    res = 0
    for i, p in enumerate(l):
        bit.add(p, 1)
        res += i + 1 - bit.sum(p)
    return res


def inversion_num_compress(l):
    s = set(l)
    d = {x: i for i, x in enumerate(sorted(s))}
    r = [d[x] + 1 for x in l]
    bit = BIT(len(s) + 1)
    res = 0
    for i, p in enumerate(r):
        bit.add(p, 1)
        res += i + 1 - bit.sum(p)
    return res


class BIT:
    # Binary Indexed Tree (Fenwick Tree)
    def __init__(self, n):
        self.n = n
        self.data = [0] * (n + 1)
        self.el = [0] * (n + 1)

    def sum(self, i):
        s = 0
        while i > 0:
            s += self.data[i]
            i -= i & -i
        return s

    def add(self, i, x):
        # assert i > 0
        self.el[i] += x
        while i <= self.n:
            self.data[i] += x
            i += i & -i

    def get(self, i, j=None):
        if j is None:
            return self.el[i]
        return self.sum(j) - self.sum(i)


print(inversion_num([16, 6, 15, 10, 18, 13, 17, 11]))
