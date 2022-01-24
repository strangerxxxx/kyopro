def iteraccumulate(l):
    from itertools import accumulate
    print(list(accumulate(l)))
    print(list(accumulate(l, initial=0)))  # PyPy3(7.3.0)では使えない


def accumulate(l, initial=None):
    n = len(l)
    if initial is None:
        a = [None] * n
        a[0] = l[0]
        for i in range(1, n):
            a[i] = a[i - 1] + l[i]
    else:
        a = [None] * (n + 1)
        a[0] = initial
        for i in range(n):
            a[i + 1] = a[i] + l[i]
    return a


def accumulate_2d(l, initial=None):
    h = len(l)
    w = len(l[0])
    if initial is None:
        a = [[None] * w for _ in range(h)]
        for i in range(h):
            a[i][0] = l[i][0]
        for i in range(h):
            for j in range(1, w):
                a[i][j] = a[i][j - 1] + l[i][j]
        for i in range(1, h):
            for j in range(w):
                a[i][j] += a[i - 1][j]
    else:
        a = [[None] * (w + 1) for _ in range(h + 1)]
        for i in range(h + 1):
            a[i][0] = initial
        for j in range(1, w + 1):
            a[0][j] = initial
        for i in range(h):
            for j in range(w):
                a[i + 1][j + 1] = a[i + 1][j] + l[i][j]
        for j in range(w):
            for i in range(h):
                a[i + 1][j + 1] += a[i][j + 1]
    return a


class static_range_sum():
    def __init__(self, l) -> None:
        n = len(l)
        self.a = [None] * (n + 1)
        self.a[0] = 0
        for i in range(n):
            self.a[i + 1] = self.a[i] + l[i]

    def sum(self, l, r):
        return self.a[r] - self.a[l]


l = [1, 8, 7, 3, 2]
print(accumulate(l))
print(accumulate(l, 0))
