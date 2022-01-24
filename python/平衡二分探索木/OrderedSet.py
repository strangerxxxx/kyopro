class BIT():
    def __init__(self, n):
        self.n = n
        self.data = [0] * n

    def build(self, arr):
        for i, a in enumerate(arr):
            self.data[i] = a
        for i in range(1, self.n + 1):
            if i + (i & -i) <= self.n:
                self.data[i + (i & -i) - 1] += self.data[i - 1]

    def add(self, p, x):
        p += 1
        while p <= self.n:
            self.data[p - 1] += x
            p += p & -p

    def sum(self, r):
        s = 0
        while r:
            s += self.data[r - 1]
            r -= r & -r
        return s

    def get(self, p):
        return self.range_sum(p, p + 1)

    def range_sum(self, l, r):
        return self.sum(r) - self.sum(l)

    def bisect_left(self, x):
        if x <= 0:
            return 0
        res = 0
        k = 1 << self.n.bit_length()
        while k:
            if res + k <= self.n and self.data[res + k - 1] < x:
                x -= self.data[res + k - 1]
                res += k
            k >>= 1
        return res + 1


class OrderedSet():
    def __init__(self, n):
        self.n = n
        self.bit = BIT(n)

    def build(self, arr):
        self.bit.build(arr)

    def is_exist(self, k):
        return self.bit.get(k)

    def size(self):
        return self.bit.sum(self.n)

    def insert(self, k):
        if self.is_exist(k):
            return 0
        self.bit.add(k, 1)
        return 1

    def delete(self, k):
        if not self.is_exist(k):
            return 0
        self.bit.add(k, -1)
        return 1

    def get(self, i):
        sz = self.size()
        if sz <= i or i + sz < 0:
            return -1
        if i < 0:
            return self.get(sz + i)
        ret = self.bit.bisect_left(i + 1) - 1
        return ret

    def max(self):
        return self.get(-1)

    def min(self):
        return self.get(0)

    def index(self, k):
        if not self.is_exist(k):
            return -1
        ret = self.bit.sum(k)
        return ret

    def median(self, ceil=False):
        sz = self.size()
        if sz == 0:
            return -1
        if sz % 2 == 1 or ceil:
            return self.get(sz // 2)
        else:
            return self.get((sz - 1) // 2)

    def lower_bound(self, k):
        ret = self.bit.bisect_left(self.bit.sum(k) + 1) - 1
        return ret if ret != self.n else -1

    def upper_bound(self, k):
        ret = self.bit.bisect_left(self.bit.sum(k)) - 1
        return ret

    def __contains__(self, key):
        return self.is_exist(key)

    def __delitem__(self, key):
        return self.delete(key)
