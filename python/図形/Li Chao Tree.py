from bisect import bisect_left


class LiChaoTree:
    def __init__(self, xs, INF=10**20):
        self.INF = INF
        xs = sorted(set(xs))
        n = len(xs)
        self.size = 1 << (n - 1).bit_length()
        self.comp_xs = {x: i for i, x in enumerate(xs)}
        self.xs = xs + [self.INF] * (self.size - n)
        self.data = [None] * (self.size + self.size)

    def update(self, line, k, l, r):
        while True:
            if self.data[k] is None:
                self.data[k] = line
                return

            mid = (l + r) >> 1
            lx = self.xs[l]
            mx = self.xs[mid]
            rx = self.xs[r - 1]
            lu = self.f(line, lx) < self.f(self.data[k], lx)
            mu = self.f(line, mx) < self.f(self.data[k], mx)
            ru = self.f(line, rx) < self.f(self.data[k], rx)

            if lu and ru:
                self.data[k] = line
                return
            if not lu and not ru:
                return
            if mu:
                self.data[k], line = line, self.data[k]
            if lu != mu:
                r, k = mid, k << 1
            else:
                l, k = mid, k << 1 | 1

    def add_line(self, line):
        # 直線line(a,b):y=ax+bを追加
        self.update(line, 1, 0, self.size)

    def add_seg(self, line, l, r):
        # 線分line(a,b):y=ax+b、[l,r)を追加
        l = bisect_left(self.xs, l)
        r = bisect_left(self.xs, r)
        l0, r0 = l + self.size, r + self.size
        size = 1
        while l0 < r0:
            if l0 & 1:
                self.update(line, l0, l, l + size)
                l0 += 1
                l += size
            if r0 & 1:
                r0 -= 1
                r -= size
                self.update(line, r0, r, r + size)
            l0 >>= 1
            r0 >>= 1
            size <<= 1

    def f(self, line, x):
        a, b = line
        return a * x + b

    def get_min(self, x):
        k = self.comp_xs[x] + self.size
        res = self.INF
        while k > 0:
            if self.data[k] is not None:
                res = min(res, self.f(self.data[k], x))
            k >>= 1
        return res


class LiChaoTree_max:
    def __init__(self, xs, INF=10**20):
        self.INF = INF
        xs = sorted(list(set(xs)))
        n = len(xs)
        self.size = 1 << (n - 1).bit_length()
        self.comp_xs = {x: ind for ind, x in enumerate(xs)}
        self.xs = xs + [self.INF] * (self.size - n)
        self.data = [None] * (self.size + self.size)

    def update(self, line, k, l, r):
        while True:
            if self.data[k] is None:
                self.data[k] = line
                return

            mid = (l + r) >> 1
            lx = self.xs[l]
            mx = self.xs[mid]
            rx = self.xs[r - 1]
            lu = self.f(line, lx) > self.f(self.data[k], lx)
            mu = self.f(line, mx) > self.f(self.data[k], mx)
            ru = self.f(line, rx) > self.f(self.data[k], rx)

            if lu and ru:
                self.data[k] = line
                return
            if not lu and not ru:
                return
            if mu:
                self.data[k], line = line, self.data[k]
            if lu != mu:
                r, k = mid, k << 1
            else:
                l, k = mid, k << 1 | 1

    def add_line(self, line):
        # 直線line(a,b):y=ax+bを追加
        self.update(line, 1, 0, self.size)

    def add_seg(self, line, l, r):
        # 線分line(a,b):y=ax+b、[l,r)を追加
        l = bisect_left(self.xs, l)
        r = bisect_left(self.xs, r)
        l0, r0 = l + self.size, r + self.size
        size = 1
        while l0 < r0:
            if l0 & 1:
                self.update(line, l0, l, l + size)
                l0 += 1
                l += size
            if r0 & 1:
                r0 -= 1
                r -= size
                self.update(line, r0, r, r + size)
            l0 >>= 1
            r0 >>= 1
            size <<= 1

    def f(self, line, x):
        a, b = line
        return a * x + b

    def get_max(self, x):
        k = self.comp_xs[x] + self.size
        res = -self.INF
        while k > 0:
            if self.data[k] is not None:
                res = max(res, self.f(self.data[k], x))
            k >>= 1
        return res
