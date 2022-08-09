class BIT():
    def __init__(self, init_val):
        if hasattr(init_val, "__iter__"):
            self.n = len(init_val) + 1
            self.data = [0] * self.n
            self.build(init_val)
        else:
            self.n = init_val
            self.data = [0] * self.n

    def build(self, arr):
        # assert len(arr) <= n
        for i, a in enumerate(arr):
            self.data[i] = a
        for i in range(1, self.n + 1):
            if i + (i & -i) <= self.n:
                self.data[i + (i & -i) - 1] += self.data[i - 1]

    def add(self, i, x):
        # assert 0 <= i < self.n
        i += 1
        while i <= self.n:
            self.data[i - 1] += x
            i += i & -i

    def update(self, i, x):
        # assert 0 <= i < self.n
        self.add(i, x - self.get(i))

    def sum(self, r):
        # sum 0<=i<r
        # assert 0 <= r <= self.n
        s = 0
        while r:
            s += self.data[r - 1]
            r -= r & -r
        return s

    def query(self, l, r):
        # sum l<=i<r
        # assert 0 <= l <= r <= self.n
        return self.sum(r) - self.sum(l)

    def get(self, i):
        # assert 0 <= i < self.n
        return self.query(i, i + 1)


class BIT2:
    # Binary Index Tree (2-dimension)
    def __init__(self, h, w):
        self.h = h
        self.w = w
        self.data = [{} for _ in range(h + 1)]

    # O(logH*logW)
    def sum(self, i, j):
        s = 0
        data = self.data
        while i > 0:
            el = data[i]
            k = j
            while k > 0:
                s += el.get(k, 0)
                k -= k & -k
            i -= i & -i
        return s

    # O(logH*logW)
    def add(self, i, j, x):
        w = self.w
        h = self.h
        data = self.data
        while i <= h:
            el = data[i]
            k = j
            while k <= w:
                el[k] = el.get(k, 0) + x
                k += k & -k
            i += i & -i

    # [x0, x1) x [y0, y1)
    def query(self, x0, x1, y0, y1):
        return self.sum(x1, y1) - self.sum(x1, y0) - self.sum(x0, y1) + self.sum(x0, y0)
