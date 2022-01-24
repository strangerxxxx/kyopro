class RSQandRUQ:
    def __init__(self, n):
        self.n = n
        self.data = [0] * n * 2
        self.lazy = [None] * n * 2

    def propagate(self, i):
        if i > 1:
            self.propagate(i >> 1)
        v = self.lazy[i]
        if v is not None:
            v >>= 1
            self.lazy[i + i] = v
            self.data[i + i] = v
            self.lazy[i - ~i] = v
            self.data[i - ~i] = v
            self.lazy[i] = None

    def update(self, l, r, x):
        l += self.n
        r += self.n
        L, R = l // (l & -l) >> 1, r // (r & -r) >> 1
        self.propagate(L)
        self.propagate(R)
        while l < r:
            if l & 1:
                self.lazy[l] = x
                self.data[l] = x
                l += 1
            if r & 1:
                r -= 1
                self.lazy[r] = x
                self.data[r] = x
            l >>= 1
            r >>= 1
            x <<= 1
        while L:
            self.data[L] = self.data[L + L] + self.data[L - ~L]
            L >>= 1
        while R:
            self.data[R] = self.data[R + R] + self.data[R - ~R]
            R >>= 1

    def query(self, l, r):
        l += self.n
        r += self.n
        self.propagate(l // (l & -l) >> 1)
        self.propagate(r // (r & -r) >> 1)
        s = 0
        while l < r:
            if l & 1:
                s += self.data[l]
                l += 1
            if r & 1:
                r -= 1
                s += self.data[r]
            l >>= 1
            r >>= 1
        return s
