class RMQ:
    def __init__(
        self,
        init_val,
        function=min,
        element=float("inf"),
        default: int = 0,
        ope=min,
        default_lazy=float("inf"),
    ) -> None:
        if hasattr(init_val, "__iter__"):
            self.n = len(init_val)
        else:
            self.n = init_val
        self.LV = (self.n - 1).bit_length()
        self.N0 = 2**self.LV
        self.data = [element] * 2 * self.N0
        if hasattr(init_val, "__iter__"):
            for i, j in enumerate(init_val):
                self.data[self.N0 + i] = j
        else:
            for i in range(self.n):
                self.data[self.N0 + i] = default

        self.default_lazy = default_lazy
        self.lazy = [self.default_lazy] * (2 * self.N0)
        self.ope = ope
        self.element = element
        self.function = function

    def gindex(self, l, r):
        L = (l + self.N0) >> 1
        R = (r + self.N0) >> 1
        lc = 0 if l & 1 else (L & -L).bit_length()
        rc = 0 if r & 1 else (R & -R).bit_length()
        for i in range(self.LV):
            if rc <= i:
                yield R
            if L < R and lc <= i:
                yield L
            L >>= 1
            R >>= 1

    def propagates(self, *ids):
        for i in reversed(ids):
            v = self.lazy[i - 1]
            if v is None:
                continue
            self.lazy[2 * i - 1] = self.ope(self.lazy[2 * i - 1], v)
            self.lazy[2 * i] = self.ope(self.lazy[2 * i], v)
            self.data[2 * i - 1] = self.ope(self.data[2 * i - 1], v)
            self.data[2 * i] = self.ope(self.data[2 * i], v)
            self.lazy[i - 1] = self.default_lazy

    def update(self, l, r, x):
        (*ids,) = self.gindex(l, r)
        self.propagates(*ids)

        L = self.N0 + l
        R = self.N0 + r
        while L < R:
            if R & 1:
                R -= 1
                self.lazy[R - 1] = self.ope(x, self.lazy[R - 1])
                self.data[R - 1] = self.ope(x, self.data[R - 1])
            if L & 1:
                self.lazy[L - 1] = self.ope(x, self.lazy[L - 1])
                self.data[L - 1] = self.ope(x, self.data[L - 1])
                L += 1
            L >>= 1
            R >>= 1
        for i in ids:
            self.data[i - 1] = self.function(self.data[2 * i - 1], self.data[2 * i])

    def query(self, l, r):
        # 区間[l, r)内の最小値を求める
        self.propagates(*self.gindex(l, r))
        L = self.N0 + l
        R = self.N0 + r

        s = self.element
        while L < R:
            if R & 1:
                R -= 1
                s = self.function(s, self.data[R - 1])
            if L & 1:
                s = self.function(s, self.data[L - 1])
                L += 1
            L >>= 1
            R >>= 1
        return s

    def get(self, l=0, r=None) -> list:
        """
        [l, r)の配列を返す
        """
        if r is None:
            r = self.n
        return [self.query(x, x + 1) for x in range(l, r)]
