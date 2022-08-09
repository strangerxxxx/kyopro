class LazySegmentTree_add:
    def __init__(self, init_val, function=min, element=float("inf"),
                 default: int = 0) -> None:
        if hasattr(init_val, "__iter__"):
            self.n = len(init_val)
            self.tree = [element] * self.n + list(init_val)
        else:
            self.n = init_val
            self.tree = [default] * 2 * self.n
        self.lazy = [self.default_lazy] * 2 * self.n
        self.element = element
        self.default_lazy = 0
        import operator
        self.mapping = operator.add
        self.tree_func = function
        self.lazy_func = operator.add
        for i in range(self.n - 1, 0, -1):
            self.tree[i] = self.tree_func(self.tree[i << 1],
                                          self.tree[i << 1 | 1])

    def _eval_at(self, i: int) -> int:
        if self.lazy[i] == self.default_lazy:
            return self.tree[i]
        return self.mapping(self.tree[i], self.lazy[i])

    def _propagate_at(self, i: int) -> None:
        if self.lazy[i] == self.default_lazy:
            return
        self.tree[i] = self._eval_at(i)
        if self.lazy[i] != self.default_lazy:
            self.lazy[i << 1] = self.lazy_func(self.lazy[i << 1], self.lazy[i])
            self.lazy[i << 1 | 1] = self.lazy_func(self.lazy[i << 1 | 1],
                                                   self.lazy[i])
            self.lazy[i] = self.default_lazy

    def _propagate_above(self, i: int):
        bitlen = i.bit_length() - 1
        for h in range(bitlen, 0, -1):
            self._propagate_at(i >> h)

    def _recalc_above(self, i: int):
        while i > 1:
            i >>= 1
            self.tree[i] = self.tree_func(self._eval_at(i << 1),
                                          self._eval_at(i << 1 | 1))

    def set_val(self, i: int, x: int):
        i += self.n
        self._propagate_above(i)
        self.tree[i] = x
        self.lazy[i] = self.default_lazy
        self._recalc_above(i)

    def operate_range(self, l: int, r: int, x: int) -> int:
        # assert 0 <= l < self.n
        # assert 0 < r <= self.n
        l += self.n
        r += self.n
        l0 = l // (l & -l)
        r0 = r // (r & -r)
        self._propagate_above(l0)
        self._propagate_above(r0)
        while l < r:
            if l & 1:
                if self.lazy[l] == self.default_lazy:
                    self.lazy[l] = x
                else:
                    self.lazy[l] = self.lazy_func(self.lazy[l], x)
                l += 1
            if r & 1:
                r -= 1
                if self.lazy[r] == self.default_lazy:
                    self.lazy[r] = x
                else:
                    self.lazy[r] = self.lazy_func(self.lazy[r], x)
            l >>= 1
            r >>= 1
        self._recalc_above(l0)
        self._recalc_above(r0)

    def query(self, l: int, r: int) -> int:
        # assert 0 <= l < self.n
        # assert 0 < r <= self.n
        l += self.n
        r += self.n
        self._propagate_above(l // (l & -l))
        self._propagate_above(r // (r & -r))
        res_l = res_r = self.element
        while l < r:
            if l & 1:
                res_l = self.tree_func(res_l, self._eval_at(l))
                l += 1
            if r & 1:
                r -= 1
                res_r = self.tree_func(self._eval_at(r), res_r)
            l >>= 1
            r >>= 1
        return self.tree_func(res_l, res_r)
