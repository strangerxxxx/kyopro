class SegmentTree:
    def __init__(self, init_val, function=min, element=float("inf"),
                 default: int = 0, enable_cutoff=True) -> None:
        """
        init_val: 配列の初期値
        function: 区間にしたい操作
        element: 単位元
        n: 要素数
        tree: セグメント木(1-index)
        """
        if hasattr(init_val, "__iter__"):
            self.n = len(init_val)
            self.tree = [element] * self.n + list(init_val)
        else:
            self.n = init_val
            self.tree = [default] * 2 * self.n
        self.function = function
        self.element = element
        self.enable_cutoff = enable_cutoff
        for i in range(self.n - 1, 0, -1):
            self.tree[i] = self.function(self.tree[i << 1],
                                         self.tree[i << 1 | 1])

    def set(self, i: int, x: int) -> None:
        # assert 0 <= i < self.n
        i += self.n
        self.tree[i] = x
        self._propagates(i)

    def add(self, i: int, x: int) -> None:
        # assert 0 <= i < self.n
        i += self.n
        self.tree[i] += x
        self._propagates(i)

    def _propagates(self, i: int) -> None:
        # assert self.n <= i < self.n * 2
        if self.enable_cutoff:
            while i:
                i >>= 1
                before = self.function(
                    self.tree[i << 1], self.tree[i << 1 | 1])
                if self.tree[i] == before:
                    return
                self.tree[i] = before
        else:
            while i:
                i >>= 1
                self.tree[i] = self.function(
                    self.tree[i << 1], self.tree[i << 1 | 1])

    def query(self, l: int, r: int) -> int:
        # assert 0 <= l < self.n
        # assert 0 < r <= self.n
        l += self.n
        r += self.n
        res = self.element
        while l < r:
            if l & 1:
                res = self.function(res, self.tree[l])
                l += 1
            if r & 1:
                r -= 1
                res = self.function(res, self.tree[r])
            l >>= 1
            r >>= 1
        return res

    def get(self, index: int) -> int:
        # assert 0 <= index < self.n
        return self.tree[index + self.n]

    def get_list(self, l: int = 0, r: int = None) -> list:
        # assert 0 <= l < self.n
        if r is None:
            r = self.n
        # assert 0 < r <= self.n
        return self.tree[l + self.n: r + self.n]

    # def max_right(self, l: int = 0, function=lambda x: x < 0):
    #     # function(tree[l, r)) == Trueとなる最大のrを返す
    #     # assert 0 <= l <= self.n
    #     if l == self.n:
    #         return self.n
    #     i = l + self.n
    #     sm = self.element
    #     while True:
    #         while not (i & 1):
    #             i >>= 1
    #         if not function(self.function(sm, self.tree[i])):
    #             while i < self.n:
    #                 i <<= 1
    #                 if function(self.function(sm, self.tree[i])):
    #                     sm = self.function(sm, self.tree[i])
    #                     i += 1
    #             return i - self.n
    #         sm = self.function(sm, self.tree[i])
    #         if i <= 1:
    #             break
    #         i += 1
    #     return self.n

    # def min_left(self, r: int = None, function=lambda x: x > 0):
    #     # function(tree[l, r)) == Trueとなる最小のlを返す
    #     # assert 0 <= r <= self.n
    #     if r == 0:
    #         return 0
    #     if r is None:
    #         r = self.n
    #     i = r + self.n
    #     sm = self.element
    #     while True:
    #         i -= 1
    #         while i > 1 and (i & 1):
    #             i >>= 1
    #         if not function(self.function(sm, self.tree[i])):
    #             while i < self.n:
    #                 i <<= 1
    #                 i += 1
    #                 if function(self.function(sm, self.tree[i])):
    #                     sm = self.function(sm, self.tree[i])
    #                     i -= 1
    #             return i + 1 - self.n
    #         sm = self.function(sm, self.tree[i])
    #         if i <= 1:
    #             break
    #     return 0
