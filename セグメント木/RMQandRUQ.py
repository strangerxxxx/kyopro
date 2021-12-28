class RMQandRUQ:
    """
    init(init_val, element): 配列init_valで初期化 O(N)
    update(l, r, x): 区間[l, r)をxに更新 O(logN)
    query(l, r): 区間[l, r)にfunctionを適用したものを返す O(logN)
    """

    def __init__(self, init_val, function=min, element=float("inf"), default: int = 0) -> None:
        """
        init_val: 配列の初期値
        function: 区間にしたい操作
        element: 単位元
        num: n以上の最小の2のべき乗
        data: 値配列(1-index)
        lazy: 遅延配列(1-index)
        """
        if hasattr(init_val, "__iter__"):
            self.n = len(init_val)
        else:
            self.n = init_val
        self.function = function
        self.element = element
        self.num = 1 << (self.n - 1).bit_length()
        self.data = [element] * 2 * self.num
        self.lazy = [None] * 2 * self.num
        if hasattr(init_val, "__iter__"):
            for i, j in enumerate(init_val):
                self.data[self.num + i] = j
        else:
            for i in range(self.n):
                self.data[self.num + i] = default
        for i in range(self.num - 1, 0, -1):
            self.data[i] = self.function(
                self.data[i << 1], self.data[i << 1 | 1])

    def gindex(self, l, r):
        """
            伝搬する対象の区間を求める
            lm: 伝搬する必要のある最大の左閉区間
            rm: 伝搬する必要のある最大の右開区間
            """
        l += self.num
        r += self.num
        lm = l >> (l & -l).bit_length()
        rm = r >> (r & -r).bit_length()

        while r > l:
            if l <= lm:
                yield l
            if r <= rm:
                yield r
            r >>= 1
            l >>= 1
        while l:
            yield l
            l >>= 1

    def propagates(self, *ids) -> None:
        """
        遅延伝搬処理
        ids: 伝搬する対象の区間 
        """
        for i in reversed(ids):
            v = self.lazy[i]
            if v is None:
                continue
            self.lazy[i << 1] = v
            self.lazy[i << 1 | 1] = v
            self.data[i << 1] = v
            self.data[i << 1 | 1] = v
            self.lazy[i] = None

    def update(self, l, r, x) -> None:
        """
        区間[l, r)の値をxに更新
        l, r: index(0-index)
        x: update value
        """
        *ids, = self.gindex(l, r)
        self.propagates(*ids)
        l += self.num
        r += self.num
        while l < r:
            if l & 1:
                self.lazy[l] = x
                self.data[l] = x
                l += 1
            if r & 1:
                r -= 1
                self.lazy[r] = x
                self.data[r] = x
            r >>= 1
            l >>= 1
        for i in ids:
            self.data[i] = self.function(
                self.data[i << 1], self.data[i << 1 | 1])

    def query(self, l, r):
        """
        [l, r)にfunctionを適用したものを返す O(logN)
        l: index(0-index)
        r: index(0-index)
        """
        *ids, = self.gindex(l, r)
        self.propagates(*ids)

        res = self.element

        l += self.num
        r += self.num
        while l < r:
            if l & 1:
                res = self.function(res, self.data[l])
                l += 1
            if r & 1:
                res = self.function(res, self.data[r - 1])
            l >>= 1
            r >>= 1
        return res

    def get(self, l=0, r=None) -> list:
        """
        [l, r)の配列を返す
        """
        if r is None:
            r = self.n
        return [self.query(x, x + 1) for x in range(l, r)]
