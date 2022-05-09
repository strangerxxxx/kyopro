class LiChaoTree(object):
    def __init__(self, X: int):
        # X:管理する点の集合
        X = sorted(set(X))
        N = 1 << (len(X) - 1).bit_length()
        self.INF = float("inf")
        self._tree = [None] * (N << 1)
        self._N, self._X = N, X + [self.INF] * (N - len(X))
        self._X_inv = {x: i for i, x in enumerate(X)}

    def _add_line(self, a: int, b: int, l: int, r: int) -> None:
        # [l,r)の範囲でy=ax+bを追加
        tree, X = self._tree, self._X
        i = 1
        while r - l:
            if tree[i] is None:
                tree[i] = (a, b)
                return
            m = (l + r) >> 1
            xl, xm, xr = X[l], X[m], X[r - 1]
            ai, bi = tree[i]
            left = a * xl + b < ai * xl + bi
            mid = a * xm + b < ai * xm + bi
            right = a * xr + b < ai * xr + bi

            if left is right:
                if left:
                    tree[i] = (a, b)
                return
            if mid:
                tree[i], a, b = (a, b), ai, bi
            if left is not mid:
                i, r = i << 1, m
            else:
                i, l = i << 1 | 1, m

    def add_line(self, a: int, b: int) -> None:
        # y=ax+bを追加
        return self._add_line(a, b, 0, self._N)

    def get_min(self, x: int) -> int:
        # xでの最小値
        i = self._X_inv[x]
        i += self._N
        res = self.INF
        tree = self._tree
        while i:
            if tree[i] is not None:
                a, b = tree[i]
                res = min(res, a * x + b)
            i >>= 1
        return res

    def val(a: int, b: int, x: int) -> int:
        return a * x + b
