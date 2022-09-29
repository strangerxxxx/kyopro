class RollingHash:
    # verified: https://bit.ly/3LGs1ch
    def __init__(self, r: int = None) -> None:
        self.m = (1 << 61) - 1
        if r is None:
            import random
            r = random.randint(2, self.m - 2)
        self.r = r
        self.msk30 = (1 << 30) - 1
        self.msk31 = (1 << 31) - 1
        self.rp = {}

    def _mul(self, a: int, b: int) -> int:
        au = a >> 31
        ad = a & self.msk31
        bu = b >> 31
        bd = b & self.msk31
        mid = ad * bu + au * bd
        midu = mid >> 30
        midd = mid & self.msk30
        return (au * bu * 2 + midu + (midd << 31) + ad * bd) % self.m

    def _powr(self, n: int) -> int:
        if n not in self.rp:
            self.rp[n] = pow(self.r, n, self.m)
        return self.rp[n]

    def hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = self._mul(x, self.r)
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self._mul(ord(k), denom)
            x = self._mul(x, self.r)
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x = self._mul(x, self.r)
            x += ord(i)
        x %= self.m
        yield x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self._mul(ord(k), denom)
            x = self._mul(x, self.r)
            x += ord(j)
            x %= self.m
            yield x

    def all_hash_list(self, s: str):
        return [self.hash_list(s, i + 1) for i in range(len(s))]

    def head_hash_list(self, s: str):
        # res[i]:前方i文字のHash
        res = [0] * (len(s) + 1)
        for i, j in enumerate(s):
            res[i + 1] = (self._mul(res[i], self.r) + ord(j)) % self.m
        return res

    def tail_hash_list(self, s: str):
        # res[i]:後方i文字のHash
        res = [0] * (len(s) + 1)
        x = 1
        for i, j in enumerate(reversed(s)):
            res[i + 1] = (res[i] + self._mul(x, ord(j))) % self.m
            x = self._mul(x, self.r)
        return res

    def get_hash(self, s_or_head_list, l: int = 0, r: int = None) -> int:
        if type(s_or_head_list) is str:
            if r is None:
                return next(self.gen_hash_list(s_or_head_list[l:],
                                               len(s_or_head_list) - l))
            else:
                if l == r:
                    return 0
                g = self.gen_hash_list(s_or_head_list[l:], r - l)
                for _ in range(r - l - 1):
                    next(g)
                return next(g)
        # verified: https://bit.ly/3ScyApH
        if r is None:
            r = len(s_or_head_list) + 1
        return (s_or_head_list[r] - s_or_head_list[l]
                * self._powr(r - l)) % self.m


class RollingHash_m:
    def __init__(self, r: int = None, m: int = (1 << 61) - 1) -> None:
        if r is None:
            import random
            r = random.randint(2, self.m - 2)
        self.r = r
        self.m = m
        self.rp = {}

    def _powr(self, n: int) -> int:
        if n not in self.rp:
            self.rp[n] = pow(self.r, n, self.m)
        return self.rp[n]

    def hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = x * self.r % self.m
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom % self.m
            x = x * self.r % self.m
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x = x * self.r % self.m
            x += ord(i)
        x %= self.m
        yield x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom % self.m
            x = x * self.r % self.m
            x += ord(j)
            x %= self.m
            yield x

    def all_hash_list(self, s: str):
        return [self.hash_list(s, i + 1) for i in range(len(s))]

    def head_hash_list(self, s: str):
        # res[i]:前方i+1文字のHash
        res = [0] * (len(s) + 1)
        for i, j in enumerate(s):
            res[i + 1] = (res[i] * self.r + ord(j)) % self.m
        return res

    def tail_hash_list(self, s: str):
        # res[i]:後方i+1文字のHash
        res = [0] * (len(s) + 1)
        x = 1
        for i, j in enumerate(reversed(s)):
            res[i + 1] = (res[i] + x * ord(j)) % self.m
            x = x * self.r % self.m
        return res

    def get_hash(self, s_or_head_list, l: int = 0, r: int = None) -> int:
        if type(s_or_head_list) is str:
            if r is None:
                return next(self.gen_hash_list(s_or_head_list[l:],
                                               len(s_or_head_list) - l))
            else:
                if l == r:
                    return 0
                g = self.gen_hash_list(s_or_head_list[l:], r - l)
                for _ in range(r - l - 1):
                    next(g)
                return next(g)
        if r is None:
            r = len(s_or_head_list) + 1
        return (s_or_head_list[r] - s_or_head_list[l]
                * self._powr(r - l)) % self.m


class RollingHash2d:
    # verified: https://bit.ly/3xRdbdi
    def __init__(self, r: int = None, r2: int = None) -> None:
        self.m = (1 << 61) - 1
        import random
        if r is None:
            r = random.randint(2, self.m - 2)
        if r2 is None:
            r2 = random.randint(2, self.m - 2)
        self.r = r
        self.r2 = r2
        self.msk30 = (1 << 30) - 1
        self.msk31 = (1 << 31) - 1
        self.pr = {}
        self.pr2 = {}

    def _mul(self, a: int, b: int) -> int:
        au = a >> 31
        ad = a & self.msk31
        bu = b >> 31
        bd = b & self.msk31
        mid = ad * bu + au * bd
        midu = mid >> 30
        midd = mid & self.msk30
        return (au * bu * 2 + midu + (midd << 31) + ad * bd) % self.m

    def _powr(self, n: int):
        if n not in self.pr:
            self.pr[n] = pow(self.r, n, self.m)
        return self.pr[n]

    def _powr2(self, n: int):
        if n not in self.pr2:
            self.pr2[n] = pow(self.r2, n, self.m)
        return self.pr2[n]

    def _hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = self._mul(x, self.r)
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self._mul(ord(k), denom)
            x = self._mul(x, self.r)
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def _gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x = self._mul(x, self.r)
            x += ord(i)
        x %= self.m
        yield x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self._mul(ord(k), denom)
            x = self._mul(x, self.r)
            x += ord(j)
            x %= self.m
            yield x

    def hash_list_2d(self, s: list, h: int, w: int) -> list:
        n = len(s)
        n2 = len(s[0])
        res = [[None] * (n2 - w + 1) for _ in range(n - h + 1)]
        x = [0] * (n2 - w + 1)
        hash_lists = [None] * n
        for i in range(h):
            hash_lists[i] = self._hash_list(s[i], w)
            for j, k in enumerate(hash_lists[i]):
                x[j] = self._mul(x[j], self.r2)
                x[j] += k
        for j in range(n2 - w + 1):
            x[j] %= self.m
            res[0][j] = x[j]
        denom = self._powr2(h - 1)
        for i in range(n - h):
            hash_lists[i + h] = self._hash_list(s[i + h], w)
            for j in range(n2 - w + 1):
                x[j] -= self._mul(hash_lists[i][j], denom)
                x[j] = self._mul(x[j], self.r2)
                x[j] += hash_lists[i + h][j]
                x[j] %= self.m
                res[i + 1][j] = x[j]
        return res

    def _get_hash(self, s: str) -> int:
        res = 0
        for i in s:
            res = self._mul(res, self.r)
            res += ord(i)
        res %= self.m
        return res

    def get_hash_2d(self, s: list) -> int:
        return self.hash_list_2d(s, len(s), len(s[0]))[0][0]
