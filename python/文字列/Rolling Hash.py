class RollingHash:
    # verified: https://bit.ly/3X12Gzg
    m = None
    r = None
    rp = {}

    def __init__(self, r: int = None, m=2305843009213693951) -> None:
        if self.m is None:
            self.m = m  # (1 << 61) - 1
        if self.r is None:
            if r is None:
                import random

                r = random.randint(2, self.m - 2)
            self.r = r

    def _powr(self, n: int) -> int:
        if n not in self.rp:
            self.rp[n] = pow(self.r, n, self.m)
        return self.rp[n]

    def hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x *= self.r
            x += ord(i)
            x %= self.m
        res[0] = x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom
            x *= self.r
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x *= self.r
            x += ord(i)
            x %= self.m
        yield x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom
            x *= self.r
            x += ord(j)
            x %= self.m
            yield x

    def all_hash_list(self, s: str):
        return [self.hash_list(s, i + 1) for i in range(len(s))]

    def head_hash_list(self, s: str):
        # res[i]:前方i文字のHash
        res = [0] * (len(s) + 1)
        for i, j in enumerate(s):
            res[i + 1] = (res[i] * self.r + ord(j)) % self.m
        return res

    def tail_hash_list(self, s: str):
        # res[i]:後方i文字のHash
        res = [0] * (len(s) + 1)
        x = 1
        for i, j in enumerate(reversed(s)):
            res[i + 1] = (res[i] + x * ord(j)) % self.m
            x = x * self.r
        return res

    def get_hash(self, s_or_head_list, l: int = 0, r: int = None) -> int:
        if type(s_or_head_list) is str:
            if r is None:
                return next(
                    self.gen_hash_list(s_or_head_list[l:], len(s_or_head_list) - l)
                )
            else:
                if l == r:
                    return 0
                g = self.gen_hash_list(s_or_head_list[l:], r - l)
                for _ in range(r - l - 1):
                    next(g)
                return next(g)
        # verified: http://bit.ly/3Uswhjl
        if r is None:
            r = len(s_or_head_list) + 1
        return (s_or_head_list[r] - s_or_head_list[l] * self._powr(r - l)) % self.m


class RollingHash2d:
    # verified: http://bit.ly/3X42Vto
    m = 2305843009213693951  # (1 << 61) - 1
    r = None
    r2 = None
    pr = {}
    pr2 = {}

    def __init__(self, r: int = None, r2: int = None) -> None:
        import random

        if self.r is None:
            if r is None:
                r = random.randint(2, self.m - 2)
            self.r = r
        if self.r2 is None:
            if r2 is None:
                r2 = random.randint(2, self.m - 2)
            self.r2 = r2

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
            x *= self.r
            x += ord(i)
            x %= self.m
        res[0] = x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom
            x %= self.m
            x *= self.r
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def _gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x *= self.r
            x += ord(i)
            x %= self.m
        yield x
        denom = self._powr(length - 1)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= ord(k) * denom
            x %= self.m
            x *= self.r
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
                x[j] *= self.r2
                x[j] += k
                x[j] %= self.m
        for j in range(n2 - w + 1):
            x[j] %= self.m
            res[0][j] = x[j]
        denom = self._powr2(h - 1)
        for i in range(n - h):
            hash_lists[i + h] = self._hash_list(s[i + h], w)
            for j in range(n2 - w + 1):
                x[j] -= hash_lists[i][j] * denom
                x[j] %= self.m
                x[j] *= self.r2
                x[j] += hash_lists[i + h][j]
                x[j] %= self.m
                res[i + 1][j] = x[j]
        return res

    def _get_hash(self, s: str) -> int:
        res = 0
        for i in s:
            res *= self.r
            res += ord(i)
            res %= self.m
        return res

    def get_hash_2d(self, s: list) -> int:
        return self.hash_list_2d(s, len(s), len(s[0]))[0][0]
