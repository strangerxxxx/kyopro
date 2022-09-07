class RollingHash:
    # verified: https://onlinejudge.u-aizu.ac.jp/status/users/stranger/submissions/1/ALDS1_14_B/judge/6938514/Python3
    def __init__(self, r: int = None) -> None:
        self.m = (1 << 61) - 1
        if r is None:
            import random
            r = random.randint(2, self.m - 2)
        self.r = r
        self.msk30 = (1 << 30) - 1
        self.msk31 = (1 << 31) - 1

    def mul(self, a: int, b: int) -> int:
        au = a >> 31
        ad = a & self.msk31
        bu = b >> 31
        bd = b & self.msk31
        mid = ad * bu + au * bd
        midu = mid >> 30
        midd = mid & self.msk30
        return (au * bu * 2 + midu + (midd << 31) + ad * bd) % self.m

    def hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = pow(self.r, length - 1, self.m)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self.mul(ord(k), denom)
            x = self.mul(x, self.r)
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        yield x
        denom = pow(self.r, length - 1, self.m)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self.mul(ord(k), denom)
            x = self.mul(x, self.r)
            x += ord(j)
            x %= self.m
            yield x

    def all_hash_list(self, s: str):
        return [self.hash_list(s, i + 1) for i in range(len(s))]

    def head_hash_list(self, s: str):
        # res[i]:前方i+1文字のHash
        res = [0] * len(s)
        for i, j in enumerate(s):
            res[i] = (self.mul(res[i - 1], self.r) + ord(j)) % self.m
        return res

    def tail_hash_list(self, s: str):
        # res[i]:後方i+1文字のHash
        res = [0] * len(s)
        x = 1
        for i, j in enumerate(reversed(s)):
            res[i] = (res[i - 1] + self.mul(x, ord(j))) % self.m
            x = self.mul(x, self.r)
        return res

    def get_hash(self, s: str) -> int:
        return self.hash_list(s, len(s))[0]


class RollingHash_m:
    def __init__(self, r: int = None, m: int = (1 << 61) - 1) -> None:
        if r is None:
            import random
            r = random.randint(2, self.m - 2)
        self.r = r
        self.m = m

    def hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = x * self.r % self.m
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = pow(self.r, length - 1, self.m)
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
        denom = pow(self.r, length - 1, self.m)
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
        res = [0] * len(s)
        for i, j in enumerate(s):
            res[i] = (self.mul(res[i - 1], self.r) + ord(j)) % self.m
        return res

    def tail_hash_list(self, s: str):
        # res[i]:後方i+1文字のHash
        res = [0] * len(s)
        x = 1
        for i, j in enumerate(reversed(s)):
            res[i] = (res[i - 1] + x * ord(j)) % self.m
            x = x * self.r % self.m
        return res

    def get_hash(self, s: str) -> int:
        return self.hash_list(s, len(s))[0]


class RollingHash2d:
    # verified: https://onlinejudge.u-aizu.ac.jp/status/users/stranger/submissions/1/ALDS1_14_C/judge/6938890/Python3
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

    def mul(self, a: int, b: int) -> int:
        au = a >> 31
        ad = a & self.msk31
        bu = b >> 31
        bd = b & self.msk31
        mid = ad * bu + au * bd
        midu = mid >> 30
        midd = mid & self.msk30
        return (au * bu * 2 + midu + (midd << 31) + ad * bd) % self.m

    def _hash_list(self, s: str, length: int) -> list:
        n = len(s)
        res = [None] * (n - length + 1)
        x = 0
        for i in s[:length]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = pow(self.r, length - 1, self.m)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self.mul(ord(k), denom)
            x = self.mul(x, self.r)
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def _gen_hash_list(self, s: str, length: int) -> int:
        x = 0
        for i in s[:length]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        yield x
        denom = pow(self.r, length - 1, self.m)
        for i, (j, k) in enumerate(zip(s[length:], s[:-length])):
            x -= self.mul(ord(k), denom)
            x = self.mul(x, self.r)
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
                x[j] = self.mul(x[j], self.r2)
                x[j] += k
        for j in range(n2 - w + 1):
            x[j] %= self.m
            res[0][j] = x[j]
        denom = pow(self.r2, h - 1, self.m)
        for i in range(n - h):
            hash_lists[i + h] = self._hash_list(s[i + h], w)
            for j in range(n2 - w + 1):
                x[j] -= self.mul(hash_lists[i][j], denom)
                x[j] = self.mul(x[j], self.r2)
                x[j] += hash_lists[i + h][j]
                x[j] %= self.m
                res[i + 1][j] = x[j]
        return res

    def _get_hash(self, s: str) -> int:
        res = 0
        for i in s:
            res = self.mul(res, self.r)
            res += ord(i)
        res %= self.m
        return res

    def get_hash_2d(self, s: list) -> int:
        return self.hash_list_2d(s, len(s), len(s[0]))[0][0]
