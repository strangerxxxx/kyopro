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

    def hash_list(self, s: str, l: int) -> list:
        n = len(s)
        res = [None] * (n - l + 1)
        x = 0
        for i in s[:l]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        res[0] = x
        denom = pow(self.r, l - 1, self.m)
        for i, (j, k) in enumerate(zip(s[l:], s[:-l])):
            x -= ord(k) * denom % self.m
            x = self.mul(x, self.r)
            x += ord(j)
            x %= self.m
            res[i + 1] = x
        return res

    def gen_hash_list(self, s: str, l: int) -> int:
        x = 0
        for i in s[:l]:
            x = self.mul(x, self.r)
            x += ord(i)
        x %= self.m
        yield x
        denom = pow(self.r, l - 1, self.m)
        for i, (j, k) in enumerate(zip(s[l:], s[:-l])):
            x -= ord(k) * denom % self.m
            x = self.mul(x, self.r)
            x += ord(j)
            x %= self.m
            yield x

    def all_hash_list(self, s: str):
        return [self.hash_list(s, i + 1) for i in range(len(s))]

    def get_hash(self, s: str) -> int:
        res = 0
        for i in s:
            res = self.mul(res, self.r)
            res += ord(i)
        res %= self.m
        return res
