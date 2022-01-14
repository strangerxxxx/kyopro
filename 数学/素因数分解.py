from random import randint


# from functools import lru_cache
# @lru_cache(maxsize=None)
def prime_factorization(n: int):
    # dictを返す
    from collections import defaultdict
    pf = defaultdict(int)
    for i in range(2, int(n ** 0.5) + 1):
        while n % i == 0:
            pf[i] += 1
            n //= i
    if n > 1:
        pf[n] = 1
    return pf


def prime_decomposition(n: int):
    # nを素因数分解したリストを返す
    i = 2
    res = []
    for i in range(2, int(n ** 0.5) + 1):
        while n % i == 0:
            res.append(i)
            n //= i
    if n > 1:
        res.append(n)
    return res


def prime_factorization_gen(n: int):
    # 素因数を返すジェネレーター
    for i in range(2, int(n ** 0.5) + 1):
        if n % i == 0:
            yield i
            n //= i
            while n % i == 0:
                n //= i
    if n > 1:
        yield n


class PrimeFactorization:
    """
    前計算あり素因数分解
    n: 前計算する最大の数
    前計算     O(n log log n)
    素因数分解 O(log n)
    """

    def __init__(self, n: int) -> None:
        self.n = n
        self.spf = [None] * 4 + [2, None] * (n // 2)
        for i in range(3, int(n ** 0.5) + 1, 2):
            if self.spf[i]:
                continue
            for j in range(i * 2, n + 1, i):
                if not self.spf[j]:
                    self.spf[j] = i

    def __call__(self, i: int) -> dict:
        assert 0 <= i <= self.n
        res = {}
        while self.spf[i]:
            min_pk = self.spf[i]
            res[min_pk] = res.get(min_pk, 0) + 1
            i //= min_pk
        if i > 1:
            res[i] = res.get(i, 0) + 1
        return res


class Prime():
    def __init__(self, m=10 ** 6):
        self.m = m
        self.isprime = [True] * (m + 1)
        self.minfac = [-1] * (m + 1)
        self.mobius = [1] * (m + 1)
        self.isprime[0] = self.isprime[1] = False
        self.minfac[1] = 1
        for p in range(2, m + 1):
            if not self.isprime[p]:
                continue
            self.minfac[p] = p
            self.mobius[p] = -1
            for q in range(2 * p, m + 1, p):
                self.isprime[q] = False
                if self.minfac[q] == -1:
                    self.minfac[q] = p
                if (q // p) % p:
                    self.mobius[q] *= -1
                else:
                    self.mobius[q] = 0

    def gcd(self, x, y):
        while y:
            x, y = y, x % y
        return x

    def lcm(self, x, y):
        return x // self.gcd(x, y) * y

    def is_prime(self, n):
        if n <= self.m:
            return self.isprime[n]
        if not n & 1:
            return False
        return self.miller_rabin(n)

    def miller_rabin(self, n):
        if n < 4294967296:
            p = [2, 7, 61]
        elif n < 281474976710656:
            p = [2, 3, 5, 7, 11, 13, 17]
        else:
            p = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
        d = n - 1
        d = d // (d & -d)
        for a in p:
            t = d
            y = pow(a, t, n)
            if y == 1:
                continue
            while y != n - 1:
                y = (y * y) % n
                if y == 1 or t == n - 1:
                    return False
                t <<= 1
        return True

    def factorize(self, n):
        if n <= self.m:
            return self.factorize_fast(n)
        if self.is_prime(n):
            return [n]
        res = []
        stack = [n]
        while stack:
            tmp = stack.pop()
            if tmp <= self.m:
                res.extend(self.factorize_fast(tmp))
                continue
            p = self.pollard_rho(tmp)
            q = tmp // p
            pri_p = self.is_prime(p)
            pri_q = self.is_prime(q)
            if pri_p and pri_q:
                res.append(p)
                res.append(q)
            elif pri_p:
                res.append(p)
                while not q % p:
                    res.append(p)
                    q //= p
                if self.is_prime(q):
                    res.append(q)
                else:
                    stack.append(q)
            elif pri_q:
                res.append(q)
                while not p % q:
                    res.append(q)
                    p //= q
                if self.is_prime(p):
                    res.append(p)
                else:
                    stack.append(p)
            else:
                stack.append(p)
                stack.append(q)
        return sorted(res)

    def factorize_fast(self, n):
        res = []
        while n > 1:
            p = self.minfac[n]
            while self.minfac[n] == p:
                n //= p
                res.append(p)
        return sorted(res)

    def factorize_naive(self, n):
        res = []
        x, y = n, 2
        while y * y <= x:
            while not x % y:
                res.append(y)
                x //= y
            y += 1
        if x > 1:
            res.append(x)
        return sorted(res)

    def pollard_rho(self, n):
        m = int(n**0.125) + 1
        s = randint(1, 1000)
        while True:
            y, r, q = 2, 1, 1
            d = 1
            while d == 1:
                x = y
                for _ in range(r):
                    y = (y * y + s) % n
                for k in range(0, r, m):
                    ys = y
                    for i in range(min(m, r - k)):
                        y = (y * y + s) % n
                        q = q * abs(x - y) % n
                    d = self.gcd(n, q)
                    if d != 1:
                        break
                r <<= 1
            if d == n:
                while d == 1:
                    ys = (ys * ys + s) % n
                    d = self.gcd(n, abs(x - ys))
            if d != n:
                break
            s += 1
        return d
