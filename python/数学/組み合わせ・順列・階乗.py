def scipycomb(n, r):
    # modなし組み合わせ
    from scipy.special import comb

    return comb(n, r, exact=True)
    # import math
    # print(math.perm(n, r))


def cmb(n: int, r: int, m=998244353):
    # modあり組み合わせ
    if r < 0 or n < r:
        return 0
    if r > n - r:
        return cmb(n, n - r, m)
    c = d = 1
    for i in range(r):
        c *= n - i
        d *= r - i
        c %= m
        d %= m
    return c * pow(d, m - 2, m) % m


def perm(n: int, r: int = None, m: int = 998244353) -> int:
    # modあり順列 n==rで階乗
    if r is None:
        r = n
    if r < 0 or n < r:
        return 0
    c = 1
    for i in range(r):
        c *= n - i
        c %= m
    return c


def hom(n: int, r: int, m=998244353):
    # modあり重複組み合わせ nHr=n+r-1Cr, n:separator+1
    def cmb(n: int, r: int, m=998244353):
        if r < 0 or n < r:
            return 0
        if r > n - r:
            return cmb(n, n - r, m)
        c = d = 1
        for i in range(r):
            c *= n - i
            d *= r - i
            c %= m
            d %= m
        return c * pow(d, m - 2, m) % m

    return cmb(n + r - 1, r, m)


def factorial(n: int, m: int = 998244353) -> int:
    # modあり階乗
    c = 1
    for i in range(n):
        c *= n - i
        c %= m
    return c


def split_combination(a):
    # リストの分割の組み合わせ
    q = [(0, [])]
    while q:
        l, res = q.pop()
        for i in range(l + 1, len(a)):
            q.append((i, res + [a[l:i]]))
        yield res + [a[l:]]


class MemorizeCombination:
    """
    メモ化modなし組み合わせ
    """

    def __init__(self):
        import sys

        sys.setrecursionlimit = 998244353
        self.cmb = {}

    def __call__(self, n: int, r: int) -> int:
        if r < 0 or n < r:
            return 0
        if r * 2 > n:
            return self.__call__(n, n - r)
        if r == 0:
            return 1
        if (n, r) in self.cmb:
            return self.cmb[(n, r)]
        self.cmb[(n, r)] = self.__call__(n - 1, r - 1) + self.__call__(n - 1, r)
        return self.cmb[(n, r)]

    def init_calc(self, n, r):
        for i in range(n + 1):
            for j in range(max(i, r + 1)):
                self.__call__(i, j)


class Combination:
    """
    前計算modあり組み合わせ
    """

    def __init__(self, n_max, mod=998244353):
        self.n_max = n_max
        self.mod = mod
        self.modinv = self.make_modinv_list(n_max)
        self.fac, self.facinv = self.make_factorial_list(n_max)

    def __call__(self, n, r):
        if r < 0 or n < r:
            return 0
        if r > self.n_max:
            raise ValueError("n is larger than n_max.")
        return self.fac[n] * self.facinv[r] % self.mod * self.facinv[n - r] % self.mod

    def make_modinv_list(self, n):
        # 0からnまでのmod逆元のリスト
        modinv = [0, 1] + [0] * (n - 1)
        for i in range(2, n + 1):
            modinv[i] = self.mod - self.mod // i * modinv[self.mod % i] % self.mod
        return modinv

    def make_factorial_list(self, n):
        # 階乗のリストと階乗のmod逆元のリスト
        fac = [1] + [0] * n
        facinv = [1] + [0] * n
        for i in range(1, n + 1):
            fac[i] = fac[i - 1] * i % self.mod
            facinv[i] = facinv[i - 1] * self.modinv[i] % self.mod
        return fac, facinv


class Permutation:
    """
    前計算modあり組み合わせ
    """

    def __init__(self, n_max, mod=998244353):
        self.n_max = n_max
        self.mod = mod
        self.modinv = self.make_modinv_list(n_max)
        self.fac, self.facinv = self.make_factorial_list(n_max)

    def __call__(self, n, r):
        if r < 0 or n < r:
            return 0
        if r > self.n_max:
            raise ValueError("n is larger than n_max.")
        return self.fac[n] * self.facinv[n - r] % self.mod

    def make_modinv_list(self, n):
        # 0からnまでのmod逆元のリスト
        modinv = [0, 1] + [0] * (n - 1)
        for i in range(2, n + 1):
            modinv[i] = self.mod - self.mod // i * modinv[self.mod % i] % self.mod
        return modinv

    def make_factorial_list(self, n):
        # 階乗のリストと階乗のmod逆元のリスト
        fac = [1] + [0] * n
        facinv = [1] + [0] * n
        for i in range(1, n + 1):
            fac[i] = fac[i - 1] * i % self.mod
            facinv[i] = facinv[i - 1] * self.modinv[i] % self.mod
        return fac, facinv


class Combinatorics:
    """
    前計算modあり組み合わせ
    """

    def __init__(self, n_max, mod=998244353):
        self.n_max = n_max
        self.mod = mod
        self.modinv = self.make_modinv_list(n_max)
        self.fac, self.facinv = self.make_factorial_list(n_max)

    def combination(self, n, r):
        """
        return nCr
        """
        if r < 0 or n < r:
            return 0
        if r > self.n_max:
            raise ValueError("n is larger than n_max.")
        return self.fac[n] * self.facinv[r] % self.mod * self.facinv[n - r] % self.mod

    def permutation(self, n, r=None):
        """
        return nPr
        """
        if r is None:
            r = n
        if r < 0 or n < r:
            return 0
        if r > self.n_max:
            raise ValueError("n is larger than n_max.")
        return self.fac[n] * self.facinv[n - r] % self.mod

    def homogeneous(self, n, r):
        """
        return nHr
        n:separator + 1
        """
        if n <= 0 or r < 0:
            return 0
        if n + r > self.n_max:
            raise ValueError("n+r is larger than n_max.")
        return self.combination((n + r - 1) % self.mod, r)

    def make_modinv_list(self, n):
        # 0からnまでのmod逆元のリスト
        modinv = [0, 1] + [0] * (n - 1)
        for i in range(2, n + 1):
            modinv[i] = self.mod - self.mod // i * modinv[self.mod % i] % self.mod
        return modinv

    def make_factorial_list(self, n):
        # 階乗のリストと階乗のmod逆元のリスト
        fac = [1] + [0] * n
        facinv = [1] + [0] * n
        for i in range(1, n + 1):
            fac[i] = fac[i - 1] * i % self.mod
            facinv[i] = facinv[i - 1] * self.modinv[i] % self.mod
        return fac, facinv


class BinomialCoefficient:
    # 任意mod二項係数
    # 前計算O(nlogm/loglogm+m), クエリO(lognlogm/loglogm)
    def __init__(self, m):
        self.MOD = m
        self.factorization = self._factorize(m)
        self.facs = []
        self.invs = []
        self.coeffs = []
        self.pows = []
        for p, pe in self.factorization:
            fac = [1] * pe
            for i in range(1, pe):
                fac[i] = fac[i - 1] * (i if i % p else 1) % pe
            inv = [1] * pe
            inv[-1] = fac[-1]
            for i in range(1, pe)[::-1]:
                inv[i - 1] = inv[i] * (i if i % p else 1) % pe
            self.facs.append(fac)
            self.invs.append(inv)
            # coeffs
            c = self._modinv(m // pe, pe)
            self.coeffs.append(m // pe * c % m)
            # pows
            powp = [1]
            while powp[-1] * p != pe:
                powp.append(powp[-1] * p)
            self.pows.append(powp)

    def __call__(self, n, k):
        if k < 0 or k > n:
            return 0
        if k == 0 or k == n:
            return 1 % self.MOD
        res = 0
        for i, (p, pe) in enumerate(self.factorization):
            res += (
                self._choose_pe(n, k, p, pe, self.facs[i], self.invs[i], self.pows[i])
                * self.coeffs[i]
            )
            res %= self.MOD
        return res

    def _E(self, n, k, r, p):
        res = 0
        while n:
            n //= p
            k //= p
            r //= p
            res += n - k - r
        return res

    def _choose_pe(self, n, k, p, pe, fac, inv, powp):
        r = n - k
        e0 = self._E(n, k, r, p)
        if e0 >= len(powp):
            return 0
        res = powp[e0]
        if (p != 2 or pe == 4) and self._E(
            n // (pe // p), k // (pe // p), r // (pe // p), p
        ) % 2:
            res = pe - res
        while n:
            res = res * fac[n % pe] % pe * inv[k % pe] % pe * inv[r % pe] % pe
            n //= p
            k //= p
            r //= p
        return res

    def _factorize(self, N):
        factorization = []
        for i in range(2, N + 1):
            if i * i > N:
                break
            if N % i:
                continue
            c = 0
            while N % i == 0:
                N //= i
                c += 1
            factorization.append((i, i**c))
        if N != 1:
            factorization.append((N, N))
        return factorization

    def _modinv(self, a, MOD):
        r0, r1, s0, s1 = a, MOD, 1, 0
        while r1:
            r0, r1, s0, s1 = r1, r0 % r1, s1, s0 - r0 // r1 * s1
        return s0 % MOD
