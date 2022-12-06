MOD = 998244353


class Matrix():
    def __init__(self, n, m, mat=None):
        self.n = n
        self.m = m
        self.mat = [[0] * self.m for _ in range(self.n)]
        if mat:
            # assert len(mat) == n and len(mat[0]) == m
            for i in range(self.n):
                self.mat[i] = mat[i].copy()

    def is_square(self):
        return self.n == self.m

    def determinant(self):
        # assert self.is_square()
        res = 1
        tmp = Matrix(self.n, self.n, self.mat)
        for j in range(self.n):
            if tmp[j][j] == 0:
                for i in range(j + 1, self.n):
                    if tmp[i][j]:
                        break
                else:
                    return 0
                tmp.mat[j], tmp.mat[i] = tmp.mat[i], tmp.mat[j]
                res *= -1
            tmp_j = tmp[j]
            inv = pow(tmp_j[j], MOD - 2, MOD)
            for i in range(j + 1, self.n):
                tmp_i = tmp[i]
                c = -inv * tmp_i[j] % MOD
                for k in range(self.n):
                    tmp_i[k] += c * tmp_j[k]
                    tmp_i[k] %= MOD
        for i in range(self.n):
            res *= tmp[i][i]
            res %= MOD
        return res

    def inverse(self):
        # assert self.is_square()
        res = Matrix.id(self.n)
        tmp = Matrix(self.n, self.n, self.mat)
        for j in range(self.n):
            if tmp[j][j] == 0:
                for i in range(j + 1, self.n):
                    if tmp[i][j]:
                        break
                else:
                    return -1
                tmp.mat[j], tmp.mat[i] = tmp.mat[i], tmp.mat[j]
                res.mat[j], res.mat[i] = res.mat[i], res.mat[j]
            tmp_j, res_j = tmp[j], res[j]
            inv = pow(tmp_j[j], MOD - 2, MOD)
            for k in range(self.n):
                tmp_j[k] *= inv
                tmp_j[k] %= MOD
                res_j[k] *= inv
                res_j[k] %= MOD
            for i in range(self.n):
                if i == j:
                    continue
                c = tmp[i][j]
                tmp_i, res_i = tmp[i], res[i]
                for k in range(self.n):
                    tmp_i[k] -= tmp_j[k] * c
                    tmp_i[k] %= MOD
                    res_i[k] -= res_j[k] * c
                    res_i[k] %= MOD
        return res

    def linear_equations(self, vec):
        # assert self.n == len(vec)
        aug = [self[i] + [vec[i]] for i in range(self.n)]
        rank = 0
        p = []
        q = []
        for j in range(self.m + 1):
            for i in range(rank, self.n):
                if aug[i][j]:
                    break
            else:
                q.append(j)
                continue
            if j == self.m:
                return -1, [], []
            p.append(j)
            aug[rank], aug[i] = aug[i], aug[rank]
            inv = pow(aug[rank][j], MOD - 2, MOD)
            aug_rank = aug[rank]
            for k in range(self.m + 1):
                aug_rank[k] *= inv
                aug_rank[k] %= MOD
            for i in range(self.n):
                if i == rank:
                    continue
                aug_i = aug[i]
                c = -aug_i[j]
                for k in range(self.m + 1):
                    aug_i[k] += c * aug_rank[k]
                    aug_i[k] %= MOD
            rank += 1
        dim = self.m - rank
        sol = [0] * self.m
        for i in range(rank):
            sol[p[i]] = aug[i][-1]
        vecs = [[0] * self.m for _ in range(dim)]
        for i in range(dim):
            vecs[i][q[i]] = 1
        for i in range(dim):
            vecs_i = vecs[i]
            for j in range(rank):
                vecs_i[p[j]] = -aug[j][q[i]] % MOD
        return dim, sol, vecs

    def __getitem__(self, key):
        if not isinstance(key, slice):
            assert key >= 0
        return self.mat[key]

    def id(n):
        res = Matrix(n, n)
        for i in range(n):
            res[i][i] = 1
        return res

    def __len__(self):
        return len(self.mat)

    def __str__(self):
        return '\n'.join(' '.join(map(str, self[i])) for i in range(self.n))

    def times(self, k):
        res = [[0] * self.m for _ in range(self.n)]
        for i in range(self.n):
            res_i, self_i = res[i], self[i]
            for j in range(self.m):
                res_i[j] = k * self_i[j] % MOD
        return Matrix(self.n, self.m, res)

    def __pos__(self):
        return self

    def __neg__(self):
        return self.times(-1)

    def __add__(self, other):
        # assert self.n == other.n and self.m == other.m
        res = [[0] * self.m for _ in range(self.n)]
        for i in range(self.n):
            res_i, self_i, other_i = res[i], self[i], other[i]
            for j in range(self.m):
                res_i[j] = (self_i[j] + other_i[j]) % MOD
        return Matrix(self.n, self.m, res)

    def __sub__(self, other):
        # assert self.n == other.n and self.m == other.m
        res = [[0] * self.m for _ in range(self.n)]
        for i in range(self.n):
            res_i, self_i, other_i = res[i], self[i], other[i]
            for j in range(self.m):
                res_i[j] = (self_i[j] - other_i[j]) % MOD
        return Matrix(self.n, self.m, res)

    def __mul__(self, other):
        if other.__class__ == Matrix:
            # assert self.m == other.n
            res = [[0] * other.m for _ in range(self.n)]
            for i in range(self.n):
                res_i, self_i = res[i], self[i]
                for k in range(self.m):
                    self_ik, other_k = self_i[k], other[k]
                    for j in range(other.m):
                        res_i[j] += self_ik * other_k[j]
                        res_i[j] %= MOD
            return Matrix(self.n, other.m, res)
        else:
            return self.times(other)

    def __rmul__(self, other):
        return self.times(other)

    def __pow__(self, k):
        # assert self.is_square()
        tmp = Matrix(self.n, self.n, self.mat)
        res = Matrix.id(self.n)
        i = abs(k)
        while i:
            if i & 1:
                res *= tmp
            tmp *= tmp
            i >>= 1
        if k < 0:
            res = res.inverse()
        return res


def transpose(x):
    return [y for y in zip(*x)]


def rotate(x):
    return [y[::-1] for y in zip(*x)]


def mat_pow(a, n: int, mod=998244353):
    y = [[0] * len(a) for _ in range(len(a))]
    for i in range(len(a)):
        y[i][i] = 1
    while n > 0:
        if n & 1:
            y = mat_mul(a, y, mod)
        a = mat_mul(a, a, mod)
        n >>= 1
    return y


def mat_mul(a, b, mod=998244353):
    c = [[0] * len(b[0]) for _ in range(len(a))]
    if mod is None:
        for i in range(len(a)):
            for j in range(len(b[0])):
                for k in range(len(b)):
                    c[i][j] += a[i][k] * b[k][j]
    else:
        for i in range(len(a)):
            for j in range(len(b[0])):
                for k in range(len(b)):
                    c[i][j] += a[i][k] * b[k][j]
                c[i][j] %= mod
    return c


def mat_add(a, b, mod=998244353):
    # assert len(a) == len(b)
    # assert len(a[0]) == len(b[0])
    c = [[None] * len(a[0]) for _ in range(len(a))]
    if mod is None:
        for i in range(len(a)):
            for j in range(len(a[0])):
                c[i][j] = a[i][j] + b[i][j]
    else:
        for i in range(len(a)):
            for j in range(len(a[0])):
                c[i][j] = a[i][j] + b[i][j]
                c[i][j] = a[i][j] + b[i][j] % mod
    return c


def mat_xor_pow(a, n: int):
    y = [[0] * len(a) for _ in range(len(a))]
    for i in range(len(a)):
        y[i][i] = -1
    while n > 0:
        if n & 1:
            y = mat_xor_mul(a, y)
        a = mat_xor_mul(a, a)
        n >>= 1
    return y


def mat_xor_mul(a, b):
    c = [[0] * len(b[0]) for _ in range(len(a))]
    for i in range(len(a)):
        for j in range(len(b[0])):
            for k in range(len(b)):
                c[i][j] ^= a[i][k] & b[k][j]
    return c


def matIndex(x, s):
    for index, i in enumerate(x):
        if s in i:
            return i.index(s), index
    return None


def determinant(A, replace=False, mod=998244353):
    if not replace:
        from copy import deepcopy
        A = deepcopy(A)
    n = len(A)
    res = 1
    for i, a_i in enumerate(A):
        if a_i[i] == 0:
            for j in range(i + 1, n):
                if A[j][i]:
                    break
            else:
                return 0
            A[i], A[j] = A[j], A[i]
            a_i = A[i]
            res = -res
        inv = pow(a_i[i], mod - 2, mod)
        for j in range(i + 1, n):
            a_j = A[j]
            t = a_j[i] * inv % mod
            for k in range(i + 1, n):
                a_j[k] -= t * a_i[k]
                a_j[k] %= mod
    for i in range(n):
        res *= A[i][i]
        res %= mod
    return res


class BitMatrix:
    __slots__ = ['n', 'mat', 'mask', 'u', 'v']

    def __init__(self, n):
        self.n = n
        self.mat = 0
        self.u = u = 2 ** n - 1
        self.v = ((u + 1) ** n - 1) // u

    def copy(self, other):
        # assert self.n == other.n
        self.mat = other.mat
        return self

    def set(self, i, j, b):
        bit = 1 << (i * self.n + j)
        if b:
            self.mat |= bit
        else:
            self.mat &= ~bit
        return self

    def setZ(self):
        self.mat = 0
        return self

    def setI(self):
        n = self.n
        self.mat = (2 ** ((n + 1) * n) - 1) / (2 ** (n + 1) - 1)
        return self

    def get(self, i, j):
        return (self.mat >> (i * self.n + j)) & 1

    def __add__(self, other):
        res = BitMatrix(self.n)
        res.mat = self.mat ^ other.mat
        return res

    def __iadd__(self, other):
        self.mat ^= other.mat
        return self

    def __mul__(self, other):
        n = self.n
        u = self.u
        v = self.v
        res = BitMatrix(n)
        c = 0
        a = self.mat
        b = other.mat
        while a and b:
            c ^= ((a & v) * u) & ((b & u) * v)
            a >>= 1
            b >>= n
        res.mat = c
        return res

    def __imul__(self, other):
        n = self.n
        u = self.u
        v = self.v
        c = 0
        a = self.mat
        b = other.mat
        while a and b:
            c ^= ((a & v) * u) & ((b & u) * v)
            a >>= 1
            b >>= n
        self.mat = c
        return self

    def __pow__(self, k):
        res = BitMatrix(self.n).setI()
        A = BitMatrix(self.n).copy(self)
        while k:
            if k & 1:
                res *= A
            A *= A
            k >>= 1
        return res

    def __ipow__(self, k):
        if k == 0:
            return self.setI()
        A = BitMatrix(self.n).copy(self)
        k -= 1
        while k:
            if k & 1:
                self *= A
            A *= A
            k >>= 1
        return self
