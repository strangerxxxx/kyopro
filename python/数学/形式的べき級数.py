class FPS:
    # source: https://github.com/shakayami/ACL-for-python/blob/master/fps.py
    # wiki  : https://github.com/shakayami/ACL-for-python/wiki/fps
    mod = 998244353
    Func = [0]
    sum_e = (
        911660635,
        509520358,
        369330050,
        332049552,
        983190778,
        123842337,
        238493703,
        975955924,
        603855026,
        856644456,
        131300601,
        842657263,
        730768835,
        942482514,
        806263778,
        151565301,
        510815449,
        503497456,
        743006876,
        741047443,
        56250497,
    )
    sum_ie = (
        86583718,
        372528824,
        373294451,
        645684063,
        112220581,
        692852209,
        155456985,
        797128860,
        90816748,
        860285882,
        927414960,
        354738543,
        109331171,
        293255632,
        535113200,
        308540755,
        121186627,
        608385704,
        438932459,
        359477183,
        824071951,
    )

    def __init__(self, L):
        self.Func = [x % self.mod for x in L]

    def butterfly(self, a):
        n = len(a)
        h = (n - 1).bit_length()
        for ph in range(1, h + 1):
            w = 1 << (ph - 1)
            p = 1 << (h - ph)
            now = 1
            for s in range(w):
                offset = s << (h - ph + 1)
                for i in range(p):
                    l = a[i + offset]
                    r = a[i + offset + p] * now % self.mod
                    a[i + offset] = (l + r) % self.mod
                    a[i + offset + p] = (l - r) % self.mod
                now *= self.sum_e[(~s & -~s).bit_length() - 1]
                now %= self.mod
        return a

    def butterfly_inv(self, a):
        n = len(a)
        h = (n - 1).bit_length()
        for ph in range(h, 0, -1):
            w = 1 << (ph - 1)
            p = 1 << (h - ph)
            inow = 1
            for s in range(w):
                offset = s << (h - ph + 1)
                for i in range(p):
                    l = a[i + offset]
                    r = a[i + offset + p]
                    a[i + offset] = (l + r) % self.mod
                    a[i + offset + p] = (l - r) * inow % self.mod
                inow *= self.sum_ie[(~s & -~s).bit_length() - 1]
                inow %= self.mod
        return a

    def __mul__(self, other):
        if isinstance(other, int):
            return FPS([x * other for x in self.Func])
        a = self.Func[:]
        b = other.Func[:]
        n = len(a)
        m = len(b)
        if not n or not m:
            return FPS([])
        if min(n, m) <= 40:
            if n < m:
                n, m = m, n
                a, b = b, a
            res = [0] * (n + m - 1)
            for i in range(n):
                for j in range(m):
                    res[i + j] += a[i] * b[j]
                    res[i + j] %= self.mod
            return FPS(res)
        z = 1 << ((n + m - 2).bit_length())
        a += [0] * (z - n)
        b += [0] * (z - m)
        self.butterfly(a)
        self.butterfly(b)
        c = [i * j % self.mod for i, j in zip(a, b)]
        self.butterfly_inv(c)
        iz = pow(z, self.mod - 2, self.mod)
        return FPS([c[i] * iz for i in range(n + m - 1)])

    def __imul__(self, other):
        self = self * other
        return self

    def __add__(self, other):
        from itertools import zip_longest

        return FPS([x + y for x, y in zip_longest(self.Func, other.Func, fillvalue=0)])

    def __iadd__(self, other):
        self = self + other
        return self

    def __sub__(self, other):
        from itertools import zip_longest

        return FPS([x - y for x, y in zip_longest(self.Func, other.Func, fillvalue=0)])

    def __isub__(self, other):
        self = self - other
        return self

    def inv(self, size=None):
        n = len(self.Func)
        # assert n != 0 and self.Func[0] != 0
        if size is None:
            size = n
        # assert size > 0
        res = [pow(self.Func[0], self.mod - 2, self.mod)]
        while len(res) < size:
            m = len(res)
            f = [self.Func[i] for i in range(min(n, 2 * m))]
            r = res[:]

            if len(f) < 2 * m:
                f += [0] * (2 * m - len(f))
            elif len(f) > 2 * m:
                f = f[: 2 * m]
            if len(r) < 2 * m:
                r += [0] * (2 * m - len(r))
            elif len(r) > 2 * m:
                r = r[: 2 * m]
            f = self.butterfly(f)
            r = self.butterfly(r)
            for i in range(2 * m):
                f[i] *= r[i]
                f[i] %= self.mod
            f = self.butterfly_inv(f)
            f = f[m:]
            if len(f) < 2 * m:
                f += [0] * (2 * m - len(f))
            elif len(f) > 2 * m:
                f = f[: 2 * m]
            f = self.butterfly(f)
            for i in range(2 * m):
                f[i] *= r[i]
                f[i] %= self.mod
            f = self.butterfly_inv(f)
            iz = pow(2 * m, self.mod - 2, self.mod)
            iz *= -iz
            iz %= self.mod
            res += [x * iz % self.mod for x in f[:m]]
        return FPS(res[:size])

    def __truediv__(self, other):
        if isinstance(other, int):
            invother = pow(other, self.mod - 2, self.mod)
            return FPS([x * invother for x in self.Func])
        # assert other.Func[0] != 0
        return self * (other.inv())

    def __itruediv__(self, other):
        self = self / other
        return self

    def __lshift__(self, d):
        n = len(self.Func)
        return FPS([0] * min(d, n) + self.Func[:-d])

    def __ilshift__(self, d):
        n = len(self.Func)
        self.Func = [0] * min(d, n) + self.Func[:-d]
        return self

    def __rshift__(self, d):
        return FPS(self.Func[d:])

    def __irshift__(self, d):
        del self.Func[:d]
        return self

    def __str__(self):
        return f"FPS({self.Func})"

    def diff(self):
        return FPS([(j * i) % self.mod for i, j in enumerate(self.Func[1:], start=1)])

    def integral(self):
        return FPS(
            [0]
            + [
                j * pow(i + 1, self.mod - 2, self.mod) % self.mod
                for i, j in enumerate(self.Func)
            ]
        )

    def log(self, deg=-1):
        # assert self.Func[0] == 1
        n = len(self.Func)
        if deg == -1:
            deg = n
        return (self.diff() * self.inv()).integral()

    def mod_sqrt(self, a):
        p = self.mod
        # assert 0 <= a and a < p
        if a < 2:
            return a
        if pow(a, (p - 1) // 2, p) != 1:
            return -1
        b = 1
        one = 1
        while pow(b, (p - 1) >> 1, p) == 1:
            b += one
        m = p - 1
        e = 0
        while m % 2 == 0:
            m >>= 1
            e += 1
        x = pow(a, (m - 1) >> 1, p)
        y = (a * x * x) % p
        x *= a
        x %= p
        z = pow(b, m, p)
        while y != 1:
            j = 0
            t = y
            while t != one:
                j += 1
                t *= t
                t %= p
            z = pow(z, 1 << (e - j - 1), p)
            x *= z
            x %= p
            z *= z
            z %= p
            y *= z
            y %= p
            e = j
        return x

    def sqrt(self, deg=-1):
        n = len(self.Func)
        if deg == -1:
            deg = n
        if n == 0:
            return FPS([0 for i in range(deg)])
        if self.Func[0] == 0:
            for i in range(1, n):
                if self.Func[i] != 0:
                    if i & 1:
                        return FPS([])
                    if deg - i // 2 <= 0:
                        break
                    ret = (self >> i).sqrt(deg - i // 2)
                    if len(ret.Func) == 0:
                        return FPS([])
                    ret = ret << (i // 2)
                    if len(ret.Func) < deg:
                        ret.Func += [0] * (deg - len(ret.Func))
                    return ret
            return FPS([0] * deg)
        sqr = self.mod_sqrt(self.Func[0])
        if sqr == -1:
            return FPS([])
        # assert sqr * sqr % self.mod == self.Func[0]
        ret = FPS([sqr])
        inv2 = (self.mod + 1) // 2
        i = 1
        while i < deg:
            ret = (ret + FPS(self.Func[: i << 1]) * ret.inv(i << 1)) * inv2
            i <<= 1
        return FPS(ret.Func[:deg])

    def resize(self, size=None):
        if size is None or len(self.Func) == size:
            return self
        if len(self.Func) < size:
            self.Func += [0] * (size - len(self.Func))
        else:
            del self.Func[size:]
        return self

    def exp(self, deg=-1):
        n = len(self.Func)
        # assert n > 0 and self.Func[0] == 0
        if deg == -1:
            deg = n
        # assert deg >= 0
        g = [1]
        g_fft = [1, 1]
        self.Func[0] = 1
        self.resize(deg)
        h_drv = self.diff()
        m = 2
        while m < deg:
            f_fft = self.Func[:m] + [0] * m
            self.butterfly(f_fft)

            # step 2.a
            _g = [f_fft[i] * g_fft[i] % self.mod for i in range(m)]
            self.butterfly_inv(_g)
            _g = _g[m // 2 : m] + [0] * (m // 2)
            self.butterfly(_g)
            for i in range(m):
                _g[i] *= g_fft[i]
                _g[i] %= self.mod
            self.butterfly_inv(_g)
            tmp = pow(-m * m, self.mod - 2, self.mod)
            for i in range(m):
                _g[i] *= tmp
                _g[i] %= self.mod
            g += _g[: m // 2]
            # step 2.b--2.d
            t = FPS(self.Func[:m]).diff()
            r = h_drv.Func[: m - 1] + [0]
            self.butterfly(r)
            for i in range(m):
                r[i] *= f_fft[i]
                r[i] %= self.mod
            self.butterfly_inv(r)
            tmp = pow(-m, self.mod - 2, self.mod)
            for i in range(m):
                r[i] *= tmp
                r[i] %= self.mod
            t = (t + FPS(r)).Func
            t = [t[-1]] + t
            t.pop()
            # step 2.e
            if 2 * m < deg:
                if len(t) < 2 * m:
                    t += [0] * (2 * m - len(t))
                elif len(t) > 2 * m:
                    t = t[: 2 * m]
                self.butterfly(t)
                g_fft = g[:]
                if len(g_fft) < 2 * m:
                    g_fft += [0] * (2 * m - len(g_fft))
                elif len(g_fft) > 2 * m:
                    g_fft = g_fft[: 2 * m]
                self.butterfly(g_fft)
                for i in range(2 * m):
                    t[i] *= g_fft[i]
                    t[i] %= self.mod
                self.butterfly_inv(t)
                tmp = pow(2 * m, self.mod - 2, self.mod)
                t = t[:m]
                for i in range(m):
                    t[i] *= tmp
                    t[i] %= self.mod
            else:
                g1 = g[m // 2 :]
                s1 = t[m // 2 :]
                t = t[: m // 2]
                g1 += [0] * (m - len(g1))
                s1 += [0] * (m - len(s1))
                t += [0] * (m - len(t))

                self.butterfly(g1)
                self.butterfly(t)
                self.butterfly(s1)
                for i in range(m):
                    s1[i] = (g_fft[i] * s1[i] + g1[i] * t[i]) % self.mod
                for i in range(m):
                    t[i] *= g_fft[i]
                    t[i] %= self.mod
                self.butterfly_inv(t)
                self.butterfly_inv(s1)
                for i in range(m // 2):
                    t[i + m // 2] += s1[i]
                    t[i + m // 2] %= self.mod
                tmp = pow(m, self.mod - 2, self.mod)
                for i in range(m):
                    t[i] *= tmp
                    t[i] %= self.mod
            # step 2.f
            v = self.Func[m : min(deg, 2 * m)] + [0] * (2 * m - min(deg, 2 * m))
            t = [0] * (m - 1) + t
            t = FPS(t).integral().Func
            for i in range(m):
                v[i] -= t[m + i]
                v[i] %= self.mod
            # step 2.g
            if len(v) < 2 * m:
                v += [0] * (2 * m - len(v))
            else:
                v = v[: 2 * m]
            self.butterfly(v)
            for i in range(2 * m):
                v[i] *= f_fft[i]
                v[i] %= self.mod
            self.butterfly_inv(v)
            v = v[:m]
            tmp = pow(2 * m, self.mod - 2, self.mod)
            for i in range(m):
                v[i] *= tmp
                v[i] %= self.mod
            # step 2.h
            for i in range(min(deg - m, m)):
                self.Func[m + i] = v[i]
            m *= 2
        return self

    def powfps(self, k):
        # サイズを保ったまま累乗
        res = self.Func[:]
        n = len(self.Func)
        if k == 0:
            return FPS([int(i == 0) for i in range(n)])
        l = 0
        while l < n and not res[l]:
            l += 1
        if l * k >= n:
            return FPS([0] * n)
        ic = pow(res[l], self.mod - 2, self.mod)
        pc = pow(res[l], k, self.mod)
        res = FPS([res[i] * ic for i in range(l, len(res))]).log()
        res *= k
        res = res.exp()
        res *= pc
        res = [0] * (l * k) + res.Func[: n - l * k]
        return FPS(res)

    def pow(self, k):
        if k == -1:
            return self.inv()
        res = FPS(self.Func).resize((len(self) - 1) * k + 1)
        return res.powfps(k)

    __pow__ = pow

    def __iter__(self):
        yield from self.Func

    def __len__(self):
        return len(self.Func)

    def __getitem__(self, key):
        return self.Func[key]

    def __setitem__(self, key, value):
        self.Func[key] = value


def bostan_mori(p, q, n, mod=998244353):
    # [x^n] P(x)/Q(x)
    while n:
        qi = FPS([mod - x if i & 1 and x else x for i, x in enumerate(q)])
        u = p * qi
        v = q * qi
        q = FPS(v[::2])
        p = FPS(u[n & 1 :: 2])
        n >>= 1
    return p[0] * pow(q[0], mod - 2, mod) % mod


def kitamasa(m, k, c, a):
    p = FPS(a)
    q = FPS([1] + [998244353 - x for x in c])
    p = (p * q).resize(k)
    return bostan_mori(p, q, m)
