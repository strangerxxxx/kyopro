# https://maspypy.com/%e6%95%b0%e5%ad%a6%e3%83%bbnumpy-%e9%ab%98%e9%80%9f%e3%83%95%e3%83%bc%e3%83%aa%e3%82%a8%e5%a4%89%e6%8f%9bfft%e3%81%ab%e3%82%88%e3%82%8b%e7%95%b3%e3%81%bf%e8%be%bc%e3%81%bf
def ifft():
    # https://atcoder.jp/contests/atc001/tasks/fft_c
    '''
    i円のものがa[i]個とb[i]個ある食堂でのk(<=2n)円になる組み合わせ
    '''
    import numpy as np
    n = int(input())
    qrtn = 2 ** int(np.ceil(np.log2(2 * n - 1)))
    a = np.zeros(qrtn)
    b = np.zeros(qrtn)
    for i in range(n):
        a[i], b[i] = map(int, input().split())
    # a = np.array(input().split(), dtype=int)
    # b = np.array(input().split(), dtype=int)
    c = np.fft.ifft(np.fft.fft(a) * np.fft.fft(b))
    ans = ["0"] + ["{}".format(int(x)) for x in np.real(c[:2 * n - 1] + 0.5)]
    for i in ans:
        print(i)


def convolve():
    import numpy as np
    a = np.array(input().split(), dtype=int)
    b = np.array(input().split(), dtype=int)
    c = np.convolve(a, b)
    print(*c)


class FFT_MOD():
    def primitive_root_constexpr(self, m):
        if m == 2:
            return 1
        if m == 167772161:
            return 3
        if m == 469762049:
            return 3
        if m == 754974721:
            return 11
        if m == 998244353:
            return 3
        divs = [0] * 20
        divs[0] = 2
        cnt = 1
        x = (m - 1)//2
        while(x % 2 == 0):
            x //= 2
        i = 3
        while(i * i <= x):
            if (x % i == 0):
                divs[cnt] = i
                cnt += 1
                while(x % i == 0):
                    x //= i
            i += 2
        if x > 1:
            divs[cnt] = x
            cnt += 1
        g = 2
        while(1):
            ok = True
            for i in range(cnt):
                if pow(g, (m - 1)//divs[i], m) == 1:
                    ok = False
                    break
            if ok:
                return g
            g += 1

    def bsf(self, x):
        res = 0
        while(x % 2 == 0):
            res += 1
            x //= 2
        return res
    butterfly_first = True
    butterfly_inv_first = True
    sum_e = [0] * 30
    sum_ie = [0] * 30

    def __init__(self, MOD):
        self.mod = MOD
        self.g = self.primitive_root_constexpr(self.mod)

    def butterfly(self, a):
        n = len(a)
        h = (n - 1).bit_length()
        if self.butterfly_first:
            self.butterfly_first = False
            es = [0] * 30
            ies = [0] * 30
            cnt2 = self.bsf(self.mod - 1)
            e = pow(self.g, (self.mod - 1) >> cnt2, self.mod)
            ie = pow(e, self.mod - 2, self.mod)
            for i in range(cnt2, 1,  -1):
                es[i - 2] = e
                ies[i - 2] = ie
                e = (e * e) % self.mod
                ie = (ie * ie) % self.mod
            now = 1
            for i in range(cnt2 - 2):
                self.sum_e[i] = ((es[i] * now) % self.mod)
                now *= ies[i]
                now %= self.mod
        for ph in range(1, h + 1):
            w = 1 << (ph - 1)
            p = 1 << (h - ph)
            now = 1
            for s in range(w):
                offset = s << (h - ph + 1)
                for i in range(p):
                    l = a[i + offset]
                    r = a[i + offset + p] * now
                    r %= self.mod
                    a[i + offset] = l + r
                    a[i + offset] %= self.mod
                    a[i + offset + p] = l - r
                    a[i + offset + p] %= self.mod
                now *= self.sum_e[(~s & -~s).bit_length() - 1]
                now %= self.mod

    def butterfly_inv(self, a):
        n = len(a)
        h = (n - 1).bit_length()
        if self.butterfly_inv_first:
            self.butterfly_inv_first = False
            es = [0] * 30
            ies = [0] * 30
            cnt2 = self.bsf(self.mod - 1)
            e = pow(self.g, (self.mod - 1) >> cnt2, self.mod)
            ie = pow(e, self.mod - 2, self.mod)
            for i in range(cnt2, 1,  -1):
                es[i - 2] = e
                ies[i - 2] = ie
                e = (e * e) % self.mod
                ie = (ie * ie) % self.mod
            now = 1
            for i in range(cnt2 - 2):
                self.sum_ie[i] = ((ies[i] * now) % self.mod)
                now *= es[i]
                now %= self.mod
        for ph in range(h, 0,  -1):
            w = 1 << (ph - 1)
            p = 1 << (h - ph)
            inow = 1
            for s in range(w):
                offset = s << (h - ph + 1)
                for i in range(p):
                    l = a[i + offset]
                    r = a[i + offset + p]
                    a[i + offset] = l + r
                    a[i + offset] %= self.mod
                    a[i + offset + p] = (l - r) * inow
                    a[i + offset + p] %= self.mod
                inow *= self.sum_ie[(~s & -~s).bit_length() - 1]
                inow %= self.mod

    def convolution(self, a, b):
        n = len(a)
        m = len(b)
        if not(a) or not(b):
            return []
        if min(n, m) <= 40:
            if n < m:
                n, m = m, n
                a, b = b, a
            res = [0] * (n + m - 1)
            for i in range(n):
                for j in range(m):
                    res[i + j] += a[i] * b[j]
                    res[i + j] %= self.mod
            return res
        z = 1 << ((n + m - 2).bit_length())
        a = a + [0] * (z - n)
        b = b + [0] * (z - m)
        self.butterfly(a)
        self.butterfly(b)
        c = [0] * z
        for i in range(z):
            c[i] = (a[i] * b[i]) % self.mod
        self.butterfly_inv(c)
        iz = pow(z, self.mod - 2, self.mod)
        for i in range(n + m - 1):
            c[i] = (c[i] * iz) % self.mod
        return c[:n + m - 1]
