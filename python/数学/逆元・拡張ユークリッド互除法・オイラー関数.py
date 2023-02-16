def modinv(a: int, p: int = 1, m: int = 998244353) -> int:
    # 逆元 a^(-p) == a^(m-1-p) mod m : mは素数
    return pow(a, m - 1 - p, m)


def make_modinv_list(n, mod=998244353):
    # 0からnまでのmod逆元のリスト
    modinv = [0] * (n + 1)
    modinv[1] = 1
    for i in range(2, n + 1):
        modinv[i] = mod - mod // i * modinv[mod % i] % mod
    return modinv


def extgcd(a: int, b: int) -> int:
    # 拡張ユークリッド互除法 ax + by = gcd(a,b) の最小整数解
    # return gcd(a,b),a,b
    if b:
        d, y, x = extgcd(b, a % b)
        y -= (a // b) * x
        return d, x, y
    else:
        return a, 1, 0


def inv(a, m=998244353):
    # aのmod m逆元 : mは非素数でも可
    d, res, _ = extgcd(a, m)
    if d != 1:
        return None
    return res


def inv2(a, m=998244353):
    # aのmod m逆元 : mは非素数でも可
    x, y = 1, 0
    while m != 0:
        x, y = y, x - (a // m) * y
        a, m = m, a % m
    return x


def inv_calc(a, b, m=998244353):
    # ax == b mod mとなる最小のx : mは非素数でも可
    import math
    import functools

    d = functools.reduce(math.gcd, (a, b, m))
    a, b, m = a // d, b // d, m // d
    x = inv(a, m)
    if x is None:
        return None
    return b * x % m


def euler_phi(n: int) -> int:
    # オイラー関数 φ(n) : 1,2,...,n-1のうちnと素であるものの数
    # a^φ(n)==1 mod n (aとnが素なとき)
    res = n
    for x in range(2, int(n**0.5) + 1):
        if n % x == 0:
            res = res // x * (x - 1)
            while n % x == 0:
                n //= x
    if n > 1:
        res = res // n * (n - 1)
    return res


def euler_list(n: int):
    # φ(x) for 0 <= x <= M
    (*phi,) = range(n + 1)
    for x in range(2, n + 1):
        if phi[x] == x:
            for y in range(x, n + 1, x):
                phi[y] = phi[y] // x * (x - 1)
    return phi
