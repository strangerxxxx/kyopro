def modinv(a: int, m: int = 10 ** 9 + 7) -> int:
    # 逆元 a^(-1) == a^(m-2) mod m
    return pow(a, m - 2, m)


def extgcd(a: int, b: int) -> int:
    # 拡張ユーグリッド互除法 gcd(a,b) と ax + by = gcd(a,b) の最小整数解
    if b:
        d, y, x = extgcd(b, a % b)
        y -= (a // b) * x
        return d, x, y
    else:
        return a, 1, 0


def euler_phi(n: int) -> int:
    # オイラー関数 φ(n) : 1,2,...,n-1のうちnと素であるものの数
    res = n
    for x in range(2, int(n ** 0.5) + 1):
        if n % x == 0:
            res = res // x * (x - 1)
            while n % x == 0:
                n //= x
    return res


def euler_list(n: int):
    # φ(x) for 0 <= x <= M
    *phi, = range(n + 1)
    for x in range(2, n + 1):
        if phi[x] == x:
            for y in range(x, n + 1, x):
                phi[y] = phi[y] // x * (x - 1)
    return phi
