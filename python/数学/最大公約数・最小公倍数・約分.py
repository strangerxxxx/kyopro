def gcd(a: int, b: int) -> int:
    # 最大公約数
    if a < b:
        return gcd(b, a)
    while b:
        a, b = b, a % b
    return a


def lcm(a: int, b: int) -> int:
    # 最小公倍数
    return b // gcd(a, b) * a


def gcdl(l) -> int:
    # リスト最大公約数
    from math import gcd
    from functools import reduce
    return reduce(lambda x, y: gcd(x, y), l)


def reduction(l):
    # 約分
    from math import gcd
    from functools import reduce
    r = reduce(lambda x, y: gcd(x, y), l)
    return [x // r for x in l]


def lcml(l) -> int:
    # リスト最小公倍数
    def lcm(a: int, b: int) -> int:
        from math import gcd
        return b // gcd(a, b) * a
    from functools import reduce
    return reduce(lambda x, y: lcm(x, y), l)


def PrimeFactorization(m: int):
    # リストの最小公倍数1/2
    pf = {}
    for i in range(2, int(m ** 0.5) + 1):
        while m % i == 0:
            pf[i] = pf.get(i, 0) + 1
            m //= i
    if m > 1:
        pf[m] = 1
    return pf


def lcmlist(l: list) -> int:
    # リストの最小公倍数2/2
    from collections import defaultdict
    d = defaultdict(int)
    for i in set(l):
        for k, v in PrimeFactorization(i).items():
            if v > d[k]:
                d[k] = v
    n = 1
    for k, v in d.items():
        n *= k ** v
    return n
