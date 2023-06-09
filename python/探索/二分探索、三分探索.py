def binary_search():
    # 二分探索(答えが整数)
    def check(mid) -> bool:
        # 実装
        return True

    ok, ng = 0, 10**20
    while abs(ok - ng) > 1:
        mid = (ok + ng) >> 1
        if check(mid):
            ok = mid
        else:
            ng = mid
    print(ok)


def float_binary_search():
    # 二分探索(答えがfloat)
    def check(mid) -> bool:
        # 実装
        return True

    delta = 10**-13
    ok, ng = 0, 10**10
    count = 0
    while abs(ok - ng) > delta and count < 1000:
        # mid = (ok * ng) ** 0.5  # 相対誤差
        mid = (ok + ng) / 2
        if check(mid):
            ok = mid
        else:
            ng = mid
        count += 1
    print(f"{ok:.15f}")


def bisection_method_scipy():
    # 二分法(float:scipy)
    from scipy.optimize import bisect
    import numpy as np

    def f(x):
        return x  # 関数

    p, q = map(int, input().split())
    print(bisect(f, 1, 10**15))  # 関数, 下限, 上限


def ternary_search():
    # 三分探索
    def f(x) -> float:
        return x + p / 2 ** (x / 1.5)

    p = float(input())
    left, right = 0, 100
    delta = 10**-13
    count = 0
    while right - left > delta and count < 1000:
        midl = (left * 2 + right) / 3
        midr = (left + right * 2) / 3
        if f(midl) < f(midr):  # 上に凸なら逆
            right = midr
        else:
            left = midl
    mid = (left + right) / 2
    print(f(mid))


def golden_section_search():
    # 黄金分割探索
    def f(mid) -> float:
        # 実装
        return mid

    delta = 10**-13
    gamma = (-1 + 5**0.5) / 2
    left, right = 0, 100
    diff = right - left
    midl = left + diff * (1 - gamma)
    midr = left + diff * gamma
    while right - left > delta:
        if f(midl) < f(midr):
            right = midr
            midr = midl
            midl = left + (right - left) * (1 - gamma)
        else:
            left = midl
            midl = midr
            midr = left + (right - left) * gamma
    print(f(left))


def fibonacci_search():
    memo = {}

    def f(x):
        if x not in memo:
            memo[x] = x
        return memo[x]

    def fibl(n):
        l = [1]
        i = j = 1
        while j < n:
            l.append(i + j)
            i = j
            j = l[-1]
        return l

    left, right = 0, 10**18
    fib = fibl(right - left)
    length = fib.pop()
    if fib:
        i = fib.pop()
        midl = length - i
        midr = left + i
    for i in reversed(fib):
        if f(midl) < f(midr):
            right = midr
            midr = midl
            midl = right - i
        else:
            left = midl
            midl = midr
            midr = left + i
    print(min(f(left), f(midl), f(midr), f(right)))
