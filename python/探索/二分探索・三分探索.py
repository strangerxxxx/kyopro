def binary_search():
    # 二分探索(答えが整数)
    def f(mid) -> bool:
        # 実装
        return True
    ok, ng = 0, 10 ** 20
    while abs(ok - ng) > 1:
        mid = (ok + ng) // 2
        if f(mid):
            ok = mid
        else:
            ng = mid
    print(ok)


def float_binary_search():
    # 二分探索(答えがfloat)
    def f(mid) -> bool:
        # 実装
        return True
    delta = 10 ** -13
    ok, ng = 0, 10 ** 10
    while abs(ok - ng) > delta:
        # mid = (ok * ng) ** 0.5  # 相対誤差
        mid = (ok + ng) / 2
        if f(mid):
            ok = mid
        else:
            ng = mid
    print(ok)


def bisection_method_scipy():
    # 二分法(float:scipy)
    from scipy.optimize import bisect
    import numpy as np

    def f(x):
        return x  # 関数
    p, q = map(int, input().split())
    print(bisect(f, 1, 10 ** 15))  # 関数, 下限, 上限


def ternary_search():
    # 三分探索
    def f(x) -> float:
        return x + p / 2 ** (x / 1.5)
    p = float(input())
    left, right = 0, 100
    delta = 10 ** -13
    while right - left > delta:
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
    delta = 10 ** -13
    gamma = (-1 + 5 ** 0.5) / 2
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
    def fibl(n):
        l = [1]
        i = j = 1
        while j < n:
            l.append(i + j)
            i = j
            j = l[-1]
        return l

    def f(mid) -> int:
        if memo[mid] is None:
            # 実装
            memo[mid] = mid
        return memo[mid]
    n = int(input())
    fib = fibl(n)
    length = fib.pop()
    memo = [None] * n
    lmergin = (length - n) // 2
    memo = [-lmergin + x for x in range(lmergin)] + \
        memo + [-i - 1 for i in range(length - n - lmergin)
                ]  # 左右にfiller、適宜値を大きくする
    left, right = 0, length
    if fib:
        i = fib.pop()
        midl = length - i
        midr = left + i
    for i in reversed(fib):
        if f(midl) > f(midr):
            right = midr
            midr = midl
            midl = right - i
        else:
            left = midl
            midl = midr
            midr = left + i
    f(left)
    ans = max(x for x in memo if not x is None)
    print(ans)
