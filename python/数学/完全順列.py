def derangement(i: int, mod=None) -> int:
    # 完全順列 Ai!=iの順列(1,2,...,n)の総数
    res = 0
    for j in range(2, i + 1):
        res *= j
        if j % 2 == 0:
            res += 1
        else:
            res -= 1
        if mod:
            res %= mod
    return res
