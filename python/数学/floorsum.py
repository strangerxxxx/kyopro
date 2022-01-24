def floor_sum(n: int, m: int, a: int, b: int) -> int:
    '''
    Σ[0, n-1]((a*i+b)//m)
    ref: https://qiita.com/R_olldIce/items/3e2c80baa6d5e6f3abe9
    '''
    res = 0
    while True:
        if not 0 <= a < m:
            res += n * (n - 1) * (a // m) // 2
            a %= m
        if not 0 <= b < m:
            res += n * (b // m)
            b %= m
        y_max = a * n + b
        if y_max < m:
            break
        n, b = divmod(y_max, m)
        m, a = a, m
    return res
