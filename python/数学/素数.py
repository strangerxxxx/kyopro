def faster_eratosthenes(n: int):
    """
    ref : https://qiita.com/peria/items/54499b9ce9d5c1e93e5a
    verify: https://judge.yosupo.jp/submission/110267
    """
    if n < 30:
        return [x for x in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] if x <= n]
    remains = [1, 7, 11, 13, 17, 19, 23, 29]
    inv_remains = {x: i for i, x in enumerate(remains)}
    msk = 255  # (1 << 8) - 1
    div30 = [i * j // 30 for j in remains for i in remains]
    mod30 = [inv_remains[i * j % 30] for j in remains for i in remains]
    shift = [1 << i for i in range(8)]
    msk8 = [msk - shift[i] for i in range(8)]
    inv_shift = {shift[i]: i for i in range(8)}
    res = [2, 3, 5]
    max_k = n // 30
    import bisect

    max_m = bisect.bisect_right(remains, n % 30) - 1
    sqrtn = int(n**0.5) + 1
    max_sqrt_k = sqrtn // 30
    max_sqrt_m = bisect.bisect_right(remains, sqrtn % 30) - 1
    table = bytearray([msk] * (max_k + 1))
    table[max_k] = (1 << (max_m + 1)) - 1
    table[0] -= 1  # remove 1
    for k in range(max_sqrt_k + 1):
        for m in range(8):
            if k == max_sqrt_k and m > max_sqrt_m:
                break
            if table[k] & shift[m]:
                # k_before = k
                m_before = m
                i = k * (30 * k + 2 * remains[m]) + div30[(m << 3) + m]
                j = mod30[(m << 3) + m]
                while i < max_k or (i == max_k and j <= max_m):
                    table[i] &= msk8[j]
                    if m_before == 7:
                        i += 2 * k + remains[m] + div30[m << 3] - div30[(m << 3) + 7]
                        j = mod30[m << 3]
                        # k_before += 1
                        m_before = 0
                    else:
                        i += (
                            k * (remains[m_before + 1] - remains[m_before])
                            + div30[(m << 3) + m_before + 1]
                            - div30[(m << 3) + m_before]
                        )
                        j = mod30[(m << 3) + m_before + 1]
                        m_before += 1
    i30 = 0
    for i in table:
        while i:
            j = inv_shift[i & (-i)]
            res.append(i30 + remains[j])
            i &= i - 1
        i30 += 30
    return res


def faster_eratosthenes_gen(n: int):
    """
    ref : https://qiita.com/peria/items/54499b9ce9d5c1e93e5a
    """
    if n < 30:
        for i in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]:
            if i > n:
                break
            yield i
        return
    remains = [1, 7, 11, 13, 17, 19, 23, 29]
    inv_remains = {x: i for i, x in enumerate(remains)}
    msk = 255  # (1 << 8) - 1
    div30 = [i * j // 30 for j in remains for i in remains]
    mod30 = [inv_remains[i * j % 30] for j in remains for i in remains]
    shift = [1 << i for i in range(8)]
    msk8 = [msk - shift[i] for i in range(8)]
    inv_shift = {shift[i]: i for i in range(8)}
    for i in (2, 3, 5):
        yield i
    max_k = n // 30
    import bisect

    max_m = bisect.bisect_right(remains, n % 30) - 1
    sqrtn = int(n**0.5) + 1
    max_sqrt_k = sqrtn // 30
    max_sqrt_m = bisect.bisect_right(remains, sqrtn % 30) - 1
    table = bytearray([msk] * (max_k + 1))
    table[max_k] = (1 << (max_m + 1)) - 1
    table[0] -= 1  # remove 1
    for k in range(max_sqrt_k + 1):
        for m in range(8):
            if k == max_sqrt_k and m > max_sqrt_m:
                break
            if table[k] & shift[m]:
                # k_before = k
                m_before = m
                i = k * (30 * k + 2 * remains[m]) + div30[(m << 3) + m]
                j = mod30[(m << 3) + m]
                while i < max_k or (i == max_k and j <= max_m):
                    table[i] &= msk8[j]
                    if m_before == 7:
                        i += 2 * k + remains[m] + div30[m << 3] - div30[(m << 3) + 7]
                        j = mod30[m << 3]
                        # k_before += 1
                        m_before = 0
                    else:
                        i += (
                            k * (remains[m_before + 1] - remains[m_before])
                            + div30[(m << 3) + m_before + 1]
                            - div30[(m << 3) + m_before]
                        )
                        j = mod30[(m << 3) + m_before + 1]
                        m_before += 1
    i30 = 0
    for i in table:
        while i:
            j = inv_shift[i & (-i)]
            yield i30 + remains[j]
            i &= i - 1
        i30 += 30


def eratosthenes(limit: int, minLimit: int = 0) -> list:
    """
    [minLimit, limit]の素数リスト
    """
    # assert 0 <= minLimit <= limit:
    isPrime = [True] * max(limit + 1, 2)
    isPrime[0] = False
    isPrime[1] = False
    res = []
    for p in range(2, limit + 1):
        if not isPrime[p]:
            continue
        if p >= minLimit:
            res.append(p)
        for i in range(p * p, limit + 1, p):
            isPrime[i] = False
    return res


def eratosthenes_gen(limit: int, minLimit: int = 0):
    """
    [minLimit, limit]の素数ジェネレーター
    """
    # assert 0 <= minLimit <= limit:
    isPrime = [True] * max(limit + 1, 2)
    isPrime[0] = False
    isPrime[1] = False
    for p in range(2, limit + 1):
        if not isPrime[p]:
            continue
        if p >= minLimit:
            yield p
        for i in range(p * p, limit + 1, p):
            isPrime[i] = False


def isPrime(n: int) -> bool:
    # nの素数判定
    if n < 2:
        return False
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True


def count_primes(n):
    if n < 2:
        return 0
    v = int(n**0.5) + 1
    smalls = [i // 2 for i in range(1, v + 1)]
    smalls[1] = 0
    s = v // 2
    roughs = [2 * i + 1 for i in range(s)]
    larges = [(n // (2 * i + 1) + 1) // 2 for i in range(s)]
    skip = [False] * v

    pc = 0
    for p in range(3, v):
        if smalls[p] <= smalls[p - 1]:
            continue

        q = p * p
        pc += 1
        if q * q > n:
            break
        skip[p] = True
        for i in range(q, v, 2 * p):
            skip[i] = True

        ns = 0
        for k in range(s):
            i = roughs[k]
            if skip[i]:
                continue
            d = i * p
            larges[ns] = (
                larges[k] - (larges[smalls[d] - pc] if d < v else smalls[n // d]) + pc
            )
            roughs[ns] = i
            ns += 1
        s = ns
        for j in range((v - 1) // p, p - 1, -1):
            c = smalls[j] - pc
            e = min((j + 1) * p, v)
            for i in range(j * p, e):
                smalls[i] -= c

    for k in range(1, s):
        m = n // roughs[k]
        s = larges[k] - (pc + k - 1)
        for l in range(1, k):
            p = roughs[l]
            if p * p > m:
                break
            s -= smalls[m // p] - (pc + l - 1)
        larges[0] -= s

    return larges[0]


def miller_rabin(n: int, k: int = 100) -> bool:
    # ミラー–ラビン素数判定法
    import random

    if n == 2:
        return True
    if n == 1 or n & 1 == 0:
        return False
    d = (n - 1) >> 1
    while d & 1 == 0:
        d >>= 1
    for _ in range(k):
        a = random.randint(1, n - 1)
        t = d
        y = pow(a, t, n)
        while t != n - 1 and y != 1 and y != n - 1:
            y = (y * y) % n
            t <<= 1
        if y != n - 1 and t & 1 == 0:
            return False
    return True
