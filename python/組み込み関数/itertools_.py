def next_permutation(a: list, l: int = 0, r: int = None) -> bool:
    # a[l,r)の次の組み合わせ
    if r is None:
        r = len(a)
    for i in range(r - 2, l - 1, -1):
        if a[i] < a[i + 1]:
            for j in range(r - 1, i, -1):
                if a[i] < a[j]:
                    a[i], a[j] = a[j], a[i]
                    p, q = i + 1, r - 1
                    while p < q:
                        a[p], a[q] = a[q], a[p]
                        p += 1
                        q -= 1
                    return True
    return False


def distinct_permutations(a: list, l: int = 0, r: int = None):
    res = a[:]
    # res = sorted(a[:])
    while True:
        yield res
        if not next_permutation(res, l, r):
            break


def prev_permutation(a: list, l: int = 0, r: int = None) -> bool:
    # a[l,r)の前の組み合わせ
    if r is None:
        r = len(a)
    for i in range(r - 2, l - 1, -1):
        if a[i] > a[i + 1]:
            for j in range(r - 1, i, -1):
                if a[i] > a[j]:
                    a[i], a[j] = a[j], a[i]
                    p, q = i + 1, r - 1
                    while p < q:
                        a[p], a[q] = a[q], a[p]
                        p += 1
                        q -= 1
                    return True
    return False


def partitions(a):
    # グループ分けの方法
    q = [(0, [])]
    while q:
        l, res = q.pop()
        for i in range(l + 1, len(a)):
            q.append((i, res + [a[l:i]]))
        yield res + [a[l:]]


def bits(n: int):
    for j in range(1 << n):
        yield [j >> i & 1 for i in range(n)]


def powerset(a):
    # 冪集合
    n = len(a)
    for j in range(1 << n):
        yield [x for i, x in enumerate(a) if j >> i & 1]


def pairwise(n: int):
    # assert a % 2 == 0
    q = [([], (1 << n) - 1)]
    while q:
        a, b = q.pop()
        if b:
            first = (b & (-b)).bit_length() - 1
            b -= 1 << first
            second = 1
            c = b >> 1
            while c:
                if c & 1:
                    q.append((a + [[first, second]], b - (1 << second)))
                c >>= 1
                second += 1
        else:
            yield a


def sum_combination(n, s):
    # 合計がsとなるn個の非負整数の組み合わせ
    if n == 1:
        yield [s]
        return
    q = list(([x], x) for x in range(s + 1))
    while q:
        i, t = q.pop()
        if len(i) == n - 1:
            yield i + [s - t]
        else:
            for j in range(s - t + 1):
                q.append((i + [j], t + j))
