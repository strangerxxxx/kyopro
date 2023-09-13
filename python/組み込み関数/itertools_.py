def accumulate(a: list, start: int = None):
    if start is None:
        s = 0
    else:
        s = start
        yield s
    for i in a:
        s += i
        yield s


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


def pairwise(n: int, out=None):
    if n % 2:
        if out is None:
            for i in reversed(range(n)):
                yield from pairwise(n, i)
            return
        else:
            q = [([], (1 << n) - 1 - (1 << out))]
    else:
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


teams: list[list] = []


def divide_team(n: int, i: int = 0, max_len=-1) -> list[list]:
    # n人のチーム分けの仕方
    if i == n:
        yield teams
        return
    x = len(teams)
    for j in range(x + 1):
        if j == len(teams):
            if len(teams) == max_len:
                break
            teams.append([i])
            yield from divide_team(n, i + 1)
            teams.pop()
        else:
            teams[j].append(i)
            yield from divide_team(n, i + 1)
            teams[j].pop()


teams: list[list] = []


def divide_m_team(n: int, m: int, i: int = 0) -> list[list]:
    # n人のm人ずつのチーム分けの仕方
    assert n % m == 0
    if i == n:
        yield teams
        return
    x = len(teams)
    for j in range(x + 1):
        if j == len(teams):
            if len(teams) == n // m:
                break
            teams.append([i])
            yield from divide_m_team(n, m, i + 1)
            teams.pop()
        else:
            if len(teams[j]) < m:
                teams[j].append(i)
                yield from divide_m_team(n, m, i + 1)
                teams[j].pop()


def sum_combination(n: int, s: int):
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


def sum_cmb(s: int):
    # 合計がsとなる正整数の組み合わせ
    q = [([], s, float("inf"))]
    while q:
        a, remain, mx = q.pop()
        if mx >= remain:
            yield a + [remain]
        for i in range(1, min(mx + 1, remain)):
            q.append((a + [i], remain - i, i))


def brackets(a: list[str]):
    # 括弧の付け方の全通り
    import itertools

    ops = "+-*/"
    if len(a) == 1:
        yield a[0]
        return
    for i in range(1, len(a)):
        for b, c in itertools.product(brackets(a[:i]), brackets(a[i:])):
            for op in ops:
                bi = b
                ci = c
                # if op in "*/" and len(b) > 1:
                #     bi = "(" + b + ")"
                # if op in "*/" and len(c) > 1:
                #     ci = "(" + c + ")"
                yield bi + op + ci


def number_of_permutation(a, mod: int = None):
    # aの考えられる順列で、aが辞書順で何番目か
    n = len(a)
    perm_mod = [1]
    for i in range(1, n + 1):
        if mod is None:
            perm_mod.append((i * perm_mod[-1]))
        else:
            perm_mod.append((i * perm_mod[-1]) % mod)

    X = {s: i for i, s in enumerate(sorted(a), 1)}
    T = FenwickTree(n, seq=[1] * n)

    ans = 1
    for i, s in enumerate(a, 1):
        m = T.cumsum(X[s] - 1)
        # まだ使っていない自分未満の要素の数
        ans += m * perm_mod[n - i]
        T.add(X[s], -1)
    return ans


def k_th_permutation(a, k: int):
    # aを並び替えて考えられる順列で、k番目は何か
    n = len(a)
    perm = [1]
    for i in range(1, n + 1):
        perm.append((i * perm[-1]))

    if k <= 0 or k > perm[-1]:
        return None

    X = {i: s for i, s in enumerate(sorted(a), 1)}
    T = FenwickTree(n, seq=[1] * n)

    j = k - 1  # 0-indexedで考える
    ans = []
    for i in range(n - 1, -1, -1):
        idx = j // perm[i]
        p, _ = T.lower_bound(idx + 1)
        val = X[p]
        ans.append(val)
        j -= idx * perm[i]
        T.add(p, -1)
    return ans


class FenwickTree:
    def __init__(self, n, seq=None):
        self.size = n
        self.tree = [0] * (n + 1)
        self.depth = n.bit_length()
        if seq:
            for i, s in enumerate(seq, 1):
                self.add(i, s)

    def cumsum(self, i):
        s = 0
        while i > 0:
            s += self.tree[i]
            i -= i & -i
        return s

    def rangesum(self, lft, rgt):
        return self.cumsum(rgt) - self.cumsum(lft - 1)

    def add(self, i, x):
        while i <= self.size:
            self.tree[i] += x
            i += i & -i

    def lower_bound(self, x):
        if x <= 0:
            return 0, 0
        s = 0
        pos = 0
        for i in range(self.depth, -1, -1):
            k = pos + (1 << i)
            if k <= self.size and s + self.tree[k] < x:
                s += self.tree[k]
                pos += 1 << i
        return pos + 1, s
