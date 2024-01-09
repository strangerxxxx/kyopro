def mos_algorithm():
    # https://atcoder.jp/contests/abc174/tasks/abc174_f
    def add(i):
        nonlocal nowans
        d[i] += 1
        if d[i] == 1:
            nowans += 1

    def rem(i):
        nonlocal nowans
        d[i] -= 1
        if d[i] == 0:
            nowans -= 1

    import sys

    input = sys.stdin.readline
    n, q = map(int, input().split())
    c = tuple(map(int, input().split()))
    block_range = int(n / q**0.5) + 1
    # block_range = int(3**0.5 * n / (q << 1) ** 0.5) + 1
    qs = [[] for _ in range(n // block_range + 1)]
    for i in range(q):
        l, r = map(lambda x: int(x) - 1, input().split())
        qs[l // block_range].append((r, l, i))
    ans = [0] * q
    nowans = 0
    d = [0] * (n + 1)
    left = 0
    right = 0
    for index, qb in enumerate(qs):
        for r, l, i in sorted(qb, reverse=index % 2, key=lambda x: x[0]):
            while right <= r:
                add(c[right])
                right += 1
            while left > l:
                left -= 1
                add(c[left])
            while right > r + 1:
                right -= 1
                rem(c[right])
            while left < l:
                rem(c[left])
                left += 1
            ans[i] = nowans
    print(*ans, sep="\n")


def mos_algorithm_hilbert():
    # https://atcoder.jp/contests/abc242/submissions/42740143
    def add(i):
        nonlocal nowans
        d[i] += 1
        if d[i] == 1:
            nowans += 1

    def rem(i):
        nonlocal nowans
        d[i] -= 1
        if d[i] == 0:
            nowans -= 1

    class HilbertOrder:
        def __init__(self, n: int) -> None:
            # n : x, yの最大値
            self.maxn = 1 << (n - 1).bit_length()

        def __call__(self, x: int, y: int):
            assert 0 <= x < self.maxn
            assert 0 <= y < self.maxn
            d = 0
            s = self.maxn >> 1
            while s:
                rx = (x & s) > 0
                ry = (y & s) > 0
                d += s**2 * ((rx * 3) ^ ry)
                if ry:
                    s >>= 1
                    continue
                if rx:
                    x = self.maxn - 1 - x
                    y = self.maxn - 1 - y
                x, y = y, x
                s >>= 1
            return d

    import sys

    input = sys.stdin.readline
    n, q = map(int, input().split())
    a = tuple(map(int, input().split()))
    qs = [None] * q
    hilbertorder = HilbertOrder(n)
    h = [0] * q
    for i in range(q):
        l, r = map(lambda x: int(x) - 1, input().split())
        qs[i] = (r, l, i)
        h[i] = hilbertorder(l, r)
    ans = [0] * q
    nowans = 0
    d = [0] * (n + 1)
    left = 0
    right = 0
    for id in sorted(range(q), key=lambda x: h[x]):
        r, l, i = qs[id]
        while right <= r:
            add(c[right])
            right += 1
        while left > l:
            left -= 1
            add(c[left])
        while right > r + 1:
            right -= 1
            rem(c[right])
        while left < l:
            rem(c[left])
            left += 1
        ans[i] = nowans
    print(*ans, sep="\n")
