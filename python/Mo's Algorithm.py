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
    x = 0
    y = 0
    for index, qb in enumerate(qs):
        for r, l, i in sorted(qb, reverse=index % 2, key=lambda x: x[0]):
            while y <= r:
                add(c[y])
                y += 1
            while y > r + 1:
                y -= 1
                rem(c[y])
            while x < l:
                rem(c[x])
                x += 1
            while x > l:
                x -= 1
                add(c[x])
            ans[i] = nowans
    print(*ans, sep="\n")
