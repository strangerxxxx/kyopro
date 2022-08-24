def resolve():
    # https://atcoder.jp/contests/abc174/tasks/abc174_f
    def add(i):
        nonlocal diffs
        d[i] += 1
        if d[i] == 1:
            diffs += 1

    def rem(i):
        nonlocal diffs
        d[i] -= 1
        if d[i] == 0:
            diffs -= 1
    import sys
    input = sys.stdin.readline
    n, q = map(int, input().split())
    c = tuple(map(int, input().split()))
    block_range = int(n / q ** 0.5) + 1
    qs = [[] for _ in range(n // block_range + 1)]
    for i in range(q):
        x, y = map(lambda x: int(x) - 1, input().split())
        qs[x // block_range].append((y, x, i))
    ans = [None] * q
    diffs = 0
    d = [0] * (n + 1)
    x = 0
    y = 0
    for index, qb in enumerate(qs):
        for r, l, i in sorted(qb, reverse=index % 2, key=lambda x: x[0]):
            while y <= r:
                add(c[y])
                y += 1
            while y > r + 1:
                rem(c[y - 1])
                y -= 1
            while x < l:
                rem(c[x])
                x += 1
            while x > l:
                add(c[x - 1])
                x -= 1
            ans[i] = diffs
    print(*ans, sep="\n")


if __name__ == '__main__':
    resolve()
