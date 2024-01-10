def slide_min(a: list, k: int) -> list:
    # res[i]:min(a[i:i+k])
    from collections import deque

    n = len(a)
    res = [0] * (n - k + 1)
    q = deque()
    for i, j in enumerate(a):
        while q and a[q[-1]] >= j:
            q.pop()
        q.append(i)
        if q[0] == i - k:
            q.popleft()
        if i >= k - 1:
            res[i - k + 1] = a[q[0]]
    return res


def slide_op(a: list, k: int, op=min) -> list:
    # res[i]:op(a[i:i+k])
    from collections import deque

    n = len(a)
    res = [0] * (n - k + 1)
    q = deque()
    for i, j in enumerate(a):
        while q and op(a[q[-1]], j) == j:
            q.pop()
        q.append(i)
        if q[0] == i - k:
            q.popleft()
        if i >= k - 1:
            res[i - k + 1] = a[q[0]]
    return res


def first_exceeder(a: list):
    # res[i]:a[i]より後ろで最初に自分より小さくなるindex
    n = len(a)
    res = [0] * n
    q = []
    for i in range(0, n):
        while q and a[q[-1]] > a[i]:  # 逆ならここの不等号を反転する
            res[q[-1]] = i
            q.pop()
        q.append(i)
    for i in q:
        res[i] = n
    return res


def histgram_max_rectangle(a: list):
    # ヒストグラムの最大長方形
    n = len(a)
    l = [n - x for x in first_exceeder(a[::-1])[::-1]]
    r = first_exceeder(a)
    return max(ai * (y - x) for ai, x, y in zip(a, l, r))
