def next_permutation(a: list, l: int = 0, r: int = None) -> bool:
    if r is None:
        r = len(a) - 1
    for i in range(r - 1, l - 1, -1):
        if a[i] < a[i + 1]:
            for j in range(r, i, -1):
                if a[i] < a[j]:
                    a[i], a[j] = a[j], a[i]
                    p, q = i + 1, r
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
    if r is None:
        r = len(a) - 1
    for i in range(r - 1, l - 1, -1):
        if a[i] > a[i + 1]:
            for j in range(r, i, -1):
                if a[i] > a[j]:
                    a[i], a[j] = a[j], a[i]
                    p, q = i + 1, r
                    while p < q:
                        a[p], a[q] = a[q], a[p]
                        p += 1
                        q -= 1
                    return True
    return False


def pertitions(a):
    q = [(0, [])]
    while q:
        l, res = q.pop()
        for i in range(l + 1, len(a)):
            q.append((i, res + [a[l:i]]))
        yield res + [a[l:]]
