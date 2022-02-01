def next_permutation(a: list, l: int = 0, r: int = None) -> bool:
    if r is None:
        r = len(a) - 1
    for i in range(r - 1, l - 1, -1):
        if a[i] < a[i + 1]:
            for j in range(r, i, -1):
                if a[i] < a[j]:
                    a[i], a[j] = a[j], a[i]
                    p, q = i + 1, r
                    for k in range((r-i-1)//2):
                        p, q = r - k, k + 1 + i
                        a[p], a[q] = a[q], a[p]
                    return True
    return False


a = list(range(3))
while True:
    print(a)
    if not next_permutation(a):
        break
