def has_intersect(a, b):
    # 図形が重なっているか判定
    p, q, r, s = max(a[0][0], b[0][0]), min(a[1][0], b[1][0]), max(
        a[0][1], b[0][1]), min(a[1][1], b[1][1])
    return (p < q and r <= s) or (p <= q and r < s)
