def has_intersect(a, b):
    # 図形が重なっているか判定
    p, q, r, s = max(a[0][0], b[0][0]), min(a[1][0], b[1][0]), max(
        a[0][1], b[0][1]), min(a[1][1], b[1][1])
    return (p < q and r <= s) or (p <= q and r < s)


def inside_polygon(p0, qs):
    # 点が図形の内部であるか判定
    cnt = 0
    L = len(qs)
    x, y = p0
    for i in range(L):
        x0, y0 = qs[i-1]
        x1, y1 = qs[i]
        x0 -= x
        y0 -= y
        x1 -= x
        y1 -= y

        cv = x0 * x1 + y0 * y1
        sv = x0 * y1 - x1 * y0
        if sv == 0 and cv <= 0:
            # a point is on a segment
            return True

        if not y0 < y1:
            x0, x1 = x1, x0
            y0, y1 = y1, y0

        if y0 <= 0 < y1 and x0 * (y1 - y0) > y0 * (x1 - x0):
            cnt ^= 1
    return bool(cnt)
