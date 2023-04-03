def has_intersect(a, b):
    # 図形が重なっているか判定
    p, q, r, s = (
        max(a[0][0], b[0][0]),
        min(a[1][0], b[1][0]),
        max(a[0][1], b[0][1]),
        min(a[1][1], b[1][1]),
    )
    return (p < q and r <= s) or (p <= q and r < s)


def inside_polygon(p0, qs):
    # 点が図形の内部であるか判定
    cnt = 0
    L = len(qs)
    x, y = p0
    for i in range(L):
        x0, y0 = qs[i - 1]
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


def intersect(p1, p2, p3, p4, both_ends=True):
    """直線p1p2と線分p3p4が重なっているか判定
    both_ends==Trueだと両端も判定対象に含む"""
    tc1 = (p1[0] - p2[0]) * (p3[1] - p1[1]) + (p1[1] - p2[1]) * (p1[0] - p3[0])
    tc2 = (p1[0] - p2[0]) * (p4[1] - p1[1]) + (p1[1] - p2[1]) * (p1[0] - p4[0])
    td1 = (p3[0] - p4[0]) * (p1[1] - p3[1]) + (p3[1] - p4[1]) * (p3[0] - p1[0])
    td2 = (p3[0] - p4[0]) * (p2[1] - p3[1]) + (p3[1] - p4[1]) * (p3[0] - p2[0])
    if both_ends:
        return tc1 * tc2 < 0 and td1 * td2 <= 0
    return tc1 * tc2 < 0 and td1 * td2 < 0


def intersect_seg(p1, p2, p3, p4, both_ends=True):
    """線分p1p2と線分p3p4が重なっているか判定
    both_ends==Trueだと両端も判定対象に含む"""
    return intersect(p1, p2, p3, p4, both_ends) and intersect(p3, p4, p1, p2, both_ends)
