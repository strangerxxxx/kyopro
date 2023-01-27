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
