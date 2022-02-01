def index_left(a, x):
    'Locate the leftmost value exactly equal to x'
    import bisect
    i = bisect.bisect_left(a, x)
    if i != len(a) and a[i] == x:
        return i
    raise ValueError


def index_right(a, x):
    'Locate the rightmost value exactly equal to x'
    import bisect
    i = bisect.bisect_right(a, x) - 1
    if i != len(a) and a[i] == x:
        return i
    raise ValueError


def find_lt(a, x):
    'Find rightmost value less than x'
    import bisect
    i = bisect.bisect_left(a, x)
    if i:
        return a[i - 1]
    raise ValueError


def find_le(a, x):
    'Find rightmost value less than or equal to x'
    import bisect
    i = bisect.bisect_right(a, x)
    if i:
        return a[i - 1]
    raise ValueError


def find_gt(a, x):
    'Find leftmost value greater than x'
    import bisect
    i = bisect.bisect_right(a, x)
    if i != len(a):
        return a[i]
    raise ValueError


def find_ge(a, x):
    'Find leftmost item greater than or equal to x'
    import bisect
    i = bisect.bisect_left(a, x)
    if i != len(a):
        return a[i]
    raise ValueError


def count_lt(a, x):
    'Count value less than x'
    import bisect
    i = bisect.bisect_left(a, x)
    return i


def count_le(a, x):
    'Count value less than or equal to x'
    import bisect
    i = bisect.bisect_right(a, x)
    return i


def count_gt(a, x):
    'Count value greater than x'
    import bisect
    i = bisect.bisect_right(a, x)
    return len(a) - i


def count_ge(a, x):
    'Count item greater than or equal to x'
    import bisect
    i = bisect.bisect_left(a, x)
    return len(a) - i
