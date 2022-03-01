def index_left(a, x: int) -> int:
    'Locate the leftmost value exactly equal to x'
    from bisect import bisect_left
    i = bisect_left(a, x)
    if i != len(a) and a[i] == x:
        return i
    raise ValueError


def index_right(a, x: int) -> int:
    'Locate the rightmost value exactly equal to x'
    from bisect import bisect_right
    i = bisect_right(a, x)
    if i != len(a) and a[i] == x:
        return i
    raise ValueError


def find_lt(a, x: int) -> int:
    'Find rightmost value less than x'
    from bisect import bisect_left
    i = bisect_left(a, x)
    if i:
        return a[i - 1]
    raise ValueError


def find_le(a, x: int) -> int:
    'Find rightmost value less than or equal to x'
    from bisect import bisect_right
    i = bisect_right(a, x)
    if i:
        return a[i - 1]
    raise ValueError


def find_gt(a, x: int) -> int:
    'Find leftmost value greater than x'
    from bisect import bisect_right
    i = bisect_right(a, x)
    if i != len(a):
        return a[i]
    raise ValueError


def find_ge(a, x: int) -> int:
    'Find leftmost item greater than or equal to x'
    from bisect import bisect_left
    i = bisect_left(a, x)
    if i != len(a):
        return a[i]
    raise ValueError


def find_bothsides(a, x: int) -> tuple:
    from bisect import bisect_left
    i = bisect_left(a, x)
    if i:
        if i < len(a):
            return a[i - 1], a[i]
        else:
            return a[i - 1], None
    if a:
        return None, a[0]
    return None, None


def count_lt(a, x: int) -> int:
    'Count value less than x'
    from bisect import bisect_left
    i = bisect_left(a, x)
    return i


def count_le(a, x: int) -> int:
    'Count value less than or equal to x'
    from bisect import bisect_right
    i = bisect_right(a, x)
    return i


def count_gt(a, x: int) -> int:
    'Count value greater than x'
    from bisect import bisect_right
    i = bisect_right(a, x)
    return len(a) - i


def count_ge(a, x: int) -> int:
    'Count item greater than or equal to x'
    from bisect import bisect_left
    i = bisect_left(a, x)
    return len(a) - i
