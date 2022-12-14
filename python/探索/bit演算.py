def subsets(i: int) -> int:
    # bitの部分集合
    j = i
    while j:
        yield j
        j -= 1
        j &= i
    # yield 0


def under_bit(i: int) -> int:
    # 最下位bit
    return i & (-i)


def under_bit_pop(i: int) -> int:
    # 最下位bitを0にする
    return i & (i - 1)


def upper_bit(i: int) -> int:
    # 最上位bit
    # assert -(1 << 63) <= n < 1 << 63
    c = i
    if c & 0xFFFFFFFF00000000:
        c &= 0xFFFFFFFF00000000
    if c & 0xFFFF0000FFFF0000:
        c &= 0xFFFF0000FFFF0000
    if c & 0xFF00FF00FF00FF00:
        c &= 0xFF00FF00FF00FF00
    if c & 0xF0F0F0F0F0F0F0F0:
        c &= 0xF0F0F0F0F0F0F0F0
    if c & 0xCCCCCCCCCCCCCCCC:
        c &= 0xCCCCCCCCCCCCCCCC
    if c & 0xAAAAAAAAAAAAAAAA:
        c &= 0xAAAAAAAAAAAAAAAA
    return c


def upper_bit_pop(i: int) -> int:
    # 最上位bitを0にする
    # assert -(1 << 63) <= n < 1 << 63
    c = i
    if c & 0xFFFFFFFF00000000:
        c &= 0xFFFFFFFF00000000
    if c & 0xFFFF0000FFFF0000:
        c &= 0xFFFF0000FFFF0000
    if c & 0xFF00FF00FF00FF00:
        c &= 0xFF00FF00FF00FF00
    if c & 0xF0F0F0F0F0F0F0F0:
        c &= 0xF0F0F0F0F0F0F0F0
    if c & 0xCCCCCCCCCCCCCCCC:
        c &= 0xCCCCCCCCCCCCCCCC
    if c & 0xAAAAAAAAAAAAAAAA:
        c &= 0xAAAAAAAAAAAAAAAA
    return i - c


def popcount(n: int) -> int:
    # assert -(1 << 63) <= n < 1 << 63
    c = (n & 0x5555555555555555) + ((n >> 1) & 0x5555555555555555)
    c = (c & 0x3333333333333333) + ((c >> 2) & 0x3333333333333333)
    c = (c & 0x0F0F0F0F0F0F0F0F) + ((c >> 4) & 0x0F0F0F0F0F0F0F0F)
    c = (c & 0x00FF00FF00FF00FF) + ((c >> 8) & 0x00FF00FF00FF00FF)
    c = (c & 0x0000FFFF0000FFFF) + ((c >> 16) & 0x0000FFFF0000FFFF)
    return (c & 0x00000000FFFFFFFF) + ((c >> 32) & 0x00000000FFFFFFFF)


def popcount32(n: int):
    c = (n & 0x55555555) + (n >> 1 & 0x55555555)
    c = (c & 0x33333333) + (c >> 2 & 0x33333333)
    c = (c & 0x0F0F0F0F) + (c >> 4 & 0x0F0F0F0F)
    c = (c & 0x00FF00FF) + (c >> 8 & 0x00FF00FF)
    return (c & 0x0000FFFF) + (c >> 16 & 0x0000FFFF)


def popcount16(n: int):
    c = (n & 0x5555) + (n >> 1 & 0x5555)
    c = (c & 0x3333) + (c >> 2 & 0x3333)
    c = (c & 0x0F0F) + (c >> 4 & 0x0F0F)
    return (c & 0x00FF) + (c >> 8 & 0x00FF)


def multiple_popcount(n: int) -> int:
    mask_length = 63
    msk = (1 << mask_length) - 1
    res = 0
    while n:
        res += popcount(n & msk)
        n >>= mask_length
    return res


class Popcount:
    def __init__(self) -> None:
        self.POPCOUNT_TABLE16 = [0] * (1 << 16)
        for index in range(len(self.POPCOUNT_TABLE16)):
            self.POPCOUNT_TABLE16[index] = (index & 1) + self.POPCOUNT_TABLE16[
                index >> 1
            ]

    def __call__(self, v: int) -> int:
        return (
            self.POPCOUNT_TABLE16[v & 0xFFFF]
            + self.POPCOUNT_TABLE16[(v >> 16) & 0xFFFF]
            + self.POPCOUNT_TABLE16[(v >> 32) & 0xFFFF]
            + self.POPCOUNT_TABLE16[(v >> 48)]
        )
