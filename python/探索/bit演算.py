def subsets(i: int) -> int:
    # bitの部分集合
    j = i
    while j:
        yield j
        j -= 1
        j &= i


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
    if c & 0xffffffff00000000:
        c &= 0xffffffff00000000
    if c & 0xffff0000ffff0000:
        c &= 0xffff0000ffff0000
    if c & 0xff00ff00ff00ff00:
        c &= 0xff00ff00ff00ff00
    if c & 0xf0f0f0f0f0f0f0f0:
        c &= 0xf0f0f0f0f0f0f0f0
    if c & 0xcccccccccccccccc:
        c &= 0xcccccccccccccccc
    if c & 0xaaaaaaaaaaaaaaaa:
        c &= 0xaaaaaaaaaaaaaaaa
    return c


def upper_bit_pop(i: int) -> int:
    # 最上位bitを0にする
    # assert -(1 << 63) <= n < 1 << 63
    c = i
    if c & 0xffffffff00000000:
        c &= 0xffffffff00000000
    if c & 0xffff0000ffff0000:
        c &= 0xffff0000ffff0000
    if c & 0xff00ff00ff00ff00:
        c &= 0xff00ff00ff00ff00
    if c & 0xf0f0f0f0f0f0f0f0:
        c &= 0xf0f0f0f0f0f0f0f0
    if c & 0xcccccccccccccccc:
        c &= 0xcccccccccccccccc
    if c & 0xaaaaaaaaaaaaaaaa:
        c &= 0xaaaaaaaaaaaaaaaa
    return i - c


def popcount(n: int) -> int:
    # assert -(1 << 63) <= n < 1 << 63
    c = (n & 0x5555555555555555) + ((n >> 1) & 0x5555555555555555)
    c = (c & 0x3333333333333333) + ((c >> 2) & 0x3333333333333333)
    c = (c & 0x0f0f0f0f0f0f0f0f) + ((c >> 4) & 0x0f0f0f0f0f0f0f0f)
    c = (c & 0x00ff00ff00ff00ff) + ((c >> 8) & 0x00ff00ff00ff00ff)
    c = (c & 0x0000ffff0000ffff) + ((c >> 16) & 0x0000ffff0000ffff)
    c = (c & 0x00000000ffffffff) + ((c >> 32) & 0x00000000ffffffff)
    return c


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
            self.POPCOUNT_TABLE16[index] = (
                index & 1) + self.POPCOUNT_TABLE16[index >> 1]

    def __call__(self, v: int) -> int:
        return (self.POPCOUNT_TABLE16[v & 0xffff] +
                self.POPCOUNT_TABLE16[(v >> 16) & 0xffff] +
                self.POPCOUNT_TABLE16[(v >> 32) & 0xffff] +
                self.POPCOUNT_TABLE16[(v >> 48)])
