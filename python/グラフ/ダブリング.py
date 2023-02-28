class Doubling:
    def __init__(self, nexts, max_n):
        self.n = len(nexts)
        self.max_bl = max_n.bit_length()
        self.max_n = (1 << self.max_bl) - 1
        self.table = [None] * self.max_bl
        self.table[0] = nexts[:]
        for i in range(self.max_bl - 1):
            self.table[i + 1] = [0] * self.n
            for index, j in enumerate(self.table[i]):
                self.table[i + 1][index] = self.table[i][j]

    def __call__(self, start, n):
        assert 0 <= start < self.n
        assert 0 <= n <= self.max_n
        res = start
        i = 0
        while n:
            if n & 1:
                res = self.table[i][res]
            i += 1
            n >>= 1
        return res
