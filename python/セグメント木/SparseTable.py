class SparseTable():
    # 不変配列のmin,maxを高速で求める
    def __init__(self, arr, op=min):
        self.op = op
        self.n = len(arr)
        self.h = self.n.bit_length()
        self.table = [[0] * self.n for _ in range(self.h)]
        self.table[0] = [a for a in arr]
        for k in range(1, self.h):
            t, p = self.table[k], self.table[k - 1]
            l = 1 << (k - 1)
            for i in range(self.n - l * 2 + 1):
                t[i] = op(p[i], p[i + l])

    def prod(self, l, r):
        assert 0 <= l < r <= self.n
        k = (r - l).bit_length() - 1
        return self.op(self.table[k][l], self.table[k][r - (1 << k)])
