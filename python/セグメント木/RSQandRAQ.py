class RSQandRAQ():
    """区間加算、区間取得クエリをそれぞれO(logN)で答える
    add: 区間[l, r)にvalを加える
    query: 区間[l, r)の和を求める
    l, rは0-indexed
    """

    def __init__(self, n):
        self.n = n
        self.bit0 = [0] * (n + 1)
        self.bit1 = [0] * (n + 1)

    def _add(self, bit, i, val):
        i = i + 1
        while i <= self.n:
            bit[i] += val
            i += i & -i

    def _get(self, bit, i):
        s = 0
        while i > 0:
            s += bit[i]
            i -= i & -i
        return s

    def add(self, l, r, val):
        """区間[l, r)にvalを加える"""
        self._add(self.bit0, l, -val * l)
        self._add(self.bit0, r,  val * r)
        self._add(self.bit1, l,  val)
        self._add(self.bit1, r, -val)

    def query(self, l, r):
        """区間[l, r)の和を求める"""
        return self._get(self.bit0, r) + r * self._get(self.bit1, r) \
            - self._get(self.bit0, l) - l * self._get(self.bit1, l)
