class Imos:
    def __init__(self, n: int, default=0):
        self.n = n
        if hasattr(default, "__iter__"):
            self.data = [0] + default[:]
        else:
            self.data = [default] + [0] * n
        self.accumulate_data = None

    def add(self, start: int, end: int, value=1) -> None:
        # 区間[start, end)にvalueを加算する
        self.data[start] += value
        self.data[end] -= value

    def construct(self):
        # 累積和を計算する
        self.accumulate_data = [0] * (self.n + 1)
        for i, j in enumerate(self.data):
            self.accumulate_data[i + 1] = self.accumulate_data[i] + j

    def query(self, l, r, recalc=False):
        # 区間[start, end)の合計を計算する
        if self.accumulate_data is None or recalc:
            self.construct()
        return self.accumulate_data[r] - self.accumulate_data[l]


class Imos2d:
    def __init__(self, h: int, w: int, default=0):
        self.h = h
        self.w = w
        if hasattr(default, "__iter__"):
            self.data = [[0] * (w + 1) for _ in range(h + 1)]
            self.init_from_iter(default)
        else:
            self.data = [[default] * (w + 1) for _ in range(h + 1)]
        self.accumulate_data = None

    def init_from_iter(self, a) -> None:
        for i in range(self.h):
            for j in range(self.w):
                self.data[i + 1][j + 1] = a[i][j]

    def add(self, start_y, start_x, end_y=None, end_x=None, value=1) -> None:
        # 区間[(start_x, start_y), (end_x, end_y))にvalueを加算する
        if end_y is None:
            end_y = start_y
        if end_x is None:
            end_x = start_x
        self.data[start_y][start_x] += value
        self.data[start_y][end_x] -= value
        self.data[end_y][start_x] -= value
        self.data[end_y][end_x] += value

    def construct(self):
        # 縦横に累積和を計算し取得する
        self.accumulate_data = [[0] * (self.w + 1) for _ in range(self.h + 1)]
        for h in range(self.h):
            for w in range(self.w):
                self.accumulate_data[h + 1][w + 1] = (
                    self.accumulate_data[h + 1][w] + self.data[h + 1][w + 1]
                )
        for w in range(self.w):
            for h in range(1, self.h):
                self.accumulate_data[h + 1][w + 1] += self.accumulate_data[h][w + 1]

    def query(self, start_y, start_x, end_y, end_x, recalc=False):
        if self.accumulate_data is None or recalc:
            self.construct()
        return (
            self.accumulate_data[end_y][end_x]
            - self.accumulate_data[end_y][start_x]
            - self.accumulate_data[start_y][end_x]
            + self.accumulate_data[start_y][start_x]
        )
