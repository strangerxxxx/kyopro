from collections import deque


class ConvexHullTrick:
    def __init__(self) -> None:
        self.deq = deque()

    def _check(self, f1, f2, f3):
        return (f2[0] - f1[0]) * (f3[1] - f2[1]) >= (f2[1] - f1[1]) * (f3[0] - f2[0])

    def _f(self, f1, x):
        return f1[0] * x + f1[1]

    # add f_i(x) = a*x + b
    def add_line(self, a, b):
        f1 = (a, b)
        while len(self.deq) >= 2 and self._check(self.deq[-2], self.deq[-1], f1):
            self.deq.pop()
        self.deq.append(f1)

    # min f_i(x)
    def query(self, x):
        while len(self.deq) >= 2 and self._f(self.deq[0], x) >= self._f(self.deq[1], x):
            self.deq.popleft()
        return self._f(self.deq[0], x)
