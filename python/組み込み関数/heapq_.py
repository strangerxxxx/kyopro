import heapq
from typing import Any


class Heap:
    def __init__(self, a=[]) -> None:
        self.q: list = a[:]
        heapq.heapify(self.q)

    def pop(self):
        return heapq.heappop(self.q)

    def push(self, item) -> None:
        heapq.heappush(self.q, item)

    def top(self):
        return self.q[0]

    def poppush(self, item):
        return heapq.heapreplace(self.q, item)

    def pushpop(self, item):
        return heapq.heappushpop(self.q, item)

    def __call__(self) -> list:
        return self.q

    def __len__(self):
        return len(self.q)

    def __bool__(self):
        return bool(self.q)

    def __str__(self):
        return str(type(self)) + "(" + str(self.q) + ")"

    def __iter__(self):
        yield from self.q


class Heap_max:
    def __init__(self, a=[]) -> None:
        self.q: list = a[:]
        heapq._heapify_max(self.q)

    def pop(self):
        return heapq._heappop_max(self.q)

    def top(self):
        return self.q[0]

    def push(self, item) -> None:
        self._heappush_max(item)

    def poppush(self, item):
        return heapq._heapreplace_max(self.q, item)

    def _heappush_max(self, item):
        self.q.append(item)
        heapq._siftdown_max(self.q, 0, len(self.q) - 1)

    def __call__(self) -> list:
        return self.q

    def __len__(self):
        return len(self.q)

    def __bool__(self):
        return bool(self.q)

    def __str__(self):
        return str(type(self)) + "(" + str(self.q) + ")"

    def __iter__(self):
        yield from self.q
