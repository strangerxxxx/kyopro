import heapq


class Heap_max_kth:
    # 大きい方からk個のみ格納
    def __init__(self, k: int, a=[]) -> None:
        self.q: list = sorted(a, reverse=True)[:k]
        heapq.heapify(self.q)
        self.k = k
        self.s = sum(self.q)

    def pop(self):
        res = heapq.heappop(self.q)
        self.s -= res
        return res

    def push(self, item) -> None:
        self.s += item
        if len(self.q) == self.k:
            self.s -= self.pushpop(item)
        else:
            heapq.heappush(self.q, item)

    def decrement_k(self):
        if len(self.q) == self.k:
            self.s -= self.pop()
        self.k -= 1

    def sum(self) -> int:
        return self.s

    def top(self):
        return self.q[0]

    def pushpop(self, item):
        self.s += item
        res = heapq.heappushpop(self.q, item)
        self.s -= res
        return res

    def poppush(self, item):
        self.s += item
        res = heapq.heapreplace(self.q, item)
        self.s -= res
        return res

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


class Heap_min_kth:
    # 小さい方からk個のみ格納
    def __init__(self, k: int, a=[]) -> None:
        self.q: list = sorted(a)[:k]
        heapq._heapify_max(self.q)
        self.k = k
        self.s = sum(self.q)

    def pop(self):
        res = heapq._heappop_max(self.q)
        self.s -= res
        return res

    def top(self):
        return self.q[0]

    def push(self, item) -> None:
        self.s += item
        if len(self.q) == self.k:
            self.s -= self.pushpop(item)
        else:
            self._heappush_max(self.q, item)

    def decrement_k(self):
        if len(self.q) == self.k:
            self.pop()
        self.k -= 1

    def pushpop(self, item):
        self.s += item
        self._heappush_max(item)
        res = self.pop()
        self.s -= res
        return res

    def poppush(self, item):
        self.s += item
        res = heapq._heapreplace_max(self.q, item)
        self.s -= res
        return res

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
