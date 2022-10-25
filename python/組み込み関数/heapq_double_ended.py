from heapq import heapify, heappop, heappush


class MinMaxHeap():
    def __init__(self, a=[]):
        self.max_heap = [-x for x in a]
        self.min_heap = a[:]
        heapify(self.max_heap)
        heapify(self.min_heap)
        self.max_deleted = []
        self.min_deleted = []
        self.size = len(a)

    def pop_max(self):
        # assert self.size
        while True:
            v = -heappop(self.max_heap)
            if self.min_deleted and self.min_deleted[0] == -v:
                heappop(self.min_deleted)
            else:
                self.size -= 1
                heappush(self.max_deleted, v)
                return v

    def pop_min(self):
        # assert self.size
        while True:
            v = heappop(self.min_heap)
            if self.max_deleted and self.max_deleted[0] == v:
                heappop(self.max_deleted)
            else:
                self.size -= 1
                heappush(self.min_deleted, -v)
                return v

    def get_max(self):
        # assert self.size
        while True:
            v = -self.max_heap[0]
            if self.min_deleted and self.min_deleted[0] == -v:
                heappop(self.min_deleted)
                heappop(self.max_heap)
            else:
                return v

    def get_min(self):
        # assert self.size
        while True:
            v = self.min_heap[0]
            if self.max_deleted and self.max_deleted[0] == v:
                heappop(self.max_deleted)
                heappop(self.min_heap)
            else:
                return v

    def push(self, v):
        self.size += 1
        heappush(self.max_heap, -v)
        heappush(self.min_heap, v)
