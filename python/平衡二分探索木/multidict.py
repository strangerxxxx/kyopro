import heapq


class HeapDict:
    def __init__(self):
        self.h = []
        self.d = {}

    def insert(self, x):
        heapq.heappush(self.h, x)
        if x not in self.d:
            self.d[x] = 1
        else:
            self.d[x] += 1

    def erase(self, x):
        if x not in self.d or self.d[x] == 0:
            raise KeyError(f"{x} is not in HeapDict")
        else:
            self.d[x] -= 1

        while len(self.h) != 0:
            if self.d[self.h[0]] == 0:
                heapq.heappop(self.h)
            else:
                break

    def is_exist(self, x):
        return x in self.d and self.d[x] != 0

    def get_min(self):
        return self.h[0]


class multiDict:
    def __init__(self):
        self.d = {}

    def insert(self, x):
        if x not in self.d:
            self.d[x] = 1
        else:
            self.d[x] += 1

    def erase(self, x):
        if x not in self.d or self.d[x] == 0:
            raise KeyError(f"{x} is not in multiDict")
        else:
            self.d[x] -= 1

    def eraseall(self, x):
        del self.d[x]

    def is_exist(self, x):
        return x in self.d and self.d[x] != 0

    def count(self, x):
        if x not in self.d:
            return 0
        return self.d[x]
