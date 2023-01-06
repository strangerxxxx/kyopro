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


class multiDict(dict):
    def insert(self, key, val=1):
        if key not in self:
            self[key] = val
        else:
            self[key] += val

    def erase(self, key, val=1):
        if key not in self:
            raise KeyError(f"{key} is not in multiDict")
        if self[key] < val:
            raise KeyError(
                f"value({val}) is larger than multiDict[{key}] contains({self[key]})"
            )
        if self[key] == val:
            del self[key]
        else:
            self[key] -= val

    def eraseall(self, key):
        del self[key]

    def discard(self, key, val=1):
        if key in self:
            if self[key] > val:
                self[key] -= val
            else:
                del self[key]

    def count(self, key):
        return self.get(key, 0)
