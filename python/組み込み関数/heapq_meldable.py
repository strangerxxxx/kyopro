class SHNode:
    def __init__(self, val):
        self.left = None
        self.right = None
        self.val = val
        self.add = 0

    def lazy(self):
        if self.left is not None:
            self.left.add += self.add
        if self.right is not None:
            self.right.add += self.add
        self.val += self.add
        self.add = 0


class SkewHeap:
    def __init__(self):
        self.root = None

    def heapmeld(self, h1, h2):
        if h1 is None:
            return h2
        if h2 is None:
            return h1
        if h1.val + h1.add > h2.val + h2.add:
            h1, h2 = h2, h1
        h1.lazy()
        h1.right = self.heapmeld(h2, h1.right)
        h1.left, h1.right = h1.right, h1.left
        return h1

    def heappop(self):
        res = self.root
        res.lazy()
        self.root = self.heapmeld(res.left, res.right)
        return res.val

    def heappush(self, x):
        nh = SHNode(x)
        self.root = self.heapmeld(self.root, nh)

    def heaptop(self):
        if self.root is None:
            return None
        return self.root.val

    def heapadd(self, val):
        self.root.add += val
