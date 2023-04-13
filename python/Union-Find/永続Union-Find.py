class PersistentUnionFind(object):
    # https://judge.yosupo.jp/submission/30721
    k = 20

    def to_tree(self, A):
        queue = [(A[0], [None] * k)]
        for i, node in enumerate(queue):
            for j in range(k):
                ni = k * i + j + 1
                if ni < len(A):
                    node[1][j] = (A[ni], [None] * k)
                    queue.append(node[1][j])
        return queue[0]

    def get_value(self, i, root):
        path = []
        while i:
            i, r = divmod(i - 1, k)
            path.append(r)
        for i in path[::-1]:
            root = root[1][i]
        return root[0]

    def set_value(self, i, value, root):
        path = []
        while i:
            i, r = divmod(i - 1, k)
            path.append(r)
        stack = []
        for i in path[::-1]:
            stack.append((root[0], root[1][:]))
            root = root[1][i]
        node = (value, root[1])
        for i in path:
            stack[-1][1][i] = node
            node = stack.pop()
        return node

    def __init__(self, n):
        if isinstance(n, int):
            self._par = self.to_tree(list(range(n)))
            self._size = self.to_tree([1] * n)
        else:
            self._par, self._size = n

    def root(self, i):
        root, par = i, self.get_value(i, self._par)
        while root != par:
            root, par = par, self.get_value(par, self._par)
        return root

    def unite(self, i, j):
        i, j = self.root(i), self.root(j)
        if i == j:
            return self
        if self.size(i) < self.size(j):
            i, j = j, i
        par = self.set_value(j, i, self._par)
        size = self.set_value(i, self.size(i) + self.size(j), self._size)
        return PersistentUnionFind((par, size))

    def is_connected(self, i, j):
        return self.root(i) == self.root(j)

    def size(self, i):
        return self.get_value(i, self._size)
