def LIS(l) -> int:
    from bisect import bisect_left
    dp = []
    for i in l:
        try:
            if i > dp[-1]:
                dp.append(i)
            else:
                dp[bisect_left(dp, i)] = i
        except IndexError:
            dp.append(i)
    return len(dp)


def restore_LIS(l):
    # 復元版
    from bisect import bisect_left
    dp = []
    id = [None] * len(l)
    for ind, i in enumerate(l):
        try:
            if i > dp[-1]:
                dp.append(i)
                id[ind] = len(dp) - 1
            else:
                index = bisect_left(dp, i)
                dp[index] = i
                id[ind] = index
        except IndexError:
            dp.append(i)
            id[ind] = len(dp) - 1
    index = len(dp) - 1
    res = [None] * (index + 1)
    for i in range(len(l) - 1, -1, -1):
        if id[i] == index:
            res[index] = l[i]
            index -= 1
    return res


def improper_LIS(l) -> int:
    # 広義単調増大部分列の場合
    from bisect import bisect_right
    dp = []
    for i in l:
        try:
            if i >= dp[-1]:
                dp.append(i)
            else:
                dp[bisect_right(dp, i)] = i
        except IndexError:
            dp.append(i)
    return len(dp)


def LIS_list(l):
    # 先頭からのLISのリストを返す
    from bisect import bisect_left
    dp = []
    res = [None] * len(l)
    for index, i in enumerate(l):
        try:
            if i > dp[-1]:
                dp.append(i)
            else:
                dp[bisect_left(dp, i)] = i
        except IndexError:
            dp.append(i)
        res[index] = len(dp)
    return res


def LDS(l) -> int:
    from bisect import bisect_left
    dp = []
    for j in l:
        i = ~j
        try:
            if i > dp[-1]:
                dp.append(i)
            else:
                dp[bisect_left(dp, i)] = i
        except IndexError:
            dp.append(i)
    return len(dp)


def improper_LDS(l) -> int:
    from bisect import bisect_right
    dp = []
    for j in l[1:]:
        i = ~j
        try:
            if i >= dp[-1]:
                dp.append(i)
            else:
                dp[bisect_right(dp, i)] = i
        except IndexError:
            dp.append(i)
    return len(dp)


def LDS_list(l):
    # LDSのリストを返す
    from bisect import bisect_left
    dp = []
    res = [None] * len(l)
    for index, j in enumerate(l):
        i = ~j
        try:
            if i > dp[-1]:
                dp.append(i)
            else:
                dp[bisect_left(dp, i)] = i
        except IndexError:
            dp.append(i)
        res[index] = len(dp)
    return res


# TODO
# def LIS_Segtree(l):
#     s = sorted(set(l))
#     d = {e: i for i, e in enumerate(s)}
#     a = list(map(d.get, l))
#     segtree = SegmentTree([0] * len(l), max, -float("inf"))


# def compress(l):
#     return


# def convertToCompressedArray(l, d):
#     return


# class SegmentTree:
#     """
#     Segment Tree
#     """

#     def __init__(self, init_val, function=min, element=float('inf')):
#         """
#         初期化
#         init_val: 配列の初期値
#         利用方法 hoge = SegmentTree(配列、function、単位元)
#         function、単位元は以下の通り
#         操作        function        単位元
#         最小値      min             float('inf')
#         最大値      max             -float('inf')
#         区間和      operator.add    0
#         最大公約数  math.gcd        0
#         """
#         self.n = len(init_val)
#         self.function = function
#         self.element = element
#         self.num = 1 << (self.n - 1).bit_length()
#         self.tree = [element] * 2 * self.num
#         for i in range(self.n):
#             self.tree[self.num + i] = init_val[i]
#         for i in range(self.num - 1, 0, -1):
#             self.tree[i] = function(self.tree[2 * i], self.tree[2 * i + 1])

#     def update(self, k, x):
#         """
#         k番目の値をxに更新
#         k: index(0-index)
#         x: update value
#         """
#         k += self.num
#         self.tree[k] = x
#         while k > 1:
#             k >>= 1
#             self.tree[k] = self.function(self.tree[k], self.tree[k ^ 1])

#     def add(self, k, x):
#         """
#         k番目の値にxを加算(未検証)
#         k: index(0-index)
#         x: add value
#         """
#         k += self.num
#         self.tree[k] += x
#         while k > 1:
#             k >>= 1
#             self.tree[k] = self.function(self.tree[k], self.tree[k ^ 1])

#     def query(self, l, r):
#         """
#         [l, r)のfunctionしたものを得る
#         l: index(0-index)
#         r: index(0-index)
#         """
#         res = self.element
#         l += self.num
#         r += self.num
#         while l < r:
#             if l & 1:
#                 res = self.function(res, self.tree[l])
#                 l += 1
#             if r & 1:
#                 res = self.function(res, self.tree[r - 1])
#             l >>= 1
#             r >>= 1
#         return res

#     def get(self, l=0, r=None):
#         """
#         [l, r)の配列を返す
#         l: index(0-index)
#         r: index(0-index)
#         """
#         if r is None:
#             r = self.n - 1
#         return [self.query(x, x + 1) for x in range(l, r + 1)]
