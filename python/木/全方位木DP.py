def resolve():
    # ref : https://qiita.com/Kiri8128/items/a011c90d25911bdb3ed3
    import sys
    input = sys.stdin.readline
    MOD = 10 ** 9 + 7
    n = int(input())
    childs = [[] for _ in range(n)]
    for _ in range(n - 1):
        a, b = map(int, input().split())
        childs[a - 1].append(b - 1)
        childs[b - 1].append(a - 1)
    order, tree, parent = TopoSort(n, childs)
    unit = 1
    def merge(a, b): return a * b % MOD
    def adj_bottomup(a, i): return a + 1
    def adj_topdown(a, i, p): return a + 1
    def adj_fin(a, i): return a
    ME = [unit] * n
    dp = [0] * n
    for i in reversed(order[1:]):
        dp[i] = adj_bottomup(ME[i], i)
        p = parent[i]
        ME[p] = merge(ME[p], dp[i])
    dp[order[0]] = adj_fin(ME[order[0]], order[0])
    TD = [unit] * n

    for i in order:
        # 左からDP（結果はTDに入れている）
        ac = TD[i]
        for j in tree[i]:
            TD[j] = ac
            ac = merge(ac, dp[j])
        # 右からDP（結果はacに入れている）
        ac = unit
        for j in reversed(tree[i]):
            TD[j] = adj_topdown(merge(TD[j], ac), j, i)
            ac = merge(ac, dp[j])
            dp[j] = adj_fin(merge(ME[j], TD[j]), j)

    # print("TD =", TD) # Top-down after adj
    # print("dp =", dp) # Final Result
    print(*dp, sep="\n")


def TopoSort(n, childs, start=0):
    # 行きがけの順と親を含まない木を返す
    reached = [False] * n
    queue = [~start, start]
    tree = [[] for _ in range(n)]
    parent = [None] * n
    order = []
    while queue:
        i = queue.pop()
        if i >= 0:
            reached[i] = True
            order.append(i)
            # for a in reversed(sorted(childs[i])):
            for a in childs[i]:
                if reached[a]:
                    continue
                tree[i].append(a)
                parent[a] = i
                queue.append(a)
    return order, tree, parent


if __name__ == '__main__':
    resolve()
