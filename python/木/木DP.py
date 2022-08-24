def resolve():
    # https://atcoder.jp/contests/dp/tasks/dp_p
    import sys
    input = sys.stdin.readline
    MOD = 10 ** 9 + 7

    n = int(input())
    edges = [[] for _ in range(n)]
    for _ in range(n - 1):
        x, y = map(int, input().split())
        edges[x - 1].append(y - 1)
        edges[y - 1].append(x - 1)

    memo = [[1] * 2 for _ in range(n)]
    start = 0
    order, tree, _ = TopoSort(n, edges)
    for i in reversed(order):
        for j in tree[i]:
            memo[i][0] *= memo[j][0] + memo[j][1]
            memo[i][1] *= memo[j][0]
            memo[i][0] %= MOD
            memo[i][1] %= MOD
    print((memo[start][0] + memo[start][1]) % MOD)


def TopoSort(n, edges, start=0):
    # 行きがけの順と親を含まない木を返す
    parent = [None] * n
    queue = [start]
    tree = [[] for _ in range(n)]
    order = []
    parent[start] = -1
    while queue:
        i = queue.pop()
        order.append(i)
        # for a in reversed(sorted(edges[i])):
        for a in edges[i]:
            if parent[a] is None:
                tree[i].append(a)
                queue.append(a)
                parent[a] = i
    return order, tree, parent


if __name__ == '__main__':
    resolve()
