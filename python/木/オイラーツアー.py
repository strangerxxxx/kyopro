def EulerTour(n, edges, start=0):
    reached = [False] * n
    queue = [start]
    res = []
    while queue:
        i = queue.pop()
        if i >= 0:
            reached[i] = True
            res.append(i)
            for a in edges[i]:
                # for a in reversed(sorted(edges[i])):
                if reached[a]:
                    continue
                queue.append(~i)
                queue.append(a)
        else:
            res.append(~i)
    return res


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


def EulerTour2(n, edges, start=0):
    reached = [False] * n
    queue = [~start, start]
    res = []
    left = [None] * n
    right = [None] * n
    depth = [None] * n
    childs = [[] for _ in range(n)]
    index = -1
    dep = -1
    while queue:
        i = queue.pop()
        index += 1
        if i >= 0:
            dep += 1
            reached[i] = True
            left[i] = index
            right[i] = index
            depth[i] = dep
            res.append(i)
            for a in reversed(edges[i]):
                if reached[a]:
                    continue
                queue.append(~a)
                queue.append(a)
                childs[i].append(a)
        else:
            dep -= 1
            if ~i == res[-1]:
                index -= 1
            else:
                res.append(~i)
                right[~i] = index
    return res, left, right, depth, childs


print(EulerTour(4, [[1, 2], [0], [0, 3], [2]]))
