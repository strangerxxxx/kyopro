def is_rectangle(a, b, c, d):
    l = [a, b, c, d]
    v = [[x - y for x, y in zip(l[i - 1], l[i])] for i in range(4)]
    for i in range(3):
        if sum(x * y for x, y in zip(v[i], v[i + 1])) > 0:
            return False
    return True
