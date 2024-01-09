def declinination_sort(l):
    from functools import cmp_to_key

    upper = []
    zero = []
    under = []
    for i in l:
        if i[1] > 0:
            upper.append(i)
        elif i[1] < 0:
            under.append(i)
        elif i[0] > 0:
            under.append(i)
        elif i[0] < 0:
            upper.append(i)
        else:
            zero.append(i)

    def arg_sort(x, y):
        return x[1] * y[0] - x[0] * y[1]

    return (
        sorted(under, key=cmp_to_key(arg_sort))
        + zero
        + sorted(upper, key=cmp_to_key(arg_sort))
    )
