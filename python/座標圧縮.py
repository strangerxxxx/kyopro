def compress(l):
    return {e: i for i, e in enumerate(sorted(set(l)))}


def convertToCompressedArray(l, d):
    return list(map(d.get, l))


def restorableCompress(l):
    d = {e: i for i, e in enumerate(sorted(set(l)))}
    return d, {v: k for k, v in d.items()}


def restoreFromCompressedArray(l, id):
    return list(map(id.get, l))


# TODO 二次元版

if __name__ == "__main__":
    l = [486430, 856, 44, 337, 44, 0, 6687]
    d, id = compress(l)
    c = convertToCompressedArray(l, d)
    print(c)
    print(restoreFromCompressedArray(c, id))
