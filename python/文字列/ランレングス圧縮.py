def runLengthEncode(s: str) -> list[tuple[str, int]]:
    from itertools import groupby

    # RUN LENGTH ENCODING str -> list[tuple[str, int]]
    # example) "aabbbbaaca" -> [('a', 2), ('b', 4), ('a', 2), ('c', 1), ('a', 1)]
    grouped = groupby(s)
    res = []
    for k, v in grouped:
        res.append((k, int(len(list(v)))))
    return res


def runLengthDecode(a: list[tuple]) -> str:
    # RUN LENGTH DECODING list[tuple[str, int]] -> str
    # example) [('a', 2), ('b', 4), ('a', 2), ('c', 1), ('a', 1)] -> "aabbbbaaca"
    res = []
    for c, n in a:
        res.append(c * int(n))
    return "".join(res)


def runLengthEncodeToString(S: str) -> str:
    from itertools import groupby

    # RUN LENGTH ENCODING str -> str
    # example) "aabbbbaaca" -> "a2b4a2c1a1"
    grouped = groupby(S)
    res = ""
    for k, v in grouped:
        res += k + str(len(list(v)))
    return res
