def runLengthEncode(S: str) -> "List[tuple(str, int)]":
    from itertools import groupby
    # RUN LENGTH ENCODING str -> list(tuple())
    # example) "aabbbbaaca" -> [('a', 2), ('b', 4), ('a', 2), ('c', 1), ('a', 1)]
    grouped = groupby(S)
    res = []
    for k, v in grouped:
        res.append((k, int(len(list(v)))))
    return res


def runLengthDecode(L: "list[tuple]") -> str:
    from itertools import groupby
    # RUN LENGTH DECODING list(tuple()) -> str
    # example) [('a', 2), ('b', 4), ('a', 2), ('c', 1), ('a', 1)] -> "aabbbbaaca"
    res = ""
    for c, n in L:
        res += c * int(n)
    return res


def runLengthEncodeToString(S: str) -> str:
    from itertools import groupby
    # RUN LENGTH ENCODING str -> str
    # example) "aabbbbaaca" -> "a2b4a2c1a1"
    grouped = groupby(S)
    res = ""
    for k, v in grouped:
        res += k + str(len(list(v)))
    return res
