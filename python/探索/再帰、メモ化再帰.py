def resolve():
    import sys

    RECURSION_LIMIT = 10**6
    sys.setrecursionlimit(RECURSION_LIMIT)

    from functools import lru_cache

    MEMORIZE_LIMIT = None

    @lru_cache(maxsize=MEMORIZE_LIMIT)
    def func(i: int) -> int:
        pass

    # Python3.9から
    # from functools import cache

    # @cache
    # def func2(i: int) -> int:
    #     pass

    # import pypyjit
    # pypyjit.set_param('max_unroll_recursion=-1')


def dfs(n: int, m: int, a: list = []) -> list:
    # [0, m)をn個選ぶ
    if len(a) == n:
        yield a
        return
    for v in range(m):
        a.append(v)
        yield from dfs(n, m, a)
        a.pop()


def dfs2(n: int, m: int, a: list = []) -> list:
    # [0, m)をn個選ぶ(広義単調増加)
    if len(a) == n:
        yield a
        return
    for v in range(a[-1] if a else 0, m):
        a.append(v)
        yield from dfs2(n, m, a)
        a.pop()


def dfs3(n: int, m: int, a: list = []) -> None:
    if len(a) == n:
        print(*a)  # ここに処理を書く
        return
    for v in range(a[-1] + 1 if a else 0, m):
        a.append(v)
        dfs3(n, m, a)
        a.pop()


if __name__ == "__main__":
    resolve()
    for i in dfs2(4, 5):
        print(i)
