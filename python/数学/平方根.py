def floor_sqrt(n: int, r: int = 2) -> int:
    x = int(n ** (1 / r))
    for i in range(x + 5, x - 10, -1):
        if i ** r <= n:
            return i
    raise ValueError
