def geometric_progression_sum(a: int, r: int, n: int) -> int:
    # 等比数列の和、初項a、公比r、項数n
    return a * (r ** n - 1) / (r - 1)


def arithmetic_progression_sum(a: int, d: int, n: int) -> int:
    # 等差数列の和、初項a、公差d、項数n
    return n * (2 * a + (n - 1) * d) // 2


def arithmetic_progression_sum_2(a: int, l: int, n: int) -> int:
    # 等差数列の和、初項a、末項l、項数n
    return n * (a + l) // 2


def geometric_progression_sum_mod(a: int, r: int, n: int, mod: int = 10 ** 9 + 7) -> int:
    # 等比数列の和、初項a、公比r、項数n
    if n <= 0:
        return 0
    if n == 1:
        return a % mod
    x = geometric_progression_sum_mod(a, r, n // 2, mod)
    res = (x + pow(r, n // 2, mod) * x) % mod
    if n % 2 == 1:
        res = (a + r * res) % mod
    return res


def arithmetic_progression_sum_mod(a: int, d: int, n: int, mod: int = 10 ** 9 + 7) -> int:
    # 等差数列の和、初項a、公差d、項数n
    return (n * (2 * a + (n - 1) * d) // 2) % mod


def arithmetic_progression_sum_mod_2(a: int, l: int, n: int, mod: int = 10 ** 9 + 7) -> int:
    # 等差数列の和、初項a、末項l、項数n
    return n * (a + l) // 2 % mod
