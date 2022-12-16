def make_divisors(n: int):
    lower_divisors, upper_divisors = [], []
    i = 1
    for i in range(1, int(n ** 0.5) + 1):
        if n % i == 0:
            lower_divisors.append(i)
            if i != n // i:
                upper_divisors.append(n // i)
    return lower_divisors + upper_divisors[::-1]


def divisors_gen(n: int):
    for i in range(1, int(n ** 0.5) + 1):
        if n % i == 0:
            yield i
            if i != n // i:
                yield n // i


def sum_of_divisors(n: int) -> int:
    # 約数の総和
    sumd = 0
    for i in range(1, int(n ** 0.5)):
        if n % i == 0:
            sumd += i
            sumd += n // i
    i += 1
    if n % i == 0:
        sumd += i
        if i != n // i:
            sumd += n // i
    return sumd


def pi_of_divisors(n: int) -> int:
    # 約数の総積
    pid = 1
    for i in range(1, int(n ** 0.5)):
        if n % i == 0:
            pid *= i
            pid *= n // i
    i += 1
    if n % i == 0:
        pid *= i
        if i != n // i:
            pid *= n // i
    return pid
