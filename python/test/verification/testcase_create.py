def createcase():
    from random import randrange, randint

    maxa = 10**3
    maxb = 10**5
    maxc = 10**9
    n = randint(1, maxa)
    m = randint(1, maxc)
    print(n, m)
    a = [randint(1, m) for _ in range(n)]
    print(*a)


if __name__ == "__main__":
    createcase()
