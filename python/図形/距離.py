def distance(a: int, b: int) -> float:
    # 2点間のユークリッド距離(普通の距離)
    d = 0
    for i, j in zip(a, b):
        d += (i - j) ** 2
    return d ** 0.5


def euclidean_scipy(a: int, b: int) -> float:
    # 2点間のユークリッド距離(普通の距離)
    from scipy.spatial.distance import euclidean
    return euclidean(a, b)


def manhattan_distance(a: int, b: int) -> int:
    # 2点間のマンハッタン距離
    d = 0
    for i, j in zip(a, b):
        d += abs(i - j)
    return d


def manhattan_scipy(a: int, b: int) -> int:
    # 2点間のマンハッタン距離(Scipy)
    from scipy.spatial.distance import cityblock
    return cityblock(a, b)


def euclidean_numpy(a: int, b: int) -> float:
    # Numpy編 np.arrayがinput マンハッタン距離はord=1
    from numpy.linalg import norm
    return norm(a - b)
