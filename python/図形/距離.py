def distance(a, b) -> float:
    # 2点間のユークリッド距離
    return sum((i - j) ** 2 for i, j in zip(a, b)) ** 0.5


def euclidean_scipy(a, b) -> float:
    # 2点間のユークリッド距離
    from scipy.spatial.distance import euclidean
    return euclidean(a, b)


def manhattan_distance(a, b) -> int:
    # 2点間のマンハッタン距離
    return sum(abs(i - j) for i, j in zip(a, b))


def manhattan_scipy(a, b) -> int:
    # 2点間のマンハッタン距離(Scipy)
    from scipy.spatial.distance import cityblock
    return cityblock(a, b)


def euclidean_numpy(a, b) -> float:
    # Numpy編 np.arrayがinput マンハッタン距離はord=1
    from numpy.linalg import norm
    return norm(a - b)
