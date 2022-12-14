def kitamasa(m, k, c, a, mod=998244353):
    # a_(n+k)=c_1*a_(n+k-1)+...+c_k*a_nのときのa_m%MODを返す

    C0 = [0] * k
    C1 = [0] * k
    if m == 0:
        return a[0]
    C0[1] = 1

    p = 32
    while (m >> p) & 1 == 0:
        p -= 1

    D0 = [0] * k
    D1 = [0] * k
    while p:
        p -= 1

        # dbl(k, C0, C1)
        D0[:] = C0[:]
        for j in range(k):
            C1[j] = C0[0] * C0[j] % mod
        for i in range(1, k):
            # inc(k, D0, D1)
            D1[0] = D0[k - 1] * c[0] % mod
            for j in range(k - 1):
                D1[j + 1] = (D0[j] + D0[k - 1] * c[j + 1]) % mod

            for j in range(k):
                C1[j] += C0[i] * D1[j] % mod
            D0, D1 = D1, D0
        for i in range(k):
            C1[i] %= mod
        C0, C1 = C1, C0

        if (m >> p) & 1:
            # inc(k, C0, C1)
            C1[0] = C0[k - 1] * c[0] % mod
            for i in range(k - 1):
                C1[i + 1] = (C0[i] + C0[k - 1] * c[i + 1]) % mod
            C0, C1 = C1, C0

    return sum(C0[i] * a[i] for i in range(k)) % mod
