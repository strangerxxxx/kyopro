def z_algo(S: str):
    # SとS[i:]の最長共通接頭辞 Z Algorithm
    N = len(S)
    arr = [0] * N
    arr[0] = N
    i, j = 1, 0
    while i < N:
        while i + j < N and S[j] == S[i + j]:
            j += 1
        arr[i] = j
        if not j:
            i += 1
            continue
        k = 1
        while i + k < N and k + arr[k] < j:
            arr[i + k] = arr[k]
            k += 1
        i += k
        j -= k
    return arr
