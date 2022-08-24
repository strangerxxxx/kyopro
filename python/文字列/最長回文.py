def man(S: str):
    # 最長回文 Manacherのアルゴリズム
    i = 0
    j = 0
    n = len(S)
    R = [0] * n
    while i < n:
        while i - j >= 0 and i + j < n and S[i - j] == S[i + j]:
            j += 1
        R[i] = j
        k = 1
        while i - k >= 0 and i + k < n and k + R[i - k] < j:
            R[i + k] = R[i - k]
            k += 1
        i += k
        j -= k
    return R


def manacher(S):
    # R[2*i] = L: S[i]を中心とする奇数長の最大回文
    # R[2*i+1] = L: S[i:i+2]を中心とする偶数長の最大回文
    C = []
    for a in S:
        C.append(a)
        C.append(0)
    C.pop()

    L = len(C)

    R = [0]*L

    i = j = 0
    while i < L:
        while j <= i < L-j and C[i-j] == C[i+j]:
            j += 1
        R[i] = j
        k = 1
        while j-R[i-k] > k <= i < L-k:
            R[i+k] = R[i-k]
            k += 1
        i += k
        j -= k
    return R
