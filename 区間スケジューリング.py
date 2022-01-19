def scheduling():
    n = int(input())
    a = sorted([tuple(map(int, input().split())) for _ in range(n)],
               key=lambda x: x[1])
    index = -1
    ans = 0
    for i, j in a:
        if i <= index:
            continue
        ans += 1
        index = j - 1
    print(ans)
