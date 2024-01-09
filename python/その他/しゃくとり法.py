def shakutori():
    def conditions() -> bool:
        return cnt + a[right] <= k

    n, k = map(int, input().split())
    a = list(map(int, input().split()))
    ans = 0
    right = 0
    cnt = 0
    for left in range(n):
        while right < n and conditions():  # 条件
            cnt += a[right]  # 処理
            right += 1
            ans = max(ans, right - left)
        cnt -= a[left]  # 逆処理
        if left == right:
            cnt += a[right]  # 追いついたときの処理
            right += 1
    print(ans)
