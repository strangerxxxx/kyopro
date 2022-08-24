# ref:https://judge.yosupo.jp/submission/54727
class FullyIndexableDictionary():
    """完備辞書 FID ビット列B[0..n)に対し、次の操作が可能
    access(B,idx):B[idx]を返す
    rank(B,pos,b):B[0..pos)中のbの出現回数を返す
    select(B,k,b):k番目(0-indexed)のbの出現位置を返す
    (ざっくり言うと、ビット列を2**32進数表記で持って累積和
    32個単位でビットを持つからその分だけ累積和の空間量を節約可)"""

    def __init__(self, n: int) -> None:
        self.n = n  # 元のビット列Bの長さ
        self.blocknum = (self.n+31) >> 5  # 実際に持つ配列bitの長さ
        self.bit = [0] * self.blocknum  # Bを32bit整数に変換して持つ
        self.cum = [0] * self.blocknum  # その累積和

    def _popcount(self, x: int) -> int:  # xは32bit整数
        # https://qiita.com/zawawahoge/items/8bbd4c2319e7f7746266
        # 0x5=0b0101 0x3=0b0011 0x3f=0b111111(0b100000==32)
        x = x - ((x >> 1) & 0x55555555)  # 2bit毎に数える 以下4,8,16,32
        x = (x & 0x33333333) + ((x >> 2) & 0x33333333)
        x = (x + (x >> 4)) & 0x0f0f0f0f
        x = x + (x >> 8)
        x = x + (x >> 16)
        return x & 0x0000003f

    def set(self, idx: int) -> None:
        """B[idx]に1を入れる O(1)"""
        # bit[i]↔B[32*i:32*(i+1)] idx=(idx>>5)*32+(idx&31)
        self.bit[idx >> 5] |= 1 << (idx & 31)

    def build(self) -> None:
        """累積和を計算 O(n/32)"""
        for i in range(1, self.blocknum):
            self.cum[i] = self.cum[i - 1] + self._popcount(self.bit[i - 1])

    def access(self, idx: int) -> int:  # 0 or 1
        """B[idx]を返す O(1)"""
        return (self.bit[idx >> 5] >> (idx & 31)) & 1

    def rank(self, pos: int, b: int) -> int:
        """B[0..pos)中のb(0 or 1)の出現回数を返す O(1)"""
        r = self.cum[pos >> 5] + \
            self._popcount(self.bit[pos >> 5] & ((1 << (pos & 31)) - 1))
        return r if b else pos - r

    def select(self, k: int, b: int) -> int:
        """k番目(0-indexed)のbの出現位置を返す O(log(n))"""
        assert 0 <= k < self.rank(self.n, b)
        left, right = 0, self.n  # 二分探索
        while right - left > 1:
            mid = (left + right) // 2
            if self.rank(mid, b) >= k + 1:
                right = mid
            else:
                left = mid
        return left


class WaveletMatrix:
    """https://www.slideshare.net/pfi/ss-15916040 後半を見る事
    07214367 25047263 元の整数列arr
    01001011 01011010 0b0?? or 0b1??
           ↙↘
    02132023 74675476
    """

    def __init__(self, arr: list) -> None:
        log = len(bin(max(arr)))-2
        assert max(arr) < 2**log
        self.n = len(arr)  # 整数列(値域[0,m)])の長さ
        self.logs = log  # 木の高さ arrの上限mのlog_2
        self.mat = [None] * log  # 各要素はFID
        self.nzd = [None] * log  # 振り分けの境目 mat[i]の0の数
        self._build(arr)

    def _build(self, arr: list) -> None:
        """時間計算量 O(nlogm)"""
        for d in range(self.logs)[::-1]:  # 何桁目のbitに従って振り分けるか
            self.mat[d] = FullyIndexableDictionary(self.n + 1)
            leftside = []  # WaveletTreeと同じ節点が存在するが
            rightside = []  # 必ずしも子が自分の直下には存在しない
            for i, a in enumerate(arr):
                if (a >> d) & 1:
                    self.mat[d].set(i)
                    rightside.append(a)
                else:
                    leftside.append(a)
            self.mat[d].build()
            self.nzd[d] = len(leftside)  # =self.mat[d].rank(self.n,0)
            arr = leftside + rightside

    def access(self, idx: int) -> int:
        """arr[idx]を返す O(logm)"""
        ret = 0
        for d in range(self.logs)[::-1]:
            b = self.mat[d].access(idx)
            idx = self.mat[d].rank(idx, b)
            if b:
                ret |= b << d  # d桁目のbを復元
                idx += self.nzd[d]
        return ret

    def rank(self, pos: int, c: int) -> int:  # スライドp.60
        """arr[0..pos)中のcの出現回数を返す O(logm)"""
        l, r = 0, pos
        for d in range(self.logs)[::-1]:
            b = (c >> d) & 1  # cのlevel番目のbit
            l = self.mat[d].rank(l, b)+b*self.nzd[d]
            r = self.mat[d].rank(r, b)+b*self.nzd[d]
        return r - l

    def range_rank(self, c: int, l: int = 0, r: int = None) -> int:
        """arr[l..r)中のcの出現回数を返す O(logm)"""
        if r is None:
            r = self.n
        assert l < r
        return self.rank(r, c)-self.rank(l, c)

    def select(self, c: int, i: int) -> int:
        """i番目(0-indexed)のcの出現位置を返す O(logm)"""
        ret = 0
        for d in range(self.logs)[::-1]:
            b = (c >> d) & 1
            ret = self.mat[d].rank(ret, b)+b*self.nzd[d]
        ret += i  # i番目のcの、一番下のbit列における出現位置
        for d in range(self.logs):
            b = (c >> d) & 1
            ret = self.mat[d].select(ret - b*self.nzd[d], b)
        return ret

    def kth_smallest(self, k: int, l: int = 0, r: int = None) -> int:
        """arr[l..r)のk番目(0-indexed)に小さい値を返す O(logm)"""
        if r is None:
            r = self.n
        ret = 0
        for d in range(self.logs)[::-1]:
            cnt = self.mat[d].rank(r, 0) - self.mat[d].rank(l, 0)
            if cnt <= k:  # cntは区間内の0の出現回数
                ret |= 1 << d
                k -= cnt
                l = self.mat[d].rank(l, 1) + self.nzd[d]
                r = self.mat[d].rank(r, 1) + self.nzd[d]
            else:
                l = self.mat[d].rank(l, 0)
                r = self.mat[d].rank(r, 0)
        return ret

    def kth_largest(self, k: int, l: int = 0, r: int = None) -> int:
        """arr[l..r)のk番目(0-indexed)に大きい値を返す O(logm)"""
        if r is None:
            r = self.n
        return self.kth_smallest(l, r, r-l-k-1)

    def _range_freq(self, z: int, l: int = 0, r: int = None) -> int:
        if r is None:
            r = self.n
        ret = 0
        sz, ez = l, r
        for d in range(self.logs)[::-1]:
            sz0 = self.mat[d].rank(sz, 0)
            ez0 = self.mat[d].rand(ez, 0)
            if (z >> d) & 1:  # (a>>d)%1==0なるaはzより小さくかつ左に流れる
                ret += (ez0-sz0)  # 左に流れる数
                sz += (self.nzd[d]-sz0)  # 自身より右側にある0の数
                ez += (self.nzd[d]-ez0)  # これらも左に流れるのでidxが増える
            else:
                sz = sz0
                ez = ez0
        return ret

    def range_freq(self, l: int, r: int, x: int, y: int) -> int:
        """arr[l..r)の[x..y)内の値の個数 O(logm)"""
        assert l < r
        return self._range_freq(l, r, y)-self._range_freq(l, r, x)

    def range_list(self, l: int, r: int, x: int, y: int) -> list:
        """range_freqを(値、個数)のリスト形式にして返す O((y-x)logm)
        計算量最善か分からない 参照した文献なし"""
        ret = []
        for z in range(x, y):
            cnt = self.range_rank(l, r, z)
            if cnt != 0:
                ret.append(z, cnt)
        return ret
