from heapq import heapify, heappush, heappop


class Heapq:
    '''
    Heapq()    : 本体qと削除用pの2本のheapqを用意する
    build(a)   : 配列aからプライオリティキューqを構築する
    push(x)    : プライオリティキューにxを追加
    erase(x)   : プライオリティーキューからxを(疑似的に)削除
    clean()    : 削除予定でトップに来た要素をq,pからpop
    pop((exc)) : トップの要素をqからpop (qが空の場合、excを返す)
    top((exc)) : トップの要素の値を取得  (qが空の場合、excを返す)
    '''

    def __init__(self):
        self.q = []
        self.p = []

    def build(self, a):
        self.q = a
        heapify(self.q)

    def push(self, x):
        heappush(self.q, x)

    def erase(self, x):
        heappush(self.p, x)
        self.clean()

    def clean(self):
        while self.p and self.q[0] == self.p[0]:
            heappop(self.q)
            heappop(self.p)

    def pop(self, exc=None):
        self.clean()
        if self.q:
            return heappop(self.q)
        return exc

    def top(self, exc=None):
        self.clean()
        if self.q:
            return self.q[0]
        return exc
