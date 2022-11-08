import copy


class Node:
    """ノード

    Attributes:
        key (any): ノードのキー。比較可能なものであれば良い。(1, 4)などタプルも可。
        val (any): ノードの値。
        lch (Node): 左の子ノード。
        rch (Node): 右の子ノード。
        bias (int): 平衡度。(左部分木の高さ)-(右部分木の高さ)。
        size (int): 自分を根とする部分木の大きさ
    """

    def __init__(self, key, val):
        self.key = key
        self.val = val
        self.lch = None
        self.rch = None
        self.bias = 0
        self.size = 1


class AVLTree:
    """https://qiita.com/tomato1997/items/2f9b3ac919d230ac68ec
    非再帰AVL木

    Attributes:
        root (Node): 根ノード。初期値はNone。
        default (any): ノード値のデフォルト値。デフォルトではNone。（数値、リストなど可）
        deepcopy (bool): defaultのコピーを深く取るかどうか。デフォルトではFalse（shallow copy）
    """

    def __init__(self, default=None, deepcopy=False):
        self.root = None
        self.default = default
        self.deepcopy = deepcopy

    def rotate_left(self, v):
        u = v.rch
        u.size = v.size
        v.size -= u.rch.size + 1 if u.rch is not None else 1
        v.rch = u.lch
        u.lch = v
        if u.bias == -1:
            u.bias = v.bias = 0
        else:
            u.bias = 1
            v.bias = -1
        return u

    def rotate_right(self, v):
        u = v.lch
        u.size = v.size
        v.size -= u.lch.size + 1 if u.lch is not None else 1
        v.lch = u.rch
        u.rch = v
        if u.bias == 1:
            u.bias = v.bias = 0
        else:
            u.bias = -1
            v.bias = 1
        return u

    def rotateLR(self, v):
        u = v.lch
        t = u.rch
        t.size = v.size
        v.size -= u.size - (t.rch.size if t.rch is not None else 0)
        u.size -= t.rch.size + 1 if t.rch is not None else 1
        u.rch = t.lch
        t.lch = u
        v.lch = t.rch
        t.rch = v
        self.update_bias_double(t)
        return t

    def rotateRL(self, v):
        u = v.rch
        t = u.lch
        t.size = v.size
        v.size -= u.size - (t.lch.size if t.lch is not None else 0)
        u.size -= t.lch.size + 1 if t.lch is not None else 1
        u.lch = t.rch
        t.rch = u
        v.rch = t.lch
        t.lch = v
        self.update_bias_double(t)
        return t

    def update_bias_double(self, v):
        if v.bias == 1:
            v.rch.bias = -1
            v.lch.bias = 0
        elif v.bias == -1:
            v.rch.bias = 0
            v.lch.bias = 1
        else:
            v.rch.bias = 0
            v.lch.bias = 0
        v.bias = 0

    def insert(self, key, val=None):
        """挿入

        指定したkeyを挿入する。valはkeyのノード値。

        Args:
            key (any): キー。
            val (any): 値。（指定しない場合はself.defaultが入る）

        Note:
            同じキーがあった場合は上書きする。
        """
        if val is None:
            if self.deepcopy:
                val = copy.deepcopy(self.default)
            else:
                val = copy.copy(self.default)
        if self.root is None:
            self.root = Node(key, val)
            return

        v = self.root
        history = []
        while v is not None:
            if key < v.key:
                history.append((v, 1))
                v = v.lch
            elif v.key < key:
                history.append((v, -1))
                v = v.rch
            elif v.key == key:
                v.val = val
                return

        p, pdir = history[-1]
        if pdir == 1:
            p.lch = Node(key, val)
        else:
            p.rch = Node(key, val)

        while history:
            v, direction = history.pop()
            v.bias += direction
            v.size += 1

            new_v = None
            b = v.bias
            if b == 0:
                break

            if b == 2:
                u = v.lch
                if u.bias == -1:
                    new_v = self.rotateLR(v)
                else:
                    new_v = self.rotate_right(v)
                break
            if b == -2:
                u = v.rch
                if u.bias == 1:
                    new_v = self.rotateRL(v)
                else:
                    new_v = self.rotate_left(v)
                break

        if new_v is not None:
            if len(history) == 0:
                self.root = new_v
                return
            p, pdir = history.pop()
            p.size += 1
            if pdir == 1:
                p.lch = new_v
            else:
                p.rch = new_v

        while history:
            p, pdir = history.pop()
            p.size += 1

    def _del(self, key):
        v = self.root
        history = []
        while v is not None:
            if key < v.key:
                history.append((v, 1))
                v = v.lch
            elif v.key < key:
                history.append((v, -1))
                v = v.rch
            else:
                res = v.val
                break
        else:
            if self.deepcopy:
                val = copy.deepcopy(self.default)
            else:
                val = copy.copy(self.default)
            return False, val
        if v.lch is not None:
            history.append((v, 1))
            lmax = v.lch
            while lmax.rch is not None:
                history.append((lmax, -1))
                lmax = lmax.rch

            v.key = lmax.key
            v.val = lmax.val

            v = lmax

        c = v.rch if v.lch is None else v.lch

        if history:
            p, pdir = history[-1]
            if pdir == 1:
                p.lch = c
            else:
                p.rch = c
        else:
            self.root = c
            return True, res

        while history:
            new_p = None

            p, pdir = history.pop()
            p.bias -= pdir
            p.size -= 1

            b = p.bias
            if b == 2:
                if p.lch.bias == -1:
                    new_p = self.rotateLR(p)
                else:
                    new_p = self.rotate_right(p)

            elif b == -2:
                if p.rch.bias == 1:
                    new_p = self.rotateRL(p)
                else:
                    new_p = self.rotate_left(p)

            elif b != 0:
                break

            if new_p is not None:
                if len(history) == 0:
                    self.root = new_p
                    return True, res
                gp, gpdir = history[-1]
                if gpdir == 1:
                    gp.lch = new_p
                else:
                    gp.rch = new_p
                if new_p.bias != 0:
                    break

        while history:
            p, pdir = history.pop()
            p.size -= 1

        return True, res

    def delete(self, key):
        """削除

        指定したkeyの要素を削除する。

        Args:
            key (any): キー。

        Return:
            bool: 指定したキーが存在するならTrue、しないならFalse。
        """
        return self._del(key)[0]

    def pop(self, key):
        """削除(pop)

        指定したkeyの要素を削除する。

        Args:
            key (any): キー。

        Return:
            bool: 指定したキーが存在するならそのvalue、しないならdefault。
        """
        return self._del(key)[1]

    def contains(self, key):
        """キーの存在チェック

        指定したkeyがあるかどうか判定する。

        Args:
            key (any): キー。

        Return:
            bool: 指定したキーが存在するならTrue、しないならFalse。
        """
        v = self.root
        while v is not None:
            if key < v.key:
                v = v.lch
            elif v.key < key:
                v = v.rch
            else:
                return True
        return False

    def get(self, key, default=None):
        """値の取り出し

        指定したkeyの値を返す。

        Args:
            key (any): キー。

        Return:
            any: 指定したキーが存在するならそのオブジェクト。存在しなければdefaultを返す。
                 defaultがNoneであればself.defaultをinsertし、self.defaultを返す。
        """
        v = self.root
        while v is not None:
            if key < v.key:
                v = v.lch
            elif v.key < key:
                v = v.rch
            else:
                return v.val
        if default is None:
            if self.deepcopy:
                val = copy.deepcopy(self.default)
            else:
                val = copy.copy(self.default)
            self.insert(key, val)
            return val
        return default

    def lower_bound(self, key):
        """下限つき探索

        指定したkey以上で最小のキーを見つける。[key,inf)で最小。

        Args:
            key (any): キーの下限。

        Return:
            any: 条件を満たすようなキー。そのようなキーが一つも存在しないならNone。

        """
        ret = None
        v = self.root
        while v is not None:
            if v.key >= key:
                if ret is None or ret > v.key:
                    ret = v.key
                v = v.lch
            else:
                v = v.rch
        return ret

    ge = lower_bound

    def gt(self, key):
        ret = None
        v = self.root
        while v is not None:
            if v.key > key:
                if ret is None or ret > v.key:
                    ret = v.key
                v = v.lch
            else:
                v = v.rch
        return ret

    def upper_bound(self, key):
        """上限つき探索

        指定したkey未満で最大のキーを見つける。[-inf,key)で最大。

        Args:
            key (any): キーの上限。

        Return:
            any: 条件を満たすようなキー。そのようなキーが一つも存在しないならNone。

        """
        ret = None
        v = self.root
        while v is not None:
            if v.key < key:
                if ret is None or ret < v.key:
                    ret = v.key
                v = v.rch
            else:
                v = v.lch
        return ret

    lt = upper_bound

    def le(self, key):
        ret = None
        v = self.root
        while v is not None:
            if v.key <= key:
                if ret is None or ret < v.key:
                    ret = v.key
                v = v.rch
            else:
                v = v.lch
        return ret

    def find_kth_element(self, k):
        """小さい方からk番目の要素を見つける。

        Args:
            k (int): 何番目の要素か(0オリジン)。

        Return:
            any: 小さい方からk番目のキーの値。
        """
        v = self.root
        s = 0
        while v is not None:
            t = s + v.lch.size if v.lch is not None else s
            if t == k:
                return v.key
            elif t < k:
                s = t + 1
                v = v.rch
            else:
                v = v.lch
        return None

    def getmin(self):
        """
        Return:
            any: 存在するキーの最小値
        """
        if self.root is None:
            raise IndexError("getmin from empty AVLTree")
        ret = None
        v = self.root
        while True:
            ret = v
            v = v.lch
            if v is None:
                break
        return ret.key

    def getmax(self):
        """
        Return:
            any: 存在するキーの最大値
        """
        if self.root is None:
            raise IndexError("getmax from empty AVLTree")
        ret = None
        v = self.root
        while True:
            ret = v
            v = v.rch
            if v is None:
                break
        return ret.key

    def popmin(self):
        """存在するキーの最小値をpopする。

        Return:
            any: popした値
        """
        if self.root is None:
            raise IndexError("popmin from empty AVLTree")
        ret = None
        v = self.root
        while True:
            ret = v
            v = v.lch
            if v is None:
                break
        return self.pop(ret.key)

    def popmax(self):
        """存在するキーの最大値をpopする。

        Return:
            any: popした値
        """
        if self.root is None:
            raise IndexError("popmax from empty AVLTree")
        ret = None
        v = self.root
        while True:
            ret = v
            v = v.rch
            if v is None:
                break
        return self.pop(ret.key)

    def popkth(self, k):
        """存在するキーの小さい方からk番目をpopする。

        Return:
            any: popした値
        """
        key = self.find_kth_element(k)
        return self.pop(key)

    def get_key_val(self):
        """
        Return:
            dict: 存在するキーとノード値をdictで出力
        """
        return dict(self.items())

    def _sorted_nodes(self):
        if self.root is None:
            return
        q = [(self.root, False)]
        while q:
            node, iscenter = q.pop()
            if iscenter:
                yield node
            else:
                if node.rch is not None:
                    q.append((node.rch, False))
                if node.lch is None:
                    yield node
                else:
                    q.append((node, True))
                    q.append((node.lch, False))

    def keys(self):
        for i in self._sorted_nodes():
            yield i.key

    def values(self):
        for i in self._sorted_nodes():
            yield i.val

    def items(self):
        for i in self._sorted_nodes():
            yield i.key, i.val

    def get_range(self, l, r):
        """keyが[l,r)の範囲を返す。"""
        if self.root is None:
            return
        q = [(self.root, False)]
        while q:
            node, iscenter = q.pop()
            if iscenter:
                yield node.key, node.val
            else:
                if node.rch is not None and node.key < r:
                    q.append((node.rch, False))
                if l <= node.key < r:
                    q.append((node, True))
                if node.lch is not None and node.key >= l:
                    q.append((node.lch, False))

    def __iter__(self):
        return self.keys()

    def __contains__(self, key):
        return self.contains(key)

    def __getitem__(self, key):
        return self.get(key)

    def __setitem__(self, key, val):
        return self.insert(key, val)

    def __delitem__(self, key):
        return self.delete(key)

    def __bool__(self):
        return self.root is not None

    def __len__(self):
        return self.root.size if self.root is not None else 0

    def __str__(self):
        return str(type(self)) + "(" + str(self.get_key_val()) + ")"

    __repr__ = __str__
