class Modint(int):
    mod = 998244353

    def modpow(self, i: int, r: int) -> int:
        return pow(i, r, Modint.mod)

    def modinv(self, i: int, r: int = 1) -> int:
        return pow(i, Modint.mod - 1 - r, Modint.mod)

    def __add__(self, other):
        return Modint(int.__add__(self, other) % Modint.mod)

    def __sub__(self, other):
        return Modint(int.__sub__(self, other) % Modint.mod)

    def __mul__(self, other):
        return Modint(int.__mul__(self, other) % Modint.mod)

    def __floordiv__(self, other):
        return Modint(int.__mul__(self, self.modinv(other)) % Modint.mod)

    __truediv__ = __floordiv__

    def __pow__(self, other):
        return Modint(self.modpow(self, other))

    def __radd__(self, other):
        return Modint(int.__add__(other, self) % Modint.mod)

    def __rsub__(self, other):
        return Modint(int.__sub__(other, self) % Modint.mod)

    def __rmul__(self, other):
        return Modint(int.__mul__(other, self) % Modint.mod)

    def __rfloordiv__(self, other):
        return Modint(int.__mul__(other, self.modinv(self)) % Modint.mod)

    __rtruediv__ = __rfloordiv__

    def __rpow__(self, other):
        return Modint(self.modpow(other, self))

    def __iadd__(self, other):
        self = self.__add__(other)
        return self

    def __isub__(self, other):
        self = self.__sub__(other)
        return self

    def __imul__(self, other):
        self = self.__mul__(other)
        return self

    def __ifloordiv__(self, other):
        self = self.__mul__(self.modinv(other))
        return self

    __itruediv__ = __ifloordiv__

    def __ipow__(self, other):
        self = Modint(self.modpow(self, other))
        return self


class ModInt:
    mod = 998244353

    def __init__(self, v: int = 0) -> None:
        self.val = int(v) % ModInt.mod

    def setMod(self, mod):
        Modint.mod = mod

    def modpow(self, i: int, r: int) -> int:
        return pow(i, r, ModInt.mod)

    def modinv(self, i: int, r: int = 1) -> int:
        return pow(i, ModInt.mod - 1 - r, ModInt.mod)

    def __add__(self, other):
        if isinstance(other, ModInt):
            res = self.val + other.val
        else:
            res = self.val + other
        return ModInt(res)

    def __sub__(self, other):
        if isinstance(other, ModInt):
            res = self.val - other.val
        else:
            res = self.val - other
        return ModInt(res)

    def __mul__(self, other):
        if isinstance(other, ModInt):
            res = self.val * other.val
        else:
            res = self.val * other
        return ModInt(res)

    def __floordiv__(self, other):
        if isinstance(other, ModInt):
            res = self.val * self.modinv(other.val)
        else:
            res = self.val * self.modinv(other)
        return ModInt(res)

    def __pow__(self, other):
        # assert isinstance(other, int) and other >= 0
        return ModInt(self.modpow(self.val, other))

    def __radd__(self, other):
        return ModInt(other + self.val)

    def __rsub__(self, other):
        return ModInt(other - self.val)

    def __rmul__(self, other):
        return ModInt(other * self.val)

    def __rfloordiv__(self, other):
        return ModInt(other * self.modinv(self.val))

    def __rpow__(self, other):
        return ModInt(self.modpow(other, self.val))

    def __iadd__(self, other):
        if isinstance(other, ModInt):
            self.val += other.val
        else:
            self.val += other
        self.val %= ModInt.mod
        return self

    def __isub__(self, other):
        if isinstance(other, ModInt):
            self.val -= other.val
        else:
            self.val -= other
        self.val %= ModInt.mod
        return self

    def __imul__(self, other):
        if isinstance(other, ModInt):
            self.val *= other.val
        else:
            self.val *= other
        self.val %= ModInt.mod
        return self

    def __ifloordiv__(self, other):
        if isinstance(other, ModInt):
            self.val *= self.modinv(other.val)
        else:
            self.val *= self.modinv(other)
        self.val %= ModInt.mod
        return self

    def __ipow__(self, other):
        # assert isinstance(other, int) and other >= 0
        self.val = self.modpow(self.val, other)
        return self

    def __eq__(self, other) -> bool:
        if isinstance(other, ModInt):
            return self.val == other.val
        return self.val == other % ModInt.mod

    def __ne__(self, other) -> bool:
        if isinstance(other, ModInt):
            return self.val != other.val
        return self.val != other % ModInt.mod

    def __lt__(self, other) -> bool:
        if isinstance(other, ModInt):
            return self.val < other.val
        return self.val < other % ModInt.mod

    def __gt__(self, other) -> bool:
        if isinstance(other, ModInt):
            return self.val > other.val
        return self.val > other % ModInt.mod

    def __and__(self, other):
        if isinstance(other, ModInt):
            return self.val & other.val
        if not 0 <= other < ModInt.mod:
            other %= ModInt.mod
        return ModInt(self.val & other)

    def __or__(self, other):
        if isinstance(other, ModInt):
            return self.val | other.val
        if not 0 <= other < ModInt.mod:
            other %= ModInt.mod
        return ModInt(self.val | other)

    def __xor__(self, other):
        if isinstance(other, ModInt):
            return self.val ^ other.val
        if not 0 <= other < ModInt.mod:
            other %= ModInt.mod
        return ModInt(self.val ^ other)

    def __iand__(self, other):
        if isinstance(other, ModInt):
            self.val &= other.val
        else:
            self.val &= other % ModInt.mod
        return self

    def __ior__(self, other):
        if isinstance(other, ModInt):
            self.val |= other.val
        else:
            self.val |= other % ModInt.mod
        return self

    def __ixor__(self, other):
        if isinstance(other, ModInt):
            self.val ^= other.val
        else:
            self.val ^= other % ModInt.mod
        return self

    def __str__(self) -> str:
        return str(self.val)

    __repr__ = __str__

    def __int__(self) -> int:
        return self.val
