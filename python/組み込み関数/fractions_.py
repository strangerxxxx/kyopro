import math


class Fraction:
    def __init__(self, a: int = 0, b: int = 1) -> None:
        if isinstance(a, Fraction):
            self.a, self.b = a.a, a.b
            return
        a, b = int(a), int(b)
        if b == 0:
            raise ZeroDivisionError(f"{a}/{b}")
        if b < 0:
            a, b = -a, -b
        self.a, self.b = a, b
        self._reducion()

    def _reducion(self):
        g = math.gcd(self.a, self.b)
        self.a //= g
        self.b //= g

    def __add__(self, other):
        if isinstance(other, Fraction):
            g = math.gcd(self.b, other.b)
            x = other.b // g * self.a
            y = self.b // g * other.a
            return Fraction(x + y, self.b // g * other.b)
        return Fraction(self.a + other * self.b, self.b)

    def __iadd__(self, other):
        if isinstance(other, Fraction):
            g = math.gcd(self.b, other.b)
            self.a *= other.b // g
            self.a += self.b // g * other.a
            self.b *= other.b // g
        else:
            self.a += other * self.b
        self._reducion()
        return self

    __radd__ = __add__

    def __sub__(self, other):
        if isinstance(other, Fraction):
            return self.__add__(-other)
        return self.__add__(-other)

    def __isub__(self, other):
        if isinstance(other, Fraction):
            return self.__iadd__(-other.a, other.b)
        return self.__iadd__(-other)

    def __rsub__(self, other):
        return -self + other

    def __mul__(self, other):
        if isinstance(other, Fraction):
            return Fraction(self.a * other.a, self.b * other.b)
        else:
            return Fraction(self.a * other, self.b)

    def __imul__(self, other):
        if isinstance(other, Fraction):
            self.a *= other.a
            self.b *= other.b
        else:
            self.a *= other
        self._reducion()
        return self

    __rmul__ = __mul__

    def __floordiv__(self, other):
        if isinstance(other, Fraction):
            return self.__mul__(other.inverse())
        return Fraction(self.a, self.b * other)

    def __ifloordiv__(self, other):
        if isinstance(other, Fraction):
            return self.__imul__(other.inverse())
        self.b *= other
        self._reducion()
        return self

    def __rfloordiv__(self, other):
        return self.inverse() * other

    __truediv__ = __floordiv__
    __itruediv__ = __ifloordiv__
    __rtruediv__ = __rfloordiv__

    def __pow__(self, other):
        if isinstance(other, Fraction):
            if other.b == 1:
                return self.__pow__(other.a)
            raise NotImplementedError
        return Fraction(self.a**other, self.b**other)

    def __ipow__(self, other):
        if isinstance(other, Fraction):
            if other.b == 1:
                return self.__ipow__(other.a)
            raise NotImplementedError
        self.a **= other
        self.b **= other
        return self

    def __rpow__(self, other):
        if self.b != 1:
            raise NotImplementedError
        return other**self.a

    def __floor__(self) -> int:
        return self.a // self.b

    def __ceil__(self) -> int:
        return (self.a + self.b - 1) // self.b

    __int__ = __floor__

    def __float__(self):
        return self.a / self.b

    def inverse(self):
        if self.a == 0:
            raise ZeroDivisionError(f"try to calcuate inverse of {self.a}/{self.b}")
        return Fraction(self.b, self.a)

    def __pos__(self):
        return Fraction(self.a, self.b)

    def __neg__(self):
        return Fraction(-self.a, self.b)

    def __abs__(self):
        return Fraction(abs(self.a), self.b)

    def __eq__(self, other) -> bool:
        if isinstance(other, Fraction):
            return self.a == other.a and self.b == other.b
        return self.a == self.b * other

    def __gt__(self, other):
        if isinstance(other, Fraction):
            return self.a * other.b > other.a * self.b
        return self.a > self.b * other

    def __ge__(self, other):
        if isinstance(other, Fraction):
            return self.a * other.b >= other.a * self.b
        return self.a >= self.b * other

    def __lt__(self, other):
        if isinstance(other, Fraction):
            return self.a * other.b < other.a * self.b
        return self.a < self.b * other

    def __le__(self, other):
        if isinstance(other, Fraction):
            return self.a * other.b <= other.a * self.b
        return self.a <= self.b * other

    def __str__(self) -> str:
        return f"{self.a}/{self.b}"

    __repr__ = __str__

    def __hash__(self) -> int:
        return hash(self.__str__())
