class pow_calc:
    def __init__(self, base: int, max_exp: int, mod: int = None) -> None:
        self.base = base
        self.max_exp = max_exp
        self.p = [None] * (max_exp + 1)
        if mod is None:
            self.p[0] = base
            for i in range(1, max_exp + 1):
                self.p[i] = self.p[i - 1] * base
        else:
            self.p[0] = base % mod
            for i in range(1, max_exp + 1):
                self.p[i] = self.p[i - 1] * base % mod

    def __call__(self, exp: int):
        # assert 0 <= exp <= self.max_exp
        return self.p[exp]
