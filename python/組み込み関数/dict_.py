class safedict(dict):
    # veryfied: https://bit.ly/3hjT5nb
    def __init__(self, missing=None):
        import random

        self.r = random.randrange(1 << 63)
        self.missing = missing

    def __contains__(self, key: object) -> bool:
        return super().__contains__(key ^ self.r)

    def __getitem__(self, key):
        return super().__getitem__(key ^ self.r)

    def __setitem__(self, key, value):
        return super().__setitem__(key ^ self.r, value)

    def __delitem__(self, key):
        return super().__delitem__(key ^ self.r)

    def get(self, key, default=None):
        return super().get(key ^ self.r, default)

    def setdefault(self, key, default=None):
        return super().setdefault(key ^ self.r, default)

    def pop(self, key, default=None):
        return super().pop(key ^ self.r, default)

    def __missing__(self, key):
        if self.missing is not None:
            return self.missing
        return super().__missing__(key)
