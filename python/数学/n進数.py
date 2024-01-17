def DecimalNumber(n: int, k: int) -> str:
    # nをk進法にする
    orda = ord("A")
    if n == 0:
        return "0"
    bi = []
    while n != 0:
        j = n % abs(k)
        bi.append(str(j) if j < 10 else chr(j - 10 + orda))
        if k < 0:
            n = -(-n // k)
        else:
            n = n // k
    return "".join(reversed(bi))


def ReverseDecimalNumber(n: str, j: int, k: int = 10) -> str:
    # j進法のnをk進法にする
    orda = ord("A")
    if str(n) == "0":
        return "0"
    n = int(str(n), j)
    bi = []
    while n != 0:
        j = n % abs(k)
        bi.append(str(j) if j < 10 else chr(j - 10 + orda))
        if k < 0:
            n = -(-n // k)
        else:
            n = n // k
    return "".join(reversed(bi))


def Base_n_to_10(n: str, j: int) -> int:
    # j進法のnを10進法にする
    out = 0
    m = 1
    x = int(n)
    for i in range(1, len(x) + 1):
        out += int(x[-i]) * m
        m *= j
    return out


def BinNumber(n: int, numberOfDigits: None) -> str:
    if numberOfDigits is None:
        return bin(10)[2:]
    else:
        return bin(n)[2:].zfill(numberOfDigits)


int("30", 5)  # j進法のnを10進法にする
format(30, "b")  # 2進数に変換
format(30, "o")  # 8進数に変換
format(30, "x")  # 16進数(小文字)に変換
format(30, "X")  # 16進数(大文字)に変換
format(30, "010b")  # 2進数の桁揃え
format(30, "b").zfill(10)  # 2進数の桁揃え
