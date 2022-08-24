from decimal import Decimal
from decimal import ROUND_CEILING, ROUND_FLOOR, ROUND_HALF_DOWN, ROUND_HALF_UP

x = Decimal("3.1415")
print(x.sqrt())
print(x.exp())
print(x.ln())
print(x.log10())
print(x ** Decimal("2"))

print(x.quantize(Decimal("0")))
print(x.to_integral_value(ROUND_CEILING))
print(x.to_integral_value(ROUND_FLOOR))
print(x.to_integral_value(ROUND_HALF_UP))
print(x.to_integral_value(ROUND_HALF_DOWN))

print(x.quantize(Decimal("1E-3"), ROUND_HALF_UP))
print(x.quantize(Decimal("1E-3"), ROUND_HALF_DOWN))
