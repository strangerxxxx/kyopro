import sys
from io import StringIO
from time import perf_counter
import unittest
from verify_method import verify
from testcode import resolve
from testcase_create import createcase


class TestClass(unittest.TestCase):
    def assertIO(self):
        starttime = perf_counter()
        limit = 10
        while perf_counter() - starttime < limit:
            sys.stdout, sys.stdin = StringIO(), StringIO()
            createcase()
            sys.stdout.seek(0)
            input = sys.stdout.read()
            sys.stdout, sys.stdin = StringIO(), StringIO(input)
            resolve()
            sys.stdout.seek(0)
            out = sys.stdout.read()
            sys.stdout, sys.stdin = StringIO(), StringIO(out)
            self.assertTrue(verify(), f"error \ninput is \n'{input}'")
            # インプットとアウトプットを両方入力させるときはこちら
            # sys.stdout, sys.stdin = StringIO(), StringIO(input + out)
            # self.assertTrue(verify(), f"error input is \n'{input}', output is \n'{out}'")

    def test_1(self):
        self.assertIO()


if __name__ == "__main__":
    unittest.main()
