import sys
from io import StringIO
from time import perf_counter
import unittest
from verify_method import verify
from testcode import resolve
from testcase_create import createcase


class TestClass(unittest.TestCase):
    def assertIO(self):
        stdout, stdin = sys.stdout, sys.stdin
        starttime = perf_counter()
        timelimit = 10
        while perf_counter() - starttime < timelimit:
            sys.stdout, sys.stdin = StringIO(), StringIO()
            createcase()
            sys.stdout.seek(0)
            input = sys.stdout.read()
            sys.stdout, sys.stdin = StringIO(), StringIO(input)
            resolve()
            sys.stdout.seek(0)
            out = sys.stdout.read()
            sys.stdout, sys.stdin = StringIO(), StringIO(out)
            self.assertTrue(verify(), "error input is {}".format(input))

    def test_1(self):
        self.assertIO()


if __name__ == "__main__":
    unittest.main()
