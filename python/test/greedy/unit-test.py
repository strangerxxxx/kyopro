import sys
from io import StringIO
from time import perf_counter
import unittest
from greedy_method import greedy
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
            greedy()
            sys.stdout.seek(0)
            out1 = sys.stdout.read()
            sys.stdout, sys.stdin = StringIO(), StringIO(input)
            resolve()
            sys.stdout.seek(0)
            out2 = sys.stdout.read()
            sys.stdout, sys.stdin = stdout, stdin
            self.assertEqual(out1, out2, "error input is {}".format(input))

    def test_1(self):
        self.assertIO()


if __name__ == "__main__":
    unittest.main()
