def main():
    import os
    import glob
    import sys

    # filepath = os.path.join(os.path.dirname(__file__), "out2.txt")
    scorepath = os.path.join(os.path.dirname(__file__), r"score", "*")
    scores = []
    maxtime = sumtime = 0
    cnt = 0
    for i in glob.glob(scorepath):
        with open(i, "r", encoding="utf-8") as f:
            for line in f.readlines():
                if line.startswith("Score"):
                    x = line.rstrip()[8:]
                    scores.append(int(x))
                if line.startswith("Time"):
                    x = line.rstrip()[7:]
                    maxtime = max(maxtime, float(x))
                    sumtime += float(x)
                    cnt += 1

    print("{:,}".format(sum(scores)))
    print("Sum Score =", "{:,}".format(sum(scores)), file=sys.stderr)
    print("Max Time =", maxtime, file=sys.stderr)
    print("Average Time =", sumtime / max(cnt, 1), file=sys.stderr)


if __name__ == "__main__":
    main()
