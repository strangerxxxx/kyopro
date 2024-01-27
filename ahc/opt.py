import statistics
import os
import joblib
import time
import optuna
import subprocess


n_parallel = 10
n_files = 100
home = os.path.expanduser("~")
workdir = os.path.join(home, "Documents", "Optuna")
os.makedirs(workdir, exist_ok=True)


def calc_score_each(seed: int, *args):
    in_file = f"in/{seed:04}.txt"
    out_file = os.path.join(workdir, f"out{seed:04}.txt")
    # print(in_file, out_file)
    subprocess.run(
        f"ahc000_a.exe " + " ".join(map(str, args)) + f" < {in_file}",
        shell=True,
        encoding="utf8",
        capture_output=True,
    )
    out = subprocess.run(
        f"vis.exe {in_file} {out_file}",
        capture_output=True,
        text=True,
        shell=True,
        encoding="utf8",
    )
    stdout = out.stdout
    # print(f"{stdout=}")
    for i in stdout.split("\n"):
        if i.startswith("Score = "):
            return int(i.rstrip()[8:])
    return 0


def calc_scores(*args):
    scores = joblib.Parallel(n_jobs=n_parallel)(
        joblib.delayed(calc_score_each)(i, *args) for i in range(n_files)
    )
    return scores


def objective(trial: optuna.trial.Trial):
    start = time.time()
    # https://optuna.readthedocs.io/en/stable/reference/generated/optuna.trial.Trial.html
    args = (
        trial.suggest_int("arg1", 1000, 100000000, log=True),
        trial.suggest_int("arg2", 1, 10000, log=True),
    )
    scores = calc_scores(*args)
    print(f"elapsed: {time.time() - start}")
    return statistics.mean(scores)


if __name__ == "__main__":
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    subprocess.run(
        "g++ ahc000_a.cpp -o ahc000_a -std=gnu++2b -Wno-unknown-pragmas -Wall -Wextra -O2 -I ."
    )

    study = optuna.create_study(
        direction="maximize",
        storage="sqlite:///optuna.db",
        study_name="ahc000",
        load_if_exists=True,
    )
    study.optimize(objective, n_trials=1000, timeout=600)
    print(study.best_trial)
    print(study.best_params)
