import subprocess
import time

def run_test():
    start = time.time()
    subprocess.run(["dune", "exec", "g3/main.exe"], check=True)
    end = time.time()
    runtime = end - start
    with open("log.txt", "a") as f:
        f.write(f"Runtime: {runtime:.4f}s\n")

if __name__ == "__main__":
    run_test()
