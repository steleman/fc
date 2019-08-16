import os
import sys
import subprocess

test_dir = "./tests/" # will be removed regardless of the result
failed_dir = "./failed" # will be kept iff there are failed test

reference_fc = "gfortran"
testing_fc = "fc"
diff_tool = "diff"

reference_fc_exe = "./" + reference_fc + ".exe"
testing_fc_exe = "./" + testing_fc + ".exe"

def cleanup():
    os.system("rm -rf " + test_dir + " &> /dev/null")
    os.system("rm -rf __pycache__ &> /dev/null")
    os.system("rm *.exe &> /dev/null")

def log_src(src):
    print('\033[92m---Start of source---\033[0m', flush = True)
    os.system(f"cat {src}")
    print('\033[92m---End of source---\033[0m', flush = True)
    os.system(f"mkdir {failed_dir} &> /dev/null")
    os.system(f"cp {src} {failed_dir}")


def inform_failed():
    print(f"Find the test case in the {failed_dir}(Sometimes " +
            "cat'ing the source trims some lines beyond screen " +
            "(maybe my tmux issue ?))", flush = True)

if len(sys.argv) == 5:
    reference_fc = sys.argv[1]
    testing_fc = sys.argv[2]
    diff_tool = sys.argv[3]
    failed_dir = sys.argv[4]

num_tests = 10

# cd into this script's directory
my_abspath = os.path.abspath(__file__)
my_dname = os.path.dirname(my_abspath)
os.chdir(my_dname)

cleanup()
os.mkdir(test_dir)

print("fort-test-gen\n-------------", flush = True)
print(f"Testing {testing_fc} with {reference_fc} by {num_tests} "+
    f"auto-generated tests with the help of {diff_tool}", flush = True)

# we want to run the main script separately so that we can do the cleanup() here
# in case it assert-fails. So this script must always exit by a cleanup()
for i in range(num_tests):
# TODO: We need some sort of progress indicator here. For some reason CMake
# invoking this script is not making that easy, ie. the stdout/err behaves
# weird.
    os.system("python ./fortran_test_gen.py autoGen" + str(i) + " " + test_dir)
    src = test_dir + "autoGen" + str(i) + ".f90"

    if os.system(reference_fc + " -o " + reference_fc_exe + " " + src + "&> /dev/null") == 0:
        if subprocess.call([testing_fc, "-o", testing_fc_exe, src]) != 0:
            log_src(src)
            sys.stderr.write(testing_fc + "\033[91m compilation failed " +
                    "\033[0m\n")
            sys.stderr.flush()
            cleanup()
            inform_failed()
            sys.exit(1)

        if os.system(f"/bin/bash -c \"{diff_tool} <({reference_fc_exe}) " +
                f"<({testing_fc_exe})\"") != 0:
            log_src(src)
            sys.stderr.write(testing_fc + "\033[91m producing different " +
                    "ouput \033[0mfrom " + reference_fc + "\n")
            sys.stderr.flush()
            cleanup()
            inform_failed()
            sys.exit(1)

cleanup()
os.system("\033[92mHooray! All generated test cases passed!\033[0m")
os.system(f"rm -rf {failed_dir} &> /dev/null")
sys.exit(0)
