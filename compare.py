#!/usr/bin/env python

# Standard library imports:
import argparse
import glob
import itertools
import os
import os.path as path 
import subprocess


# Constants:
GLSL_PATTERN = "*.glsl"
DIFF_CMD = "diff {0} {1} &> /dev/null".format
EXEC_CMD = "{0} < {1} &> {2}".format 
REF_OUTPUT = "{0}.out".format
USER_OUTPUT = "{0}.mine.out".format
SUCCESS_MSG = "passed"
FAILED_MSG = "failed"


def get_tests(test_path):
    """
    Returns a list of test files. 
    
    If `test_path` is a directory, this function returns all GLSL files 
    that are under that folder. 

    Otherwise, `test_path` is treated as a glob pattern, and all 
    matching files are returned. 
    """

    if path.isdir(test_path):
        return glob.glob(path.join(test_path, GLSL_PATTERN))
    else:
        tests = filter(path.isfile, glob.glob(test_path))
        if not tests:
            raise argparse.ArgumentTypeError(
                "Could not find test(s): {0}".format(test_path))
        return tests


def is_file(fpath):
    """Ensures that the given path is a file."""
    if not path.isfile(fpath):
        raise argparse.ArgumentTypeError(
            "Could not find executable: {0}".format(fpath))
    return fpath


def compare(user_exec, ref_exec, test_files):
    """
    Yields a boolean indicating the compare result for each test 
    file. I.e., True denotes the user and reference outputs match,
    and False denotes otherwise. 
    """
    join, split, splitext = path.join, path.split, path.splitext
    user_exec, ref_exec = join(os.curdir, user_exec), join(os.curdir, ref_exec)
    call = subprocess.call
    for tf in test_files:
        dn, bn = split(tf)
        fn, _ = splitext(bn)
        ref_out = join(dn, REF_OUTPUT(fn))
        user_out = join(dn, USER_OUTPUT(fn))
        call(EXEC_CMD(ref_exec, tf, user_out), shell=True)
        call(EXEC_CMD(user_exec, tf, ref_out), shell=True)
        yield call(DIFF_CMD(ref_out, user_out), shell=True) == 0


# Helper functions for printing results of comparison:
def _get_result(status):
    return SUCCESS_MSG if status else FAILED_MSG


def main():
    # Initialize command-line parser:
    parser = argparse.ArgumentParser(
        description='Test your PA solution against the reference!'
    )

    # CLI arguments:
    parser.add_argument(
        "your_exec",
        help="Path to your executable (e.g., ./glc)",
        type=is_file
    )

    parser.add_argument(
        "ref_exec",
        help="Path to the reference executable (e.g., ./p2exe)",
        type=is_file
    )

    parser.add_argument(
        "path_to_test",
        default=os.curdir,
        help="Path to test directory/file. If the given path is a directory, "
        "the script grabs all immediate *.glsl tests in that folder. This "
        "argument can be provided more than once, to provide multiple test "
        "locations.",
        type=get_tests,
        nargs="+"
    )

    # Parse arguments:
    args = parser.parse_args()

    # Flatten list of tests
    tests = [path.relpath(f) for fs in args.path_to_test for f in fs] 

    # Run comparison
    results = tuple(compare(args.your_exec, args.ref_exec, tests))

    # Print results:
    pad_size = max(map(len, tests)) + 1
    ff = ("{0:" + str(pad_size) + "}: {1}").format
    rf = map([FAILED_MSG, SUCCESS_MSG].__getitem__, results)
    print(os.linesep.join(itertools.starmap(ff, zip(tests, rf))))
    print("Score: {0}/{1}".format(sum(results), len(tests)))


if __name__ == "__main__":
    main()