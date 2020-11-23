#!/usr/bin/env bash

set -euo pipefail

#-------------------------------------------------------------------------------
# Configuration: which tests to run
#-------------------------------------------------------------------------------

# A table of words as a Bash array
#
# Each row is <number of invocations per physical core> <suite> <test group
# pattern> <number of tests per invocation>.
#
# INVARIANT: No whitespace in any word.
#
# We only test in multiples of 100 because otherwise it risks skewing the
# QuickCheck generator distribution (note that QuickCheck sets `maxSize stdArgs
# = 100`).
#
# We transition the ratios from few invocations-with-several-tests to several
# invocations-with-few-tests; this improves the batch's worst-case CPU
# utilization, since it makes it more likely that a small invocation will be
# the last to finish. Invocations-with-several-tests have slightly less
# overhead and also more reliable percentages in their QuickCheck statistics.
rows=(
    # From the slowest individual invocation ...
    '1 Cardano Cardano        1500'  # ~95 minutes per invocation
    '2 Shelley Shelley        2000'  # ~37 minutes per invocation
    '2 Cardano ShelleyAllegra  400'  # ~37 minutes per invocation
    '5 Cardano Cardano         200'  # ~12 minutes per invocation
    # ... to fastest individual invocation
    #
    # And the number of invocations is non-decreasing.
)

# The test suite names as a string, one name per line
listSuiteNames () {
    for row in "${rows[@]}"; do
        cols=($row)   # parse the row as another Bash array
        echo ${cols[1]}
    done
}
suites=$(listSuiteNames | sort -u)

# The test suite-group name pairs as a string, one pair per line
listSuiteGroupNames () {
    for row in "${rows[@]}"; do
        cols=($row)   # parse the row as another Bash array
        echo ${cols[1]}-${cols[2]}
    done
}
suitegroups=$(listSuiteGroupNames | sort -u)

#-------------------------------------------------------------------------------
# Script robustness
#-------------------------------------------------------------------------------

# Select the most prevalent UTF-8 locale.
#
# Exported so GNU parallel workers can see it.
export LC_ALL=en_US.utf8

# Where to accumulate temporary files
#
# See https://unix.stackexchange.com/a/84980
#
# INVARIANT: This path is absolute.
absTmpDir=$(mktemp -d 2>/dev/null || mktemp -d -t "slow-ThreadNet-tests")

# Leave a breadcrumb in case something goes horribly wrong
echo ${BUILDKITE_JOB_ID:-local} \
    1>"${absTmpDir}/ouroboros-consensus-nightly-slow-ThreadNet-tests-BuildKiteJobId.txt"

# Where to accumulate log files
#
# Exported so GNU parallel workers can see it.
#
# INVARIANT: This path is absolute.
export logAbsDir="${absTmpDir}/logs"
mkdir -p "$logAbsDir"

# Where to accumulate Nix symlinks
#
# Exported so GNU parallel workers can see it.
#
# INVARIANT: This path is absolute.
export nixAbsDir="${absTmpDir}/nixSymlinks"
mkdir -p "$nixAbsDir"

# Execute a {}-parameterized command for each found file, in sorted order.
forEachFile () {
    find "$1" -type f -name "$2" -print0 | sort -z | xargs -r0 -I{} sh -c "$3"
}

# Always do these final steps
finish () {
    # Don't abort during this exception handler
    set +e

    # Collect each suite-group pairs's logs into one artifact file
    for suitegroup in $suitegroups; do
        forEachFile "$logAbsDir" "*-${suitegroup}.log" \
            'echo "==> {} <=="; cat "{}"; echo;' \
            1>"${logAbsDir}/${suitegroup}-artifact.log"
    done

    if [ "true" = "${BUILDKITE-}" ]
    then
        # Upload the artifact files as BuildKite artifacts
        #
        # TODO how to specify the charset: UTF-8? The browser renders the
        # artifact poorly when it's clicked in the UI. BuildKite v3 might allow
        # it via `--content-type`, but it seems we're still running v2, which
        # doesn't.
        buildkite-agent artifact upload "${logAbsDir}/**/*-artifact.log"
    else
        # We're running locally, so move the artifact files to the current
        # directory instead of uploading them.
        forEachFile "$logAbsDir" "*-artifact.log" \
            'echo Moving artifact $(basename "{}") to .; mv {} .'
    fi

    # Clean-up temporary files
    rm -rf "$absTmpDir"
}
trap finish EXIT

#-------------------------------------------------------------------------------
# Prepare the exes
#-------------------------------------------------------------------------------

# The directory containing the repository's default.nix
nixdir="$(dirname $0)/.."

# Ensure the invocations below of Nix-built exes see and use the desired locale
#
# See https://nixos.org/nixpkgs/manual/#locales
nix build -f "$nixdir" nightly-checks.glibcLocales -o "${nixAbsDir}/glibcLocales"
export LOCALE_ARCHIVE="${nixAbsDir}/glibcLocales"/lib/locale/locale-archive

# We use GNU parallel to manage multiple processes
#
# This BuildKite job runs on the benchmarking BuildKite queue, which means
# it'll be running alone on the machine. Thus, we want to keep the CPU
# saturated.
nix build -f "$nixdir" nightly-checks.gnuparallel -o "${nixAbsDir}/parallel"

# Appeasement
echo 'will cite' | \
    "${nixAbsDir}/parallel/bin/parallel" --citation 1>/dev/null 2>&1 || true

# Build/fetch the exes that run the ThreadNet tests
for suite in $suites; do
    nix build -f "$nixdir" "nightly-checks.$suite" -o "${nixAbsDir}/${suite}"
done

#-------------------------------------------------------------------------------
# Run the exes
#-------------------------------------------------------------------------------

# The command that runs a single test suite
#
# GNU parallel invokes this multiple times during this script.
innerCommand () {
    uniqueInvocationId="$(printf %03d $PARALLEL_SEQ)"
    suite=$1
    group=$2
    n=$3

    logfile="${logAbsDir}/${uniqueInvocationId}-${suite}-${group}.log"

    # Run the specified tests with the nightly flag set
    "${nixAbsDir}/${suite}/bin/test" \
        --pattern "$group ThreadNet" \
        --quickcheck-tests=$n \
        --iohk-enable-nightly-tests \
        1>"$logfile" 2>&1

    xc=$?

    # Notify the user
    #
    # Likely atomic, since it's almost surely less than PIPE_BUF.
    #
    # https://arto.s3.amazonaws.com/notes/posix#pipe-buf
    echo Completed Invocation-${uniqueInvocationId}, $suite $group ${n}: \
        $(tail -n1 "$logfile")

    return $xc
}
# Exported so GNU parallel workers can see it.
export -f innerCommand

# The number of *physical* cores on this machine
#
# We avoid HyperThreads et al, at least for now.
numCores=$("${nixAbsDir}/parallel/bin/parallel" --number-of-cores)

# Generate all of the command lines that GNU parallel will run
genCommands () {
    # Expand a row into some number of complete command lines
    expandRow () {
        numInvocations=$(expr $numCores "*" $1)
        suite=$2
        group=$3
        numTests=$4

        for i in $(seq $numInvocations); do
            echo innerCommand $suite $group $numTests
        done
    }

    for row in "${rows[@]}"; do
        expandRow $row
    done
}

# Notify the user
echo "Scheduled invocations (once per each of the $numCores cores):"
for row in "${rows[@]}"; do
    echo "    $row"
done

# Run the invocations, but never more at once than the number of physical cores
genCommands | "${nixAbsDir}/parallel/bin/parallel" -j$numCores
