#!/bin/bash

set -e

# we only want to build documentation from the "reblocks" branch
# and want to make it only once.
# Also, we need to check if TRAVIS_PULL_REQUEST=false because Travis
# builds which are running for pull requests will have
# TRAVIS_BRANCH=master, but TRAVIS_PULL_REQUEST_BRANCH=the-branch
# and TRAVIS_PULL_REQUEST=42 where 42 is a pull request number
sudo pip install -r docs/requirements.txt

if [ "$LISP" = "ccl-bin" ]; then
    if [ "$TRAVIS_BRANCH" = "master" -a "$TRAVIS_PULL_REQUEST" = "false"]; then
        echo "Building and uploading documentaion."
        ./build-docs.ros --push
    else
        echo "Checking how does documentation build."
        ./build-docs.ros
    fi
fi
