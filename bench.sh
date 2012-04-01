#!/usr/bin/env bash
set -ex

TESTS="shm_pipe_test tcp_pipe_test"

rm -rf obj
mkdir -p obj

clients=1
SIZES="16 64 128 256 1024 4096 8192"
iters=200000
cd obj
for t in ${TESTS}; do
  for size in ${SIZES}; do
    ../_build/lib_test/$t.native -client-iters $iters -data-size $size -num-clients $clients 2>> results.txt
  done
done
