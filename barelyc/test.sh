#!/bin/bash
cargo run -- hello.br
nasm -felf64 --reproducible build/out.asm
mold --static -z nosectionheader -z nodefaultlib --as-needed --gc-sections --hash-style=none --no-eh-frame-hdr -N -S -s -X -pie  build/out.o
