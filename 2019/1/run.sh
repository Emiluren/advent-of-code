#!/bin/sh
nasm -felf64 1a.s && gcc 1a.o -no-pie && ./a.out
nasm -felf64 1b.s && gcc 1b.o -no-pie && ./a.out
