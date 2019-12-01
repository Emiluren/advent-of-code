#!/bin/sh
nasm -felf64 1.s && gcc 1.o -no-pie && ./a.out
