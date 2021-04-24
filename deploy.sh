#!/bin/bash
clang -c runner.c
clang -c out.ll
clang out.o runner.o