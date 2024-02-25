#! /bin/bash
nasm -I lmacros/ -f bin transien.asm -l tsr.lst -o 123123.com "$@"
