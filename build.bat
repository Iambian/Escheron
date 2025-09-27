@echo off
tools\spasm64.exe -T src\main.z80 obj\escheron.bin
tools\wabbit.exe obj\escheron.bin bin\escheron.8xk
