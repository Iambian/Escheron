@echo off
tools\spasm64.exe src\main.z80 obj\escheron.bin
tools\wabbit.exe obj\escheron.bin bin\escheron.8xk
