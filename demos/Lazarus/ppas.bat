@echo off
SET THEFILE=C:\MeusProjetos\Ipify4Pascal\demos\Lazarus\Ipify4Pascal_Demo_Lazarus.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o C:\MeusProjetos\Ipify4Pascal\demos\Lazarus\Ipify4Pascal_Demo_Lazarus.exe C:\MeusProjetos\Ipify4Pascal\demos\Lazarus\link6864.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
