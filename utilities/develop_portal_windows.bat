ECHO OFF

set /p R_VERSION=<R_VERSION.txt

rem -----------------------------------------------------------------------
rem edit the following line to match the proper system path to R installations
rem -----------------------------------------------------------------------
set Rscript="C:\Program Files\R\R-%R_VERSION%\bin\Rscript.exe"
rem -----------------------------------------------------------------------

set GITHUB_PAT=xxxxxx

%Rscript% -e "magc.portal::develop(getwd())"

pause
