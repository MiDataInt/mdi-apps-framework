ECHO OFF

set /p R_VERSION=<R_VERSION.txt

rem -----------------------------------------------------------------------
rem edit the following line to match the proper system path to R installations
rem -----------------------------------------------------------------------
set Rscript="C:\Program Files\R\R-%R_VERSION%\bin\Rscript.exe"
rem -----------------------------------------------------------------------

set GITHUB_PAT=xxxxx
set gitUser=xxxxx

%Rscript% -e "magc.portal::install(getwd(), gitUser='%gitUser%', token='%GITHUB_PAT%', checkout='develop')"
rem , checkout='develop'

echo MAGC Portal installation complete

PAUSE

