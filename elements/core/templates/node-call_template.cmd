@echo off

rem * main
    SETLOCAL

    call "$$$$COMMAND$$$$" %*

    ENDLOCAL
    EXIT /b %errorlevel%


rem * Functions
rem Example:
rem First level parent (base dir)
rem --->with ending backslash
rem call :GetFileBaseDir dirFileBase "%pathTestFile%"
rem echo dirFileBase:               "%dirFileBase%"

rem --->Same but without ending backslash
rem call :GetFileBaseDirWithoutEndSlash dirFileBaseWithBackSlash "%pathTestFile%"
rem echo dirFileBaseWithBackSlash:  "%dirFileBaseWithBackSlash%"
:GetFileBaseDir
    :: sets the value to dirFileBase variable
    set "%~1=%~dp2"
    exit /b 0


:GetFileBaseDirWithoutEndSlash
    set "dirWithBackSlash=%~dp2"
    REM substring from the start to the end minus 1 char from the end
    set "%~1=%dirWithBackSlash:~0,-1%"
    exit /b 0


rem Example:
rem REM One level up
rem call :GetDirParentN dirFileParent1 "%pathTestFile%" ".."
rem echo dirFileParent1:            "%dirFileParent1%"
rem
rem REM Two levels up
rem call :GetDirParentN dirFileParent2 "%pathTestFile%" "..\.."
rem echo dirFileParent2:            "%dirFileParent2%"
rem
rem REM Three levels up
rem call :GetDirParentN dirFileParent3 "%pathTestFile%" "..\..\.."
rem echo dirFileParent3:            "%dirFileParent3%"
:GetDirParentN
    for %%I in ("%~2\%~3") do set "%~1=%%~fI"
    exit /b 0
