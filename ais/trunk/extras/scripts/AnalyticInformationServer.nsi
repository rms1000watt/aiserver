; AIS NSIS Script
; Written by Franklin Chua franklin.chua@gmail.com

; Include Modern UI
!include "MUI2.nsh"
!include "FileAssociation.nsh"

; General
Name "Analytic Information Server"
OutFile "ais-1.0.0.exe"

; Default installation folder
; $PROGRAMFILES, $PROGRAMFILES32, $PROGRAMFILES64
; On Windows x64, $PROGRAMFILES and $PROGRAMFILES32 point to C:\Program Files (x86)
; while $PROGRAMFILES64 points to C:\Program Files
InstallDir "$PROGRAMFILES\Analytic Information Server"
; Get installation folder from registry if available
InstallDirRegKey HKLM "Software\Analytic Information Server" ""

; Request application privileges for Windows Vista
; user|highest|admin
RequestExecutionLevel admin

; Variables
Var StartMenuFolder

; Interface settings
!define MUI_ABORTWARNING

; Pages
;!insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Docs\Modern UI\License.txt"
;!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY

; Start Menu Folder Page Configuration
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM" 
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\Analytic Information Server" 
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;Languages
!insertmacro MUI_LANGUAGE "English"

; Installer Section
Section ""

  SetOutPath "$INSTDIR"
  
  CreateDirectory "$INSTDIR\mysqlmsgs"
  CreateDirectory "$INSTDIR\Libraries"
  CreateDirectory "$INSTDIR\Demos"
  CreateDirectory "$INSTDIR\docs\onlinedocs"
  
  ; ADD YOUR OWN FILES HERE...
  File "..\..\webidedev.exe"
  File "..\..\ridedev.exe"
  File "..\..\aiseditdev.exe"
  File "..\..\aissvcdev.exe"
  File "..\..\asvcmgrdev.exe"
  File "..\..\QtCore4.dll"
  File "..\..\QtGui4.dll"
  File "..\..\QtNetwork4.dll"
  File "..\..\QtXml4.dll"
  File "..\..\mingwm10.dll"
  File "..\..\libmysqld.dll"
  File "..\..\libgcc_s_dw2-1.dll"
  File "..\..\libstdc++-6.dll"
  File "/oname=mysqlmsgs\errmsg.sys" "..\..\mysqlmsgs\errmsg.sys"
  File /r /x *.db /x .svn /x *.log /x mysqldata "..\..\Libraries"
  File /r /x *.db /x .svn /x *.log /x mysqldata "..\..\Demos"
  File "..\config\ais.ini"
  File "..\config\aisinstall.ini"
  File "..\config\rideinstall.ini"
  File "..\config\context.ini"
  File "..\config\contextusers.ini"
  File "..\config\AStartup.sl"
  File "..\..\parameters.txt"
  
  SetOutPath "$INSTDIR\docs\onlinedocs"
  File /r /x .svn "..\..\docs\onlinedocs\*.*"
  
  SetOutPath "$INSTDIR"
  
  ; Store installation folder
  WriteRegStr HKLM "Software\Analytic Information Server" "" $INSTDIR
  
  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
  ; Create shortcuts
  CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\AIS IDE.lnk" "$INSTDIR\webidedev.exe"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\AIS RIDE.lnk" "$INSTDIR\ridedev.exe"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\AIS Editor.lnk" "$INSTDIR\aiseditdev.exe"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\AIS Service Manager.lnk" "$INSTDIR\asvcmgrdev.exe"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\GSM Demo.lnk" "$INSTDIR\webidedev.exe" "$\"$INSTDIR\Demos\GSMDemo\AStartup.sl$\""
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\RunQueue Demo.lnk" "$INSTDIR\webidedev.exe" "$\"$INSTDIR\Demos\RunQueueDemo\RunQueueMain\AStartup.sl$\""
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  ; Setup file association
  ${registerExtension} "$INSTDIR\webidedev.exe" ".sl" "AIS Lisp Source File"
 
  ; Add entry in Add/Remove Programs
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\AIS" "DisplayName" "Analytic Information Server"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\AIS" "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
 
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

; Uninstaller Section
Section "Uninstall"
  ;; Remove AIS Service
  ExecWait '"$INSTDIR\aissvcdev.exe" --remove' $0

  ; ADD YOUR OWN FILES HERE...  
  Delete "$INSTDIR\*.exe"
  Delete "$INSTDIR\*.dll"
  Delete "$INSTDIR\*.ini"
  Delete "$INSTDIR\*.txt"
  Delete "$INSTDIR\*.sl"
  Delete "$INSTDIR\mysqlmsgs\*.exe"
  Delete "$INSTDIR\docs\onlinedocs\index.html"

  RMDir /r "$INSTDIR\Libraries"
  RMDir /r "$INSTDIR\Demos"
  RMDir /r "$INSTDIR\mysqlmsgs"
  RMDir /r "$INSTDIR\docs\onlinedocs"
  RMDir "$INSTDIR"
  
  ; Remove file association
  ${unregisterExtension} ".sl" "AIS Lisp Source File"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder

  Delete "$SMPROGRAMS\$StartMenuFolder\*.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"  
  
  ; Remove entry in Add/Remove Programs
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\AIS"
  
  ; Remove registry entry
  DeleteRegKey /ifempty HKLM "Software\Analytic Information Server"

SectionEnd
