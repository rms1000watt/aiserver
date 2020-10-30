@ECHO OFF
REM aisdev/updateGuides.bat   Copy Modified Files to Guides Folder
REM Change History:
REM Version	Date		Who	Change
REM  1.0065	6/4/2005	tlw	Copy modified files to Guides directory.

REM               UPDATING GUIDES
REM Update Guides from GuideAisUser, GuideDeveloper, Guide Editor
REM Run this batch file from the refGuide directory.

REM GuideAisUser:
replace GuideAisUser\Document_AisUserGuide.html Guides /U
replace GuideAisUser\Essay_AisUser_Basics.html Guides /U
replace GuideAisUser\Essay_AisUser_Ide.html Guides /U
replace GuideAisUser\Essay_AisUser_Install.html Guides /U
replace GuideAisUser\Essay_AisUser_Intro.html Guides /U
replace GuideAisUser\Essay_AisUser_Monitor.html Guides /U

REM GuideDeveloper:
replace GuideDeveloper\Document_AisDeveloperGuide.html Guides /U
replace GuideDeveloper\Essay_AisDev_Amp.html Guides /U
replace GuideDeveloper\Essay_AisDev_Submit.html Guides /U

REM GuideDocumention
replace GuideDocumention\Document_DocumentationGuide.html Guides /U
replace GuideDocumention\Essay_Doc_Embedded.html Guides /U

REM GuideEditor:
replace GuideEditor\Document_EditorUserGuide.html Guides /U
replace GuideEditor\Essay_Edit_API.html Guides /U
replace GuideEditor\Essay_Edit_Configuration.html Guides /U
replace GuideEditor\Essay_Edit_Dialogs.html Guides /U
replace GuideEditor\Essay_Edit_Faqs.html Guides /U
replace GuideEditor\Essay_Edit_Operations.html Guides /U
replace GuideEditor\Essay_Edit_Search.html Guides /U
replace GuideEditor\Essay_Edit_SelectedText.html Guides /U
replace GuideEditor\Essay_Edit_Undo.html Guides /U

REM GuideProgrammer
replace GuideProgrammer\Document_ProgrammerGuide.html Guides /U
replace GuideProgrammer\Essay_Program_Html.html Guides /U
replace GuideProgrammer\Essay_Program_Xml.html Guides /U