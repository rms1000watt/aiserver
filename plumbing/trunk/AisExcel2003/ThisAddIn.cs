/**********************************************************************************
    Copyright (C) 2009 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/

using System;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualStudio.Tools.Applications.Runtime;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using System.Text;
using AppClient;
using System.Reflection;

namespace AisExcel2003
{
	public partial class ThisAddIn
	{
		private Office.CommandBar toolbar;
		private Office.CommandBarButton toolbarCommand;
        public GSMDemo cGsmDemoForm;
        public WelcomeForm cWelcomeForm;
        public AAppClient cAppClient;
        public AAsyncEventArgs cReceiver;
        public string cUsername;
        public string cPassword;
        public int cSessionId;
        public upWaitForm cWaitForm;
        public ConnectForm cConnectForm;
        public CLIForm cCLIForm;
        private bool cOnStartup = true;

		private void ThisAddIn_Startup(object sender, System.EventArgs e)
		{
			#region VSTO generated code

			this.Application = (Excel.Application)Microsoft.Office.Tools.Excel.ExcelLocale1033Proxy.Wrap(typeof(Excel.Application), this.Application);

			#endregion
			// This code adds a custom toolbar to the user's Excel spreadsheet.
			AddToolbar();
            // Retrieve the current excel application and pass it to the GSM Demo form.
            // The form needs the reference to update the worksheet.
            cWaitForm = new upWaitForm();
            cConnectForm = new ConnectForm();
            cReceiver = new AAsyncEventArgs();
            cReceiver.Completed += new System.EventHandler<AAsyncEventArgs>(defaultResponseHandler);
            cAppClient = new AAppClient("", 0, cReceiver, "GSMDemo");
            cCLIForm = new CLIForm();
            cGsmDemoForm = new GSMDemo();
            cWelcomeForm = new WelcomeForm();
		}

		private void ThisAddIn_Shutdown(object sender, System.EventArgs e)
		{
		}

		#region VSTO generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InternalStartup()
		{
			this.Startup += new System.EventHandler(ThisAddIn_Startup);
			this.Shutdown += new System.EventHandler(ThisAddIn_Shutdown);
		}

		#endregion

		private void AddToolbar()
		{
			try
			{	toolbar = Application.CommandBars.Add("AIS Toolbar", Office.MsoBarPosition.msoBarTop, false, true);
				if (toolbar != null)
				{	// Add an AIS Connect button to bring up AIS connection dialog
					toolbarCommand = (Office.CommandBarButton)
						toolbar.Controls.Add(Office.MsoControlType.msoControlButton, missing, missing, missing, true);
					toolbarCommand.Caption = "AIS Connect";
					toolbarCommand.FaceId = 59;
					toolbarCommand.Click += new Office._CommandBarButtonEvents_ClickEventHandler(toolbarCommand_Click);
					toolbar.Visible = true;
				}
			}
			catch (Exception e)
			{	MessageBox.Show(e.Message);
			}
		}
		private void toolbarCommand_Click(Office.CommandBarButton Ctrl, ref bool CancelDefault)
		{
            // Show the GSM Demo dialog modeless
            // Show the Welcome form once only.
            cGsmDemoForm.Show();
            if (cOnStartup == true)
            {
                cWelcomeForm.Show();
                cCLIForm.Show();
                // Set the x and y locations of the window forms on startup.
                System.Drawing.Point aTempPoint;
                int x = 5;
                int y = 5;
                cWelcomeForm.Location = new System.Drawing.Point(x, y);
  
                aTempPoint = cWelcomeForm.Location;
                x = aTempPoint.X;
                y = cWelcomeForm.Height + aTempPoint.Y;
                cGsmDemoForm.Location = new System.Drawing.Point(x, y);
                cOnStartup = false;
                aTempPoint = cGsmDemoForm.Location;
                x = aTempPoint.X;
                y = cGsmDemoForm.Height + aTempPoint.Y;
                cCLIForm.Location = new System.Drawing.Point(x, y);
            }


		}
        // Handler for Async Events
        private void defaultResponseHandler(object iSrc, AAsyncEventArgs iArgs)
        {
            if (iArgs.Status == 0)
            {   switch (iArgs.RequestType)
                {   case ARequestType.OpenConnection:
                        // Successfully connected, try logging on.
                        if (cAppClient.IsConnected)
                        {    cAppClient.Logon(cReceiver, cUsername, cPassword);
                        }
                        break;
                    case ARequestType.CloseConnection:
                        cConnectForm.setConnected(false);
                        cConnectForm.setConnected(false);
                        cConnectForm.enableServerIPText(true);
                        cConnectForm.enableServerPortText(true);
                        cConnectForm.enableUserNameText(true);
                        cConnectForm.enablePasswordText(true);
                        Globals.ThisAddIn.cCLIForm.enableSubmitButton(false);
                        Globals.ThisAddIn.cCLIForm.enableCommandText(false);
                        cGsmDemoForm.displayStatusBar("Disonnected");
                        cGsmDemoForm.setStatusBarImg(global::AisExcel2003.Properties.Resources.disconnected);
                        break;
                    case ARequestType.Logon:
                        if (iArgs.Status == 0)
                        {
                            cConnectForm.setConnected(true);
							cConnectForm.enableServerIPText(false);
							cConnectForm.enableServerPortText(false);
							cConnectForm.enableUserNameText(false);
							cConnectForm.enablePasswordText(false);
                            Globals.ThisAddIn.cCLIForm.enableSubmitButton(true);
                            Globals.ThisAddIn.cCLIForm.enableCommandText(true);
                            cAppClient.OpenSession(cReceiver, "GSMDemo");
                            
                        }
                        else
                            //There was an error on logon
                            displayText("Logon Error: " + iArgs.Error + Environment.NewLine);
                        break;
                    case ARequestType.OpenSession:
                        cSessionId = iArgs.ReturnValue;
                        displayText("Logged on successful" + Environment.NewLine);
                        cGsmDemoForm.displayStatusBar("Connected");
                        cGsmDemoForm.setStatusBarImg(global::AisExcel2003.Properties.Resources.connected);
                        break;
                    case ARequestType.Display:
                    case ARequestType.Eval:
                        if (iArgs.Out.StartsWith("***DisplayGenerateData***"))
                        {  displayGenerateDataResults(iArgs);
                        }
                        else
                            if (iArgs.Out.StartsWith("***DisplayWorkSheet***"))
                            {
                                Excel.Workbook aWorkbook = Globals.ThisAddIn.Application.Workbooks.get_Item(1);
                                Excel.Sheets aCurrentSheets = aWorkbook.Sheets as Excel.Sheets;
                                Excel.Worksheet aWorkSheet;
                                string aOutText = iArgs.Out.Replace("***DisplayWorkSheet***", "");
                                // Parse the results, delimited by "***Marker***" as sent in the Run Command
                                // of Generate Data form.
                                string[] aOutArray = aOutText.Split(new string[] { "***Marker***" }, StringSplitOptions.None);
                                string aTestName = "";
                                if (aOutArray.Length > 0)
                                {   aTestName = aOutArray[0];
                                }
                                if (aOutArray.Length > 1)
                                {	// Process the Test case's Results.txt file
                                    try
                                    {   aWorkSheet = aCurrentSheets.get_Item("Results") as Excel.Worksheet;
                                    }
                                    catch (Exception)
                                    {   // Worksheet does not exist, create a new one
                                        aWorkSheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                                        aWorkSheet.Name = "Results";
                                    }
                                    displayWorkSheet(aWorkSheet, aOutArray[1], 1, 1, "\n", "\t");
                                }
                                if (aOutArray.Length > 2)
                                {	// Process the Test case's Estimates.txt file
                                    try
                                    {   aWorkSheet = aCurrentSheets.get_Item("Estimates") as Excel.Worksheet;
                                    }
                                    catch (Exception)
                                    {   // Worksheet does not exist, create a new one
                                        aWorkSheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                                        aWorkSheet.Name = "Estimates";
                                    }
                                    displayWorkSheet(aWorkSheet, aOutArray[2], 1, 1, "\n", "\t");
                                }
                                if (aOutArray.Length > 3)
                                {	// Process the Test case's Statistics.txt file
                                    try
                                    {   aWorkSheet = aCurrentSheets.get_Item("Statistics") as Excel.Worksheet;
                                    }
                                    catch (Exception)
                                    {   // Worksheet does not exist, create a new one
                                        aWorkSheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                                        aWorkSheet.Name = "Statistics";
                                    }
                                    displayWorkSheet(aWorkSheet, aOutArray[3], 1, 1, "\n", "\t");
                                }
                                if (iArgs.Display != "")
                                {   string aTextToDisplay = iArgs.Display.Replace("\n", Environment.NewLine);
                                    displayText(aTextToDisplay);
                                }
                                Globals.ThisAddIn.cWaitForm.setVisible(false);
                                MessageBox.Show("GSM Test finished", "Information");
                            }
                            else
                            {   string aTextToDisplay = iArgs.Display.Replace("\n", Environment.NewLine);
                                displayText(aTextToDisplay);
                                if (iArgs.Out != "")
                                    displayText(iArgs.Out);
                            }
                        break;
                    case ARequestType.LogStatus:
                        displayText("Log: " + iArgs.Out);
                        break;
                    default:
                        displayText("Default: " + iArgs.Out);
                        break;
                }
            }
            else
            {   if (iArgs.Error.Length == 0)
                {   if (AGlobals.GetSingleton().ErrorMessages.ContainsKey(iArgs.Status))
                    {    iArgs.Error = AGlobals.GetSingleton().ErrorMessages[iArgs.Status];
                    }
                    else
                    {   iArgs.Error = AGlobals.GetSingleton().ErrorMessages[(int)AErrorCodes.Generic];
                    }
                }
                switch (iArgs.RequestType)
                {   case ARequestType.OpenConnection:
                        displayText("Open Connection Error: " + iArgs.Error);
                        break;
                    case ARequestType.CloseConnection:
                        displayText("Close Connection Error: " + iArgs.Error);
                        cConnectForm.setConnected(false);
                        cConnectForm.enableServerIPText(true);
                        cConnectForm.enableServerPortText(true);
                        cConnectForm.enableUserNameText(true);
                        cConnectForm.enablePasswordText(true);
                        Globals.ThisAddIn.cCLIForm.enableSubmitButton(false);
                        Globals.ThisAddIn.cCLIForm.enableCommandText(false);
                        cGsmDemoForm.displayStatusBar("Disonnected");
                        cGsmDemoForm.setStatusBarImg(global::AisExcel2003.Properties.Resources.disconnected);
                        break;
                    case ARequestType.Logon:
                        displayText("Logon Error" + iArgs.Error);
                        break;
                    case ARequestType.OpenSession:
                        displayText("Open Session Error" + iArgs.Error);
                        break;
                    default:
                        break;
                }
            }
        }

        /// <summary>
        /// Writes the given text to the Connect form's edit box.
        /// </summary>
        /// <param name="iText"></param>
        private void displayText(string iText)
        {
            Globals.ThisAddIn.cCLIForm.displayText(iText);
        }

        private void displayWorkSheet(Excel.Worksheet iWorksheet, string iDisplayText,
            int iRowStart, int iColStart, string iRowDelim, string iColDelim)
        {
            int aRowIndex = iRowStart;
            int aColIndex = iColStart;
            int aRowLength = 0;
            int aColLength = 0;
            string[] aRowArray = iDisplayText.Split(new string[] { iRowDelim }, System.StringSplitOptions.RemoveEmptyEntries);
            aRowLength = aRowArray.Length;
            if (aRowLength > 0)
            {	// Count the columns
                string[] aColArray = aRowArray[0].Split(new string[] { iColDelim }, System.StringSplitOptions.RemoveEmptyEntries);
                aColLength = aColArray.Length;
            }
            // Create a 2 dimensional array with the string
            object[,] aWorksheetArray = new object[aRowLength, aColLength];
            for (int aRow = 0; aRow < aRowLength; aRow++)
            {   string aColText = aRowArray[aRow];
                string[] aColArray = aColText.Split(new string[] { iColDelim }, System.StringSplitOptions.RemoveEmptyEntries);
                for (int aCol = 0; aCol < aColArray.Length; aCol++)
                {   aWorksheetArray[aRow, aCol] = aColArray[aCol];
                }
            }
            Excel.Range aRange = iWorksheet.get_Range(iWorksheet.Cells[iRowStart, iColStart], iWorksheet.Cells[iRowStart, iColStart]);
            aRange = aRange.get_Resize(aRowLength, aColLength);
            aRange.set_Value(Missing.Value, aWorksheetArray);
            aRange.Columns.AutoFit();
        }
        private void displayGenerateDataResults(AAsyncEventArgs iArgs)
        {
            Excel.Workbook aWorkbook;
            Excel.Sheets aCurrentSheets;
            Excel.Worksheet aWorksheet;
            try
            {   // Check if there is an existing workbook
                aWorkbook = Globals.ThisAddIn.Application.Workbooks.get_Item(1);
            }
            catch (Exception)
            {   // There was an error retrieving the current workbook,
                // Create a new one
                aWorkbook = Globals.ThisAddIn.Application.Workbooks.Add(Type.Missing);
            }
            aCurrentSheets = aWorkbook.Sheets as Excel.Sheets;
            string aOutText = iArgs.Out.Replace("***DisplayGenerateData***", "");
            // There are quotations from the generated data when they are strings, removing them before displaying.
            aOutText = aOutText.Replace("\"", "");
            // Parse the results, delimited by "***Marker***" as sent in the Run Command
            // of Generate Data form.
            string[] aOutArray = aOutText.Split(new string[] { "***Marker***" }, StringSplitOptions.None);
            string aTestName = "";
            if (aOutArray.Length > 0)
            {   aTestName = aOutArray[0];
            }
            if (aOutArray.Length > 1)
            {   string aParameterString = aOutArray[1];
                // Process the Test case's Parameters.txt file
                // Check if the TestDir parameter exists, if not add TestDir with default value
                if (aParameterString.IndexOf("TestDir") < 0)
                {	string aTestDir = Directory.GetCurrentDirectory();
					aParameterString += "\nTestDir\t" + aTestDir + "\\";
                }
                // Check if the ParametersFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("ParametersFile") < 0)
                {   aParameterString += "\nParametersFile\tMyTest.Parameters.ini";
                }
                // Check if the TrainingFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("TrainingFile") < 0)
                {   string aDir = Directory.GetCurrentDirectory();
					aParameterString += "\nTrainingFile\tMyTest.Training.txt";
                }
                // Check if the TestingFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("TestingFile") < 0)
                {   aParameterString += "\nTestingFile\tMyTest.Testing.txt";
                }
                // Check if the ResultsFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("ResultsFile") < 0)
                {
                    aParameterString += "\nResultsFile\tMyTest.Results.txt";
                }
                // Check if the StatisticsFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("StatisticsFile") < 0)
                {
                    aParameterString += "\nStatisticsFile\tMyTest.Statistics.txt";
                }
                // Check if the EstimatesFile parameter exists, if not add the parameter string with default value
                if (aParameterString.IndexOf("EstimatesFile") < 0)
                {
                    aParameterString += "\nEstimatesFile\tMyTest.Estimates.txt";
                }
                stringToWorksheet("Parameters", aParameterString, 1, 1, "\n", "\t");
                //addDropDownOptions("Parameters");
                addParameterComments("Parameters");
            }
            if (aOutArray.Length > 2)
            {	// Process the Test case's Training.txt file
                try
                {  aWorksheet = aCurrentSheets.get_Item("Training") as Excel.Worksheet;
                }
                catch (Exception)
                {  // Worksheet does not exist, create a new one
                    aWorksheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                    aWorksheet.Name = "Training";
                }
                displayWorkSheet(aWorksheet, aOutArray[2], 1, 1, "\n", "\t");
            }
            if (aOutArray.Length > 3)
            {	// Process the Test case's Testing.txt file
                try
                {  aWorksheet = aCurrentSheets.get_Item("Testing") as Excel.Worksheet;
                }
                catch (Exception)
                {   // Worksheet does not exist, create a new one
                    aWorksheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                    aWorksheet.Name = "Testing";
                }
                displayWorkSheet(aWorksheet, aOutArray[3], 1, 1, "\n", "\t");
            }
            if (iArgs.Display != "")
            {   string aTextToDisplay = iArgs.Display.Replace("\n", Environment.NewLine);
                displayText(aTextToDisplay);
            }
            // Focus on the Parameters worksheet
            aWorksheet = aCurrentSheets.get_Item("Parameters") as Excel.Worksheet;
            //aWorksheet.Activate();
        }
        // Returns a string equivalent of a worksheet
        // Rows are separated by newlines
        // Columns are separated by tabs
        public string worksheetToString(string aWorksheetName)
        {
            string aResult = "";
            Excel.Worksheet aWorksheet = Globals.ThisAddIn.Application.Worksheets.get_Item(aWorksheetName) as Excel.Worksheet;
            object aSelectedText = aWorksheet.UsedRange.Value2;
            object[,] aArray;
            int aRowCount = 0;
            int aColCount = 0;
            if (aSelectedText == null)
            {   MessageBox.Show("There are no values in the worksheet");
            }
            else if (aSelectedText.ToString() != "System.Object[,]")
            {  MessageBox.Show("The data in the worksheet is not a 2-dimensional array");
            }
            aArray = aSelectedText as object[,];
            aRowCount = aArray.GetUpperBound(0);
            aColCount = aArray.GetUpperBound(1);
            for (int aRow = 1; aRow <= aRowCount; aRow++)
            {  for (int aCol = 1; aCol <= aColCount; aCol++)
                {   if (aArray[aRow, aCol] != null)
                        aResult += aArray[aRow, aCol].ToString() + "\t";
                    else
                        aResult += "" + "\t";
                }
                // Removing last tab character
                aResult = aResult.Remove(aResult.Length - 1);
                aResult += "\n";
            }
            // Removing last newline character
            aResult = aResult.Remove(aResult.Length - 1);
            return aResult;
        }
        /// <summary>
        /// Parses a string and converts data into a worksheet
        /// </summary>
        /// <param name="iWorksheetName">Worksheet to create</param>
        /// <param name="iDisplayText">Text data to be converted as a worksheet</param>
        /// <param name="iRowStart">Starting row to write the data</param>
        /// <param name="iColStart">Starting column to write the data</param>
        /// <param name="iRowDelim">Row delimiter for each cell block</param>
        /// <param name="iColDelim">Column delimiter for each cell block</param>
        public static void stringToWorksheet(string iWorksheetName, string iDisplayText,
            int iRowStart, int iColStart, string iRowDelim, string iColDelim)
        {
            Excel.Workbook aWorkbook = Globals.ThisAddIn.Application.Workbooks.get_Item(1);
            Excel.Sheets aCurrentSheets = aWorkbook.Sheets as Excel.Sheets;
            Excel.Worksheet aWorksheet;
            Excel.Range aRange;
            int aRowIndex = iRowStart;
            int aColIndex = iColStart;
            int aRowLength = 0;
            int aColLength = 0;
            string[] aRowArray = iDisplayText.Split(new string[] { iRowDelim }, System.StringSplitOptions.RemoveEmptyEntries);
            aRowLength = aRowArray.Length;
            if (aRowLength > 0)
            {	// Count the columns
                string[] aColArray = aRowArray[0].Split(new string[] { iColDelim }, System.StringSplitOptions.RemoveEmptyEntries);
                aColLength = aColArray.Length;
            }
            // Create a 2 dimensional array with the string
            object[,] aWorksheetArray = new object[aRowLength, aColLength];
            for (int aRow = 0; aRow < aRowLength; aRow++)
            {   string aColText = aRowArray[aRow];
                string[] aColArray = aColText.Split(new string[] { iColDelim }, System.StringSplitOptions.RemoveEmptyEntries);
                for (int aCol = 0; aCol < aColArray.Length; aCol++)
                {   aWorksheetArray[aRow, aCol] = aColArray[aCol];
                }
            }
            try
            {   aWorksheet = aCurrentSheets.get_Item(iWorksheetName) as Excel.Worksheet;
            }
            catch (Exception)
            {   // Worksheet does not exist, create a new one
                aWorksheet = (Excel.Worksheet)aCurrentSheets.Add(aCurrentSheets[1], Type.Missing, Type.Missing, Type.Missing);
                aWorksheet.Name = iWorksheetName;
            }
            aRange = aWorksheet.get_Range(aWorksheet.Cells[iRowStart, iColStart], aWorksheet.Cells[iRowStart, iColStart]);
            aRange = aRange.get_Resize(aRowLength, aColLength);
            aRange.set_Value(Missing.Value, aWorksheetArray);
            aRange.Columns.AutoFit();
        }
		/* Right now, the call to this method is commented out in GSMDemo.cs.  This vestigal code is just
			here in case it is of use later on.
        /// <summary>
        /// Adds the valid values as a dropdown menu of each parameter values in the parameters worksheet
		/// 
        /// </summary>
        public static void addDropDownOptions(string iWorksheetName)
        {
            Excel.Worksheet aWorksheet = Globals.ThisAddIn.Application.ActiveWorkbook.Worksheets.get_Item(iWorksheetName) as Excel.Worksheet;
            Excel.Range aRange;
            string aParamString;
            string aValueString;

            object[,] aParamList = AppClient.AUtilities.worksheetToObjectArray(aWorksheet);

            if (aParamList == null)
            {   MessageBox.Show("There are no values in the Parameters worksheet");
                return;
            }
            int aRowCount = aParamList.GetUpperBound(0);
            for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
            {  if (aParamList[aCtr, 1] != null)
                {    aParamString = aParamList[aCtr, 1].ToString();
                    if (aParamList[aCtr, 2] == null)
                        aValueString = "";
                    else
                        aValueString = aParamList[aCtr, 2].ToString();
                    if (aParamString == "ModelName")
                    {   Excel.DropDowns aDropDowns = ((Excel.DropDowns)aWorksheet.DropDowns(Type.Missing));
                        Excel.DropDown aDropDown;
                        int aSelected = 0;
                        aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 2], aWorksheet.Cells[aCtr, 2]);
                        aDropDown = aDropDowns.Add((double)aRange.Left,
                            (double)aRange.Top, (double)aRange.Width, (double)aRange.Height, true);
                        int aCount = Enum.GetValues(typeof(ceModelNames)).Length;
                        System.Object[] aTestCases = new System.Object[aCount];
                        string[] aTestCaseNames = Enum.GetNames(typeof(ceModelNames));
                        for (int aCtr2 = 0; aCtr2 < aCount; aCtr2++)
                        {   aDropDown.AddItem(aTestCaseNames[aCtr2], aCtr2 + 1);
                             if (aTestCaseNames[aCtr2] == aValueString)
                                 aSelected = aCtr2 + 1;
                        }
                        aDropDowns.Value = aSelected;
                    }
                    if (aParamString == "SVMKernelID")
                    {  Excel.DropDowns aDropDowns = ((Excel.DropDowns)aWorksheet.DropDowns(Type.Missing));
                        Excel.DropDown aDropDown;
                        int aSelected = 0;
                        aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 2], aWorksheet.Cells[aCtr, 2]);
                        aDropDown = aDropDowns.Add((double)aRange.Left,(double)aRange.Top, (double)aRange.Width,
							(double)aRange.Height, true);
                        int aCount = Enum.GetValues(typeof(ceSVMKernelIDs)).Length;
                        System.Object[] aTestCases = new System.Object[aCount];
                        string[] aTestCaseNames = Enum.GetNames(typeof(ceSVMKernelIDs));
                        for (int aCtr2 = 0; aCtr2 < aCount; aCtr2++)
                        {  aDropDown.AddItem(aTestCaseNames[aCtr2], aCtr2 + 1);
                            if (aTestCaseNames[aCtr2] == aValueString)
                                aSelected = aCtr2 + 1;
                        }
                        aDropDowns.Value = aSelected;
                    }
                }
            }
        }
		 */
        /// <summary>
        /// Add parameter comments to a worksheet
        /// </summary>
        /// <param name="iWorksheetName">parameter worksheet name</param>
        public static void addParameterComments(string iWorksheetName)
        {
            Excel.Worksheet aWorksheet;
            Excel.Range aRange;
            object[,] aParamList;
            string aParamString;
            string aValueString;
            int aRowCount;
            try
            {   aWorksheet = Globals.ThisAddIn.Application.ActiveWorkbook.Worksheets.get_Item(iWorksheetName) as Excel.Worksheet;
            }
            catch (Exception)
            {  // Worksheet is not found.  Do nothing for now.
                return;
            }
            aParamList = AppClient.AUtilities.worksheetToObjectArray(aWorksheet);
            if (aParamList == null)
            {    MessageBox.Show("There are no values in the Parameters worksheet");
                return;
            }
            aRowCount = aParamList.GetUpperBound(0);
            for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
            {   if (aParamList[aCtr, 1] != null)
                {   aParamString = aParamList[aCtr, 1].ToString();
                    if (aParamList[aCtr, 2] == null)
                        aValueString = "";
                    else
                        aValueString = aParamList[aCtr, 2].ToString();
                    if (aParamString == "ModelName")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of the regression model");
                    }
                    else
                    if (aParamString == "SVMKernelID")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Support Vector Machine Kernel ID");
                    }
                    else
                    if (aParamString == "Generations")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Number of Generations");
                    }
                    else
                    if (aParamString == "MaxTime")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Maximum Time for running GSM Test");
                    }
                    else
                    if (aParamString == "HaltingScore")
                    {  aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Stop regression if error is less than this score");
                    }
                    else
                    if (aParamString == "NumChampions")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Number of Champions to be generated");
                    }
                    else
                    if (aParamString == "TestDir")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Directory where the GSM test set files are located.");
                    }
                    else
                    if (aParamString == "ParametersFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of parameter file that will be used when saving the test parameters");
                    }
                    else
                    if (aParamString == "TrainingFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of training file that will be used when saving the training data");
                    }
                    else
                    if (aParamString == "TestingFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of testing file that will be used when saving the testing data");
                    }
                    else
                    if (aParamString == "ResultsFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of results file that will be used when saving the results");
                    }
                    else
                    if (aParamString == "StatisticsFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of statistics file that will be used when saving the statistics");
                    }
                    else
                    if (aParamString == "EstimatesFile")
                    {   aRange = aWorksheet.get_Range(aWorksheet.Cells[aCtr, 3], aWorksheet.Cells[aCtr, 3]);
                        aRange.set_Value(Missing.Value, "Name of estimates file that will be used when saving the estimates");
                    }
                }
            }
            aRange = aWorksheet.UsedRange;
            aRange.Columns.AutoFit();
        }
	}
}
