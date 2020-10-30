using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using AppClient;

namespace AisExcel2003
{
    public partial class RunTest : Form
    {
        public RunTest()
        {
            InitializeComponent();
            upTestName.Text = "MyTest";
        }
        /// <summary>
        /// RunTest_Load will populate the parameter list by retrieving
        /// the values from the Parameters worksheet.
        /// This is a generated event. 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void RunTest_Load(object sender, EventArgs e)
        {
            // Disable the Run button first, this will be enabled if validation is finished.
            upRunButton.Enabled = false;
            int aWorksheetCount = 0;
            bool aParamFlag = false;
            bool aTrainingFlag = false;
            Excel.Worksheet aTrainingWorkSheet = null, aTestingWorkSheet = null;
            bool aTestingFlag = false;
            string aParamString = "";
            string aValueString = "";
            string aParamValuePair = "";
            try
            {    aWorksheetCount = Globals.ThisAddIn.Application.Worksheets.Count;
            }
            catch (Exception)
            {  MessageBox.Show("There is no open workbook");
                Close();
                return;
            }
            for (int aCtr = 1; aCtr <= aWorksheetCount; aCtr++)
            {   Excel.Worksheet aWorkSheet = Globals.ThisAddIn.Application.Worksheets.get_Item(aCtr) as Excel.Worksheet;
                if (aWorkSheet.Name == "Parameters")
                    aParamFlag = true;
                if (aWorkSheet.Name == "Training")
                {   aTrainingFlag = true;
					aTrainingWorkSheet = aWorkSheet;
				}
                if (aWorkSheet.Name == "Testing")
				{	aTestingFlag = true;
					aTestingWorkSheet = aWorkSheet;
				}
            }
            if (aParamFlag == false)
            {   MessageBox.Show("Parameters worksheet does not exist, please run Generate Data or load a Parameters file");
                Close();
                return;
            }
            if (aTrainingFlag == false)
            {   MessageBox.Show("Training worksheet does not exist, please run Generate Data or load a Training file");
                Close();
                return;
            }
            if (aTestingFlag == false)
            {   MessageBox.Show("Testing worksheet does not exist, please run Generate Data or load a Testing file");
                Close();
                return;
            }
            // Validate the training worksheet
            bool aIsValid = true;
            Excel.Application aApp = Globals.ThisAddIn.Application;
			aApp.ScreenUpdating = false;
            if (!AUtilities.validateSheet(aTrainingWorkSheet))
				aIsValid = false;
			if (!AUtilities.validateSheet(aTestingWorkSheet))
				aIsValid = false;
			if (!AUtilities.compareHeaders(aTrainingWorkSheet, aTestingWorkSheet))
				aIsValid = false;
            aApp.ScreenUpdating = true;
            if (!aIsValid)
            {   MessageBox.Show("Please fix errors in the Training Worksheet and the Testing Worksheet and then try again.");
                Close(); 
                return;
            }
            // Scan through the list of parameters and display it in the list box.
            Excel.Worksheet aParamWorksheet = Globals.ThisAddIn.Application.Worksheets.get_Item("Parameters") as Excel.Worksheet;
            object[,] aParamList = AppClient.AUtilities.worksheetToObjectArray(aParamWorksheet);
            if (aParamList == null)
            {   MessageBox.Show("There are no values in the Parameters worksheet");
                return;
            }
            upParameterList.Items.Clear();
            int aRowCount = aParamList.GetUpperBound(0);
            for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
            {   if (aParamList[aCtr, 1] != null)
                {   aParamString = aParamList[aCtr, 1].ToString();
                    if (aParamList[aCtr, 2] == null)
                        aValueString = "";
                    else
                        aValueString = aParamList[aCtr, 2].ToString();
                    aParamValuePair = aParamString + "  =  " + aValueString;
                    upParameterList.Items.Add(aParamValuePair);
                }
            }
            upRunButton.Enabled = true;
        }

        private void upCancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void upRunButton_Click(object sender, EventArgs e)
        {
            if (upTestName.Text == "")
            {   MessageBox.Show("Please specify a test name.", "Error");
                return;
            }
            if (AppClient.AUtilities.hasWhiteSpace(upTestName.Text))
            {  MessageBox.Show("The test name has whitespaces.", "Error");
                return;
            }
            // Check if we have the ff. worksheets in the application:
            // Parameters / Training / Testing
            int aWorksheetCount = Globals.ThisAddIn.Application.Worksheets.Count;
            string[] aWorksheetNames = new string[aWorksheetCount];
            bool aParamFlag = false;
            bool aTrainingFlag = false;
            bool aTestingFlag = false;
            for (int aCtr = 1; aCtr <= aWorksheetCount; aCtr++)
            {   Excel.Worksheet aWorkSheet = Globals.ThisAddIn.Application.Worksheets.get_Item(aCtr) as Excel.Worksheet;
                if (aWorkSheet.Name == "Parameters")
                    aParamFlag = true;
                if (aWorkSheet.Name == "Training")
                    aTrainingFlag = true;
                if (aWorkSheet.Name == "Testing")
                    aTestingFlag = true;
            }
            if (aParamFlag == false)
            {   MessageBox.Show("Parameters worksheet does not exist, please run Generate Data or load a Parameters file");
                return;
            }
            if (aTrainingFlag == false)
            {  MessageBox.Show("Training worksheet does not exist, please run Generate Data or load a Training file");
                return;
            }
            if (aTestingFlag == false)
            {   MessageBox.Show("Testing worksheet does not exist, please run Generate Data or load a Testing file");
                return;
            }
            // Load the values in the worksheet and write it to it's appropriate file in the server
            // If the test case is named MyTest then the following files will be written to the server:
            // MyTest.Parameters.txt, MyTest.Training.txt and MyTest.Testing.txt
            string aParametersString = Globals.ThisAddIn.worksheetToString("Parameters");
            // Add lisp code to write worksheet contents to the Parameter file in the server
            aParametersString = "(setq aFileName (append aTestName \".Parameters.txt\"))" +
                "(setq aFileID (fileOpen aFileName 1 0))" +
                "(fileWrite aFileID {" + aParametersString + "})" +
                "(fileClose aFileID 1)";
            string aTrainingString = Globals.ThisAddIn.worksheetToString("Training");
            // Add lisp code to write worksheet contents to the Training file in the server
            aTrainingString = "(setq aFileName (append aTestName \".Training.txt\"))" +
                "(setq aFileID (fileOpen aFileName 1 0))" +
                "(fileWrite aFileID {" + aTrainingString + "})" +
                "(fileClose aFileID 1)";
            string aTestingString = Globals.ThisAddIn.worksheetToString("Testing");
            // Add lisp code to write worksheet contents to the Testing file in the server
            aTestingString = "(setq aFileName (append aTestName \".Testing.txt\"))" +
                "(setq aFileID (fileOpen aFileName 1 0))" +
                "(fileWrite aFileID {" + aTestingString + "})" +
                "(fileClose aFileID 1)";

            string aGenerateDataCmd = "";
            string aFormattedText = string.Format("(setq aTestName \"{0}\")", upTestName.Text);
            aGenerateDataCmd = "((lambda()" +
                "vars:(aFileID aFileName aText aTestName aGsmResult)" +
                aFormattedText + aParametersString + aTrainingString + aTestingString +
                "(setq aGsmResult (gsmDemo aTestName))" +
                //"(setq aGsmResult true)" +
                "(if (= aGsmResult true)" +
                "    (begin" +
                "    (setq aText (append \"***DisplayWorkSheet***\" aTestName))" +
                "    (setq aFileID (fileOpen (append aTestName {.Results.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (setq aFileID (fileOpen (append aTestName {.Estimates.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (setq aFileID (fileOpen (append aTestName {.Statistics.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (writeln \"Done.\")" +
                "    )" +
                "else" +
                "    (setq aText {Error running gsmDemo})" +
                ")" +
                "aText))";
            Globals.ThisAddIn.cWaitForm.setVisible(true); // Modeless dialog box
            submit(aGenerateDataCmd);
            Close();
        }

        /// <summary>
        /// submit sends an amp eval message to the server
        /// </summary>
        /// <param name="iText">string to be evaluated</param>
        private void submit(string iText)
        {
            // prepend _ais|eval|exp|
            string aAmpMsg = string.Format("_ais{0}eval{0}exp{0}{1}", "\x7F", iText);
            Globals.ThisAddIn.cAppClient.Submit(Globals.ThisAddIn.cReceiver, ref aAmpMsg);
        }
    }
}
