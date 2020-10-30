using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using AppClient;
using BalloonCS;

namespace AisExcel2003
{
    public partial class GSMDemo : Form
    {
        #region Delegates
        // required for the cross-thread component update
        private delegate void setEnableGenerateDataButton(bool iEnabled);
        private delegate void setEnableRunButton(bool iEnabled);
        private delegate void displayToStatusBar(string iDisplayText);
        private delegate void setImgToStatusBar(Bitmap iImage);
        #endregion

		public static string mSplitFileText = @"C:\GSM\Data\TestData.txt";
        private RunTest cRunTestForm;
        private ConnectForm cConnectForm;
        private AAsyncEventArgs cReceiver;
        private MessageBalloon cConnectHelp = new MessageBalloon();
        private MessageBalloon cGenerateHelp = new MessageBalloon();
        private MessageBalloon cLoadHelp = new MessageBalloon();
        private MessageBalloon cSplitHelp = new MessageBalloon();
        private MessageBalloon cRunHelp = new MessageBalloon();
        private MessageBalloon cSaveHelp = new MessageBalloon();
        private MessageBalloon cWelcomeHelp = new MessageBalloon();
        private MessageBalloon cAisMsgHelp = new MessageBalloon();
        private MessageBalloon cCloseHelp = new MessageBalloon();
        private MessageBalloon cHelpHelp = new MessageBalloon();

        private bool cConnectHelpShown = false;
        private bool cGenerateHelpShown = false;
        private bool cLoadHelpShown = false;
        private bool cSplitHelpShown = false;
        private bool cRunHelpShown = false;
        private bool cSaveHelpShown = false;
        private bool cWelcomeHelpShown = false;
        private bool cAisMsgHelpShown = false;
        private bool cCloseHelpShown = false;
        private bool cHelpHelpShown = false;
        private bool cShowWelcomePanel = true;
        private bool cShowMessagePanel = true;

        public GSMDemo()
        {
			InitializeComponent();
            cReceiver = new AAsyncEventArgs();
            cConnectForm = Globals.ThisAddIn.cConnectForm;
            upConnectButton.Enabled = true;
            upGenerateDataButton.Enabled = false;
            upLoadButton.Enabled = true;
            upSplitButton.Enabled = true;
            upRunButton.Enabled = false;
            upSaveButton.Enabled = true;
            upCloseButton.Enabled = true;

            BalloonAlignment ba = BalloonAlignment.RightMiddle;
            cConnectHelp.Parent = upConnectButton;
            cConnectHelp.Title = "Connect to AIS";
            cConnectHelp.TitleIcon = TooltipIcon.Info;
            cConnectHelp.Text = "You must connect to an Analytic Information Server(AIS) running the GSM Demo server application" +
                "to enable the Generate Test button and the Run GSM Test button.";
            cConnectHelp.Align = ba;
            cConnectHelp.CenterStem = false;
            cConnectHelp.UseAbsolutePositioning = false;
            cRunHelp.Parent = upRunButton;
            cRunHelp.Title = "Run GSM Test";
            cRunHelp.TitleIcon = TooltipIcon.Info;
            cRunHelp.Text = "After the above worksheets have been filled and modified as necessary, select Run GSM Test to" +
                " conduct a regression.  This test may take a very long time for large data sets.";
            cRunHelp.Align = ba;
            cRunHelp.CenterStem = false;
            cRunHelp.UseAbsolutePositioning = false;
            cGenerateHelp.Parent = upGenerateDataButton;
            cGenerateHelp.Title = "Generate Test Set";
            cGenerateHelp.TitleIcon = TooltipIcon.Info;
            cGenerateHelp.Text = "GSM requires data obtained by generating data or loading data from a local  file.  Click" +
                " on Generate Test to select one of 26 sets of test data.  Three worksheets are addedto your workboolk, " +
                " Parameters, Testing Data, Training Data.  Be sure to add the corresponding file names for each of these" +
                " worksheets to the parameters worksheet if you wish to export these sheets to files.";
            cGenerateHelp.Align = ba;
            cGenerateHelp.CenterStem = false;
            cGenerateHelp.UseAbsolutePositioning = false;
            cLoadHelp.Parent = upLoadButton;
            cLoadHelp.Title = "Load Test Set";
            cLoadHelp.TitleIcon = TooltipIcon.Info;
            cLoadHelp.Text = "If you have previously saved a test set consisting of these three files on your local file" +
                " system, you can select Load Test button as an alternate way to obtain a test set.";
            cLoadHelp.Align = ba;
            cLoadHelp.CenterStem = false;
            cLoadHelp.UseAbsolutePositioning = false;
            cSplitHelp.Parent = upSplitButton;
            cSplitHelp.Title = "Split Test Data";
            cSplitHelp.TitleIcon = TooltipIcon.Info;
            cSplitHelp.Text = "Split divides a data set into a Training set and a Testing set. Import these sets into a" +
                " Training worksheet and a Testing worksheet.";
            cSplitHelp.Align = ba;
            cSplitHelp.CenterStem = false;
            cSplitHelp.UseAbsolutePositioning = false;
            cWelcomeHelp.Parent = upShowWelcomeText;
            cWelcomeHelp.Title = "Toggle Welcome Text Pane";
            cWelcomeHelp.TitleIcon = TooltipIcon.Info;
			cWelcomeHelp.Text = "Expand/collapse the Welcome Text Pane. Expand to view the Welcome Text. Collapse to save space.";
            cWelcomeHelp.Align = ba;
            cWelcomeHelp.CenterStem = false;
            cWelcomeHelp.UseAbsolutePositioning = false;
            cAisMsgHelp.Parent = upShowAISMessages;
            cAisMsgHelp.Title = "Toggle AIS Messages Pane";
            cAisMsgHelp.TitleIcon = TooltipIcon.Info;
			cAisMsgHelp.Text = "Expand/collapse the Ais Messages Pane. Expand to view Ais Messages & to enter AIS commands. Collapse" +
				" to save space.";
			cAisMsgHelp.Align = ba;
            cAisMsgHelp.CenterStem = false;
            cAisMsgHelp.UseAbsolutePositioning = false;
            cSaveHelp.Parent = upSaveButton;
            cSaveHelp.Title = "Save Test Set";
            cSaveHelp.TitleIcon = TooltipIcon.Info;
            cSaveHelp.Text = "Select Save Test to save these three worksheets as a test set (three files) in your local file" +
                " system.  The TestDir parameter selects the folder in your file system where the test set is saved. ";
            cSaveHelp.Align = ba;
            cSaveHelp.CenterStem = false;
            cSaveHelp.UseAbsolutePositioning = false;
            cCloseHelp.Parent = upCloseButton;
            cCloseHelp.Title = "Close";
            cCloseHelp.TitleIcon = TooltipIcon.Info;
            cCloseHelp.Text = "Select Close to dismiss this dialog.  Close does \r\nnot disconnect you from the se" +
                    "rver or terminate \r\nan ongoing test.  You can reopen this dialog from \r\nthe Smil" +
                    "ey Face icon in the custom toolbar.";
            cCloseHelp.Align = ba;
            cCloseHelp.CenterStem = false;
            cCloseHelp.UseAbsolutePositioning = false;
            
            cHelpHelp.Parent = upHelpButton;
            cHelpHelp.Title = "Help on Help";
            cHelpHelp.TitleIcon = TooltipIcon.Info;
            cHelpHelp.Text = "Click on Help to send an email usiing your local email client, if any.  " +
				"Please include a very detailed description of the exact steps required " +
				"to reproduce the problem.  If an email client is not available, just " +
				"send email to tedwilliams@alum.mit.edu.  Thanks!";
            cHelpHelp.Align = ba;
            cHelpHelp.CenterStem = false;
            cHelpHelp.UseAbsolutePositioning = false;
        }
        
        private void upRunButton_Click(object sender, EventArgs e)
        {
            cRunTestForm = new RunTest();
            cRunTestForm.Show();
        }
        private void upLoadButton_Click(object sender, EventArgs e)
        {
            Excel.Worksheet aWorksheet;
            Excel.Workbook aWorkbook;
            string aFilespec = "";
            string aFilename = "";
            string aTestDir = "";
            string aParameter = "";
            string aValue = "";
            OpenFileDialog aBrowseDialog = new OpenFileDialog();
            aBrowseDialog.Filter = "Ini Files (*.ini)|*.ini|All Files (*.*)|*.*";
            aBrowseDialog.Title = "Locate Parameter File";
            if (aBrowseDialog.ShowDialog() == DialogResult.Cancel)
                return;
            try
            {   aFilespec = aBrowseDialog.FileName;
				aTestDir = Path.GetDirectoryName(aFilespec) + @"\";
				aFilename = Path.GetFileName(aFilespec);
            }
            catch (Exception iEx)
            {   MessageBox.Show("Open Parameter file error " + iEx.ToString(), "File Error",
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }
            if (aFilespec.EndsWith(".ini") || aFilespec.EndsWith(".txt"))
            {   aWorkbook = Globals.ThisAddIn.Application.Workbooks.Add(Type.Missing);
                Excel.Worksheet aParameterWorksheet;
                Excel.Workbook aParameterWorkbook;
                aParameterWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aFilespec, 0, false, 6,
                    "", "", true, Excel.XlPlatform.xlWindows, "\t", false, false, 0, true, 1, 0);
                aParameterWorksheet = aParameterWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                aParameterWorksheet.UsedRange.Columns.AutoFit();
                aParameterWorksheet.Name = "Parameters";
                aParameterWorksheet.Copy(Type.Missing, aWorkbook.Worksheets.get_Item(1));
                aParameterWorkbook.Close(false, Type.Missing, Type.Missing);

                // Retrieve the Parameters worksheet values to open the other
                // test worksheets( Training/Testing )
                aWorksheet = aWorkbook.Worksheets.get_Item("Parameters") as Excel.Worksheet;
                // Try to load Training and Testing file if it exists in the parameter file
                object[,] aLoadedData = AppClient.AUtilities.worksheetToObjectArray(aWorksheet);
                int aRowCount = aLoadedData.GetUpperBound(0);
                Excel.Workbook aTestingWorkbook = null, aTrainingWorkbook = null;
                for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
                {   aParameter = aLoadedData[aCtr, 1].ToString();
					// Make sure that TestDir value and ParameterFile value match the fetched value
					if (aParameter == "TestDir")
					{	aValue = aLoadedData[aCtr, 2].ToString();		// We could just set the value without testing
						if (aTestDir != aValue)
							aWorksheet.Cells[aCtr, 2] = aTestDir;
					}
					else if (aParameter == "ParametersFile")
					{	aValue = aLoadedData[aCtr, 2].ToString();		// We could just set the value without testing
						if (aFilename != aValue)
							aWorksheet.Cells[aCtr, 2] = aFilename;
					}
                    else if (aParameter == "TestingFile")
                    {   try
                        {   Excel.Worksheet aTestingWorksheet;
                            aValue = aLoadedData[aCtr, 2].ToString();
                            aTestingWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aValue, 0, true, 6,
								"", "", true, Excel.XlPlatform.xlWindows, "\t", false, false, 0, true, 1, 0);
                            aTestingWorksheet = aTestingWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                            object aWorksheetValues = aTestingWorksheet.UsedRange.Value2;
                            aTestingWorksheet.Name = "Testing";
                            aTestingWorksheet.Copy(Type.Missing, aWorkbook.Worksheets.get_Item(1));
                            aTestingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                        catch (Exception iEx)
                        {   MessageBox.Show("Load " + aValue + " error: " + iEx.ToString());
							if (aTestingWorkbook != null)
							  aTestingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                    }
                    else if (aParameter == "TrainingFile")
                    {   try
                        {   Excel.Worksheet aTrainingWorksheet;
                            aValue = aLoadedData[aCtr, 2].ToString();
                            aTrainingWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aValue, 0, true, 6,
								"", "", true, Excel.XlPlatform.xlWindows, "\t", false, false, 0, true, 1, 0);
                            aTrainingWorksheet = aTrainingWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                            object aWorksheetValues = aTrainingWorksheet.UsedRange.Value2;
                            aTrainingWorksheet.Name = "Training";
                            aTrainingWorksheet.Copy(Type.Missing, aWorkbook.Worksheets.get_Item(1));
                            aTrainingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                        catch (Exception iEx)
                        {  MessageBox.Show("Load " + aValue + " error: " + iEx.ToString());
							if (aTrainingWorkbook != null)
								aTrainingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                    }
                }
            }
            //ThisAddIn.addDropDownOptions("Parameters");
            ThisAddIn.addParameterComments("Parameters");
        }

        private void upGenerateDataButton_Click(object sender, EventArgs e)
        {
            Generate_Data aGenerateDataForm = new Generate_Data();
            aGenerateDataForm.ShowDialog();
        }

        private void upSaveButton_Click(object sender, EventArgs e)
        {
            Excel.Workbook aWorkbook;
            Excel.Worksheet aParamSheet;
            Excel.Worksheet aTrainSheet;
            Excel.Worksheet aTestSheet;
            Excel.Worksheet aResultsSheet;
            Excel.Worksheet aStatisticsSheet;
            Excel.Worksheet aEstimatesSheet;

            string aParameter;
            string aValue;
            bool aTrainFileFound = false;
            bool aTestFileFound = false;
            bool aParameterFileFound = false;
            Excel.XlFileFormat aFileFormat;
            try
            {  aWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;
            }
            catch (Exception)
            {   MessageBox.Show("There is no existing workbook", "Export error");
                return;
            }
            try
            {  aParamSheet = aWorkbook.Worksheets.get_Item("Parameters") as Excel.Worksheet;
            }
            catch (Exception)
            {   MessageBox.Show("Parameters worksheet does not exist", "Export error");
                return;
            }
            try
            {   aTrainSheet = aWorkbook.Worksheets.get_Item("Training") as Excel.Worksheet;
            }
            catch (Exception)
            {   MessageBox.Show("Training worksheet does not exist", "Export error");
                return;
            }
            try
            {    aTestSheet = aWorkbook.Worksheets.get_Item("Testing") as Excel.Worksheet;
            }
            catch (Exception)
            {	MessageBox.Show("Testing worksheet does not exist");
                return;
            }
            // Check if the 3 resulting files(Results/Estimates/Statistics)
            // of a GSM tests exist and save them
            try
            {   aResultsSheet = aWorkbook.Worksheets.get_Item("Results") as Excel.Worksheet;
            }
            catch (Exception)
            {    // Do nothing set the worksheet to null
                aResultsSheet = null;
            }
            try
            {  aEstimatesSheet = aWorkbook.Worksheets.get_Item("Estimates") as Excel.Worksheet;
            }
            catch (Exception)
            {   // Do nothing set the worksheet to null
                aEstimatesSheet = null;
            }
            try
            {   aStatisticsSheet = aWorkbook.Worksheets.get_Item("Statistics") as Excel.Worksheet;
            }
            catch (Exception)
            {  // Do nothing set the worksheet to null
                aStatisticsSheet = null;
            }
            // Traverse the current parameter list
            // Load Training and Testing file as worksheets if it exists in the parameter file
            object[,] aLoadedData = AppClient.AUtilities.worksheetToObjectArray(aParamSheet);
            int aRowCount = aLoadedData.GetUpperBound(0);
            int aTestRow = 0;
            string aTestDir = "";
            int aResultsRow = -1;
            int aStatisticsRow = -1;
            int aEstimatesRow = -1;
            string aResultsLocation = "";
            string aStatisticsLocation = "";
            string aEstimatesLocation = "";
            for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
            {   if (aLoadedData[aCtr, 1] != null)
                {  aParameter = aLoadedData[aCtr, 1].ToString();
                    // Store Results/Estimates/Statistics Location if needed 
                    // for later when saving the worksheets of a GSM Test
                    if (aParameter == "ResultsFile")
                    {   aResultsRow = aCtr;
                        if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
                        {    aResultsLocation = aLoadedData[aCtr, 2].ToString();
                        }
                    }
                    else
                    if (aParameter == "StatisticsFile")
                    {   aStatisticsRow = aCtr;
                        if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
                        {   aStatisticsLocation = aLoadedData[aCtr, 2].ToString();
                        }
                    }
                    if (aParameter == "EstimatesFile")
                    {  aEstimatesRow = aCtr;
                        if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
                        {  aEstimatesLocation = aLoadedData[aCtr, 2].ToString();
                        }
                    } 
					if (aParameter == "TestDir")
					{	aTestRow = aCtr;
						if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
						{   aTestDir = aLoadedData[aCtr, 2].ToString();
							if (!Directory.Exists(aTestDir))
								Directory.CreateDirectory(aTestDir);
							if (!aTestDir.EndsWith(@"\"))
								aTestDir += @"\";
						}
						else
						{	MessageBox.Show("No path provided for the TestDir parameter. Use upcoming browse dialog to locate folder.", "Error");
						}
					}
					if (aParameter == "ParametersFile")
					{	aParameterFileFound = true;
						try
					   {	if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
							{   aValue = aLoadedData[aCtr, 2].ToString();
								if (aValue.ToLower().EndsWith(".xls"))
								{  aFileFormat = Excel.XlFileFormat.xlExcel2;
								}
								else if (aValue.ToLower().EndsWith(".csv"))
								{   aFileFormat = Excel.XlFileFormat.xlCSV;
								}
								else
								{   aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
								}
								if (aTestDir.Length <= 0)
								{	FolderBrowserDialog aDirBrowser = new FolderBrowserDialog();
									if (aDirBrowser.ShowDialog() == DialogResult.OK)
									{	aTestDir = aDirBrowser.SelectedPath + @"\";
										if (aTestRow > 0)
											aParamSheet.Cells[aTestRow, 2] = aTestDir;
									}
								}
								saveWorksheet(aTestDir + aValue, "Parameters", aFileFormat);
							}
							else
							{  MessageBox.Show("Invalid filename for the ParametersFile parameter", "Error");
							}
						}
						catch (Exception iEx)
						{  MessageBox.Show("Save Parameters worksheet error: " + iEx.ToString(), "Save Parameters Error");
						}
					}
                    if (aParameter == "TrainingFile")
                    {  aTrainFileFound = true;
                        try
                        {  if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
                            {	aValue = aLoadedData[aCtr, 2].ToString();
								if (aValue.ToLower().EndsWith(".xls"))
                                {  aFileFormat = Excel.XlFileFormat.xlExcel2;
                                }
                                else if (aValue.ToLower().EndsWith(".csv"))
                                {   aFileFormat = Excel.XlFileFormat.xlCSV;
                                }
                                else
                                {   aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
                                }
                                saveWorksheet(aTestDir + aValue, "Training", aFileFormat);
                            }
                            else
                            {  MessageBox.Show("Invalid filename for the TrainingFile parameter", "Error");
                            }
                        }
                        catch (Exception iEx)
						{   MessageBox.Show("Save Training worksheet error: " + iEx.ToString(), "Save Training Error");
                        }
                    }
                    if (aParameter == "TestingFile")
                    {   aTestFileFound = true;
                        try
                        {  if (aLoadedData[aCtr, 2] != null && aLoadedData[aCtr, 2].ToString().Trim() != "")
                            {	aValue = aLoadedData[aCtr, 2].ToString();
                                if (aValue.ToLower().EndsWith(".xls"))
                                {   aFileFormat = Excel.XlFileFormat.xlExcel2;
                                }
                                else if (aValue.ToLower().EndsWith(".csv"))
                                {   aFileFormat = Excel.XlFileFormat.xlCSV;
                                }
                                else
                                {    aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
                                }
                                saveWorksheet(aTestDir + aValue, "Testing", aFileFormat);
                            }
                            else
                            {   MessageBox.Show("Invalid filename for the TestingFile parameter", "Error");
                            }
                        }
                        catch (Exception iEx)
						{   MessageBox.Show("Save Testing worksheet error: " + iEx.ToString(), "Save Testing Error");
                        }
                    }
				}
            }
            // Save the worksheets of the GSM Test results
            if (aResultsSheet != null)
            {  if (aResultsLocation != null && aResultsLocation.ToString().Trim() != "")
                {   if (aResultsLocation.ToLower().EndsWith(".xls"))
                    {   aFileFormat = Excel.XlFileFormat.xlExcel2;
                    }
                    else if (aResultsLocation.ToLower().EndsWith(".csv"))
                    {   aFileFormat = Excel.XlFileFormat.xlCSV;
                    }
                    else
                    {   aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
                    }
                    saveWorksheet(aTestDir + aResultsLocation, "Results", aFileFormat);
                }
                else
                {   MessageBox.Show("Invalid filename for the ResultsFile parameter", "Error");
                }
            }
            if (aStatisticsSheet != null)
            {   if (aStatisticsLocation != null && aStatisticsLocation.ToString().Trim() != "")
                {   if (aStatisticsLocation.ToLower().EndsWith(".xls"))
                    {   aFileFormat = Excel.XlFileFormat.xlExcel2;
                    }
                    else if (aStatisticsLocation.ToLower().EndsWith(".csv"))
                    {   aFileFormat = Excel.XlFileFormat.xlCSV;
                    }
                    else
                    {  aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
                    }
                    saveWorksheet(aTestDir + aStatisticsLocation, "Statistics", aFileFormat);
                }
                else
                {   MessageBox.Show("Invalid filename for the StatisticsFile parameter", "Error");
                }
            }
            if (aEstimatesSheet != null)
            {   if (aEstimatesLocation != null && aEstimatesLocation.ToString().Trim() != "")
                {  if (aEstimatesLocation.ToLower().EndsWith(".xls"))
                    {   aFileFormat = Excel.XlFileFormat.xlExcel2;
                    }
                    else if (aEstimatesLocation.ToLower().EndsWith(".csv"))
                    {   aFileFormat = Excel.XlFileFormat.xlCSV;
                    }
                    else
                    {  aFileFormat = Excel.XlFileFormat.xlTextMSDOS;
                    }
                    saveWorksheet(aTestDir + aEstimatesLocation, "Statistics", aFileFormat);
                }
                else
                {   MessageBox.Show("Invalid filename for the EstimatesFile parameter", "Error");
                }
            }
            if (aParameterFileFound == false)
            {   MessageBox.Show("Parameter file location is not in the parameters worksheet. Worksheet not saved.");
            }
          if (aTrainFileFound == false)
            {   MessageBox.Show("Training file location is not in the parameters worksheet. Worksheet not saved.");
            }
            if (aTestFileFound == false)
            {   MessageBox.Show("Testing file location is not in the parameters worksheet. Worksheet not saved.");
            }
        }
        
        private void saveWorksheet(string iFilename, string iWorksheetName, Excel.XlFileFormat iFileFormat)
        {
			Excel.Worksheet aTempSheet = Globals.ThisAddIn.Application.ActiveWorkbook.Worksheets.get_Item(iWorksheetName) as Excel.Worksheet;
            Excel.Workbook aTempWorkbook = Globals.ThisAddIn.Application.Workbooks.Add(Type.Missing) as Excel.Workbook;
            aTempSheet.Copy(Type.Missing, aTempWorkbook.Worksheets.get_Item(1));
            aTempWorkbook.SaveAs(iFilename, iFileFormat, Type.Missing, Type.Missing, Type.Missing,
			Type.Missing, Excel.XlSaveAsAccessMode.xlExclusive, Type.Missing, Type.Missing, Type.Missing, Type.Missing, Type.Missing);    
            aTempWorkbook.Close(false, Type.Missing, Type.Missing);            
        }

        private void submit(string iText)
        {
            // prepend _ais|eval|exp|
            string aAmpMsg = string.Format("_ais{0}eval{0}exp{0}{1}", "\x7F", iText);
            int aRet = Globals.ThisAddIn.cAppClient.Submit(Globals.ThisAddIn.cReceiver, ref aAmpMsg);
        }

        public void upConnectButton_Click(object sender, EventArgs e)
		{
			cConnectForm.Show();
		}

		// Enable/disable selected buttons when connected/disconnected
		public void enableButtons(bool iConnected)
		{
			if (iConnected)
			{	upGenerateDataButton.Enabled = true;
				upRunButton.Enabled = true;
			}
			else
			{	upGenerateDataButton.Enabled = false;
				upRunButton.Enabled = false;
			}
		}

        /// <summary>
        /// Display text in the status bar
        /// </summary>
        /// <param name="iDisplayText"></param>
        public void displayStatusBar(string iDisplayText)
        {
            if (upStatusBar.InvokeRequired)
            {   upStatusBar.BeginInvoke(new displayToStatusBar(this.displayStatusBar), iDisplayText);
            }
            else
            {   upStripStatusLabel.Text = iDisplayText;  
            }
        }

        /// <summary>
        /// Display text in the status bar
        /// </summary>
        /// <param name="iDisplayText"></param>
        public void setStatusBarImg(Bitmap iImage)
        {
            if (upStatusBar.InvokeRequired)
            {    upStatusBar.BeginInvoke(new setImgToStatusBar(this.setStatusBarImg), iImage);
            }
            else
            {   upStripStatusLabel.Image = iImage;
            }
        }
  
		private void upSplitButton_Click(object sender, EventArgs e)
		{
			// Toss up a dialog to select mode (banding or percentage) and file location.
			SplitForm aSplitForm = new SplitForm();
			aSplitForm.ShowDialog();
		}

        private void GSMDemo_Load(object sender, EventArgs e)
        {
            upStripStatusLabel.Image = global::AisExcel2003.Properties.Resources.disconnected;
            upStripStatusLabel.Text = "Disconnected";
        }

        private void upAisHelpLabel_Click(object sender, EventArgs e)
        {
            if (cAisMsgHelpShown == false)
            {   cAisMsgHelpShown = true;
                cAisMsgHelp.Show();
            }
            else
            {  cAisMsgHelpShown = false;
                cAisMsgHelp.Hide();
            }
        }

        private void upWelcomeHelpLabel_Click(object sender, EventArgs e)
        {
            if (cWelcomeHelpShown == false)
            {   cWelcomeHelpShown = true;
                cWelcomeHelp.Show();
            }
            else
            {	cWelcomeHelpShown = false;
                cWelcomeHelp.Hide();
            }
        }
       
        private void upWelcomeButton_Click(object sender, EventArgs e)
        {
            if (cShowWelcomePanel == false)
            {   
                cShowWelcomePanel = true;
                Globals.ThisAddIn.cWelcomeForm.Show();
            }
            else
            {   
                cShowWelcomePanel = false;
                Globals.ThisAddIn.cWelcomeForm.Hide();
            }
        }

        private void upAisMsgButton_Click(object sender, EventArgs e)
        {
            if (cShowMessagePanel == false)
            {
                cShowMessagePanel = true;
                Globals.ThisAddIn.cCLIForm.Show();
            }
            else
            {
                cShowMessagePanel = false;
                Globals.ThisAddIn.cCLIForm.Hide();
            }
        }

        /// <summary>
        /// Override the signal for WM_CLOSE(0x0010).  Hide the current window instead of closing it.
        /// </summary>
        /// <param name="m"></param>
        protected override void WndProc(ref System.Windows.Forms.Message m)
        {
            if (m.Msg == 0x0010) // Close Signal
            {    
                //Windows has send the WM_CLOSE message to your form.
                //Ignore this message will make the window stay open.
                //MessageBox.Show("Closing");
                if (upShowAISMessages.Checked == true)
                {
                    Globals.ThisAddIn.cCLIForm.Hide();
                    upShowAISMessages.Checked = false;
                }
                if (upShowWelcomeText.Checked == true)
                {
                    Globals.ThisAddIn.cWelcomeForm.Hide();
                    upShowWelcomeText.Checked = false;
                }
                Hide();
            }
            else
            {
                base.WndProc(ref m);
            }
        }

        private void upCloseButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void upCloseLabel_Click(object sender, EventArgs e)
        {
            if (cCloseHelpShown == false)
            {   cCloseHelpShown = true;
                cCloseHelp.Show();
            }
            else
            {   cCloseHelpShown = false;
                cCloseHelp.Hide();
            }
        }

		private void upHelpButton_Click(object sender, EventArgs e)
		{
            // Open a local email client, if any, so user can email the problem to us
            try
            {
                System.Diagnostics.Process.Start("mailto:tedwilliams@alum.mit.edu?subject=GSMDemo question");
            }
            catch (Exception iEx)
            {
                MessageBox.Show(iEx.Message, "HelpButton: " + iEx.Message, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
		}

        private void upShowAISMessages_CheckedChanged(object sender, EventArgs e)
        {
            if (upShowAISMessages.Checked == true)
            {
                Globals.ThisAddIn.cCLIForm.Show();
            }
            else
            {
                Globals.ThisAddIn.cCLIForm.Hide();
            }
        }

        private void upShowWelcomeText_CheckedChanged(object sender, EventArgs e)
        {
            if (upShowWelcomeText.Checked == true)
            {
                Globals.ThisAddIn.cWelcomeForm.Show();
            }
            else
            {   
                Globals.ThisAddIn.cWelcomeForm.Hide();
            }
        }

		private void upHelpLabel_Click(object sender, EventArgs e)
		{
            if (cHelpHelpShown == false)
            {
                cHelpHelpShown = true;
                cHelpHelp.Show();
            }
            else
            {
                cHelpHelpShown = false;
                cHelpHelp.Hide();
            }
        }

        private void upConnectHelpLabel_Click(object sender, EventArgs e)
        {
            if (cConnectHelpShown == false)
            {
                cConnectHelpShown = true;
                cConnectHelp.Show();
            }
            else
            {
                cConnectHelpShown = false;
                cConnectHelp.Hide();
            }
        }

        private void upLoadHelpLabel_Click(object sender, EventArgs e)
        {
            if (cLoadHelpShown == false)
            {
                cLoadHelpShown = true;
                cLoadHelp.Show();
            }
            else
            {
                cLoadHelpShown = false;
                cLoadHelp.Hide();
            }
        }

        private void upRunHelpLabel_Click(object sender, EventArgs e)
        {
            if (cRunHelpShown == false)
            {
                cRunHelpShown = true;
                cRunHelp.Show();
            }
            else
            {
                cRunHelpShown = false;
                cRunHelp.Hide();
            }
        }

        private void upGenerateHelpLabel_Click(object sender, EventArgs e)
        {
            if (cGenerateHelpShown == false)
            {
                cGenerateHelpShown = true;
                cGenerateHelp.Show();
            }
            else
            {
                cGenerateHelpShown = false;
                cGenerateHelp.Hide();
            }
        }

        private void upSplitHelpLabel_Click(object sender, EventArgs e)
        {
            if (cSplitHelpShown == false)
            {
                cSplitHelpShown = true;
                cSplitHelp.Show();
            }
            else
            {
                cSplitHelpShown = false;
                cSplitHelp.Hide();
            }
        }

        private void upSaveHelpLabel_Click(object sender, EventArgs e)
        {
            if (cSaveHelpShown == false)
            {
                cSaveHelpShown = true;
                cSaveHelp.Show();
            }
            else
            {
                cSaveHelpShown = false;
                cSaveHelp.Hide();
            }
        }
    }
}
