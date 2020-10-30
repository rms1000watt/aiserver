using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;

namespace AisExcel2003
{
    public partial class WorksheetBrowser : Form
    {
        public WorksheetBrowser(string iImportFileName)
        {
            InitializeComponent();
        }

        private void upCloseButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void upImportButton_Click(object sender, EventArgs e)
        {
            Excel.Worksheet aWorksheet;
            Excel.Workbook aWorkbook;
            Excel.Range aRange;
            string aParameter = "";
            string aValue = "";
            string aFilename = upParameterText.Text;
            if (aFilename.EndsWith(".txt") || aFilename.EndsWith(".xlsw") || aFilename.EndsWith(".csv") || aFilename.EndsWith("xls"))
            {
                aWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aFilename,
                    0,
                    false,
                    6,
                    "",
                    "",
                    true,
                    Excel.XlPlatform.xlWindows,
                    "\t",
                    false,
                    false,
                    0,
                    true,
                    1,
                    0);
                aWorksheet = aWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                aWorksheet.Name = "Parameters";
                aRange = aWorksheet.UsedRange;
                aRange.Columns.AutoFit();
                // Try to load Training and Testing file if it exists in the parameter file
                object[,] aLoadedData = AppClient.AUtilities.worksheetToObjectArray(aWorksheet);
                int aRowCount = aLoadedData.GetUpperBound(0);
                for (int aCtr = 1; aCtr <= aRowCount; aCtr++)
                {
                    aParameter = aLoadedData[aCtr, 1].ToString();
                    if (aParameter == "TestingFile")
                    {
                        try
                        {
                            Excel.Workbook aTestingWorkbook;
                            Excel.Worksheet aTestingWorksheet;
                            aValue = aLoadedData[aCtr, 2].ToString();
                            aTestingWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aValue,
                                0,
                                true,
                                6,
                                "",
                                "",
                                true,
                                Excel.XlPlatform.xlWindows,
                                "\t",
                                false,
                                false,
                                0,
                                true,
                                1,
                                0);
                            aTestingWorksheet = aTestingWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                            object aWorksheetValues = aTestingWorksheet.UsedRange.Value2;
                            aTestingWorksheet.Name = "Testing";
                            aTestingWorksheet.Copy(Type.Missing, aWorkbook.Worksheets.get_Item(1));
                            aTestingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                        catch (Exception)
                        {
                            MessageBox.Show("There was an error loading the Testing File: " + aValue);
                        }
                    }
                    if (aParameter == "TrainingFile")
                    {
                        try
                        {
                            Excel.Workbook aTrainingWorkbook;
                            Excel.Worksheet aTrainingWorksheet;
                            aValue = aLoadedData[aCtr, 2].ToString();
                            aTrainingWorkbook = Globals.ThisAddIn.Application.Workbooks.Open(aValue,
                                0,
                                true,
                                6,
                                "",
                                "",
                                true,
                                Excel.XlPlatform.xlWindows,
                                "\t",
                                false,
                                false,
                                0,
                                true,
                                1,
                                0);
                            aTrainingWorksheet = aTrainingWorkbook.Worksheets.get_Item(1) as Excel.Worksheet;
                            object aWorksheetValues = aTrainingWorksheet.UsedRange.Value2;
                            aTrainingWorksheet.Name = "Training";
                            aTrainingWorksheet.Copy(Type.Missing, aWorkbook.Worksheets.get_Item(1));
                            aTrainingWorkbook.Close(false, Type.Missing, Type.Missing);
                        }
                        catch (Exception)
                        {
                            MessageBox.Show("There was an error loading the Training File: " + aValue);
                        }
                    }
                }
            }
            Close();
        }

        private void submit(string iText)
        {
            // prepend _ais|eval|exp|
            string aAmpMsg = string.Format("_ais{0}eval{0}exp{0}{1}", "\x7F", iText);
            ConnectForm.cAppClient.Submit(ConnectForm.cReceiver, ref aAmpMsg);
        }

        private void upParameterButton_Click(object sender, EventArgs e)
        {
            OpenFileDialog aBrowseDialog = new OpenFileDialog();
            aBrowseDialog.Filter = "Text Files (*.txt)|*.txt|Excel Files (*.xls)|*.xls|CSV Files(*.csv)|*.csv";
            aBrowseDialog.Title = "Browse Excel file";
            if (aBrowseDialog.ShowDialog() == DialogResult.Cancel)
                return;
            try
            { 
                upParameterText.Text = aBrowseDialog.FileName;
            }
            catch (Exception)
            {
                MessageBox.Show("Error opening file", "File Error",
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }
    }
}
