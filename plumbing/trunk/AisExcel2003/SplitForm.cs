using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;

namespace AisExcel2003
{
	public partial class SplitForm : Form
	{
		public SplitForm()
		{
			InitializeComponent();
			upBandedCheckBox.Checked = false;		// Default to top N.
			upFileTextBox.Text = GSMDemo.mSplitFileText;
		}

		private void upBrowseButton_Click(object sender, EventArgs e)
		{
			// Browse local filesystem for a set of data to be split
			string aFilespec = "";
            OpenFileDialog aBrowseDialog = new OpenFileDialog();
            aBrowseDialog.Filter = "Tab-delimited Files (*.txt)|*.txt|Comma-separated Files (*.csv)|*.csv|Excel Files (*.xls)|*.xls";
            aBrowseDialog.Title = "Locate Data File to be Split";
            if (aBrowseDialog.ShowDialog() == DialogResult.Cancel)
                return;
            try
            {   aFilespec = aBrowseDialog.FileName;
				GSMDemo.mSplitFileText = upFileTextBox.Text = aFilespec;
            }
            catch (Exception iEx)
            {   MessageBox.Show("Locate data file error: " + iEx.ToString(), "Find Data File Error",
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }
		}

		private void upCancelButton_Click(object sender, EventArgs e)
		{
			Close();
		}

		private void upSplitButton_Click(object sender, EventArgs e)
		{
			// Initialize automatic vars
			Excel.Application aApp = Globals.ThisAddIn.Application;
			bool aIsBanded = upBandedCheckBox.CheckState == CheckState.Checked;
			string aLine;
			int aNRows = 0;						// Total number of rows in data file
			int aRow = 0;						// Number of rows read from file so far
			int aTargetPct = (int)upPctUpDown.Value;
			int aTestRows = 0;					// Number of testing rows
			int aTrainRows = 0;					// Number of training rows
			double aTrainPct = 50.0;			// Percent of rows allocated so far to Training. Let 0 / 0 = 50

			// Validate. Check inputs.  Make sure file exists.
			// pending...
			
			// Worksheets. Create Worksheets.
            Excel.Workbook aWorkbook;
			Excel.Sheets aCurrentWorkSheets;
			Excel.Worksheet aTestWorkSheet;
			Excel.Worksheet aTrainWorkSheet;
			try
            {  aWorkbook = Globals.ThisAddIn.Application.ActiveWorkbook;
            }
            catch (Exception)
            {   MessageBox.Show("There is no existing workbook", "Split error");
                return;
            }
            aCurrentWorkSheets = aWorkbook.Sheets as Excel.Sheets;
			try
            {   aTrainWorkSheet = aCurrentWorkSheets.get_Item("Training") as Excel.Worksheet;
				aTrainWorkSheet.Cells.ClearContents();
            }
            catch (Exception)
            {   aTrainWorkSheet = (Excel.Worksheet)aCurrentWorkSheets.Add(aCurrentWorkSheets[1], Type.Missing, Type.Missing, Type.Missing);
                aTrainWorkSheet.Name = "Training";
            }
            try
            {   aTestWorkSheet = aCurrentWorkSheets.get_Item("Testing") as Excel.Worksheet;
				aTestWorkSheet.Cells.ClearContents();
            }
            catch (Exception)
            {   aTestWorkSheet = (Excel.Worksheet)aCurrentWorkSheets.Add(aCurrentWorkSheets[1], Type.Missing, Type.Missing, Type.Missing);
                aTestWorkSheet.Name = "Testing";
            }
            // Screen. Eliminate screen updating
			aApp.ScreenUpdating = false;
			// Parameter Worksheet.  If no Parameters sheet, create one
			// Pending ...
			if (aIsBanded)
			{	// Banded.  Put every nth row in training such that % of training rows = Target Percent
				System.IO.StreamReader aReader = new System.IO.StreamReader(upFileTextBox.Text);
				while ((aLine = aReader.ReadLine()) != null)
				{	if (aRow == 0)
					{	// Copy header into first row of both Training and Testing worksheets.
						addRowToWorksheet(1, aLine, aTrainWorkSheet);
						addRowToWorksheet(1, aLine, aTestWorkSheet);					
					}
					else
					{	if (aTrainPct < (double)aTargetPct)
						{	// Add row to training set
							++aTrainRows;
							addRowToWorksheet(aTrainRows + 1, aLine, aTrainWorkSheet);
						}
						else
						{	// Add row to testing set
							addRowToWorksheet(++aTestRows + 1, aLine, aTestWorkSheet);
						}
						aTrainPct = (aTrainRows * 100.0) / ++aNRows;
					}
					++aRow;
				}
				aReader.Close();
			}
			else  // Top N. Put top N rows into Training Set
			{	System.IO.StreamReader aRead = new System.IO.StreamReader(upFileTextBox.Text);
				while ((aLine = aRead.ReadLine()) != null)
				{	++aNRows;
				}
				// Deduct for header row
				if (aNRows > 0)
					--aNRows;
				aRead.Close();
				aTrainRows = (aNRows * aTargetPct + 50) / 100;  // Round to nearest integer
				System.IO.StreamReader aReader = new System.IO.StreamReader(upFileTextBox.Text);
				while ((aLine = aReader.ReadLine()) != null)
				{	if (aRow == 0)
					{	// Copy header into first row of both Training and Testing worksheets.
						addRowToWorksheet(1, aLine, aTrainWorkSheet);
						addRowToWorksheet(1, aLine, aTestWorkSheet);					
					}
					else
					{	if (aRow <= aTrainRows)
						{	// Add row to training set
							addRowToWorksheet(aRow + 1, aLine, aTrainWorkSheet);
						}
						else
						{	// Add row to testing set
							addRowToWorksheet(++aTestRows + 1, aLine, aTestWorkSheet);
						}
					}
					++aRow;
				}
				aReader.Close();
			}
			aApp.ScreenUpdating = true;
			Close();
		}
		private void addRowToWorksheet(int iRow, string iLine, Excel.Worksheet iWorksheet)
		{
		    Excel.Range aRange;
            string[] aLineArray = iLine.Split(new string[] {"\t"}, System.StringSplitOptions.None);
            int aNCols = aLineArray.Length;

            // Create an object array, one entry per column
            object[] aWorksheetArray = new object[aNCols];
            for (int aCol = 0; aCol < aNCols; aCol++)
            {   aWorksheetArray[aCol] = aLineArray[aCol];
            }
			aRange = iWorksheet.get_Range(iWorksheet.Cells[iRow, 1], iWorksheet.Cells[iRow, aNCols]);
            // aRange = aRange.get_Resize(1, aNCols);
            aRange.set_Value(System.Reflection.Missing.Value, aWorksheetArray);
            aRange.Columns.AutoFit();
		}
	}
}
