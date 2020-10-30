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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using AppClient;
using Excel = Microsoft.Office.Interop.Excel;

namespace AisExcel2003
{
	public enum ceTestCases
	{
		CrossCorrelation,
		Cubic,
		CyclicSeries,
		Elipsoid,
		HiddenModel,
		HyperTangent,
		Linear,
		MixedModel,
		RandomModal,
		RandomRoot,
		Ratio,
		SquareRoot,
		Trigonometric
	};

	public enum ceModelNames
	{
		regressMVLALPS,
		// regressGSOALPS,
		regressSWARM
	};

	public enum ceSVMKernelIDs
	{
		binary,
		bipolar,
		composite,
		cosine,
		cube,
		exp,
		linear,
		log,
		poly,
		quart,
		sigmoid,
		sine,
		square,
		tan,
		tanh
	};

	public partial class Generate_Data : Form
	{
		public static upWaitForm cWaitForm;
		public Generate_Data()
		{	InitializeComponent();
            // Add List of Test Cases
			int aCount = Enum.GetValues(typeof( ceTestCases )).Length;
			System.Object[] aTestCases = new System.Object[aCount];
			upTestName.Text = "MyTest";
			string[] aTestCaseNames = Enum.GetNames(typeof(ceTestCases));
			for (int aCtr = 0; aCtr < aCount; aCtr++)
			{   aTestCases[aCtr] = aTestCaseNames[aCtr];
			}
			upTestCase.Items.AddRange(aTestCases);
			upTestCase.SelectedIndex = 0;

            // Add List of Model Names
            aCount = Enum.GetValues(typeof(ceTestCases)).Length;
            aTestCases = new System.Object[aCount];
            aTestCaseNames = Enum.GetNames(typeof(ceTestCases));
            for (int aCtr = 0; aCtr < aCount; aCtr++)
            {   aTestCases[aCtr] = aTestCaseNames[aCtr];
            }
            upTestCase.Items.AddRange(aTestCases);
            upTestCase.SelectedIndex = 0;
            // Add List of SVM Kernel IDs
			upRows.Text = "1000";  // Set default Rows to 100
			upColumns.Text = "5"; // Set default Columns to 5
			upModelName.Enabled = false;
			upKernelID.Enabled = false;
			upAmtNoise.Enabled = false;
			upNumGenerations.Enabled = false;
			upMaxTime.Enabled = false;
			upHaltScore.Enabled = false;
			upNumChampions.Enabled = false;
		}

		private void upClose_Click(object sender, EventArgs e)
		{   
			Close();
		}

		private void upRun_Click(object sender, EventArgs e)
		{
            RunTest aRunTestForm = new RunTest();
            aRunTestForm.ShowDialog();
		}

		private void upGenerateData_Click(object sender, EventArgs e)
		{
            if (validateData() == false)
            {   return;
            }
			generateData();
            Close();
		}

		private void upAdvancedOptions_CheckedChanged(object sender, EventArgs e)
		{
			if (upAdvancedOptions.Checked == true)
			{   upModelName.Enabled = true;
				upKernelID.Enabled = true;
				upAmtNoise.Enabled = true;
				upNumGenerations.Enabled = true;
				upMaxTime.Enabled = true;
				upHaltScore.Enabled = true;
				upNumChampions.Enabled = true;
			}
			else
			{	upModelName.Enabled = false;
				upKernelID.Enabled = false;
				upAmtNoise.Enabled = false;
				upNumGenerations.Enabled = false;
				upMaxTime.Enabled = false;
				upHaltScore.Enabled = false;
				upNumChampions.Enabled = false;
			}
		}

        /// <summary>
        /// validateData checks if the values for the parameter files are valid or not.
        /// </summary>
        /// <returns>true if all the parameters are valid, otherwise returns false</returns>
		private bool validateData()
		{
			string aCurrentText = "";
			long aLongValue = 0;
			double aDoubleValue = 0.0;
			aCurrentText = upRows.Text;
			try
			{   // Check if the row value is a valid integer
				aLongValue = Convert.ToInt32(aCurrentText);
                if (aLongValue <= 0)
                {   MessageBox.Show("Row should be greater than 0");
                    return false;
                }
			}
			catch (Exception)
			{   MessageBox.Show("Invalid Row Value");
				return false;
			}
			aCurrentText = upColumns.Text;
			try
			{  // Check if the columns value is a valid integer
				aLongValue = Convert.ToInt32(aCurrentText);
                if (aLongValue <= 0)
                {   MessageBox.Show("Column should be greater than 0");
                    return false;
                }
			}
			catch (Exception)
			{   MessageBox.Show("Invalid Column Value");
				return false;
			}
            if (upAdvancedOptions.Checked == true)
            {   aCurrentText = upAmtNoise.Text;
                try
                {  // Check if the noise value is a valid integer
                    aLongValue = Convert.ToInt32(aCurrentText);
                    if (aLongValue < 0)
                    {
                        MessageBox.Show("Amount of noise cannot be a negative value");
                        return false;
                    }
                }
                catch (Exception)
                {   MessageBox.Show("Invalid Noise Value");
                    return false;
                }
                aCurrentText = upNumGenerations.Text;
                try
                {   // Check if the number of generations value is a valid integer
                    aLongValue = Convert.ToInt32(aCurrentText);
                    if (aLongValue <= 0)
                    {  MessageBox.Show("Number of generations should be greater than 0");
                        return false;
                    }
                }
                catch (Exception)
                {   MessageBox.Show("Invalid Number of Generations Value");
                    return false;
                }
                aCurrentText = upMaxTime.Text;
                try
                {   // Check if the Max Time value is a valid integer
                    aLongValue = Convert.ToInt32(aCurrentText);
                    if (aLongValue <= 0)
                    {   MessageBox.Show("Max Time should be greater than 0");
                        return false;
                    }
                }
                catch (Exception)
                {   MessageBox.Show("Invalid Maximum Time Value");
                    return false;
                }
                // Check if the row value is a valid integer
                aCurrentText = upHaltScore.Text;
                try
                {   // Check if the Halting Score value is a valid integer
                    aDoubleValue = Convert.ToDouble(aCurrentText);
                    if (aDoubleValue < 0)
                    {  MessageBox.Show("Halting score cannot be a negative value");
                        return false;
                    }
                }
                catch (Exception)
                {   MessageBox.Show("Invalid Halting Score Value");
                    return false;
                }
                aCurrentText = upKernelID.Text;
                // Check if the row value is a valid integer
                aCurrentText = upNumChampions.Text;
                try
                {   // Check if the number of champions value is a valid integer
                    aLongValue = Convert.ToInt32(aCurrentText);
                    if (aLongValue <= 0 || aLongValue > 25)
                    {   MessageBox.Show("Number of champions should be greater than 0 and less than 25");
                        return false;
                    }
                }
                catch (Exception)
                {   MessageBox.Show("Invalid Number of Champions Value");
                    return false;
                }
            }
			return true;
		}
        /// <summary>
        /// Sends a command to the server to generate data with the parameters specified in the U.I.
        /// The string returned is parsed in the ConnectForm's defaultResponseHandler and displayed
        /// as worksheets.
        /// </summary>
        private void generateData()
        {
            string aGenerateDataCmd = "";
            string aFormattedText = "";
            if (upAdvancedOptions.Checked == false)
            {  aFormattedText = string.Format("(setq aTestName \"{0}\")(setq aGsmResult (gsmDemo.generateData aTestName \"{1}\" {2} {3}))",
                    upTestName.Text, upTestCase.Text, upRows.Text, upColumns.Text);
            }
            else
            {   aFormattedText = string.Format
					("(setq aTestName \"{0}\")(setq aGsmResult (gsmDemo.generateData aTestName \"{1}\" {2} {3} {4} {5} {6} \"{7}\" {8} \"{9}\" {10}))",
					upTestName.Text, upTestCase.Text, upRows.Text, upColumns.Text, upAmtNoise.Text, upNumGenerations.Text,
					upMaxTime.Text, upModelName.Text, upHaltScore.Text, upKernelID.Text, upNumChampions.Text);
            }
            aGenerateDataCmd = "((lambda()" +
                "vars:(aFileID aText aTestName aGsmResult)" +
                aFormattedText +
                "(if (= aGsmResult true)" +
                "    (begin" +
                "    (setq aText (append \"***DisplayGenerateData***\" aTestName))" +
                "    (setq aFileID (fileOpen (append aTestName {.Parameters.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (setq aFileID (fileOpen (append aTestName {.Training.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (setq aFileID (fileOpen (append aTestName {.Testing.txt}) 0 0))" +
                "    (setq aText (append aText {***Marker***} (fileRead aFileID)))" +
                "    (fileClose aFileID 1)" +
                "    (writeln \"Done.\")" +
                "    )" +
                "else" +
                "    (setq aText {Error running gsmDemo})" +
                ")" +
                "aText))";
            submit(aGenerateDataCmd);
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
