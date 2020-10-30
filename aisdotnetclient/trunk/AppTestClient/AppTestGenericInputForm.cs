using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace AppTestClient
{
    public partial class AppTestGenericInputForm : Form
    {
        public AppTestGenericInputForm()
        {
            InitializeComponent();

            _ComponentList = new List<Component>();
        }

        public void SetItems(int iNum)
        {
            uItemsPanel.Controls.Clear();
            _ComponentList.Clear();

            for (int i = 0; i < iNum; i++)
            {
                FlowLayoutPanel aNewEntry = new FlowLayoutPanel();
                Label aNewLabel = new Label();
                TextBox aNewTextBox = new TextBox();

                aNewEntry.FlowDirection = FlowDirection.LeftToRight;
                aNewEntry.AutoSize = true;
                aNewEntry.AutoSizeMode = AutoSizeMode.GrowAndShrink;

                aNewEntry.Controls.Add(aNewLabel);
                aNewEntry.Controls.Add(aNewTextBox);

                uItemsPanel.Controls.Add(aNewEntry);

                _ComponentList.Add(aNewEntry);
            }
        }

        public void SetPrompt(int iIndex, string iPrompt)
        {
            if (iIndex < _ComponentList.Count)
            {
                FlowLayoutPanel aEntry = (FlowLayoutPanel)_ComponentList[iIndex];
                aEntry.Controls[0].Text = iPrompt;
            }
        }

        public string GetValue(int iIndex)
        {
            string aValue = null;

            if (iIndex < _ComponentList.Count)
            {
                FlowLayoutPanel aEntry = (FlowLayoutPanel)_ComponentList[iIndex];
                aValue = aEntry.Controls[1].Text;
            }

            return aValue;
        }

        private List<Component> _ComponentList;
    }
}