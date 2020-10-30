using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Windows.Forms;

namespace AppClientExcel
{
    public class ListViewColumnSorter: IComparer
    {
        private int _ColumnToSort;
        private SortOrder _SortOrder;
        private CaseInsensitiveComparer _Comparer;

        public ListViewColumnSorter()
        {
            _ColumnToSort = 0;
            _SortOrder = SortOrder.None;
            _Comparer = new CaseInsensitiveComparer();
        }

        public int Compare(object iX, object iY)
        {
            int aCompareResult;
            ListViewItem aListViewX;
            ListViewItem aListViewY;
            bool aNumeric = false;
            

            aListViewX = (ListViewItem)iX;
            aListViewY = (ListViewItem)iY;

            if (aListViewX.SubItems[_ColumnToSort].Tag != null &&
                aListViewY.SubItems[_ColumnToSort].Tag != null)
            {
                if (aListViewX.SubItems[_ColumnToSort].Tag == "number" &&
                    aListViewY.SubItems[_ColumnToSort].Tag == "number")
                {
                    aNumeric = true;
                }
            }

            if (aNumeric)
            {
                int x = 0;
                int y = 0;

                int.TryParse(aListViewX.SubItems[_ColumnToSort].Text, out x);
                int.TryParse(aListViewY.SubItems[_ColumnToSort].Text, out y);

                if (x < y)
                {
                    aCompareResult = -1;
                }
                else if (x > y)
                {
                    aCompareResult = 1;
                }
                else
                {
                    aCompareResult = 0;
                }
            }
            else
            {
                aCompareResult = _Comparer.Compare(aListViewX.SubItems[_ColumnToSort].Text, aListViewY.SubItems[_ColumnToSort].Text);
            }
            
            if (_SortOrder == SortOrder.Ascending)
            {
                return aCompareResult;
            }
            else if (_SortOrder == SortOrder.Descending)
            {
                return (-aCompareResult);
            }
            else
            {
                return 0;
            }
        }

        public int SortColumn
        {
            set { _ColumnToSort = value; }
            get { return _ColumnToSort; }
        }

        public SortOrder Order
        {
            set { _SortOrder = value; }
            get { return _SortOrder; }
        }
    }
}
