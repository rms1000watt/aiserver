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
using System.Text;

namespace AppClient
{
    /// <summary>
    /// Node Information.
    /// </summary>
    public class ANodeInfo
    {
        #region Constructors
        /// <summary>
        /// Default constructor.
        /// </summary>
        public ANodeInfo()
        {
            _Type = null;
            _Value = null;
            _Size = null;
            _Date = null;
            _Time = null;
            _Version = null;
            _Symbol = null;
            _UniqueKey = null;
        }

        /// <summary>
        /// Parametized constructor.
        /// </summary>
        /// <param name="irType">Node Type.</param>
        /// <param name="irValue">Node Value.</param>
        /// <param name="irSize">Size.</param>
        /// <param name="irDate">Data.</param>
        /// <param name="irTime">Time.</param>
        /// <param name="irVersion">Version.</param>
        /// <param name="irSymbol">Symbol.</param>
        /// <param name="irUniqueKey">Unique Key.</param>
        public ANodeInfo(string iType, string iValue, string iSize,
                         string iDate, string iTime, string iVersion,
                         string iSymbol, string iUniqueKey)
        {
            _Type = iType;
            _Value = iValue;
            _Size = iSize;
            _Date = iDate;
            _Time = iTime;
            _Version = iVersion;
            _Symbol = iSymbol;
            _UniqueKey = iUniqueKey;
        }
        #endregion

        #region Properties
        /// <summary>
        /// Node Type.
        /// </summary>
        public string Type
        {
            get { return _Type; }
            set { _Type = value; }
        }

        /// <summary>
        /// Node Value.
        /// </summary>
        public string Value
        {
            get { return _Value; }
            set { _Value = value; }
        }

        /// <summary>
        /// Node Size.
        /// </summary>
        public string Size
        {
            get { return _Size; }
            set { _Size = value; }
        }

        /// <summary>
        /// Date.
        /// </summary>
        public string Date
        {
            get { return _Date; }
            set { _Date = value; }
        }

        /// <summary>
        /// Time.
        /// </summary>
        public string Time
        {
            get { return _Time; }
            set { _Time = value; }
        }

        /// <summary>
        /// Version.
        /// </summary>
        public string Version
        {
            get { return _Version; }
            set { _Version = value; }
        }

        /// <summary>
        /// Symbol.
        /// </summary>
        public string Symbol
        {
            get { return _Symbol; }
            set { _Symbol = value; }
        }

        /// <summary>
        /// Unique Key.
        /// </summary>
        public string UniqueKey
        {
            get { return _UniqueKey; }
            set { _UniqueKey = value; }
        }

        #endregion

        #region Private Members

        private string _Type;
        private string _Value;
        private string _Size;
        private string _Date;
        private string _Time;
        private string _Version;
        private string _Symbol;
        private string _UniqueKey;

        #endregion
    }
}
