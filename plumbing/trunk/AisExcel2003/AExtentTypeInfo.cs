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
    /// Extent Type Information.
    /// </summary>
    public class AExtentTypeInfo
    {
        #region Constructors
        /// <summary>
        /// Default constructor.
        /// </summary>
        public AExtentTypeInfo()
        {
            _TypeName = null;
            _ActionCodes = null;
        }
        /// <summary>
        /// Parametized constructor.
        /// </summary>
        /// <param name="irTypeName">Type name.</param>
        /// <param name="irActionCodes">Allowable action codes.</param>
        public AExtentTypeInfo(ref String irTypeName, ref String irActionCodes)
        {
            _TypeName = irTypeName;
            _ActionCodes = irActionCodes;
        }
        #endregion

        #region Properties
        /// <summary>
        /// Extent type name.
        /// </summary>
        public string TypeName
        {
            get { return _TypeName; }
            set { _TypeName = value; }
        }
        /// <summary>
        /// Allowable action codes.
        /// </summary>
        public string ActionCodes
        {
            get { return _ActionCodes; }
            set { _ActionCodes = value; }
        }
        #endregion

        #region Private Members
        private string _TypeName;
        private string _ActionCodes;
        #endregion
    }
}
