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
