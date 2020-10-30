using System;
using System.Collections.Generic;
using System.Text;

namespace AppClient
{
    /// <summary>
    /// Extent Information.
    /// </summary>
    public class AExtentInfo
    {
        #region Constructors

        public AExtentInfo()
        {
            _NodePath = null;
            _Options = null;
            _Nodes = new List<ANodeInfo>();
            _ExtentTypes = new Dictionary<string, AExtentTypeInfo>();
        }

        public AExtentInfo(ref String irNodePath)
        {
            _NodePath = irNodePath;
            _Options = null;
            _Nodes = new List<ANodeInfo>();
            _ExtentTypes = new Dictionary<string, AExtentTypeInfo>();
        }

        #endregion

        #region Properties

        /// <summary>
        /// Current nodes path.
        /// </summary>
        public string NodePath
        {
            get { return _NodePath; }
            set { _NodePath = value; }
        }

        /// <summary>
        /// Options for GetNextLevel.
        /// </summary>
        public string Options
        {
            get { return _Options; }
            set { _Options = value; }
        }

        /// <summary>
        /// Current nodes.
        /// </summary>
        public List<ANodeInfo> Nodes
        {
            get { return _Nodes; }
        }

        /// <summary>
        /// Dictionary of extent types and allowed actions.
        /// </summary>
        public Dictionary<string, AExtentTypeInfo> ExtentTypes
        {
            get { return _ExtentTypes; }
        }

        #endregion

        private string _NodePath;
        private string _Options;
        private List<ANodeInfo> _Nodes;
        private Dictionary<string, AExtentTypeInfo> _ExtentTypes;
    }
}
