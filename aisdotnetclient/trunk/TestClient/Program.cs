using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using AppClient;

namespace TestClient
{
    class Program
    {
        /// <summary>
        /// Handler for Connected Event.
        /// </summary>
        /// <param name="iSource"></param>
        /// <param name="iArgs"></param>
        static void OnConnected(object iSource, EventArgs iArgs)
        {
            GMutex.WaitOne();
            Console.WriteLine();
            Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Connection accepted.");
            GMutex.ReleaseMutex();
        }

        /// <summary>
        /// Handler for Disconnected Event.
        /// </summary>
        /// <param name="iSource"></param>
        /// <param name="iArgs"></param>
        static void OnDisconnected(object iSource, EventArgs iArgs)
        {
            GMutex.WaitOne();
            Console.WriteLine();
            Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Disconnected from server.");
            GMutex.ReleaseMutex();
        }

        /// <summary>
        /// Handler for Socket Error Event.
        /// </summary>
        /// <param name="iSource"></param>
        /// <param name="iArgs"></param>
        static void OnSocketError(object iSource, SocketErrorEventArgs iArgs)
        {
            GMutex.WaitOne();
            Console.WriteLine();
            Console.WriteLine(DateTime.Now.TimeOfDay + ": WARN: Socket Status: {0}, Error: {1}", iArgs.Status, iArgs.Error);
            GMutex.ReleaseMutex();
        }

        /// <summary>
        /// Handler for Response Event.
        /// </summary>
        /// <param name="iSource"></param>
        /// <param name="iArgs"></param>
        static void OnResponse(object iSource, ResponseEventArgs iArgs)
        {
            GMutex.WaitOne();
            Console.WriteLine();
            Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Response:");
            DumpBytes(iArgs.ResponseBuffer, 0, iArgs.ResponseBuffer.Length);

            if (iArgs.DataBuffer != null)
            {
                Console.WriteLine();
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Data:", iArgs.DataBuffer.Length);
                DumpBytes(iArgs.DataBuffer, 0, iArgs.DataBuffer.Length);
            }

            Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Response, Total of {0} byte/s", iArgs.ResponseBuffer.Length);
            if (iArgs.DataBuffer != null)
            {
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Data, Total of {0} byte/s", iArgs.DataBuffer.Length);
            }

            GMutex.ReleaseMutex();
        }

        static void DoConnect()
        {
            String aIPAdd = String.Empty;
            UInt16 aPort = 0;

            Console.Write("Enter IP: ");
            aIPAdd = Console.ReadLine();

            Console.Write("Enter Port: ");
            aPort = UInt16.Parse(Console.ReadLine());

            GClient.OpenConnection(ref aIPAdd, aPort);
        }

        static void DoDisconnect()
        {
            GClient.CloseConnection();
        }

        static void DoSend()
        {
            String aRequest = String.Empty;
            String aData = String.Empty;
            byte[] aDataBytes = null;
            Int32 aDataLen = 0;

            Console.Write("Enter Request: ");
            aRequest = Console.ReadLine();

            Console.Write("Include Data? (Y/N) ");
            aData = Console.ReadLine();

            if (aData.ToLower() == "y")
            {
                Console.Write("Enter Data: ");
                aData = Console.ReadLine();
                aDataBytes = System.Text.Encoding.ASCII.GetBytes(aData);
                aDataLen = aDataBytes.Length;
            }
            else
            {
                aDataBytes = null;
                aDataLen = 0;
            }

            GClient.Submit(ref aRequest, aDataBytes, aDataLen);
        }

        static void DoSendLarge()
        {
            String aInput = String.Empty;
            String aRequest = String.Empty;
            UInt16 aNumOfBytes = 0;
            byte aByte = 0;
            byte[] aByteString = null;
            byte[] aData = null;
            Int32 aRepeat;
            Int32 aInterval;

            // Get number of bytes to send
            Console.Write("Enter Size of Request (in bytes): ");
            aInput = Console.ReadLine();
            aNumOfBytes = UInt16.Parse(aInput);

            if (aNumOfBytes > 0)
            {
                aByteString = new byte[aNumOfBytes];

                for (UInt16 i = 0; i < (aNumOfBytes); i++)
                {
                    aByte = (byte)(i % Byte.MaxValue);

                    if (aByte < 33)
                    {
                        aByte = 46;
                    }
                    else if (aByte > 127 && aByte < 160)
                    {
                        aByte = 46;
                    }

                    aByteString[i] = aByte;
                }

                aRequest = System.Text.Encoding.GetEncoding(28591).GetString(aByteString);

                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Request:");
                DumpBytes(aByteString, 0, aByteString.Length);
            }

            Console.Write("Enter Size of Data (in bytes): ");
            aNumOfBytes = UInt16.Parse(Console.ReadLine());

            if (aNumOfBytes > 0)
            {
                aData = new byte[aNumOfBytes];

                for (UInt16 i = 0; i < (aNumOfBytes); i++)
                {
                    aByte = (byte)(i % Byte.MaxValue);

                    if (aByte < 33)
                    {
                        aByte = 46;
                    }
                    else if (aByte > 127 && aByte < 160)
                    {
                        aByte = 46;
                    }

                    aData[i] = aByte;
                }

                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Data: ");
                DumpBytes(aData, 0, aData.Length);
            }
            else
                aNumOfBytes = 0;

            Console.Write("Enter No. of Times to Repeat: ");
            aRepeat = Int32.Parse(Console.ReadLine());

            Console.Write("Enter No. of Interval (in ms): ");
            aInterval = Int32.Parse(Console.ReadLine());

            if (aInterval < 0)
            {
                Console.Error.WriteLine("Error: Invalid Interval Size.");
                return;
            }

            for (int i = 0; i < aRepeat; i++)
            {
                // Send request message
                GClient.Submit(ref aRequest, aData, aNumOfBytes);
                // Sleep
                Thread.Sleep(aInterval);
            }
        }

        static void DoSendRepeat()
        {

        }

        static void DumpBytes(byte[] iMemory, int iOffset, int iCount)
        {
            if (iMemory == null)
            {
                return;
            }

            if (iOffset < 0 || iCount < 0 || iOffset + iCount > iMemory.Length)
            {
                throw new ArgumentOutOfRangeException("iOffset");
            }

            int aIdx1;
            int aIdx2;
            StringBuilder aLatin = new StringBuilder(17);

            for (aIdx1 = 0; aIdx1 < iCount; aIdx1++)
            {
                if ((aIdx1 % 16) == 0)
                    Console.Write("{0}:   ", (aIdx1 / 16).ToString("0000"));

                Console.Write(" ");

                aIdx2 = iOffset + aIdx1;
                Console.Write(iMemory[aIdx2].ToString("X2"));
                aLatin.Append(System.Text.Encoding.GetEncoding(28591).GetString(iMemory, aIdx2, 1));

                if ((aIdx1 % 16) == 15)
                {
                    Console.Write("    ");
                    Console.Write(aLatin.ToString());
                    Console.WriteLine();
                    aLatin.Remove(0, aLatin.Length);
                }
            }
            
            if ((aIdx1 % 16) != 0)
            {
                int r = 16 - (aIdx1 % 16);
                for (int i = 0; i < r; i++)
                    Console.Write(" ..");

                Console.Write("    ");
                Console.Write(aLatin.ToString());
                Console.WriteLine();
            }
        }

        static private Mutex GMutex = null;
        static private ASocket GClient = null;

        static void Main(string[] args)
        {
            GClient = new ASocket("localhost", AisProtocolType.App);
            GMutex = new Mutex();
            String aInput = String.Empty;

            // Register event handlers
            GClient.ConnectedEvent += OnConnected;
            GClient.DisconnectedEvent += OnDisconnected;
            GClient.SocketErrorEvent += OnSocketError;
            GClient.ResponseEvent += OnResponse;

            while (true)
            {
                try
                {
                    GMutex.WaitOne();
                    Console.Write("> ");
                    GMutex.ReleaseMutex();

                    aInput = Console.ReadLine().Trim();

                    if (aInput == "connect")
                    {
                        DoConnect();
                    }
                    else if (aInput == "disconnect")
                    {
                        DoDisconnect();
                    }
                    else if (aInput == "send")
                    {
                        DoSend();
                    }
                    else if (aInput == "sendlarge")
                    {
                        DoSendLarge();
                    }
                    else if (aInput == "quit")
                    {
                        DoDisconnect();
                        break;
                    }
                    else if (aInput == "sendrepeat")
                    {
                        DoSendRepeat();
                    }
                    else if (aInput.Length > 0)
                    {
                        Console.WriteLine("Unknown command");
                        Console.WriteLine("Try connect, disconnect, send, sendlarge, sendrepeate, or quit");
                    }
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine(e.Message);
                }
            }
        }
    }
}
