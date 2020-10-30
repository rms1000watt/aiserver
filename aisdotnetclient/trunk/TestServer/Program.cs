using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
using System.Net.Sockets;

namespace TestServer
{
    /// <summary>
    /// The test server is a console application that allows connections from the ASocket object,
    /// and provides command-line interface for server operations.
    /// </summary>
    class Program
    {
        static Socket aServerSocket = null;
        static Socket aClientSocket = null;

        static void AcceptComplete(object iSource, SocketAsyncEventArgs iEvent)
        {
            aClientSocket = iEvent.AcceptSocket;
            Console.WriteLine();
            Console.WriteLine(DateTime.Now.TimeOfDay +  ": INFO: Connected: " + aClientSocket.RemoteEndPoint.ToString());

            SocketAsyncEventArgs aRecvAsync = new SocketAsyncEventArgs();
            byte[] aBfr = new byte[1024];
            aRecvAsync.Completed += new EventHandler<SocketAsyncEventArgs>(ReceiveComplete);
            aRecvAsync.SetBuffer(aBfr, 0, aBfr.Length);

            if (!aClientSocket.ReceiveAsync(aRecvAsync))
            {
                ReceiveComplete(null, aRecvAsync);
            }
        }

        static void ReceiveComplete(object iSource, SocketAsyncEventArgs iEvent)
        {
            if (iEvent.BytesTransferred > 0)
            {
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Request: ");
                DumpBytes(iEvent.Buffer, 0, iEvent.BytesTransferred);
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Total of {0} byte/s", iEvent.BytesTransferred);
                
                aClientSocket.Send(iEvent.Buffer, iEvent.BytesTransferred, SocketFlags.None);
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Response sent");
                aClientSocket.ReceiveAsync(iEvent);
            }
            else
            {
                Console.WriteLine(DateTime.Now.TimeOfDay + ": INFO: Disconnected: " + aClientSocket.RemoteEndPoint.ToString());
                aClientSocket.Close();
                aClientSocket = null;

                SocketAsyncEventArgs aAsyncAccept = new SocketAsyncEventArgs();
                aAsyncAccept.Completed += new EventHandler<SocketAsyncEventArgs>(AcceptComplete);
                aServerSocket.AcceptAsync(aAsyncAccept);
            }
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

        static void Main(string[] args)
        {
            String aInput = String.Empty;
            IPAddress aIPAdd = null;
            IPEndPoint aEndP = null;
            Int32 aPort = 0;

            while (true)
            {
                try
                {
                    Console.Write("> ");
                    aInput = Console.ReadLine();

                    if (aInput == "quit")
                    {
                        break;
                    }
                    else if (aInput == "listen")
                    {
                        if (aServerSocket != null)
                        {
                            Console.WriteLine("Error: " + "Socket is already bound. Please close the socket first.");
                            continue;
                        }

                        aServerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

                        Console.Write("Enter IP: ");
                        aInput = Console.ReadLine();
                        aIPAdd = IPAddress.Parse(aInput);

                        Console.Write("Enter Port: ");
                        aInput = Console.ReadLine();
                        aPort = Int32.Parse(aInput);

                        aEndP = new IPEndPoint(aIPAdd, aPort);
                        aServerSocket.Bind(aEndP);
                        aServerSocket.Listen(10);

                        SocketAsyncEventArgs aAsyncAccept = new SocketAsyncEventArgs();
                        aAsyncAccept.Completed += new EventHandler<SocketAsyncEventArgs>(AcceptComplete);

                        aServerSocket.AcceptAsync(aAsyncAccept);
                    }
                    else if (aInput == "close")
                    {
                        if (aServerSocket.Connected)
                            aServerSocket.Disconnect(true);

                        aServerSocket.Close();
                        aServerSocket = null;                        
                    }
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine("Error: " + e.Message);
                }
            }
        }
    }
}
