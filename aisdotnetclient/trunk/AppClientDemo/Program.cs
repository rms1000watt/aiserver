using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using AppClient;

namespace AppClientDemo
{
    class Program
    {
        static AGlobals globals;

        static void Main(string[] args)
        {
            globals = AGlobals.GetSingleton();
            bool quit = false;
            string inputLine;
            string[] inputArgs;
            char[] delim = { ' ' };
            string cmd;

            //
            AReturnReceiver defaultReceiver = new AReturnReceiver();
            defaultReceiver.ReturnOutputEvent += new EventHandler<AAsyncEventArgs>(ResponseHandler);
            AAppClient appClient = new AAppClient("localhost", 8081, defaultReceiver, "Demo");

            do
            {
                Console.Write("> ");
                inputLine = Console.ReadLine();
                inputLine = inputLine.Trim();
                inputArgs = inputLine.Split(delim);
                cmd = inputArgs[0].ToLower();
                if (cmd == "quit" || cmd == "exit")
                {
                    quit = true;
                }
                else if (cmd == "connect")
                {
                    appClient.Host = inputArgs[1];
                    appClient.Port = ushort.Parse(inputArgs[2]);
                    appClient.OpenConnection(defaultReceiver);
                }
                else if (cmd == "disconnect")
                {
                    appClient.CloseConnection(defaultReceiver, 0, ACloseMode.Disconnect);
                }
                else if (cmd == "logon")
                {
                    appClient.Logon(defaultReceiver, inputArgs[1], inputArgs[2]);
                }
                else if (cmd == "logoff")
                {
                    appClient.Logoff(defaultReceiver);
                }
                else if (cmd == "getsessions")
                {
                    appClient.GetSessions(defaultReceiver, inputArgs[1]);
                }
                else if (cmd == "opensession")
                {
                    appClient.OpenSession(defaultReceiver, inputArgs[1]);
                }
                else if (cmd == "closesession")
                {
                    // close mode 
                    // 0 - default
                    // 1 - disconnect
                    // 2 - soft
                    // 3 - firm
                    // 4 - hard
                    appClient.CloseSession(defaultReceiver, int.Parse(inputArgs[1]), int.Parse(inputArgs[2]));
                }
                else if (cmd == "connectsession")
                {
                    appClient.ConnectSession(defaultReceiver, int.Parse(inputArgs[1]));
                }
                else if (cmd == "eval_display")
                {
                    appClient.Eval(defaultReceiver, "(display \"hello world\")", null, 0);
                }
                else if (cmd == "eval_add")
                {
                    appClient.Eval(defaultReceiver, "(+ 1 2)", null, 0);
                }
                else if (cmd == "help" || cmd == "?")
                {
                    Console.WriteLine("connect [host] [port]");
                    Console.WriteLine("logon [username] [password]");
                    Console.WriteLine("logoff");
                    Console.WriteLine("eval [command]");
                    Console.WriteLine("getsessions [context]");
                    Console.WriteLine("disconnect");
                    Console.WriteLine("status");
                    Console.WriteLine("quit");
                }
                else
                {
                    Console.WriteLine("Unknown command: " + cmd);
                }
            } while (!quit);
        }

        static void ResponseHandler(object iSource, AAsyncEventArgs iArgs)
        {
            Console.WriteLine("iArgs.RequestId = " + iArgs.RequestId);
            Console.WriteLine("iArgs.RequestType = " + globals.RequestNames[(int)iArgs.RequestType]);
            Console.WriteLine("iArgs.ReturnValue = " + iArgs.ReturnValue);
            if (iArgs.Status == 0)
                Console.WriteLine("iArgs.Status = OK");
            else
                Console.WriteLine("Error: " + iArgs.Error);

            if (iArgs.Out.Length > 0)
                Console.WriteLine("Out: " + iArgs.Out);

            if (iArgs.Display.Length > 0)
                Console.WriteLine("Display: " + iArgs.Display);
            Console.Write("> ");
        }
    }
}
