using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using AppClient;

namespace AppTestClient
{
    static class Program
    {
        static void ResponseHandler(object iSource, AAsyncEventArgs iEventArgs)
        {
            Console.WriteLine("Response");
            Console.WriteLine("====================");

            Console.WriteLine("Request Id: {0}", iEventArgs.RequestId);
            Console.WriteLine("Connection Id: {0}", iEventArgs.ConnectionId);
            Console.WriteLine("Out: {0}", iEventArgs.Out);
            Console.WriteLine("Status: {0}", iEventArgs.Status);
            Console.WriteLine("Display: {0}", iEventArgs.Display);
        }

        [STAThread]
        static void Main(string[] args)
        {
            //AAsyncEventArgs aReceiver = new AAsyncEventArgs();
            //aReceiver.Completed += new EventHandler<AAsyncEventArgs>(ResponseHandler);

            //AAppClient aClient = new AAppClient("127.0.0.1", 8081, aReceiver, "Test Client");
            
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new AppTestClientForm());
        }
    }
}
