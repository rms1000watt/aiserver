using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;

namespace TestSocket
{
    class Program
    {
        static void Main(string[] args)
        {
            IPAddress[] addresses = Dns.GetHostAddresses("localhost");
            foreach (IPAddress address in addresses)
            {
                Console.WriteLine(address.ToString());
            }
            Console.ReadKey();
        }
    }
}
