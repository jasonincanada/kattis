using System;
using System.Collections.Generic;
using Kattis.LinearAlgs;

namespace emoto
{
    class Program
    {
        static void Main(string[] args)
        {
            // Sample problem from [Emoto] section 3.1
            var list = new List<int>() { 2, 3, -6, 5 };

            int maxInitSum = Mis.Reduce(list);

            Console.WriteLine("{0}", maxInitSum);
        }
    }
}
