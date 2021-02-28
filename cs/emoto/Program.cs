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

            int maxInitSum     = Mis.Reduce(list);
            int evenMaxInitSum = EvenMis.Reduce(list);

            Console.WriteLine(" Mis: {0}", maxInitSum);         // 5
            Console.WriteLine("EMis: {0}", evenMaxInitSum);     // 4
        }
    }
}
