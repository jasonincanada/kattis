using System.Collections.Generic;
using System.Linq;

namespace Kattis.Parsing
{
    public static class Parse
    {
        public static int NthNumber(string s, int i)
        {
            return int.Parse(s.Split(' ')[i]);
        }

        public static List<int> NumberList(string prices)
        {
            return prices.Split(' ').Select(int.Parse).ToList();
        }
    }
}
