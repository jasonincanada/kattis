/*  Tree Shopping (difficulty 5.3) - https://open.kattis.com/problems/treeshopping

    This is the naive iterative solution to this problem, with quadratic running time.
    The time limit is exceeded on the kattis servers for the third test. The fastest C++
    solutions are 0.01s so there's surely a linear time solution to this, but if so, it
    eludes me. The semiring approach I've been experimenting with requires the predicate
    operator's set to range over the possible lengths of arbitrary tree segments, but
    this can be up to 200000 for this problem, so multiplying with this operator for each
    n seems to make the cost cubic again, which is what we turned to the semiring approach
    to avoid to begin with. ??

*/

using System;
using System.Collections.Generic;
using Kattis.Parsing;

namespace Kattis.TreeShopping
{
    class Program
    {
        static void Main(string[] args)
        {
            // Parsing
            TestCase testCase = ReadInput();

            int count = testCase.NumTrees;
            var heights = testCase.Heights;

            int lowest = int.MaxValue;

            for (int i = 0; i <= heights.Count - count; i++)
            {
                int min = int.MaxValue;
                int max = int.MinValue;

                for (int j = 0; j < count; j++)
                {
                    int height = heights[i+j];

                    if (height < min) min = height;
                    if (height > max) max = height;
                }

                if (max - min < lowest)
                    lowest = max - min;
            }

            Console.WriteLine("{0}", lowest);
        }

        private static TestCase ReadInput()
        {
            /*
            return new TestCase
            {
                NumTrees = 2,
                Heights = new List<int> { 1, 3, 5, 7, 9, 11, 13, 15, 17, 16 }
            };
            */

            string header = Console.ReadLine();
            string heights = Console.ReadLine();

            return new TestCase
            {
                NumTrees = Parse.NthNumber(header, 1),
                Heights = Parse.NumberList(heights)
            };
        }
    }


    public class TestCase
    {
        public int NumTrees { get; set; }
        public List<int> Heights { get; set; }
    }

}
