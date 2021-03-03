/*  Cent Savings (difficulty 4.7) - https://open.kattis.com/problems/centsavings

    Half-done solution that isn't correct and fails on the 5th kattis test. This was to
    be a semiring-based linear algorithm but it turns out the symbol set I'm using to
    represent arbitrary segments of items on the conveyor belt isn't a semiring
    (it's not distributive... see PropertyTests.cs)

    However, along the way we built up some more machinery from the paper referenced
    below and with luck we'll be able to use it for other challenges

    
    References:
  
    [Emoto] https://www.jstage.jst.go.jp/article/imt/7/2/7_567/_article
*/

using System;
using System.Collections.Generic;
using System.Linq;
using Kattis.HPredicate;
using Kattis.LinearAlgs;
using Kattis.Parsing;
using Kattis.Semiring;

namespace centsavings
{
    class Program
    {
        static void Main(string[] args)
        {
            PropertyTests.Run(100);
            return;

            // UnitTests.Run();
            // return;

            var testcase = ReadInput();
            var d = testcase.NumDividers;

            // An H-predicate that allows a count to be up to a certain amount
            var upToHP = new UpToNPred(d);
            var multMap = new List<List<Tuple<int, int>>>();

            for (int i = 0; i <= d + 1; i++)
            {
                var range = Enumerable.Range(0, i + 1)
                                      .Select(x => new Tuple<int, int>(x, i - x))
                                      .ToList();

                multMap.Add(range);
            }

            var conveyorSR = new ConveyorSR();

            // Embed the UpToNPred H-predicate into ConveyorNR [Emoto theorem 14]
            INestedReducer<Sym, Marked<int, bool>> conveyorNR = new ConveyorNR();
            INestedReducer<LiftedSet<Sym, int>, Marked<int, bool>> nested =
                new LiftedNestedReducer<Sym, int, Marked<int, bool>>(conveyorNR, upToHP, multMap);

            var marks = new List<bool> { true, false };

            // All-mark the lifted nested reducer with T, F and run the
            // resulting list homomorphism [Emoto lemma 18]
            IListHomomorphism<LiftedSet<Sym, int>, int> marked =
                new AllMarked<bool, LiftedSet<Sym, int>, int>(marks, nested);
            
            LiftedSet<Sym, int> result = ListHomomorphism.Run(marked, testcase.Prices);

            var final = upToHP.Symbols()
                              .Where(upToHP.Accept)
                              .Select(i => result.coefficients.ElementAt(i))
                              .Aggregate(conveyorSR.Add)
                              .Price;

            Console.WriteLine("{0}", final);
        }

        private static TestCase ReadInput()
        {
            return new TestCase
            {
                NumDividers = 1,
                Prices = new List<int> { 13, 21, 55, 60, 42 }
            };

            // return new TestCase
            // {
            //     NumDividers = 3,
            //     Prices = new List<int> { 1, 1, 1, 1, 1, 1, 1 }
            // };

            string header = Console.ReadLine();
            string prices = Console.ReadLine();

            return new TestCase
            {
                NumDividers = Parse.NthNumber(header, 1),
                Prices = Parse.NumberList(prices)
            };
        }

        public class TestCase
        {
            public int NumDividers { get; set; }
            public List<int> Prices { get; set; }
        }
    }
}
