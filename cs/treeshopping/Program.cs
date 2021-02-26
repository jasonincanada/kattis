/*  Commercials (difficulty 1.9) - https://open.kattis.com/problems/commercials

    C# port of my haskell solution using semirings, including the linear-time reducer
    found in [Emoto]

    The runtime for this on the kattis servers is 0.12s, where the fastest is 0.04s.
    We could probably gain some efficiency by using a mutable accumulator in the semiring
    reducer instead of creating a new object every time


    References:
  
    [Emoto] https://www.jstage.jst.go.jp/article/imt/7/2/7_567/_article

*/

using System;
using System.Collections.Generic;

using Kattis.Mits;
using Kattis.Parsing;
using Kattis.Semiring;

namespace Kattis.RadioCommercials
{
    class Program
    {
        static void Main(string[] args)
        {
            // Parsing
            TestCase testCase = ReadInput();

            int dollars = testCase.CostPerTimeslot;
            var listeners = testCase.Listeners;

            // Operations
            RadioSR radioSemiring = new RadioSR(dollars);

            var mits = new Mits<int, Sym>(radioSemiring, Sym.Cast);
            Sym final = mits.Reduce(listeners);

            Console.WriteLine("{0}", final.Sum - (dollars * final.Count));
        }

        private static TestCase ReadInput()
        {
            /*
            return new TestCase {
                CostPerTimeslot = 20,
                Listeners = new List<int> { 18, 35, 6, 80, 15, 21 }
            };
            */

            string header = Console.ReadLine();
            string prices = Console.ReadLine();

            return new TestCase
            {
                CostPerTimeslot = Parse.NthNumber(header, 1),
                Listeners = Parse.NumberList(prices)
            };
        }
    }

    // Our solution's custom semiring
    public class RadioSR : ISemiring<Sym>
    {
        private int _costPerTimeslot;

        public RadioSR(int costPerTimeslot)
        {
            _costPerTimeslot = costPerTimeslot;
        }

        public Sym Add(Sym a, Sym b)
        {
            if (a.NegInf) return b;
            if (b.NegInf) return a;

            if (a.Sum - (a.Count * _costPerTimeslot) > b.Sum - (b.Count * _costPerTimeslot))
                return a;
            else
                return b;
        }

        public Sym Mult(Sym a, Sym b)
        {
            if (a.NegInf || b.NegInf)
                return Sym.New(true, 0, 0);

            return Sym.New(false, a.Count + b.Count, a.Sum + b.Sum);
        }
    }

    // A value from our custom semiring's symbol set
    public class Sym
    {
        public bool NegInf { get; private set; }
        public int Count { get; private set; }
        public int Sum { get; private set; }

        public static Sym Cast(int n)
        {
            return New(false, 1, n);
        }

        public static Sym New(bool negInf, int count, int sum)
        {
            return new Sym
            {
                NegInf = negInf,
                Count = count,
                Sum = sum
            };
        }

        public override string ToString()
        {
            return NegInf ? "Sym NegInf"
                          : string.Format("Sym {0} {1}", Count, Sum);
        }
    }

    public class TestCase
    {
        public int CostPerTimeslot { get; set; }
        public List<int> Listeners { get; set; }
    }

}
