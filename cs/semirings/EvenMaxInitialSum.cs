using System.Collections.Generic;
using System.Linq;
using Kattis.Semiring;

namespace Kattis.LinearAlgs
{
    // Maximum initial even segment sum based on a linear-time semiring-based reducer
    public static class EvenMis
    {
        public static int Reduce(List<int> list)
        {
            var maxSumSR = new MaxSum();
            var evenSumHP = new EvenSum();

            // Generate the index map for the lifted multiplication op ahead of time
            // This is the same information carried by the (+)p operator in the definition
            // of a homomorphic predicate [Emoto def. 10]
            var k_even = new List<(int, int)> { (0, 0), (1, 1) };
            var k_odd = new List<(int, int)> { (0, 1), (1, 0) };
            var multMap = new List<List<(int, int)>> { k_even, k_odd };

            // Starting with our old Max Sum semiring, lift the even sum H-predicate into a lifted semiring
            ISemiring<LiftedSet<int, char>> liftedSR = new LiftedSemiring<char, int, int>(maxSumSR, evenSumHP, multMap);

            // Note this is the *same* Opt.Reduce we used for the normal maximum initial
            // segment sum problem. It has no knowledge of our goal to filter the segment
            // list to even sum segments only; we've embedded that filtering into a new,
            // lifted semiring that we use as a replacement for the unlifted MaxSum one
            var opt = new Opt<int, LiftedSet<int, char>>(liftedSR, Prepare);
            var ycp = opt.Reduce(list);

            return ycp.coefficients.ElementAt(0);
        }

        // This is fp from the homomorphic predicate in Lemma 12 - Lifted semiring
        public static LiftedSet<int, char> Prepare(int number)
        {
            if (number % 2 == 0)
                return new LiftedSet<int, char> { coefficients = new List<int>() { number, int.MinValue } };
            else
                return new LiftedSet<int, char> { coefficients = new List<int>() { int.MinValue, number } };
        }
    }

    // The e-sum H-predicate in Definition 10
    public class EvenSum : HPredicate<char, int>
    {
        public bool Accept(char a)
        {
            return a == 'E';
        }

        public char Combine(char a, char b)
        {
            if (a == b)
                return 'E';
            else
                return 'O';
        }

        public char Prepare(int element)
        {
            if (element % 2 == 0)
                return 'E';
            else
                return 'O';
        }

        public IList<char> Symbols()
        {
            return new List<char>() { 'E', 'O' };
        }
    }

    // The MaxSum semiring isn't even defined in this file, we use the same one
    // defined in MaxInitialSum.cs
    //
    // public class MaxSum : ISemiring<int> {}

}
