using System;
using System.Collections.Generic;
using Kattis.Semiring;

namespace Kattis.LinearAlgs
{
    // Maximum initial segment sum based on a linear-time semiring-based reducer
    public static class Mis
    {
        public static int Reduce(List<int> list)
        {
            var maxSumSR = new MaxSum();
            Opt<int, int> opt = new Opt<int, int>(maxSumSR, x => x);

            return opt.Reduce(list);
        }
    }

    public class MaxSum : ISemiring<int>
    {
        public int Add(int a, int b)
        {
            return Math.Max(a, b);
        }

        public int Mult(int a, int b)
        {
            if (a == AdditiveIdentity || b == AdditiveIdentity)
                return AdditiveIdentity;
            else
                return a + b;
        }

        public int AdditiveIdentity { get { return int.MinValue; } }
    }
}