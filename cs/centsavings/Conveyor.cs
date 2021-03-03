using System;
using Kattis.LinearAlgs;
using Kattis.Semiring;

namespace centsavings
{
    // I had hopes this would be a semiring but the property tests found
    // a non-left-distributive triple (see PropertyTests.cs)
    //
    // so it can't be used as a semiring and the idea of a semiring-based
    // algorithm for this challenge doesn't seem possible :(

    public class ConveyorSR : ISemiring<Sym>
    {
        public Sym AdditiveIdentity { get { return new SymInf(); } }

        public Sym Add(Sym a, Sym b)
        {
            if (a is SymInf) { return b; }
            if (b is SymInf) { return a; }

            // TODO: < or <= ?
            if (a.Price <= b.Price)
                return a;
            else
                return b;
        }

        public Sym Mult(Sym a, Sym b)
        {
            if (a is SymInf) return a;
            if (b is SymInf) return b;

            if (a is SymLoose && b is SymLoose)
                return new SymLoose((a as SymLoose).Loose + (b as SymLoose).Loose);

            if (a is SymLoose && b is SymDivs)
            {
                SymLoose A = a as SymLoose;
                SymDivs B = b as SymDivs;

                return new SymDivs(B.Dividers, A.Loose + B.Left, B.Inner, B.Right);
            }

            if (a is SymDivs && b is SymLoose)
            {
                SymDivs A = a as SymDivs;
                SymLoose B = b as SymLoose;

                return new SymDivs(A.Dividers, A.Left, A.Inner, A.Right + B.Loose);
            }

            if (a is SymDivs && b is SymDivs)
            {
                SymDivs A = a as SymDivs;
                SymDivs B = b as SymDivs;

                return new SymDivs(A.Dividers + B.Dividers,
                                   A.Left,
                                   A.Inner + Sym.Round(A.Right + B.Left) + B.Inner,
                                   B.Right);
            }

            throw new NotImplementedException("Some Sym combinations not considered");
        }
    }
  
    public class ConveyorNR : INestedReducer<Sym, Marked<int, bool>>
    {
        private ISemiring<Sym> _semiring;
        public ISemiring<Sym> Semiring => _semiring;

        public ConveyorNR()
        {
            _semiring = new ConveyorSR();
        }

        public Sym Prepare(Marked<int, bool> element) => Sym.FromMarked(element);
    }
}
