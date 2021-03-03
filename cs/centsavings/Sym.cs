using System;
using Kattis.LinearAlgs;

namespace centsavings
{
    // Represents a segment of items on a conveyor belt
    public abstract class Sym : IEquatable<Sym>
    {
        public abstract int Price { get; }
        public abstract int AdditiveValue { get; }

        public static int Round(int n)
        {
            return ((int)((n + 5) / 10)) * 10;
        }

        public static Sym FromMarked(Marked<int, bool> element)
        {
            if (element.Mark)
                return new SymDivs(1, element.Element, 0, 0);
            else
                return new SymLoose(element.Element);
        }

        public bool Equals(Sym other)
        {
            if (other == null) return false;

            if (this is SymInf && other is SymInf) return true;

            if (this is SymLoose && other is SymLoose)
                return (this as SymLoose).Equals(other as SymLoose);

            if (this is SymDivs && other is SymDivs)
                return (this as SymDivs).Equals(other as SymDivs);

            return false;
        }
    }

    // SymInf, the additive identity of ConveyorSR
    public class SymInf : Sym
    {
        public SymInf() { }
        public override int Price => throw new Exception("Called price on Inf");
        public override int AdditiveValue => throw new NotImplementedException("Called AdditiveValue on Inf");
        public override string ToString() => "SymInf";
    }

    // A segment of items with no dividers
    public class SymLoose : Sym, IEquatable<SymLoose>
    {
        public int Loose { get; private set; }
        public SymLoose(int n) { Loose = n; }
        public override int Price => Sym.Round(Loose);
        public override int AdditiveValue => Loose;
        public override string ToString() => string.Format("loose:{0}", Loose);

        public bool Equals(SymLoose other)
        {
            if (other == null)
                return false;

            return this.Loose == other.Loose;
        }
    }

    // A segment of items with at least one divider
    public class SymDivs : Sym, IEquatable<SymDivs>
    {
        // Number of dividers in this segment
        public int Dividers { get; private set; }

        // Unrounded cost of items to the left of the left-most divider
        public int Left { get; private set; }
        public int Right { get; private set; }

        // Rounded cost of items not accounted for by Left/Right
        public int Inner { get; private set; }

        public override int Price => Sym.Round(Left) + Inner + Sym.Round(Right);

        // The value used for comparison during semiring addition
        public override int AdditiveValue => Left + Inner + Right;

        // oo|oooo|ooooo|o  ->  SymDivs(3, 2, 14, 1)
        public SymDivs(int dividers, int left, int inner, int right)
        {
            Dividers = dividers;
            Left = left;
            Inner = inner;
            Right = right;
        }

        public bool Equals(SymDivs other)
        {
            if (other == null)
                return false;

            return this.Dividers == other.Dividers
                     && this.Inner == other.Inner
                     && this.Left == other.Left
                     && this.Right == other.Right;
        }

        public override string ToString()
        {
            return string.Format("divs:{0}-{1}|{2}|{3}", Dividers, Left, Inner, Right);
        }
    }
}
