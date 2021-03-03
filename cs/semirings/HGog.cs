using System;
using System.Collections.Generic;
using Kattis.HPredicate;
using Kattis.Semigroup;

namespace Kattis.HGog
{
    // Cp - hpredicate's state set
    // M  - mark type
    // T  - list element type
    public interface IHGog<Cp, M, T>
    {
        HPredicate<Cp, (M, T)> hPredicate { get; }
        bool Ok(M mark);
        IEnumerable<M> Marks { get; }
    }

    public class InitsCp : ISemigroup<InitsCp>
    {
        public bool I { get; set; }
        public bool A { get; set; }
        public bool N { get; set; }

        public InitsCp Combine(InitsCp a, InitsCp b)
        {
            return new InitsCp
            {
                I = (a.I && b.N) || (a.A && b.I),
                A = a.A && b.A,
                N = a.N && b.N
            };
        }
    }

    public class InitsGog<T> : IHGog<InitsCp, bool, T>
    {
        public HPredicate<InitsCp, (bool, T)> hPredicate => new PInits<T>();
        public IEnumerable<bool> Marks => new List<bool> { true, false };
        public bool Ok(bool mark) => mark;
    }

    internal class PInits<T> : HPredicate<InitsCp, (bool, T)>
    {
        public bool Accept(InitsCp a) => a.I;
        public InitsCp Combine(InitsCp a, InitsCp b) => a.Combine(a, b);

        public InitsCp Prepare((bool, T) element)
        {
            return new InitsCp
            {
                I = element.Item1,
                A = element.Item1,
                N = !element.Item1
            };
        }

        // TODO: do we need this
        public IList<InitsCp> Symbols()
        {
            throw new NotImplementedException();
        }
    }

}