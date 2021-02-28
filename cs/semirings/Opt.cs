using System;
using System.Collections.Generic;
using System.Linq;

namespace Kattis.Semiring
{
    // A linear-time semiring-based reducer on all prefixes (initial segments) of a list
    // From [Emoto p. 570]
    public class Opt<T, U>
    {
        private ISemiring<U> _semiring;
        private Func<T, U> _preparer;

        public Opt(ISemiring<U> semiring, Func<T, U> preparer)
        {
            _preparer = preparer;
            _semiring = semiring;
        }

        private class T2
        {
            public U i { get; private set; }
            public U s { get; private set; }

            public T2(U _i, U _s)
            {
                i = _i;
                s = _s;
            }
        }

        private static U Ex(T2 tuple)
        {
            return tuple.i;
        }

        public U Reduce(IEnumerable<T> list)
        {
            List<T2> prepared = list.Select(OptCompare).ToList();
            T2 final = prepared.Aggregate(OptCombine);

            return Ex(final);
        }

        // The opt reducer requires two copies of the initially prepared value
        private T2 OptCompare(T item)
        {
            return new T2(_preparer(item),                         
                          _preparer(item));
        }

        // This is transcribed directly from [Emoto] p. 570
        private T2 OptCombine(T2 a, T2 b)
        {
            U i = _semiring.Add(a.i, _semiring.Mult(a.s, b.i));
            U s = _semiring.Mult(a.s, b.s);

            return new T2(i, s);
        }
    }
}