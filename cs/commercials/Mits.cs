using System;
using System.Collections.Generic;
using System.Linq;
using Kattis.Semiring;

/*
    References:

    [Emoto]: https://www.jstage.jst.go.jp/article/imt/7/2/7_567/_article
*/

namespace Kattis.Mits
{
    // A linear-time reducer on all segments of a list, constructed from the operators of the
    // underlying semiring from the equivalent cubic-time reducer.  From [Emoto]
    public class Mits<T, U>
    {
        private ISemiring<U> _semiring;
        private Func<T, U> _preparer;

        public Mits(ISemiring<U> semiring, Func<T, U> preparer)
        {
            _preparer = preparer;
            _semiring = semiring;
        }

        private class T4
        {
            public U m { get; private set; }
            public U i { get; private set; }
            public U t { get; private set; }
            public U s { get; private set; }

            public T4(U _m, U _i, U _t, U _s)
            {
                m = _m;
                i = _i;
                t = _t;
                s = _s;
            }
        }

        private static U Ex(T4 tuple)
        {
            return tuple.m;
        }

        public U Reduce(IEnumerable<T> list)
        {
            List<T4> prepared = list.Select(MitsPrepare).ToList();
            T4 final = prepared.Aggregate(MitsCombine);

            return Ex(final);
        }

        // The mits reducer requires four copies of the initially prepared value
        private T4 MitsPrepare(T item)
        {
            return new T4(_preparer(item),
                          _preparer(item),
                          _preparer(item),
                          _preparer(item));
        }

        // This is transcribed directly from [Emoto] p. 568
        private T4 MitsCombine(T4 a, T4 b)
        {
            U m = _semiring.Add(_semiring.Add(a.m, _semiring.Mult(a.t, b.i)), b.m);
            U i = _semiring.Add(a.i, _semiring.Mult(a.s, b.i));
            U t = _semiring.Add(_semiring.Mult(a.t, b.s), b.t);
            U s = _semiring.Mult(a.s, b.s);

            return new T4(m, i, t, s);
        }
    }
}