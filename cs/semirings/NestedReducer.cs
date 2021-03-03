// Nested homomorphic reducers from [Emoto]

using System;
using System.Collections.Generic;
using System.Linq;
using Kattis.HPredicate;
using Kattis.Semiring;

namespace Kattis.LinearAlgs
{
    public interface INestedReducer<γ, T>
    {
        ISemiring<γ> Semiring { get; }
        γ Prepare(T element);
    }

     // Filter embedding into a nested homomorphic reducer [Emoto theorem 14]
    public class LiftedNestedReducer<γ, Cp, T> : INestedReducer<LiftedSet<γ, Cp>, T>
    {
        private INestedReducer<γ, T> _underlying;
        private ISemiring<LiftedSet<γ, Cp>> _semiring;
        private HPredicate<Cp, T> _hPredicate;

        public LiftedNestedReducer(INestedReducer<γ, T> underlying,
                                   HPredicate<Cp, T> hPredicate,
                                   List<List<Tuple<int, int>>> multMap)
        {
            _underlying = underlying;
            _semiring = new LiftedSemiring<γ, Cp, T>(underlying.Semiring, hPredicate, multMap);
            _hPredicate = hPredicate;
        }

        public ISemiring<LiftedSet<γ, Cp>> Semiring => _semiring;

        public LiftedSet<γ, Cp> Prepare(T element)
        {
            var prepped = _hPredicate.Prepare(element);
            int index = _hPredicate.Symbols().IndexOf(prepped);

            // Build a "one-hot" vector with a single coefficient set
            IList<γ> coefficients = _hPredicate.Symbols()
                                               .Select(x => _underlying.Semiring.AdditiveIdentity)
                                               .ToList();

            coefficients[index] = _underlying.Prepare(element);

            return new LiftedSet<γ, Cp>
            {
                coefficients = coefficients
            };
        }
    }
}
