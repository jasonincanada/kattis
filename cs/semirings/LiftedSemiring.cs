using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Kattis.Semiring
{
    // Homomorphic predicates [Emoto def. 10]
    public interface HPredicate<Cp, T>
    {
        bool Accept(Cp a);
        Cp Combine(Cp a, Cp b);
        Cp Prepare(T element);
        IList<Cp> Symbols();
    }

    // An instantiation of this class is a value in the lifted set with p
    public class LiftedSet<γ, Cp>
    {
        // The coefficients from γ correspond 1-to-1 with elements of the state set C
        public IEnumerable<γ> coefficients { get; set; }
    }

    // Lifted Semiring [Emoto lemma 12]
    //
    // γ  - gamma, the underlying semiring's symbol set
    // Cp - the H-predicate state set
    // T  - list element type
    public class LiftedSemiring<γ, Cp, T> : ISemiring<LiftedSet<γ, Cp>>
    {
        private ISemiring<γ> _semiring;
        private HPredicate<Cp, T> _p;

        // Have the developer specify the predicate's state symbol mapping indices to
        // avoid having to completely generalize the lifted multiplication operation
        private List<List<(int, int)>> _multMap;

        public LiftedSemiring(ISemiring<γ> semiring, HPredicate<Cp, T> p, List<List<(int, int)>> multMap)
        {
            // Sanity check to make sure there's an entry in the multiplication map for
            // each element of the predicate's state set
            Debug.Assert(multMap.Count == p.Symbols().Count);

            _semiring = semiring;
            _p = p;
            _multMap = multMap;
        }

        // Lifted addition operation
        public LiftedSet<γ, Cp> Add(LiftedSet<γ, Cp> a, LiftedSet<γ, Cp> b)
        {
            return new LiftedSet<γ, Cp>
            {
                // zip the coefficient vectors with the underlying semiring's addition
                coefficients = a.coefficients.Zip<γ, γ, γ>(b.coefficients, _semiring.Add)
            };
        }

        // Lifted multiplication operation
        public LiftedSet<γ, Cp> Mult(LiftedSet<γ, Cp> a, LiftedSet<γ, Cp> b)
        {
            List<γ> coefficients = new List<γ>();

            // Sanity check to make sure we have the right number of coefficients
            Debug.Assert(a.coefficients.Count() == _p.Symbols().Count);
            Debug.Assert(b.coefficients.Count() == _p.Symbols().Count);

            foreach (var operandIndices in _multMap)
            {
                γ k = _semiring.AdditiveIdentity;

                foreach (var (i, j) in operandIndices)
                {
                    var combined = _semiring.Mult(a.coefficients.ElementAt(i),
                                                  b.coefficients.ElementAt(j));

                    // This Add combines two values to one using the underlying semiring
                    k = _semiring.Add(k, combined);
                }

                // This is List.Add tacking the next coefficient onto the end of the list
                coefficients.Add(k);
            }

            return new LiftedSet<γ, Cp> { coefficients = coefficients };
        }

        // The lifted additive identity is just the underlying additive identity in each
        // coefficient slot, one per predicate state symbol
        public LiftedSet<γ, Cp> AdditiveIdentity
        {
            get
            {
                return new LiftedSet<γ, Cp>
                {
                    coefficients = Enumerable.Range(1, _p.Symbols().Count)
                                             .Select(x => _semiring.AdditiveIdentity)
                                             .ToList()
                };
            }
        }
    }
}
