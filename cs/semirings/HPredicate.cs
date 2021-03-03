using System;
using System.Collections.Generic;
using System.Linq;
using Kattis.LinearAlgs;

namespace Kattis.HPredicate
{
    // Homomorphic predicates [Emoto def. 10]
    public interface HPredicate<Cp, T>
    {
        bool Accept(Cp a);
        Cp Combine(Cp a, Cp b);
        Cp Prepare(T element);
        IList<Cp> Symbols();
    }

    // Valid up to and including n with addition to combine
    public class UpToNPred : HPredicate<int, Marked<int, bool>>
    {
        private int _upTo;

        public UpToNPred(int upTo) { _upTo = upTo; }

        public bool Accept(int cp) => cp <= _upTo;

        // Handle overflows by capping the sum at _upTo + 1
        // (instead of accounting for every integer above _upTo separately)
        public int Combine(int a, int b) => Math.Min(_upTo + 1, a + b);

        public int Prepare(Marked<int, bool> element) => element.Mark ? 1 : 0;

        public IList<int> Symbols() => Enumerable.Range(0, _upTo + 1 + 1)
                                                 .ToList();
    }
}
