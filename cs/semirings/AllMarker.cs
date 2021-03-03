using System;
using System.Collections.Generic;

namespace Kattis.LinearAlgs
{
    // From [Emoto sec 4.1] - All-marking generators
    
    public class Marked<T, M>
    {
        public T Element { get; private set; }
        public M Mark { get; private set; }

        public Marked(T element, M mark)
        {
            Element = element;
            Mark = mark;
        }
    }

    public class AllMarked<M, U, T> : IListHomomorphism<U, T>
    {
        private IEnumerable<M> _marks;
        private INestedReducer<U, Marked<T, M>> _reducer;

        public AllMarked(IEnumerable<M> marks, INestedReducer<U, Marked<T, M>> reducer)
        {
            _marks = marks;
            _reducer = reducer;
        }

        public U Combine(U a, U b) => _reducer.Semiring.Mult(a, b);

        public U Prepare(T element)
        {
            // Emoto lemma 18
            IListHomomorphism<U, M> inner = new InnerMarker<M, U, T>(element,
                                                                     _reducer.Prepare,
                                                                     _reducer.Semiring.Add);

            return ListHomomorphism.Run(inner, _marks);
        }
    }
    
    public class InnerMarker<M, U, T> : IListHomomorphism<U, M>
    {
        private T _element;
        private Func<Marked<T, M>, U> _prepare;
        private Func<U, U, U> _add;

        public InnerMarker(T element, Func<Marked<T, M>, U> prepare, Func<U, U, U> add)
        {
            _element = element;
            _prepare = prepare;
            _add = add;
        }

        public U Combine(U a, U b) => _add(a, b);
        public U Prepare(M mark) => _prepare(new Marked<T, M>(_element, mark));
    }
}
