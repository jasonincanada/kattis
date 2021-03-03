using System;
using System.Collections.Generic;
using System.Linq;
using Kattis.Semigroup;

namespace Kattis.LinearAlgs
{
    public interface IListHomomorphism<U, T> : ISemigroup<U>
    {
        U Prepare(T element);
    }

    public class Sum : IListHomomorphism<int, int>
    {
        public int Combine(int a, int b) => a + b;
        public int Prepare(int element) => element;
    }

    public static class ListHomomorphism
    {
        public static U Run<U, M>(IListHomomorphism<U, M> inner, IEnumerable<M> list)
        {
            return list.Select(inner.Prepare)
                       .Aggregate(inner.Combine);
        }
    }
}
