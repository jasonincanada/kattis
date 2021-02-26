namespace Kattis.Semiring
{
    public interface ISemiring<T>
    {
        T Add(T a, T b);
        T Mult(T a, T b);
    }
}
