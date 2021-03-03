namespace Kattis.Semigroup
{
    public interface ISemigroup<T>
    {
        T Combine(T a, T b);
    }
}
