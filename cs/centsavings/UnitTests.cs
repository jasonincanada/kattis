using System;

namespace centsavings
{
    public class UnitTests
    {
        public static void Run()
        {
            Sym inf = new SymInf();
            Sym loose4 = new SymLoose(4);
            Sym loose5 = new SymLoose(5);
            Sym divs3left = new SymDivs(1, 3, 0, 0);

            var semi = new ConveyorSR();

            Console.WriteLine("Starting unit tests...");

            TestEq<Sym>("Left additive identity", semi.Add(inf, loose4), loose4);
            TestEq<Sym>("Right additive identity", semi.Add(loose4, inf), loose4);

            TestEq<Sym>("Left multiplicative zero", semi.Mult(inf, loose4), inf);
            TestEq<Sym>("Right multiplicative zero", semi.Mult(loose4, inf), inf);

            TestEq<Sym>("Left multiplicative identity", semi.Mult(new SymLoose(0), loose4), loose4);
            TestEq<Sym>("Right multiplicative identity", semi.Mult(loose4, new SymLoose(0)), loose4);

            TestEq<Sym>("Sym.Add", semi.Add(loose4, loose4), loose4);
            TestEq<Sym>("Loose 4 add Loose 5", semi.Add(loose4, loose5), loose4);
            TestEq<Sym>("Loose 5 add Loose 4", semi.Add(loose5, loose4), loose4);

            TestEq<Sym>("Loose 4 (X) Loose 5", semi.Mult(loose4, loose5), new SymLoose(9));
            TestEq<Sym>("Loose 5 (X) Divs 1 3 0 0", semi.Mult(loose5, new SymDivs(1, 3, 0, 0)), new SymDivs(1, 8, 0, 0));
            TestEq<Sym>("Divs 1 3 0 0 (X) Loose 5", semi.Mult(new SymDivs(1, 3, 0, 0), loose5), new SymDivs(1, 3, 0, 5));

            Console.WriteLine("Done unit tests...");
        }

        private static void TestEq<T>(string testName, T a, T b) where T : IEquatable<T>
        {
            Console.Write("Test [{0}]: ", testName);

            if (!a.Equals(b))
                Console.WriteLine(" failed: TestEq operands were not equal");
            else
                Console.WriteLine(" passed");
        }
    }
}
