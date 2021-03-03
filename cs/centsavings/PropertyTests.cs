using System;

namespace centsavings
{
    /*
        15 Found non-left-distributive triple of Syms: loose:11, loose:7, loose:4 --- loose:15, loose:18

        Manually verifying counter-example:

              11 (x) (7 (+) 4)
            = 11 (x)        4
            =    15

              (11 (x) 7) (+) (11 (x) 4)
            =     18     (+)     15
            =            18              -- the rounded price of 18 (20) is <= rounded 19 (20)
         
        (15 /= 18)
    */

    public class PropertyTests
    {
        public static void Run(int count)
        {
            var r = new Random();
            var conveyorSR = new ConveyorSR();

            Console.WriteLine("Starting property tests");

            /* Associativity of ConveyorSR.Add over Sym */
            for (int i = 1; i <= count; i++)
            {
                Sym a = RandomSym(r);
                Sym b = RandomSym(r);
                Sym c = RandomSym(r);

                var p = conveyorSR.Add(a, conveyorSR.Add(b, c));
                var q = conveyorSR.Add(conveyorSR.Add(a, b), c);

                if (!p.Equals(q))
                    Console.WriteLine("{3} Found non-associative triple of Syms under addition: {0}, {1}, {2}",
                                      a.ToString(),
                                      b.ToString(),
                                      c.ToString(),
                                      i);
            }

            /* Associativity of ConveyorSR.Mult over Sym */
            for (int i = 1; i <= count; i++)
            {
                Sym a = RandomSym(r);
                Sym b = RandomSym(r);
                Sym c = RandomSym(r);

                var p = conveyorSR.Mult(a, conveyorSR.Mult(b, c));
                var q = conveyorSR.Mult(conveyorSR.Mult(a, b), c);

                if (!p.Equals(q))
                    Console.WriteLine("{3} Found non-associative triple of Syms under multiplication: {0}, {1}, {2} --- {4}, {5}",
                                      a.ToString(),
                                      b.ToString(),
                                      c.ToString(),
                                      i,
                                      p,
                                      q);
            }

            /* Left-distributivity of ConveyorSR over Sym */
            for (int i = 1; i <= count; i++)
            {
                Sym a = RandomSym(r);
                Sym b = RandomSym(r);
                Sym c = RandomSym(r);

                var p = conveyorSR.Mult(a, conveyorSR.Add(b, c));
                var q = conveyorSR.Add(conveyorSR.Mult(a, b), conveyorSR.Mult(a, c));

                if (!p.Equals(q))
                    Console.WriteLine("{3} Found non-left-distributive triple of Syms: {0}, {1}, {2} --- {4}, {5}",
                                      a.ToString(),
                                      b.ToString(),
                                      c.ToString(),
                                      i,
                                      p,
                                      q);
            }

            /* Right-distributivity of ConveyorSR over Sym */
            for (int i = 1; i <= count; i++)
            {
                Sym a = RandomSym(r);
                Sym b = RandomSym(r);
                Sym c = RandomSym(r);

                var p = conveyorSR.Mult(conveyorSR.Add(a, b), c);
                var q = conveyorSR.Add(conveyorSR.Mult(a, c), conveyorSR.Mult(b, c));

                if (!p.Equals(q))
                    Console.WriteLine("{3} Found non-right-distributive triple of Syms: {0}, {1}, {2}",
                                      a.ToString(),
                                      b.ToString(),
                                      c.ToString(),
                                      i);
            }

            Console.WriteLine("Done property tests");
        }

        private static Sym RandomSym(Random r)
        {
            switch (r.Next(1, 3))
            {
                case 1:
                    {
                        int divs = r.Next(1, 20);
                        int left = r.Next(0, 20);
                        int inner = r.Next(0, 20);
                        int right = r.Next(0, 20);

                        return new SymDivs(divs, left, inner, right);
                    }

                case 2: { return new SymLoose(r.Next(1, 20)); }
                case 3: { return new SymInf(); }

                default:
                    throw new Exception("Unexpected return value from r.Next");
            }
        }
    }
}
