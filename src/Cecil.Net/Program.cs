using System;
using System.Linq;
using System.IO;

namespace Cecil.Net
{
    public class Program
    {
        private static int Main(string[] args)
        {
            try
            {
                new Program().MainCore(args);
                return 0;
            }
            catch(Exception e)
            {
                Console.Error.WriteLine(e);
                return 1;
            }
        }

        private void MainCore(string[] args)
        {
            if (!args.Any())
                Console.Error.WriteLine("Reading query from console input.  Press F6, enter when done.");

            var snippets = args.Length > 0
                ? args.Select(fn => Tuple.Create(fn, File.ReadAllText(fn))).ToArray()
                : new[] { Tuple.Create("stdin", Console.In.ReadToEnd()) };

            foreach (var query in snippets)
                DemoSqlFeatures(query.Item2, query.Item1);
        }

        private static void DemoSqlFeatures(string sql, string description)
        {
            var norm = Standardizer.SqlNormalizer("\t", Environment.NewLine, 200);

            Console.WriteLine("============== " + description + " ===============");
            Console.WriteLine("------------------ Change Case Only-----------------------------");
            Console.WriteLine(norm.NormalizeCase(sql));
            Console.WriteLine();
            Console.WriteLine("------------------ Normalized -----------------------------");
            Console.WriteLine(norm.Normalize(sql));
            Console.WriteLine();
            Console.WriteLine("------------------ Widened -----------------------------");
            Console.WriteLine(norm.Widen(sql));
            Console.WriteLine();
            Console.WriteLine("------------------ Fully Normalized -----------------------------");
            Console.WriteLine(norm.Normalize(norm.Widen(sql)));
        }
    }
}
