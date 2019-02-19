using System;
using System.IO;

namespace Cecil.Net
{
    public class Program
    {
        private static readonly string InitialDirectory = Environment.CurrentDirectory;

        static Program()
        {
            Environment.CurrentDirectory = Path.GetDirectoryName(typeof(Program).Assembly.Location);
        }

        private static void Main(string[] args)
        {
            new Program().MainCore(args);
        }

        private void MainCore(string[] args)
        {
            var options = new cecil.standardize.Options(null, null, null);
            var norm = new cecil.standardize.SqlNormalizer(options);
            Environment.CurrentDirectory = InitialDirectory;
            Console.WriteLine(norm.NormalizeCase(Console.In.ReadToEnd()));
        }
    }
}
