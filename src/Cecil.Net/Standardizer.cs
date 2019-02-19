using System;
using System.IO;

namespace Cecil.Net
{
    public class Standardizer
    {
        static Standardizer()
        {
            string InitialDirectory = Environment.CurrentDirectory;
            Environment.CurrentDirectory = Path.GetDirectoryName(typeof(Program).Assembly.Location);
            var options = new cecil.standardize.Options(null, null, null);
            Environment.CurrentDirectory = InitialDirectory;
        }

        public static cecil.standardize.SqlNormalizer SqlNormalizer(string indent, string newLine, long? breakParentheticalLength) =>
            new cecil.standardize.SqlNormalizer(
                new cecil.standardize.Options(indent, newLine, breakParentheticalLength));
    }
}
