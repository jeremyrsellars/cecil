# Cecil

Cecil is a library that can partially convert Cerner CCL query syntax to PL/SQL for processing on the Oracle RDBMS.

It also offers some SQL style conversion tools.

### Background

The CCL language offers functions and capabilities that Oracle SQL does not.  Cecil only aims to support converting a few common functions, and in cases where a CCL function isn't converted, it is left intact.  CCL has both declarative and imperative constructs and Cecil is only intended to convert the declarative query syntax.

Still, there are SQL-compatible substitutions that may be made.  This is where Cecil can help.

### See [src/cecil/README.md](src/cecil/)

### Development/API
Because the core of Cecil is implemented in cross-platform Clojure (.cljc), it can be fitted for use with JavaScript, Java VM, and the .NET Framework.

An example C#/.Net utility is provided. See [src/Cecil.Net/README.md](src/Cecil.Net/).

If you do want to hack on Cecil, let me know in person, or by github issue.
