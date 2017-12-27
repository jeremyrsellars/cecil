# Cecil

Cecil is a library that can partially convert Cerner CCL query syntax to PL/SQL for processing on the Oracle RDBMS.

### Background

CCL offers functions and capabilities that Oracle SQL does not.  Cecil only aims to support converting a few common functions, and in cases where a CCL function isn't converted, it is left intact.  CCL has both declarative and imperative constructs and Cecil is only intended to convert the declarative query syntax.

Still, there are SQL-compatible substitutions that may be made.  This is where Cecil can help.

## Usage

Cecil comes in 2 forms: a [web application](https://jeremyrsellars.github.io/cecil/) and a command-line interface.

    lein run file1.ccl file2.ccl dir\file3.ccl ....

This command will create `file1.sql`, `file2.sql`, and `dir\file3.sql` respectively.

