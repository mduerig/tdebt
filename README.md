# tdebt - Technical Debt Hotspot finder

The `tdebt` utility helps finding hotspots of technical debt in code. To do this it assigns a debt metric to every source file. The bigger the debt metric value the more technical debt has been accumulated in the affected file. The debt value is the product of the normalized complexity and the normalized churn of a file:

    debt(file) = (complexity(file) / max(complexity(all files in project)))
             * (churn(file) / max(churn(all files in project)))

See [Code as a Crime Scene](https://www.adamtornhill.com/articles/crimescene/codeascrimescene.htm) for more background.

*Note*: above normalization makes the debt metric a relative number for comparing files within a project. Values of files from different projects are not comparable. For the same reason the debt metric can neither be used for tracking debt over time.

Complexity is measured either simply in number of lines of code or more sophistically using cyclomatic complexity.

Churn is measured by number of version controls revisions.
### Dependencies
* `git` for calculating churn
  * tested versions: 2.17.1 / 2.20.1
* `bash` and `wc` for counting lines of codes.
* [`PMD`](https://pmd.github.io/) for calculating cyclomatic complexities
  * tested version 6.30.0
  * make sure a symlink `pmd` to `pmd-bin-6.30.0/bin/run.sh` is on the `$PATH`. E.g. `ln -s /opt/pmd-bin-6.30.0/bin/run.sh /usr/local/bin/pmd`.

### Usage
        Usage: tdebt [<path>] [--pmd | --loc] [-b|--before <date>] [-a|--after <date>]
                    [-g|--git-dir <path>] [--churn-norm <churn norm>]
                    [--complexity-norm <complexity norm>]

        Available options:
          -h,--help                Show this help text
          <path>                   path within the Git repository
          --pmd                    use pmd complexity metric
          --loc                    use loc complexity metric
          -b,--before <date>       only include commits before the specified date
          -a,--after <date>        only include commits after the specified date
          -g,--git-dir <path>      path to the Git repository
          --churn-norm <churn norm>
                                  constant for normalizing the churn value
          --complexity-norm <complexity norm>
                                  constant for normalizing the complexity value#### Example
Running the command

    $ tdebt pmd ~/Checkouts/jackrabbit-oak

results in the following output (last 10 lines only):

    ("oak-core/src/main/java/org/apache/jackrabbit/oak/security/authorization/permission/CompiledPermissionImpl.java",Metric {churn = 91, complexity = 160, debt = 4.034023217798465e-2})
    ("oak-core/src/main/java/org/apache/jackrabbit/oak/query/QueryImpl.java",Metric {churn = 88, complexity = 218, debt = 5.315158063890505e-2})
    ("oak-store-document/src/main/java/org/apache/jackrabbit/oak/plugins/document/DocumentNodeStore.java",Metric {churn = 44, complexity = 454, debt = 5.5345911949685536e-2})
    ("oak-core/src/main/java/org/apache/jackrabbit/oak/query/ast/SelectorImpl.java",Metric {churn = 116, complexity = 177, debt = 5.6886376859778896e-2})
    ("oak-store-document/src/main/java/org/apache/jackrabbit/oak/plugins/document/rdb/RDBDocumentStore.java",Metric {churn = 53, complexity = 436, debt = 6.402349486049927e-2})
    ("oak-core/src/main/java/org/apache/jackrabbit/oak/plugins/index/AsyncIndexUpdate.java",Metric {churn = 117, complexity = 217, debt = 7.034327986036074e-2})
    ("oak-core/src/main/java/org/apache/jackrabbit/oak/query/SQL2Parser.java",Metric {churn = 70, complexity = 368, debt = 7.13711800072036e-2})
    ("oak-lucene/src/test/java/org/apache/jackrabbit/oak/plugins/index/lucene/LucenePropertyIndexTest.java",Metric {churn = 125, complexity = 215, debt = 7.446042168841603e-2})
    ("oak-jcr/src/test/java/org/apache/jackrabbit/oak/jcr/RepositoryTest.java",Metric {churn = 168, complexity = 164, debt = 7.633613165987864e-2})
    ("oak-lucene/src/main/java/org/apache/jackrabbit/oak/plugins/index/lucene/LucenePropertyIndex.java",Metric {churn = 138, complexity = 286, debt = 0.10935084365389411})


### Building `tdebt`

`tdebt` uses [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) for building

    stack build
    stack install
