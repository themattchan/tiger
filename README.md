# tiger
Working through Appel's Modern Compiler Implemantation in ML. Might try writing
this in Haskell or Coq...

## TODO

- parse strings and comments properly...

- actually test the thing

- put the parts together, generate binary http://mlton.org/CompilationManager

- installing cm2mlb on fedora:

    ```
    su
    export SMLROOT=/usr/local/sml/bin/
    export PATH=$SMLROOT:$PATH
    cd /usr/share/doc/mlton/cm2mlb

    sml
      - CM.make "cm2mlb.cm";
      - CM2MLB.export ();

    ln -s $SMLROOT/.run-sml $SMLROOT/cm2mlb
    mv cm2mlb.x86-linux $SMLROOT/.heap
    ```
