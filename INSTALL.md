## Building blank-canvas

You need to use install, because install gives access to the `static/*` files.

    % cabal sandbox init
    % cabal install

## Testing

    % cd wiki-suite

clone the documentation, which is also the test examples

    % git clone https://github.com/ku-fpg/blank-canvas.wiki.git
    % mkdir blank-canvas.wiki/tmp

build the tests

    % cabal sandbox init --sandbox ../.cabal-sandbox/
    % cabal install --only-d
    % cabal build

and run them (on a mac)

    % cabal test

you can compare output using github diff.



### Other Notes

There is a script, wiki-suite/hack.pl, that compares blank-canvas.wiki with ORIG, using image compare.







