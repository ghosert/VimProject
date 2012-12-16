## Study LESS language

Open ./less-language.html with browser, the key stuffs are highlighted and comments are there as well, I used firefox plugin named ScrapBook to do so.

## Guide to play with bootstrap

1. .zip file is coming from bootstrap official site, unzip it into "bootstrap" folder.
2. All the stuffs below can be found in ./bootstrap/README.md, see details in that file, here is the key digest.

## Install dependencies and build

```
$ cd ./bootstrap
```

```
$ sudo npm install recess connect uglify-js jshint phantomjs -g
```

So that build doc will then work:

```
$ make
```

If you want to build bootstrap try this:

```
$ make bootstrap # a "bootstrap" sub-folder will be generated, and all stuffs are there.
$ make clean     # to delete "bootstrap" folder generated above.
```

Try on make test like this:

```
$ make test
```

I got errors for not found "connect" module, to fix, in "./bootstrap" folder, run this:

```
$ npm install connect # Install connect locally, this will generate ./bootstrap/node_modules
```

and then:

```
$ make test
```

See more details in ./bootstrap/Makefile

## Samples for bootstrap

Take a look at ./test_bootstrap.html

