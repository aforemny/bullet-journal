# bullet-journal

[Bullet Journal](http://bulletjournal.com/) implementation in
[Elm](http://elm-lang.org/), backed by [Parse](https://parseplatform.org/) and
[Express](https://expressjs.com/).


## Build instructions

```sh
git clone github.com:aforemny/bujo.git
git submodule init --update
```


### Nix package manager

```sh
nix-shell --run "make"
nix-shell --run "npm run dev"
open http://localhost:1337
```

The dashboard runs on http://localhost:4040.


### Other Linux

```sh
make
npm run dev
open http://localhost:1337
```

The dashboard runs on http://localhost:4040.
