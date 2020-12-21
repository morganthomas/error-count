# Snowball

Use Nix to build a Haskell project

## Build an existing project

If you want to build an existing project

---

# Create a new project

To create a new snowball project:

```bash
bash <( curl https://gitlab.com/fresheyeball/snowball/-/raw/master/generator.sh )
```

## Nix

This project is built with [Nix](https://nixos.org/).

## Build the project

The included `default.nix` file has some arguments to customize your build. To build with GHC

```bash
nix-build
```

To build with GHCjs

```bash
nix-build --arg isJS true
```


# Hoogle

Get a hoogle server in one line

```bash
nix-shell --arg withHoogle true --command "hoogle serve"
```

### License

[![CC0](https://licensebuttons.net/p/zero/1.0/80x15.png)](http://creativecommons.org/publicdomain/zero/1.0/)

Isaac Shapira has waived all copyright and related or neighboring rights to Shpadoinkle Snowman.
This work is published from: United States
