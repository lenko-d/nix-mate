![NixMate](/images/nixmate.png)

---

# nix-mate

Nix-mate is a GUI front-end for the [Nix](https://github.com/NixOS/nix) package manager.


Nix-mate is implemented in Haskell and GTK.

## To build

### Drop into a Nix shell with all dependencies:
```
  nix-shell --pure -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [gtk3 gi-gtk haskell-gi text])"
```

### Build nix-mate:
ghc src/nix-mate.hs

### Run
```
./nix-mate
```

---

![NixMate](/images/screenshot1.png)

---

![NixMate](/images/screenshot2.png)



---

# Contributing

1. Fork it
2. Download your fork to your PC (`git clone https://github.com/your_username/nix-mate && cd nix-mate`)
3. Create your feature branch (`git checkout -b my-new-feature`)
4. Make changes and add them (`git add .`)
5. Commit your changes (`git commit -m 'Add some feature'`)
6. Push to the branch (`git push origin my-new-feature`)
7. Create new pull request

---
