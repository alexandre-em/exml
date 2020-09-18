# Installation

Pour installer l'environnement de développement nécessaire sur votre
machine, exécutez les commandes suivantes

```
# Installation des dépendances systèmes
$ sudo apt install -y git m4 bubblewrap opam

# Initalisation d'opam
$ opam init

# Installation des dépendances logicielles
$ opam install -y tuareg merlin utop js_of_ocaml dune

$ eval $(opam config env)
```

Vous devriez désormais pouvoir compiler le projet en utilisant:
```
$ make all
```

# Lancer le tableur depuis un navigateur web:
```
python3 -m http.server 8080
```
puis accédez depuis l'url http://localhost:8080