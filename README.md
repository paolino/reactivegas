# Reactivegas

> **⚠️ Status: dead code / codice morto.** This implementation is retired: no
> further fixes or features will land here. Reactivegas is being rebuilt on the
> [kelgroups](https://github.com/paolino/kelgroups) KERI substrate — follow
> milestone [Reactivegas on kelgroups](https://github.com/paolino/reactivegas/milestone/2)
> and epic [#43](https://github.com/paolino/reactivegas/issues/43). The final
> legacy state is tagged [`legacy-final`](https://github.com/paolino/reactivegas/tree/legacy-final).
>
> **⚠️ Stato: implementazione ritirata.** Nessuna correzione o funzionalità
> verrà più applicata a questo codice. Reactivegas viene ricostruito sul
> substrato KERI [kelgroups](https://github.com/paolino/kelgroups) — vedi la
> milestone e l'epica indicate sopra. Lo stato finale del codice legacy è il
> tag [`legacy-final`](https://github.com/paolino/reactivegas/tree/legacy-final).

An open-source system for managing the economic activities of solidarity purchasing groups (GAS - Gruppo di Acquisto Solidale).

Un sistema open-source per la gestione delle attività economiche dei gruppi di acquisto solidale (GAS).

## Documentation / Documentazione

**English**: https://paolino.github.io/reactivegas/

**Italiano**: https://paolino.github.io/reactivegas/it/

The first production KelGroups-based component is the
[money custody economic core](docs/money-custody.md).

## Quick Start / Avvio Rapido

```bash
# Clone and enter dev shell / Clona ed entra nella shell di sviluppo
git clone https://github.com/paolino/reactivegas.git
cd reactivegas
nix develop

# Build and run / Compila ed esegui
cabal build server
cabal run server -- /path/to/data password
```

## License / Licenza

BSD-3-Clause
