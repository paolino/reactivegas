# Utilizzo

## Concetti del Dominio

Reactivegas gestisce un **Gruppo di Acquisto Solidale** (GAS) - un gruppo di acquisto solidale dove i membri acquistano collettivamente beni direttamente dai produttori.

### Ruoli

**Responsabili**

- Pari che gestiscono le attività del gruppo
- In numero inferiore rispetto ai membri totali
- Ciascuno mantiene una copia sincronizzata dello stato del gruppo
- Possono certificare eventi e riconoscere i membri

**Utenti (Membri)**

- Membri del gruppo di acquisto
- Accedono alle attività attraverso qualsiasi responsabile
- Detengono un credito (*accredito*) con il gruppo

### Modello Economico

**Accredito**

Saldo creditore dell'utente con il gruppo. I membri depositano denaro che diventa credito utilizzabile per gli acquisti.

**Saldo**

Saldo del conto del responsabile per il tracciamento delle finanze del gruppo.

**Impegno**

Un impegno economico fatto da un membro per un acquisto futuro. Gli impegni vengono raccolti (*raccolta*) prima che gli ordini vengano inviati ai produttori.

**Acquisto**

Un ordine di acquisto del gruppo. Il ciclo di vita:

1. Il responsabile apre un acquisto
2. I membri fanno gli impegni
3. Gli impegni vengono raccolti (*raccolta*)
4. L'ordine viene inviato al produttore
5. L'acquisto viene chiuso e archiviato

### Eventi

Il sistema traccia gli eventi economici inclusi:

- Registrazione e gestione utenti (*anagrafe*)
- Depositi e prelievi di credito
- Impegni di acquisto
- Evasione ordini
- Aggiustamenti di saldo

## Persistenza

Il sistema usa un modello di persistenza duale:

**Stato Corrente**

- Persistito su file così com'è
- Rappresenta lo stato live del gruppo

**Dati Storici**

- Persistiti via SQLite
- Acquisti archiviati e transazioni completate
- Usati per reportistica e audit

## Sviluppo

### Compilazione

```bash
nix develop
cabal build all
```

### Formattazione

```bash
just format
```

### Linting

```bash
just hlint
```

### Check CI Completo

```bash
just ci
```

### Documentazione

```bash
just docs-serve   # Servi localmente su http://localhost:8000
just docs-build   # Compila il sito statico
```

## Panoramica Moduli

| Directory | Scopo |
|-----------|-------|
| `Core/` | Tipi fondamentali e gestione dello stato |
| `Eventi/` | Definizioni degli eventi economici (acquisti, crediti, ecc.) |
| `Applicazioni/` | Logica applicativa e persistenza |
| `Server/` | Server web SCGI |
| `UI/` | Componenti dell'interfaccia utente |
| `Lib/` | Librerie di utilità |
| `Voci/` | Gestione articoli/prodotti |
