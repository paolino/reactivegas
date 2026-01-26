# Usage

## Domain Concepts

Reactivegas manages a **Gruppo di Acquisto Solidale** (GAS) - a solidarity purchasing group where members collectively buy goods directly from producers.

### Roles

**Responsabili (Administrators)**

- Peers who manage group activities
- Smaller in number than total members
- Each maintains a synchronized copy of the group state
- Can certify events and recognize members

**Utenti (Users/Members)**

- Members of the purchasing group
- Access activities through any administrator
- Hold credit (*accredito*) with the group

### Economic Model

**Accredito (Credit)**

User's credit balance with the group. Members deposit money which becomes credit they can use for purchases.

**Saldo (Balance)**

Administrator's account balance for tracking group finances.

**Impegno (Commitment)**

An economic commitment or pledge made by a member for a future purchase. Commitments are collected (*raccolta*) before orders are placed with producers.

**Acquisto (Purchase)**

A group purchase order. The lifecycle:

1. Administrator opens a purchase
2. Members make commitments (*impegni*)
3. Commitments are collected (*raccolta*)
4. Order is placed with producer
5. Purchase is closed and archived

### Events

The system tracks economic events including:

- User registration and management (*anagrafe*)
- Credit deposits and withdrawals
- Purchase commitments
- Order fulfillment
- Balance adjustments

## Persistence

The system uses a dual persistence model:

**Current State**

- Persisted to files as-is
- Represents the live state of the group

**Historical Data**

- Persisted via SQLite
- Archived purchases and completed transactions
- Used for reporting and auditing

## Development

### Building

```bash
nix develop
cabal build all
```

### Formatting

```bash
just format
```

### Linting

```bash
just hlint
```

### Full CI Check

```bash
just ci
```

### Documentation

```bash
just docs-serve   # Serve locally at http://localhost:8000
just docs-build   # Build static site
```

## Module Overview

| Directory | Purpose |
|-----------|---------|
| `Core/` | Fundamental types and state management |
| `Eventi/` | Economic event definitions (purchases, credits, etc.) |
| `Applicazioni/` | Application logic and persistence |
| `Server/` | SCGI web server |
| `UI/` | User interface components |
| `Lib/` | Utility libraries |
| `Voci/` | Item/product management |
