# Reactivegas

An open-source system for managing the economic activities of solidarity purchasing groups (GAS - Gruppo di Acquisto Solidale).

## Overview

Reactivegas is built on the principle of **distributed state modification**, dividing the administration of the purchasing group among peers. The system emphasizes:

- **Prepaid payments** - Members deposit funds before purchases, eliminating individual payments at transaction time
- **Banking independence** - Distributes cashier functions among group members through information technology
- **Self-governance** - Managers administer their group through consensus, without external administrators

## How It Works

Members access the group's economic activities through any of the administrators (*responsabili*) who can recognize them as holders of credit with the GAS. The number of administrators is smaller than total membership.

### Distributed Counter Service

All managers can assist members with economic transactions. Purchase managers retain authority over order collection for their specific acquisitions.

### Distributed Cash Management

Managers physically hold portions of member credits. Cash flows through:

- Member deposits
- Purchase executions
- Inter-manager settlement transactions

### Consensus Building

Managers (*responsabili*) publicly declare economic operations, keeping the system synchronized with actual group activities. The system provides:

- **Event certification** - Cryptographic verification of economic events
- **Open event scheduling** - Programming of future activities
- **Parallel state construction** - Distributed state synchronization among administrators

## Key Concepts

| Italian Term | English | Description |
|--------------|---------|-------------|
| Responsabile | Administrator/Manager | A peer who manages group activities |
| Utente | User/Member | A member of the purchasing group |
| Accredito | Credit | User's credit balance with the group |
| Saldo | Balance | Administrator's account balance |
| Impegno | Commitment | Economic commitment/pledge for purchases |
| Acquisto | Purchase | A group purchase order |
| Anagrafe | Registry | User and administrator registry |
| Movimento | Movement | A transaction or state change |
| Dichiarazione | Declaration | A certified economic operation |

## Architecture

The system consists of:

- **Events layer** (`Eventi/`) - Defines economic events like purchases, credits, commitments
- **Persistence layer** (`Applicazioni/`) - Handles state persistence via files and SQLite
- **Core types** (`Core/`) - Fundamental type definitions and state management
- **Server** (`Server/`) - SCGI web server for the user interface

## Quick Start

```bash
# Start the server
server --port 5000 /path/to/data mypassword
```

See [Installation](installation.md) for setup instructions and [Usage](usage.md) for detailed documentation.

## Command Line Options

```
Usage: server [-p|--port PORT] [-m|--movements COUNT] [-s|--sessions COUNT]
              [-r|--memories COUNT] DIRECTORY PASSWORD

Available options:
  -h,--help                Show this help text
  -p,--port PORT           CGI server port (default: 5000)
  -m,--movements COUNT     Max group movements in memory (default: 15)
  -s,--sessions COUNT      Max sessions in memory (default: 200)
  -r,--memories COUNT      Max memories per session (default: 20)
  DIRECTORY                Working directory
  PASSWORD                 Administration password
```
