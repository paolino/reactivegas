# Reactivegas

Un sistema open-source per la gestione delle attività economiche dei gruppi di acquisto solidale (GAS).

## Panoramica

Reactivegas è costruito sul principio della **modificazione distribuita dello stato**, dividendo l'amministrazione del gruppo di acquisto tra pari. Il sistema enfatizza:

- **Pagamenti prepagati** - I membri depositano fondi prima degli acquisti, eliminando i pagamenti individuali al momento della transazione
- **Indipendenza bancaria** - Distribuisce le funzioni di cassa tra i membri del gruppo attraverso la tecnologia informatica
- **Autogoverno** - I responsabili amministrano il gruppo attraverso il consenso, senza amministratori esterni

## Come Funziona

I membri accedono alle attività economiche del gruppo attraverso qualsiasi responsabile che possa riconoscerli come titolari di un credito nei confronti del GAS. Il numero dei responsabili è inferiore al totale dei membri.

### Servizio di Sportello Distribuito

Tutti i responsabili possono assistere i membri nelle transazioni economiche. I responsabili degli acquisti mantengono l'autorità sulla raccolta degli ordini per i loro specifici acquisti.

### Gestione Distribuita della Cassa

I responsabili detengono fisicamente porzioni dei crediti dei membri. I flussi di cassa avvengono attraverso:

- Depositi dei membri
- Esecuzione degli acquisti
- Transazioni di regolamento tra responsabili

### Costruzione del Consenso

I responsabili dichiarano pubblicamente le operazioni economiche, mantenendo il sistema sincronizzato con le attività reali del gruppo. Il sistema fornisce:

- **Certificazione degli eventi** - Verifica crittografica degli eventi economici
- **Programmazione di eventi aperti** - Programmazione delle attività future
- **Costruzione parallela dello stato** - Sincronizzazione distribuita dello stato tra i responsabili

## Concetti Chiave

| Termine | Descrizione |
|---------|-------------|
| Responsabile | Un pari che gestisce le attività del gruppo |
| Utente | Un membro del gruppo di acquisto |
| Accredito | Saldo creditore dell'utente con il gruppo |
| Saldo | Saldo del conto del responsabile |
| Impegno | Impegno economico per gli acquisti |
| Acquisto | Un ordine di acquisto del gruppo |
| Anagrafe | Registro di utenti e responsabili |
| Movimento | Una transazione o cambio di stato |
| Dichiarazione | Un'operazione economica certificata |

## Architettura

Il sistema è composto da:

- **Livello Eventi** (`Eventi/`) - Definisce gli eventi economici come acquisti, crediti, impegni
- **Livello Persistenza** (`Applicazioni/`) - Gestisce la persistenza dello stato tramite file e SQLite
- **Tipi Core** (`Core/`) - Definizioni dei tipi fondamentali e gestione dello stato
- **Server** (`Server/`) - Server web SCGI per l'interfaccia utente

## Avvio Rapido

```bash
# Avvia il server
server --port 5000 /percorso/dati password
```

Vedi [Installazione](installation.md) per le istruzioni di setup e [Utilizzo](usage.md) per la documentazione dettagliata.

## Opzioni da Riga di Comando

```
Uso: server [-p|--port PORTA] [-m|--movements NUMERO] [-s|--sessions NUMERO]
            [-r|--memories NUMERO] DIRECTORY PASSWORD

Opzioni disponibili:
  -h,--help                Mostra questo testo di aiuto
  -p,--port PORTA          Porta del server CGI (default: 5000)
  -m,--movements NUMERO    Max movimenti di gruppo in memoria (default: 15)
  -s,--sessions NUMERO     Max sessioni in memoria (default: 200)
  -r,--memories NUMERO     Max ricordi per sessione (default: 20)
  DIRECTORY                Directory di lavoro
  PASSWORD                 Password di amministrazione
```
