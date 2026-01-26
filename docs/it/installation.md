# Installazione

## Prerequisiti

- [Nix](https://nixos.org/download.html) con flakes abilitati
- Un web server con supporto SCGI (nginx, lighttpd, ecc.) per il deployment in produzione

## Compilazione da Sorgente

Clona il repository ed entra nella shell di sviluppo:

```bash
git clone https://github.com/paolino/reactivegas.git
cd reactivegas
nix develop
```

Compila il server:

```bash
cabal build server
```

## Esecuzione del Server

### Uso Base

Avvia il server con una directory dati e password di amministrazione:

```bash
cabal run server -- /percorso/dati password
```

### Con Opzioni Personalizzate

```bash
cabal run server -- \
    --port 8080 \
    --sessions 100 \
    --movements 20 \
    --memories 30 \
    /percorso/dati password
```

### Riferimento Riga di Comando

| Opzione | Breve | Default | Descrizione |
|---------|-------|---------|-------------|
| `--port` | `-p` | 5000 | Porta del server SCGI |
| `--movements` | `-m` | 15 | Max movimenti di gruppo in memoria |
| `--sessions` | `-s` | 200 | Max sessioni in memoria |
| `--memories` | `-r` | 20 | Max ricordi per sessione |

**Argomenti posizionali:**

1. `DIRECTORY` - Directory di lavoro contenente i dati del gruppo
2. `PASSWORD` - Password di amministrazione per l'accesso ai token

## Struttura Directory

La directory di lavoro deve essere organizzata come segue:

```
dati/
├── static/           # Asset web statici
│   └── report.html   # Report generati
├── *.db              # Database SQLite per dati storici
└── state/            # File di stato corrente
```

## Configurazione Web Server

Il server usa il protocollo SCGI. Configura il tuo web server per fare proxy delle richieste:

### Esempio Nginx

```nginx
location /reactivegas {
    include scgi_params;
    scgi_pass 127.0.0.1:5000;
}
```

### Esempio Lighttpd

```lighttpd
server.modules += ("mod_scgi")
scgi.server = (
    "/reactivegas" => ((
        "host" => "127.0.0.1",
        "port" => 5000
    ))
)
```
