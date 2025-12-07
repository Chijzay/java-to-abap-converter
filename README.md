# Java → ABAP Converter

Ein Web-Tool, das eine Teilmenge von Java-Code in ein ABAP-Grundgerüst übersetzt - entweder als _Snippet_ (einzelne Statements) oder als _Class_ (vereinfachtes Grundgerüst).  
Das Frontend läuft auf GitHub Pages und das Backend als Spring Boot REST API auf Koyeb.

**Hinweis:** 

Das Projekt ist ein Subset-Translator. Nicht jeder Java-Konstruktion entspricht eins zu eins ABAP. Unbekannte oder komplexe Elemente werden als `TODO` markiert.

## Demo

Die Webapplikation ist unter folgendem Link erreichbar:

```
https://chijzay.github.io/java-to-abap-converter/
```

## Features

- **Modi:** `Auto` (Erkennung), `Snippet`, `Class`
- **REST API** (`/api/translate`) für Übersetzungen
- **Health Endpoint** (`/api/health` inklusive Trailing Slash Support)
- **CORS-fähig** für GitHub Pages (`chijzay.github.io`)
- **Syntax Highlighting**
  - Java via Prism.js
  - ABAP via leichtgewichtigem Tokenizer im Frontend
    
- **UX**
  - Zeilennummern (Gutter)
  - Copy to Clipboard
  - Download als `.abap`
  - Beispiel-Snippets
  - Status mit LED-Anzeige
  - Reset-Button

## Architektur

**Frontend**
- Statisches UI (HTML/CSS/JS)
- Ruft Backend per `fetch()` auf
- Zeigt Ergebnis in ABAP-Editor an (read-only)

**Backend**
- Spring Boot Anwendung
- REST Controller für Übersetzung und Health
- CORS-Konfiguration für GitHub Pages

## Projektstruktur

```
.├─ .github/
│  └─ workflows/
│     ├─ pages.yml                 # Deployt das Frontend (GitHub Pages)
│     └─ push-backend-image.yml    # Baut & pusht das Backend-Docker-Image (CI)
│
├─ j2abap-backend/
│  ├─ Dockerfile                   # Container-Build für Spring Boot Backend
│  ├─ pom.xml                      # Maven Build/Dependencies
│  ├─ .dockerignore                # Verkleinert Docker-Build-Context
│  └─ src/
│     └─ main/
│        ├─ java/
│        │  └─ de/example/j2abap/
│        │     ├─ ApiApplication.java          # Spring Boot Entry Point
│        │     ├─ core/
│        │     │  └─ JavaToAbapTranslator.java # Übersetzungslogik
│        │     └─ api/
│        │        ├─ TranslateController.java  # REST: /api/translate (POST)
│        │        ├─ HealthController.java     # REST: /api/health (GET)
│        │        └─ CorsConfig.java           # CORS für Frontend-Domain(s)
│        └─ resources/
│           └─ application.properties          # Port/Config (z.B. server.port)
│
├─ j2abap-frontend/
│  ├─ app.js                       # UI-Logik: Fetch, Highlighting, Line-Numbers
│  ├─ index.html                   # Layout, Controls und Editor-Struktur
│  └─ style.css                    # SAP/Fiori-like Styling
│
├─ .gitattributes                  # EOL/CRLF-LF Regeln
├─ .gitignore                      # Ignoriert Build/IDE/Node Artefakte
├─ LICENSE                         # Portfolio Viewing Only License
└─ README.md                       # Projektbeschreibung und Deploy
```

## Deployment

- Frontend (GitHub Pages)

  - j2abap-frontend/ als Pages-Source verwenden oder Build/Deploy via Actions

- Backend (Koyeb)

  - Build über Dockerfile
  - Port: Spring Boot läuft meist auf 8080 (Koyeb forwarded)
  - Health Check Endpoint: /api/health

- `CORS`
 
  - Da das Frontend auf `https://chijzay.github.io` liegt, muss das Backend `CORS` erlauben:
  - Access-Control-Allow-Origin: `https://chijzay.github.io`
  - Methoden: `GET`, `POST`, `OPTIONS`
  - Headers: `Content-Type`

## Limitations und bekannte Grenzen

- Fokus liegt auf Subset von Java
- Keine vollständige Java-Semantik, da keine vollständige ABAP-Entsprechung
- Unbekannte Konstrukte werden als `TODO` ausgegeben
- Ausgabe ist ABAP-ähnlich und ist als Grundlage bzw. Grundgerüst gedacht, nicht als garantierter Produktivcode

## Lizenz

Die Nutzung der Anwendung ist zu Demonstrations- und Lernzwecken erlaubt. Die kommerzielle Nutzung, Weitergabe, Vervielfältigung oder Verbreitung ist untersagt. Siehe `LICENSE` für Details.
