# Java → ABAP Converter

Ein Web-Tool, das **eine Teilmenge von Java-Code** in **ABAP-Skelettcode** übersetzt – entweder als **Snippet** (einzelne Statements) oder als **Class** (vereinfachtes Grundgerüst).  
Frontend läuft auf **GitHub Pages**, Backend als **Spring Boot REST API** (z. B. auf **Koyeb**).

> Hinweis: Das Projekt ist ein **Subset-Translator**. Nicht jeder Java-Konstruktion entspricht 1 zu 1 ABAP. Unbekannte oder komplexe Elemente werden als `TODO` markiert.


## Features

- **Modi:** `Auto` (Erkennung), `Snippet`, `Class`
- **REST API** (`/api/translate`) für Übersetzungen
- **Health Endpoint** (`/api/health` inkl. Trailing Slash Support)
- **CORS-fähig** für GitHub Pages (`chijzay.github.io`)
- **Syntax Highlighting**
  - Java via Prism.js
  - ABAP via leichtgewichtigem Tokenizer im Frontend
    
- **UX**
  - Zeilennummern (Gutter)
  - Copy to Clipboard
  - Download als `.abap`
  - Beispiel-Snippets
  - Status/LED-Anzeige
  - Reset-Button


## Demo

```
https://chijzay.github.io/java-to-abap-converter/
```


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
.
├─ j2abap-frontend/
│  ├─ index.html
│  ├─ app.js
│  └─ style.css
│
├─ j2abap-backend/
│  ├─ Dockerfile
│  ├─ pom.xml
│  └─ src/
│     └─ main/
│        ├─ java/
│        │  └─ de/example/j2abap/
│        │     ├─ ApiApplication.java
│        │     └─ api/
│        │        ├─ TranslateController.java
│        │        ├─ HealthController.java
│        │        └─ CorsConfig.java
│        └─ resources/
│           └─ application.properties
│
├─ .gitattributes
└─ README.md
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

## Limitations und Bekannte Grenzen

- Fokus liegt auf Subset von Java
- Keine vollständige Java-Semantik, da keine vollständige ABAP-Entsprechung
- Unbekannte Konstrukte werden als `TODO` ausgegeben
- Ausgabe ist ABAP-ähnlich und ist als Grundlage bzw. Grundgerüst gedacht, nicht als garantierter Produktivcode
