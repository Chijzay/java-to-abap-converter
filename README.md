# Java → ABAP Converter

Ein Web-Tool, das eine Teilmenge von Java-Code in ein ABAP-Grundgerüst übersetzt - entweder als _Snippet_ (einzelne Statements) oder als _Class_ (vereinfachtes Grundgerüst).  
Das Frontend läuft auf GitHub Pages und das Backend als Spring Boot REST API auf Koyeb.

**Hinweis:** 

Dieses Projekt ist ein Subset-Translator (Proof of Concept) auf AST-Basis. Da Java- und ABAP-Konzepte nicht immer 1:1 abbildbar sind, ist die Semantik nicht in allen Fällen garantiert. Unbekannte oder komplexe Konstrukte werden als `TODO` kommentiert und mit dem Original-Java-Code übernommen.
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
  - Java über `prism.js`
  - ABAP über leichtgewichtigem Tokenizer im Frontend
    
- **UX**
  - Zeilennummern (Gutter)
  - Copy to Clipboard
  - Download als `.abap`
  - Beispiel-Snippets
  - Status mit LED-Anzeige
  - Reset-Button

## Java und ABAP im Vergleich

- ...
- ..
- ..


## Architektur

**Frontend**
- Statisches UI (HTML, CSS und JavaScript)
- Ruft Backend per `fetch()` auf
- Zeigt Ergebnis in ABAP-Editor an (read-only)

**Backend**
- Spring Boot Anwendung
- REST Controller für Übersetzung und Health
- CORS-Konfiguration für GitHub Pages

## Projektstruktur

```
.
├─ .github/
│  └─ workflows/
│     ├─ pages.yml                 # Deployt das statische Frontend (GitHub Pages) bei Änderungen in j2abap-frontend/
│     └─ push-backend-image.yml    # Baut & pusht das Backend-Docker-Image nach GHCR bei Änderungen in j2abap-backend/
│
├─ j2abap-backend/
│  ├─ Dockerfile                   # Multi-Stage: Maven Build → schlankes JRE-Image (PORT/8080); inkl. Debug-Checks im Build
│  ├─ pom.xml                      # Maven/Spring Boot Backend; Dependencies u.a. spring-boot-starter-web + javaparser-core
│  ├─ mvnw                         # Maven Wrapper (Unix/macOS)
│  ├─ mvnw.cmd                     # Maven Wrapper (Windows)
│  ├─ .dockerignore                # Verkleinert den Docker-Build-Context (schneller/sauberer Build)
│  ├─ src/
│  │  └─ main/
│  │     ├─ java/
│  │     │  └─ de/example/j2abap/
│  │     │     ├─ ApiApplication.java          # Spring Boot Entry Point
│  │     │     ├─ JavaToAbapTranslator.java    # Kernlogik: Mode auto/snippet/class; Parsing (JavaParser) + Orchestrierung
│  │     │     ├─ StatementTranslator.java     # Übersetzt Statements (z.B. Block/If/Return/ExpressionStmt) nach ABAP
│  │     │     ├─ ExpressionTranslator.java    # Übersetzt Expressions (Literals, Binary/Unary, Calls, Assignments, …)
│  │     │     ├─ TypeMapper.java              # Java-Typen → ABAP-Typen (primitive & einfache Referenztypen)
│  │     │     ├─ MethodSignatureBuilder.java  # Baut ABAP-Methodensignaturen (IMPORTING/RETURNING) aus MethodDeclaration
│  │     │     ├─ AbapEmitter.java             # Ausgabe-Helper: Indentation + Zeilenaufbau (StringBuilder)
│  │     │     ├─ api/
│  │     │     │  ├─ TranslateController.java  # REST: POST /api/translate?mode=... (text/plain → text/plain)
│  │     │     │  ├─ HealthController.java     # REST: GET /api/health (ok) inkl. Trailing-Slash-Variante
│  │     │     │  ├─ CorsConfig.java           # CORS für GitHub Pages + lokale Dev-Hosts (GET/POST/OPTIONS)
│  │     │     │  └─ ApiExceptionHandler.java  # Fängt JavaParser ParseProblemException → 400 + hilfreicher Hinweistext
│  │     │     └─ util/
│  │     │        └─ Strings.java              # Kleine Utils (z.B. ABAP-String-Escaping via Verdopplung von ')
│  │     └─ resources/
│  │        └─ application.properties          # Container-freundlich: server.port=${PORT:8080}; Actuator health; Logging
│  └─ target/                     # (generiert) Maven-Build-Output; ist in .gitignore enthalten und sollte nicht versioniert sein
│     ├─ classes/                  # Kompilierte .class + generierte Ressourcen
│     └─ maven-status/             # Status-Dateien des Maven-Compiler-Plugins
│
├─ j2abap-frontend/
│  ├─ index.html                   # Statisches UI (Editor/Controls); bindet u.a. Prism für Java-Highlighting ein
│  ├─ app.js                       # UI-Logik: Request an Backend (/api/translate), Mode-Umschaltung, Highlighting, Copy/Download, Gutter/Line-Numbers
│  └─ style.css                    # Styling (clean, “SAP/Fiori-like” Look & Feel)
│
├─ .gitattributes                  # Git/Text-EOL-Regeln (CRLF/LF konsistent halten)
├─ .gitignore                      # Ignoriert typische Build-/IDE-Artefakte (u.a. target/)
├─ LICENSE                         # Lizenzbedingungen (Portfolio-/Lern-/Demo-Kontext)
└─ README.md                       # Projektübersicht, Demo-Link, Features, Deploy-/Betriebshinweise
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

## Limitationen und bekannte Grenzen

- Fokus liegt auf Subset von Java
- Keine vollständige Java-Semantik, da keine vollständige ABAP-Entsprechung
- Unbekannte Konstrukte werden als `TODO` ausgegeben
- Ausgabe ist ABAP-ähnlich und ist als Grundlage bzw. Grundgerüst gedacht, nicht als garantierter Produktivcode

## Lizenz

Die Nutzung der Anwendung ist zu Demonstrations- und Lernzwecken erlaubt. Die kommerzielle Nutzung, Weitergabe, Vervielfältigung oder Verbreitung ist untersagt. Siehe `LICENSE` für Details.
