# builder

A Scala 3 service for managing renovation projects. The goal: take in a project's scope, take in
contractor offers and extract their line items, compare offers side by side, mark favorites, and
keep a running total. Documents will come from a restricted Google Drive folder, read through a
service account. Right now the repo has the domain model (per-project categories), totals calculation, a
small JSON API and a German server-rendered UI (Scalatags + htmx), all over in-memory sample data.

## Requirements

- JDK 21
- sbt 2.0.11

## Commands

```sh
sbt compile      # compile
sbt testFull     # run all tests
sbt run          # start the HTTP server (port 8080, override with PORT=...)
```

```sh
curl http://localhost:8080/health
curl http://localhost:8080/projects/demo/totals
curl http://localhost:8080/          # Projektübersicht (HTML)
curl -X POST http://localhost:8080/ui/projects/demo/categories/daemmung/favorite/daemmung-b
```

## Lokal mit Docker starten

Voraussetzung: Docker mit Compose-Plugin.

```sh
cp .env.example .env        # ANTHROPIC_API_KEY eintragen (.env wird nicht committet)
docker compose up --build   # baut das Image und startet die App
```

Danach läuft die App unter http://localhost:8080 (anderer Port: `BUILDER_PORT=9090` in `.env`).
Die SQLite-Daten liegen im benannten Volume `builder-data` (`/data` im Container) und
bleiben bei `docker compose down` / `up` erhalten. Alles löschen: `docker compose down -v`.
Der Container prüft sich selbst über `/health` (Status `healthy` in `docker compose ps`).

Das Image entsteht ohne zusätzliche sbt-Plugins: `sbt stage` kopiert alle Laufzeit-Jars nach
`target/stage/lib`, das Runtime-Image (JRE 21, Nutzer ohne Root-Rechte) startet `builder.Main`.

## Formatting

```sh
sbt scalafmtAll scalafmtSbt scalafixAll   # format and lint
sbt scalafmtCheckAll                      # check only
```

## Docs

```sh
sbt docs/mdoc "docs/runMain DocsSite"
```

Sources live in `docs/`. The output goes to `target/docs/`.

## Layout (`src/main/scala/builder`)

- `Main.scala`: Ember server entry point
- `SampleData.scala`: the in-memory `demo` project "Dach" (fictional sample offers)
- `domain/`: model, money, extracted offer data, errors
- `totals/`: running-total calculation
- `repo/`: project repository (in-memory, `Ref`-based; favorites can change)
- `http/`: Tapir endpoints (JSON + HTML) and http4s routes
- `ui/`: Scalatags pages and German formatting; htmx is vendored in
  `src/main/resources/static/htmx.min.js` (htmx 2.0.11)

## Planned

- Scope intake
- Offer upload and extraction
- Offer comparison
- Google Drive folder access through a service account
