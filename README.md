# builder

A Scala 3 service for managing renovation projects. The goal: take in a project's scope, take in
contractor offers and extract their line items, compare offers side by side, mark favorites, and
keep a running total. Documents will come from a restricted Google Drive folder, read through a
service account. Right now the repo has the domain model (per-project categories), totals calculation, a
small JSON API and a German server-rendered UI (Scalatags + htmx), persisted in Postgres.

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
cp .env.example .env        # ANTHROPIC_API_KEY und POSTGRES_PASSWORD eintragen (.env wird nicht committet)
docker compose up --build   # startet Postgres, baut das Image und startet die App
```

Danach läuft die App unter http://localhost:8080 (anderer Port: `BUILDER_PORT=9090` in `.env`).
Die Datenbank ist Postgres (Service `db`, Volume `builder-pg`); die App startet erst, wenn
Postgres `healthy` ist, und legt das Schema beim Start selbst an (versionierte Migrationen in
der Tabelle `schema_migrations`). Hochgeladene Dateien liegen weiter im Volume `builder-data`
(`/data` im Container, `BUILDER_DATA_DIR`). Beides bleibt bei `docker compose down` / `up` erhalten. Alles löschen: `docker compose down -v`.
Die App ist nur lokal erreichbar (Port an `127.0.0.1` gebunden). Wer statt des Volumes ein
Verzeichnis einbindet (Bind-Mount), muss es dem App-Nutzer geben: `sudo chown -R 10001:10001 ./data`.
Der Container prüft sich selbst über `/health` (Status `healthy` in `docker compose ps`).

Das Image entsteht ohne zusätzliche sbt-Plugins: `sbt stage` kopiert alle Laufzeit-Jars nach
`target/stage/lib`, das Runtime-Image (JRE 21, Nutzer ohne Root-Rechte) startet `builder.Main`.

## Datenbank

Die App speichert in Postgres (Zugriff mit [Skunk](https://typelevel.org/skunk/)). Verbindung über
`DATABASE_URL` (`postgres://user:passwort@host:5432/db`) oder einzeln `DATABASE_HOST`,
`DATABASE_PORT` (Default 5432), `DATABASE_USER`, `DATABASE_PASSWORD`, `DATABASE_NAME`.
Das Passwort wird nie geloggt. Für `sbt run` ohne Compose z. B.:

```sh
docker compose up -d db   # nur an 127.0.0.1:5432 erreichbar (anderer Port: POSTGRES_PORT)
DATABASE_URL=postgres://builder:<passwort>@localhost:5432/builder sbt run
```

Tests laufen gegen ein echtes Postgres: ohne weitere Einstellung startet `sbt testFull` per
[Testcontainers](https://testcontainers.com/) einen `postgres:17-alpine`-Container (Docker nötig).
Alternativ einen vorhandenen Server nutzen:
`BUILDER_TEST_DATABASE_URL=postgres://postgres:<passwort>@localhost:5432/postgres sbt testFull`
(Superuser, jede Testgruppe bekommt eine eigene, danach gelöschte Datenbank).

Object Storage für die Dateien folgt in #6; dieser Schritt deckt davon nur den Datenbank-Teil
lokal ab.

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
- `repo/`: project repository (Postgres via Skunk, versioned migrations)
- `http/`: Tapir endpoints (JSON + HTML) and http4s routes
- `ui/`: Scalatags pages and German formatting; htmx is vendored in
  `src/main/resources/static/htmx.min.js` (htmx 2.0.11)

## Planned

- Scope intake
- Offer upload and extraction
- Offer comparison
- Google Drive folder access through a service account
