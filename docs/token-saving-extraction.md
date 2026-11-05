# Entwurf: tokensparende Angebotsextraktion

## Ziel und Status

Lange Angebote erhöhen Eingabetokens, Kosten und das Risiko einer bei
`max_tokens` abgeschnittenen Antwort. Eine zusätzliche Verdichtungsschicht soll
irrelevante Wiederholungen entfernen, ohne Positionen, Preise oder Ausschlüsse
still zu verlieren. Dies ist nur ein Entwurf: keine Implementierung, keine
Modellauswahl, keine bezahlten API-Aufrufe und keine UI-Änderung.

Basis ist `origin/main`, nicht der Feature-Branch von
[PR #17](https://github.com/akreit/builder/pull/17). Dieser PR ist bei Erstellung
dieses Entwurfs noch offen. `MultiPassOfferExtractor`, `PassBudget`, `Chunking`
und dessen Provider-Anbindung sind daher **künftige Integrationspunkte nach #17**,
keine auf main bereits verfügbare Verdrahtung.

## Drei Stufen: Eingaben und Ausgaben

1. **Deterministisch bereinigen:** PDFBox-Seitentext mit Seiten- und Zeilenherkunft
   → bereinigte Seiten plus Entfernungsprotokoll. Nur konservativ erkannte Kopf-/
   Fußzeilen, Seitenzahlen und wiederholte Tabellenköpfe entfernen. Wiederholung
   allein reicht nicht: Preiszeilen, Überträge, Summen, Vertragspartner und
   Bedingungen bleiben erhalten; zweifelhafte Zeilen bleiben ebenfalls erhalten.
   Regeln sind versioniert, Änderungen offline an Referenz-PDFs prüfbar.
2. **Günstig verdichten:** jeder bestehende Chunk mit Herkunft und Manifest →
   kompaktes, schema-validiertes JSON: Positionen (Beschreibung, Menge, Einheit,
   Maße, Hersteller, Modell, Einzel-/Gesamtpreis), Summen/Steuern, Ausschlüsse und
   relevante Angebotsmetadaten (Firma, Datum, Gültigkeit, Hinweise). Nur inhaltslose
   Prosa verwerfen; eine Bedingung im Fließtext ist kein entbehrlicher Text.
   Zahlen und Währung unverändert übernehmen, Unbekanntes nicht ergänzen.
   Jeder Datensatz trägt Quell-IDs; unsichere Passagen werden als strukturierte
   Restpassagen mit Originaltext statt als vermeintlich irrelevante Prosa erhalten.
3. **Stark extrahieren:** ausschließlich validierte kompakte Daten einschließlich
   Restpassagen, Seitenherkunft und bekannter Leistungen → heutiges `RawOffer`
   mit `Field`-Herkunft/Konfidenz, Leistungszuordnung und bestehenden Feldern.
   Danach vorhandenes Multipass-Merge und heutige Domänenvalidierung nutzen.
   Keine neue öffentliche Ergebnisform und keine vorgezogene Leistungszuordnung
   im kleinen Modell.

## Checksum-Vertrag und Fehlerpfad

Die Anwendung erstellt **vor** dem kleinen Modell pro Chunk ein Manifest:
Chunk-ID, stabile Quellzeilen-IDs, Seiten, Zeichenanzahl je Zeile und Gesamtzahl
(Unicode-Codepoints; Zeilenenden vorher auf LF normalisieren). Das separate
Entfernungsprotokoll zählt die vorab entfernten Zeichen und ihren Regelgrund;
die Modell-Prüfsumme bezieht sich ausschließlich auf den bereinigten Chunk.

Die Antwort muss jede Quell-ID genau einmal abdecken: entweder durch Referenz
in einem strukturierten Datensatz oder durch einen expliziten Verwerfungsbeleg
mit eng definiertem Grund für inhaltslose Prosa. Mehrere Datensätze dürfen eine
Zeile referenzieren; gezählt wird die eindeutige ID-Menge, nicht ihre Häufigkeit.
Referenzen und Verwerfungsbelege dürfen sich nicht überschneiden. Die Anwendung
berechnet aus den referenzierten Originalzeilen die Zeichenanzahl neu und
vergleicht sie mit dem Manifest sowie dem zurückgegebenen Zähler. Fehlende,
fremde oder doppelte Verwerfungs-IDs, unzulässige Gründe, Schemafehler,
Abschneiden oder Zählabweichungen machen den gesamten Chunk ungültig.
Ein bloß vom Modell zurückgespiegelter Gesamtzähler genügt ausdrücklich nicht.
Die Anzahl der ausgegebenen Positionen wird zusätzlich gegen die JSON-Liste
geprüft, ist aber keine unabhängige Sollzahl aus dem PDF.

**Grenze:** Vollständige Quellabdeckung beweist nicht semantische Treue; ein
Modell kann eine Zahl falsch lesen oder Prosa falsch klassifizieren. Numerische
Quellbelege, konservative Restpassagen und Offline-Vergleiche gegen unverdichtete
Extraktion bleiben nötig. Unsichere Verwerfungen sind ungültig, nicht akzeptiert.

Bei Fehlern keine Teilübernahme und kein stilles Weiterreichen an das starke
Modell. Vorgeschlagener sicherer Standard: Extraktion abbrechen mit
„Die Verdichtung ist unvollständig. Bitte das Angebot erneut prüfen.“ Ein
unverdichteter Fallback oder Modell-Retry darf nicht automatisch zusätzliche
Kosten auslösen; Aktivierung und Kostenrahmen müssen vorher freigegeben sein.
Logs enthalten nur IDs, Zähler und Fehlerklassen, keine Angebotstexte.

## Kleine austauschbare Schnittstelle

Nur Signaturskizze, kein kompilierbarer Produktionscode; die benannten
Transporttypen und Fehler sind neu zu entwerfen, nicht bereits vorhanden.
Der Kompressor kennt weder konkreten Anbieter noch Multipass-Orchestrierung.

```scala
trait ChunkCompressor[F[_]]:
  def compress(
      chunk: PreparedChunk,
      manifest: CoverageManifest
  ): F[Either[CompressionError, CompactChunk]]
```

`PreparedChunk` enthält bereinigte Zeilen mit IDs/Seiten;
`CoverageManifest` die erwarteten IDs/Zähler; `CompactChunk` die strukturierten
Belege, Restpassagen und Abdeckungsdaten. `CompressionError` unterscheidet
Transport-, Schema-, Budget- und Abdeckungsfehler. Die Anwendung validiert das
Ergebnis unabhängig vom Provider vor Stufe 3.
Das bestehende `OfferExtractor[F]` mit
`extract(file, fileType, known): F[Either[ExtractionError, RawOffer]]` bleibt
unverändert. Die künftige `ExtractionProvider`-artige Factory injiziert genau
`ChunkCompressor[F]`; Anbieterwechsel benötigt keine Änderung an Aufrufern.
Die Abbildung neuer Fehler auf `ExtractionError` wird erst bei Implementierung
festgelegt. Keine Scala-Stubs oder Änderungen am Laufzeitpfad in diesem PR.

## Zusammenspiel mit Multipass

Reihenfolge nach Integration von #17:

**PDFBox → strip → Chunking → compress pro Chunk → strong extract → merge**

Seitenidentitäten bleiben beim Bereinigen erhalten. `MultiPassOfferExtractor`
koordiniert weiterhin Chunks und Merge; `PassBudget` bleibt unverändert:
**maximal 4 Seiten, 24.000 Eingabetokens, `max_tokens = 16000`**.
Das gilt auch für die starke Extraktion inklusive Prompt/Schema/Leistungen;
Verdichtung ist kein Anlass, Chunks zu vergrößern oder mehr Seiten zusammenzulegen.
Kompressor-Prompt und Antwort brauchen zusätzlich eine explizite Budgetprüfung,
dürfen die bestehenden Grenzen aber nicht erhöhen. Budgetüberschreitung führt
zum Fehler, nicht zu stiller Kürzung. Das bisherige Verhalten bleibt bis zur
späteren, separat freigegebenen Integration unangetastet.

## Nicht im Umfang

- OCR/Scans und Änderungen am Bildpfad.
- Änderungen an `PassBudget`-Defaults, Chunkgrößen oder `RawOffer`.
- Implementierung, neue Dependencies, Migrationen, UI oder Modell-/API-Aufrufe.
- Behauptete Einsparquoten ohne Messung. Später offline Abdeckung und Qualität,
  danach nur mit Freigabe Tokens, Gesamtkosten beider Modelle und Latenz messen.

## Offene Fragen an Andreas

- Welcher günstige Provider und welches Kostenlimit dürfen später getestet werden?
- Fehler standardmäßig abbrechen oder vorab budgetierten unverdichteten Fallback erlauben?
- Welche anonymisierten Referenzangebote und Qualitätsgrenzen gelten als Abnahme?
- Welche Verwerfungsgründe für inhaltslose Prosa sind akzeptabel, und wie lange
  dürfen Herkunfts-/Entfernungsbelege aufbewahrt werden?
