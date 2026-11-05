# Extraction prompts

The Anthropic extractor loads these UTF-8 classpath resources once during
`ClaudeOfferExtractor.resource` acquisition, before creating its HTTP client:

- `src/main/resources/prompts/offer-extraction.md`: extraction instructions,
  including the Leistungen grouping rules.
- `src/main/resources/prompts/known-leistungen-empty.md`: instructions when the
  category has no existing Leistungen.
- `src/main/resources/prompts/known-leistungen-heading.md`: heading before the
  dynamically generated `id: Name` list.
- `src/main/resources/prompts/document-part.md`: header of a PDF text pass;
  `{label}` becomes e.g. `Teil 2/3 (Seiten 5–8)`.

There is no separate grouping/plan API prompt in this version. The JSON schema
stays in Scala: it is the structured-output contract, not natural-language
instructions. Dynamic IDs and names also remain generated in Scala.

Whitespace is intentional: the extraction and empty-category files have no
trailing newline; the heading ends with exactly one newline. The request still
joins the extraction instructions and category context with two newlines.
`ExtractionPromptsSpec` pins SHA-256 hashes of the original embedded strings.

Missing resources fail startup with `PromptLoadError.Missing`; acquisition,
UTF-8 decoding, reading and closing failures become `PromptLoadError.Unreadable`.
The streams are scoped by Cats Effect `Resource`; requests reuse the loaded text.

The existing Dockerfile copies all of `src`, runs `sbt stage`, and copies the
staged runtime JARs into `/app/lib`. `Runtime / fullClasspathAsJars` includes the
application JAR with these resources; no Dockerfile change is necessary.


## Long PDFs: text extraction and multiple passes

PDFs are no longer sent whole. `MultiPassOfferExtractor` reads the text layer
with Apache PDFBox (`PdfText`), page by page, and sends it as `text` blocks
with `=== Seite n ===` markers so page references stay absolute.
`PassBudget.default` sizes each pass:

| Setting | Value | Why |
|---|---|---|
| `contextTokens` | 200 000 | Claude Sonnet context window; input + output must fit |
| `maxOutputTokens` | 16 000 | the request's `max_tokens` (unchanged) |
| `inputTokens` | 24 000 | prompt + Leistungen + document text per pass |
| `maxPagesPerPass` | 4 | the JSON answer grows ~1–2k tokens per dense page |
| `charsPerToken` | 3 | conservative estimate for German text with numbers |

The binding limit is the answer size, not the context window: a ~10-page
offer in one pass ran out of `max_tokens`. Pages are packed in order into
chunks (≤ 4 pages, ≤ the character budget); a single overlong page is cut at
line breaks into pieces that keep its page number. Passes run in parallel
through `OfferPass.bounded` (2 concurrent Claude calls app-wide).

`RawOfferMerge` combines the parts: contractor/dates from the first part that
has them (disagreeing values become low-confidence notes), line items, notes
and exclusions concatenated in page order, totals and VAT from the last part
that has them, coverage per Leistung with the most specific status. If any
pass fails (e.g. `max_tokens`), the whole offer fails and the error names the
part, so no page is silently dropped.

PDFs without any text layer (scans) still go once as a `document` block;
JPEG/PNG keep the single `image` pass.
