# Extraction prompts

The Anthropic extractor loads these UTF-8 classpath resources once during
`ClaudeOfferExtractor.resource` acquisition, before creating its HTTP client:

- `src/main/resources/prompts/offer-extraction.md`: extraction instructions,
  including the Leistungen grouping rules.
- `src/main/resources/prompts/known-leistungen-empty.md`: instructions when the
  category has no existing Leistungen.
- `src/main/resources/prompts/known-leistungen-heading.md`: heading before the
  dynamically generated `id: Name` list.

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
