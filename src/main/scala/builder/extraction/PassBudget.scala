package builder.extraction

/** Token budget of one extraction pass (one Messages API call).
  *
  *   - `contextTokens`: context window of the model (Claude Sonnet: 200 000). Input and output of
  *     one call must fit together.
  *   - `maxOutputTokens`: the request's `max_tokens` (16 000). Long offers ran out of it on a
  *     single pass, because the JSON answer grows with the number of line items.
  *   - `inputTokens`: budget for prompt + category context + document text of one pass (24 000).
  *     Far below the context window on purpose: what limits a pass is the answer size, so the
  *     document per pass is kept small enough that its line items fit into `maxOutputTokens`.
  *   - `maxPagesPerPass`: at most this many pages per pass (4). A dense offer page yields roughly
  *     1–2k output tokens, so four pages stay well below 16 000.
  *   - `charsPerToken`: conservative estimate (3) for German text with numbers; real tokenisers
  *     usually give 3.5–4, so the estimate errs towards more, smaller passes.
  */
final case class PassBudget(
    contextTokens: Int,
    maxOutputTokens: Int,
    inputTokens: Int,
    maxPagesPerPass: Int,
    charsPerToken: Int
):
  /** Estimated tokens of a text. */
  def tokens(text: String): Int = (text.length + charsPerToken - 1) / charsPerToken

  /** Characters of document text that fit next to `prompt` (instructions + category context). */
  def documentChars(prompt: String): Int = (inputTokens - tokens(prompt)).max(1) * charsPerToken

/** Default token/character budgets for multi-pass extraction. */
object PassBudget:
  val default: PassBudget = PassBudget(
    contextTokens = 200000,
    maxOutputTokens = 16000,
    inputTokens = 24000,
    maxPagesPerPass = 4,
    charsPerToken = 3
  )

/** Pages of one pass. `part` marks a page too long for one pass that was cut into several pieces;
  * page numbers are never lost.
  */
final case class TextChunk(index: Int, total: Int, pages: List[PdfPage]):
  def firstPage: Int = pages.headOption.fold(0)(_.number)
  def lastPage: Int = pages.lastOption.fold(0)(_.number)

  /** Document text as sent to Claude: every page under a `=== Seite n ===` marker. */
  def render: String = pages.map(p => s"=== Seite ${p.number} ===\n${p.text.trim}").mkString("\n\n")

  /** German label for errors and notes, e.g. "Teil 2/3 (Seiten 5–8)". */
  def label: String =
    val range =
      if firstPage == lastPage then s"Seite $firstPage" else s"Seiten $firstPage–$lastPage"
    s"Teil $index/$total ($range)"

/** Splits page texts into page-aware chunks within a character limit. */
object Chunking:
  /** Packs whole pages greedily (in order) into chunks of at most `maxChars` characters and
    * `maxPages` pages; a single page longer than `maxChars` is cut at line breaks into several
    * pieces carrying the same page number.
    */
  def chunks(pages: List[PdfPage], maxChars: Int, maxPages: Int): List[TextChunk] =
    val groups = pack(pages.flatMap(split(_, maxChars)), maxChars, maxPages)
    groups.zipWithIndex.map((g, i) => TextChunk(i + 1, groups.size, g))

  private def size(p: PdfPage): Int = p.text.length

  private def pack(pages: List[PdfPage], maxChars: Int, maxPages: Int): List[List[PdfPage]] =
    pages
      .foldLeft(List.empty[List[PdfPage]]) {
        case (current :: done, p)
            if current.sizeIs < maxPages && current.map(size).sum + size(p) <= maxChars =>
          (p :: current) :: done
        case (acc, p) => List(p) :: acc
      }
      .map(_.reverse)
      .reverse

  /** Cuts an oversize page at line breaks (hard cut for a single overlong line). */
  private def split(page: PdfPage, maxChars: Int): List[PdfPage] =
    if size(page) <= maxChars then List(page)
    else
      page.text.linesWithSeparators
        .flatMap(_.grouped(maxChars))
        .foldLeft(List.empty[String]) {
          case (current :: done, line) if current.length + line.length <= maxChars =>
            (current + line) :: done
          case (acc, line) => line :: acc
        }
        .reverse
        .map(PdfPage(page.number, _))
