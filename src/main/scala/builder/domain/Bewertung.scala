package builder.domain

/** Reference rating of a contractor: 1 to 5 stars. */
opaque type Stars = Int

/** Smart constructor for [[Stars]] (1–5). */
object Stars:
  /** Smart constructor: only values 1–5 are accepted. */
  def from(n: Int): Either[DomainError, Stars] =
    Either.cond(n >= 1 && n <= 5, n, DomainError.InvalidStars(n))
  extension (s: Stars) def value: Int = s

/** Contractors are identified by their name, case and surrounding spaces ignored. */
opaque type ContractorKey = String

/** Builds a normalised [[ContractorKey]] from a display name. */
object ContractorKey:
  /** Builds a key from a display name (trim, lower case, collapse spaces); blank → `None`. */
  def of(name: String): Option[ContractorKey] =
    Option(name.trim.toLowerCase.replaceAll("\\s+", " ")).filter(_.nonEmpty)
  extension (k: ContractorKey) def value: String = k

/** What the user knows about a contractor's references. */
final case class Rating(stars: Stars, note: String)

/** Limits and helpers for [[Rating]]. */
object Rating:
  /** Longest note kept with a rating. */
  val MaxNote: Int = 500

/** A criterion offers are ranked by. More (e.g. Termin, Gewährleistung) can be added as cases. */
enum Criterion:
  case Preis
  case Referenzen

/** Labels and wire codes for [[Criterion]]. */
object Criterion:
  /** German label shown in the ranking UI. */
  def label(c: Criterion): String = c match
    case Preis      => "Preis"
    case Referenzen => "Referenzen"

  /** Stable code used in forms and the `weights` table. */
  def code(c: Criterion): String = c match
    case Preis      => "preis"
    case Referenzen => "referenzen"

  /** Inverse of [[code]]; unknown strings yield `None`. */
  def fromCode(s: String): Option[Criterion] = values.find(code(_) == s)

/** Relative weight of each criterion in percent points (0 to 100). */
opaque type Weights = Map[Criterion, Int]

/** Default and validated [[Weights]] maps for ranking criteria. */
object Weights:
  val default: Weights = Map(Criterion.Preis -> 50, Criterion.Referenzen -> 50)

  /** Every criterion needs a weight from 0 to 100, and together they make exactly 100. */
  def from(ws: Map[Criterion, Int]): Either[DomainError, Weights] =
    Either.cond(
      Criterion.values.forall(c =>
        ws.get(c).exists(w => w >= 0 && w <= 100)
      ) && Criterion.values.toList.map(c => ws.getOrElse(c, 0)).sum == 100,
      ws.filter((c, _) => Criterion.values.contains(c)),
      DomainError.InvalidWeights(ws.map((c, w) => Criterion.code(c) -> w))
    )

  extension (w: Weights) def of(c: Criterion): Int = w.getOrElse(c, 0)
