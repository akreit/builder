package builder.extraction

import builder.domain.*

/** Coverage of one Leistung as read by the extractor. */
final case class ReadCoverage(
    leistung: LeistungId,
    coverage: Coverage,
    page: Option[PageNumber],
    confidence: Confidence
)

/** What an extraction adds to a category: new Leistungen, the Leistung of every line item (`None`:
  * not mapped) and the coverage of each Leistung.
  */
final case class LeistungPlan(
    added: List[Leistung],
    items: List[Option[LeistungId]],
    coverage: List[ReadCoverage]
)

object LeistungPlan:
  private def norm(s: String): String = s.trim.toLowerCase

  /** Resolves the extractor's references against the category's `known` Leistungen. A proposed
    * Leistung whose name equals a known (or earlier proposed) one is mapped onto it instead of
    * being added twice. `freshIds` supplies ids for added Leistungen; references that resolve to
    * nothing are left unmapped rather than guessed.
    */
  def plan(
      raw: RawOffer,
      category: CategoryId,
      known: List[Leistung],
      freshIds: LazyList[LeistungId]
  ): LeistungPlan =
    val proposed = raw.neueLeistungen.getOrElse(Nil)
    val (added, keys, _) =
      proposed.foldLeft((List.empty[Leistung], Map.empty[String, LeistungId], freshIds)) {
        case ((acc, keys, ids), p) =>
          LeistungName.from(p.name) match
            case Left(_)     => (acc, keys, ids)
            case Right(name) =>
              (known ++ acc).find(l => norm(l.name.value) == norm(name.value)) match
                case Some(existing) => (acc, keys + (p.key -> existing.id), ids)
                case None           =>
                  ids.headOption match
                    case Some(id) =>
                      (acc :+ Leistung(id, category, name), keys + (p.key -> id), ids.drop(1))
                    case None => (acc, keys, ids)
      }
    val byId = known.map(l => l.id.value -> l.id).toMap
    def resolve(ref: String): Option[LeistungId] =
      byId.get(ref.trim).orElse(keys.get(ref.trim))
    val coverage = raw.abdeckung
      .getOrElse(Nil)
      .flatMap(a =>
        resolve(a.leistung).map(id =>
          ReadCoverage(
            id,
            Coverage.fromCode(a.status).getOrElse(Coverage.Unklar),
            PageNumber.from(a.page).toOption,
            a.confidence
          )
        )
      )
      .groupBy(_.leistung)
      .values
      .flatMap(_.headOption)
      .toList
      .sortBy(c => c.leistung.value)
    LeistungPlan(added, raw.lineItems.map(_.leistung.flatMap(resolve)), coverage)
