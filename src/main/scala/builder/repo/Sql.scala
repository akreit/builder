package builder.repo

import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.OffsetDateTime

/** Every statement of [[PostgresProjectRepository]], typed with Skunk encoders and decoders. Ids
  * are passed as their string values; `p` is always the project id.
  */
private[repo] object Sql:
  // ---- reads ---------------------------------------------------------------------------------

  val uploads: Query[String, UploadRow] =
    sql"""SELECT id, category_id, file_name, file_type, created_at, extraction,
            extraction_version, failure
          FROM uploads WHERE project_id = $text ORDER BY created_at, id"""
      .query(text *: text *: text *: text *: timestamptz *: text.opt *: int4.opt *: text.opt)
      .map { case (i, c, n, t, at, e, v, f) => UploadRow(i, c, n, t, at.toInstant, e, v, f) }

  val categories: Query[String, CategoryRow] =
    sql"""SELECT id, name, position, deleted FROM categories
          WHERE project_id = $text ORDER BY position, id"""
      .query((text *: text *: int4 *: bool).to[CategoryRow])

  val leistungen: Query[String, LeistungRow] =
    sql"""SELECT id, category_id, name FROM leistungen
          WHERE project_id = $text AND NOT deleted ORDER BY position, id"""
      .query((text *: text *: text).to[LeistungRow])

  val assignments: Query[String, AssignmentRow] =
    sql"SELECT offer_id, item, leistung_id, manual FROM item_leistung WHERE project_id = $text"
      .query((text *: int4 *: text.opt *: bool).to[AssignmentRow])

  val coverage: Query[String, CoverageRow] =
    sql"""SELECT offer_id, leistung_id, status, page, confidence, manual FROM abdeckung
          WHERE project_id = $text"""
      .query((text *: text *: text *: int4.opt *: text.opt *: bool).to[CoverageRow])

  val favorites: Query[String, (String, String)] =
    sql"SELECT category_id, offer_id FROM favorites WHERE project_id = $text ORDER BY category_id"
      .query(text *: text)

  val ratings: Query[String, (String, Int, String)] =
    sql"SELECT contractor_key, stars, note FROM ratings WHERE project_id = $text"
      .query(text *: int4 *: text)

  val weights: Query[String, (String, Int)] =
    sql"SELECT criterion, weight FROM weights WHERE project_id = $text".query(text *: int4)

  // ---- offers, uploads, extracted data -------------------------------------------------------

  /** (project, category, offer) */
  val upsertFavorite: Command[(String, String, String)] =
    sql"""INSERT INTO favorites(project_id, category_id, offer_id) VALUES ($text, $text, $text)
          ON CONFLICT (project_id, category_id) DO UPDATE SET offer_id = excluded.offer_id""".command

  /** (project, offer) */
  val insertOffer: Command[(String, String)] =
    sql"INSERT INTO offers(project_id, id) VALUES ($text, $text)".command

  /** (project, offer); for seed offers, which exist after the first start. */
  val insertSeedOffer: Command[(String, String)] =
    sql"INSERT INTO offers(project_id, id) VALUES ($text, $text) ON CONFLICT DO NOTHING".command

  /** (project, id, category, file name, file type, created, extraction, version, failure) */
  val insertUpload: Command[
    (
        String,
        String,
        String,
        String,
        String,
        OffsetDateTime,
        Option[String],
        Option[Int],
        Option[String]
    )
  ] =
    sql"""INSERT INTO uploads(project_id, id, category_id, file_name, file_type, created_at,
            extraction, extraction_version, failure)
          VALUES ($text, $text, $text, $text, $text, $timestamptz, ${text.opt}, ${int4.opt},
            ${text.opt})""".command

  /** (project, id, category, name), appended after the last position of the project. */
  val insertLeistung: Command[(String, String, String, String)] =
    sql"""INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
          VALUES ($text, $text, $text, $text,
            (SELECT COALESCE(MAX(position), 0) + 1 FROM leistungen WHERE project_id = $text),
            false)""".command
      .contramap((p, i, c, n) => (p, i, c, n, p))

  /** (project, id, category, name, position); seed Leistungen keep later renames and deletions. */
  val insertSeedLeistung: Command[(String, String, String, String, Int)] =
    sql"""INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
          VALUES ($text, $text, $text, $text, $int4, false) ON CONFLICT DO NOTHING""".command

  /** (project, offer, item, Leistung) as read by the extractor. */
  val insertReadItem: Command[(String, String, Int, Option[String])] =
    sql"""INSERT INTO item_leistung(project_id, offer_id, item, leistung_id, manual)
          VALUES ($text, $text, $int4, ${text.opt}, false)""".command

  /** (project, offer, Leistung, status, page, confidence) as read by the extractor. */
  val insertReadCoverage: Command[(String, String, String, String, Option[Int], String)] =
    sql"""INSERT INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
          VALUES ($text, $text, $text, $text, ${int4.opt}, $text, false)""".command

  // ---- manual edits --------------------------------------------------------------------------

  /** (project, category, name, deleted) */
  val upsertCategory: Command[(String, String, String, Boolean)] =
    sql"""INSERT INTO categories(project_id, id, name, position, deleted)
          VALUES ($text, $text, $text,
            (SELECT COALESCE(MAX(position), 0) + 1 FROM categories WHERE project_id = $text), $bool)
          ON CONFLICT (project_id, id)
          DO UPDATE SET name = excluded.name, deleted = excluded.deleted""".command
      .contramap((p, c, n, d) => (p, c, n, p, d))

  /** (project, category) */
  val deleteLeistungenOfCategory: Command[(String, String)] =
    sql"UPDATE leistungen SET deleted = true WHERE project_id = $text AND category_id = $text".command

  /** (project, Leistung, name) */
  val renameLeistung: Command[(String, String, String)] =
    sql"UPDATE leistungen SET name = $text WHERE project_id = $text AND id = $text".command
      .contramap((p, l, n) => (n, p, l))

  /** (project, Leistung) */
  val deleteLeistung: Command[(String, String)] =
    sql"UPDATE leistungen SET deleted = true WHERE project_id = $text AND id = $text".command

  /** (project, from, into) */
  val moveItems: Command[(String, String, String)] =
    sql"UPDATE item_leistung SET leistung_id = $text WHERE project_id = $text AND leistung_id = $text".command
      .contramap((p, f, i) => (i, p, f))

  /** (project, from, into): read statuses of `into` become "enthalten" where `from` was. */
  val mergeReadCoverage: Command[(String, String, String)] =
    sql"""UPDATE abdeckung SET status = 'enthalten'
          WHERE project_id = $text AND leistung_id = $text AND NOT manual AND offer_id IN
            (SELECT offer_id FROM abdeckung WHERE project_id = $text AND leistung_id = $text
             AND NOT manual AND status = 'enthalten')""".command
      .contramap((p, f, i) => (p, i, p, f))

  /** (project, from, into): rows that would collide with a row of `into` stay and are deleted. */
  val moveCoverage: Command[(String, String, String)] =
    sql"""UPDATE abdeckung a SET leistung_id = $text
          WHERE a.project_id = $text AND a.leistung_id = $text
          AND NOT EXISTS (SELECT 1 FROM abdeckung b WHERE b.project_id = a.project_id
            AND b.offer_id = a.offer_id AND b.manual = a.manual AND b.leistung_id = $text)""".command
      .contramap((p, f, i) => (i, p, f, i))

  /** (project, Leistung) */
  val deleteCoverageOf: Command[(String, String)] =
    sql"DELETE FROM abdeckung WHERE project_id = $text AND leistung_id = $text".command

  /** (project, Leistung) */
  val unassignItems: Command[(String, String)] =
    sql"""UPDATE item_leistung SET leistung_id = NULL
          WHERE project_id = $text AND leistung_id = $text""".command

  /** (project, offer, item, Leistung) */
  val upsertManualItem: Command[(String, String, Int, Option[String])] =
    sql"""INSERT INTO item_leistung(project_id, offer_id, item, leistung_id, manual)
          VALUES ($text, $text, $int4, ${text.opt}, true)
          ON CONFLICT (project_id, offer_id, item, manual)
          DO UPDATE SET leistung_id = excluded.leistung_id""".command

  /** (project, contractor, stars, note) */
  val upsertRating: Command[(String, String, Int, String)] =
    sql"""INSERT INTO ratings(project_id, contractor_key, stars, note)
          VALUES ($text, $text, $int4, $text)
          ON CONFLICT (project_id, contractor_key)
          DO UPDATE SET stars = excluded.stars, note = excluded.note""".command

  /** (project, contractor) */
  val deleteRating: Command[(String, String)] =
    sql"DELETE FROM ratings WHERE project_id = $text AND contractor_key = $text".command

  /** (project, criterion, weight) */
  val upsertWeight: Command[(String, String, Int)] =
    sql"""INSERT INTO weights(project_id, criterion, weight) VALUES ($text, $text, $int4)
          ON CONFLICT (project_id, criterion) DO UPDATE SET weight = excluded.weight""".command

  /** (project, offer, Leistung, status) */
  val upsertManualCoverage: Command[(String, String, String, String)] =
    sql"""INSERT INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
          VALUES ($text, $text, $text, $text, NULL, NULL, true)
          ON CONFLICT (project_id, offer_id, leistung_id, manual)
          DO UPDATE SET status = excluded.status, page = NULL, confidence = NULL""".command

  /** (project, offer, Leistung) */
  val deleteManualCoverage: Command[(String, String, String)] =
    sql"""DELETE FROM abdeckung
          WHERE project_id = $text AND offer_id = $text AND leistung_id = $text AND manual""".command

  // ---- migrations ----------------------------------------------------------------------------

  val lockMigrations: Query[Long, Int] =
    sql"SELECT 1 FROM (SELECT pg_advisory_xact_lock($int8)) AS l".query(int4)

  val createMigrationsTable: Command[Void] =
    sql"""CREATE TABLE IF NOT EXISTS schema_migrations(version INTEGER PRIMARY KEY,
            applied_at TIMESTAMPTZ NOT NULL DEFAULT now())""".command

  val appliedVersion: Query[Int, Int] =
    sql"SELECT version FROM schema_migrations WHERE version = $int4".query(int4)

  val recordVersion: Command[Int] =
    sql"INSERT INTO schema_migrations(version) VALUES ($int4)".command
