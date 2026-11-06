package builder

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class OfferIdentitySpec extends AnyFunSuite with Matchers:
  private def field(v: String) = Field(Some(v), Some(1), Confidence.High)
  private def raw(firma: String, adressat: Option[String]) =
    RawOffers.dachfenster.copy(
      contractor = field(firma),
      recipient = adressat.map(field)
    )

  private def upload(id: String, firma: String, adressat: Option[String]): Upload =
    Upload(
      OfferId.from(id).toOption.get,
      CategoryId.from("fenster").toOption.get,
      s"$id.pdf",
      OfferFileType.Pdf,
      Instant.parse("2026-11-06T10:00:00Z"),
      UploadOutcome.Read(raw(firma, adressat))
    )

  private def viewOf(uploads: List[Upload]): ProjectView =
    val cat = SampleData.project.toOption.get.categories.head
    val dummy = Fixtures.offerArgs("dummy", cat, 10_000L).build.toOption.get
    ProjectView(
      SampleData.project.toOption.get,
      uploads.map(u =>
        val r = u.outcome match
          case UploadOutcome.Read(raw) => raw
          case UploadOutcome.Failed(_) => raw("x", None)
        UploadView(u, UploadStatus.Accepted(r, dummy))
      ),
      Nil,
      Nil,
      Nil,
      Nil,
      Map.empty,
      Weights.default
    )

  test("display label keeps contractor and appends recipient without inventing one") {
    OfferLabel.display("Zimmerei Müller", Some("Andreas")) shouldBe "ZimmereiMüller_Andreas"
    OfferLabel.display("Zimmerei Müller", None) shouldBe "Zimmerei Müller"
    OfferLabel.display(raw("Zimmerei Müller", Some("Nachbar"))) shouldBe "ZimmereiMüller_Nachbar"
  }

  test("unknown recipient keys only match other unknowns") {
    RecipientKey.fromField(Option.empty[Field]) shouldBe RecipientKey.Unknown
    RecipientKey.of("  ") shouldBe RecipientKey.Unknown
    RecipientKey.of("Andreas") should not be RecipientKey.Unknown
    RecipientKey.of("Andreas") shouldBe RecipientKey.of("  andreas ")
  }

  test("same firm different recipients stay separate; same tuple replaces") {
    val a = upload("upl-a", "Zimmerei Müller", Some("Andreas"))
    val n = upload("upl-n", "Zimmerei Müller", Some("Nachbar"))
    val a2 = upload("upl-a2", "Zimmerei Müller", Some("Andreas"))
    val unknown = upload("upl-u", "Zimmerei Müller", None)
    val both = viewOf(List(a, n))
    UploadReplace.kept(both, a2) shouldBe a.id
    // Single match: in-place replace (keep a, nothing obsolete to delete).
    UploadReplace.obsolete(both, a2) shouldBe Nil
    UploadReplace.obsolete(both, n) shouldBe Nil
    val dupAndreas = viewOf(List(a, upload("upl-a-dup", "Zimmerei Müller", Some("Andreas"))))
    UploadReplace.kept(dupAndreas, a2) shouldBe a.id
    UploadReplace.obsolete(dupAndreas, a2).map(_.id.value) shouldBe List("upl-a-dup")
    val withUnknown = viewOf(List(a, unknown))
    UploadReplace.kept(withUnknown, a).value shouldBe "upl-a"
    UploadReplace.obsolete(withUnknown, a) shouldBe Nil
    UploadReplace.kept(withUnknown, unknown).value shouldBe "upl-u"
  }

  test("legacy RawOffer JSON without recipient decodes") {
    val json =
      """{"contractor":{"value":"Firma","page":1,"confidence":"high"},"date":{"value":"2026-01-01","page":1,"confidence":"high"},"validUntil":{"value":"","page":0,"confidence":"low"},"lineItems":[],"net":{"value":"100.00","page":1,"confidence":"high"},"vatRate":{"value":"19","page":1,"confidence":"high"},"vat":{"value":"19.00","page":1,"confidence":"high"},"gross":{"value":"119.00","page":1,"confidence":"high"},"notes":[],"exclusions":[]}"""
    val decoded = io.circe.parser.decode[RawOffer](json).toOption.get
    decoded.recipient shouldBe None
    UploadReplace.keyOf(decoded).get.recipient.isUnknown shouldBe true
  }
