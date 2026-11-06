package builder.http

/** Headers of the CSRF guard; checked (security input) before any body is read. */
final case class HtmxHeaders(
    hxRequest: Option[String],
    origin: Option[String],
    host: Option[String]
)
