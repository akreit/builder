package builder.http

import sttp.model.StatusCode

/** Error of an HTML endpoint: status code plus a German HTML message. */
final case class HtmlError(status: StatusCode, html: String)
