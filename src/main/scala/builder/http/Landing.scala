package builder.http

/** Response of the start route: a redirect to the first tab, or the page without categories. */
enum Landing:
  case Redirect(location: String)
  case Page(html: String)
