package builder.ui.layout

import scalatags.Text.all.*

/** The fixed header bar on top of every page. */
object Header:
  /** Name of the app, shown when a page belongs to no (named) project. */
  val AppName: String = "Builder"

  /** The project's name if the page belongs to a project with a non-blank name, else the app's. */
  def title(projectName: Option[String]): String =
    projectName.map(_.trim).filter(_.nonEmpty).getOrElse(AppName)

  private[ui] def bar(projectName: Option[String]): Frag =
    header(cls := "kopfzeile", id := "kopfzeile")(a(href := "/")(title(projectName)))
