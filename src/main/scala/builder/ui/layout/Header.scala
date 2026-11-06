package builder.ui.layout

import scalatags.Text.all.*

/** The fixed header bar on top of every page. */
object Header:
  /** Name of the app, shown when a page belongs to no (named) project. */
  val AppName: String = "Builder"

  /** "Projekt {name}" for a non-blank project name (no colon); else the app name. */
  def title(projectName: Option[String]): String =
    projectName.map(_.trim).filter(_.nonEmpty).map(n => s"Projekt $n").getOrElse(AppName)

  /** Top bar showing the project name, or the app name outside a project. */
  private[ui] def bar(projectName: Option[String]): Frag =
    header(cls := "kopfzeile", id := "kopfzeile")(a(href := "/")(title(projectName)))
