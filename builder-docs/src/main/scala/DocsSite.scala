import cats.effect.{IO, IOApp}
import laika.api.Transformer
import laika.format.{HTML, Markdown}
import laika.ast.Path.Root
import laika.helium.Helium
import laika.helium.config.{HeliumIcon, IconLink}
import laika.io.syntax.*

/** Renders the mdoc output (target/docs/mdoc) to an HTML site with Helium (target/docs/site). */
object DocsSite extends IOApp.Simple:
  def run: IO[Unit] =
    Transformer
      .from(Markdown)
      .to(HTML)
      .using(Markdown.GitHubFlavor)
      .parallel[IO]
      .withTheme(
        Helium.defaults.site
          .topNavigationBar(homeLink = IconLink.internal(Root / "index.md", HeliumIcon.home))
          .build
      )
      .build
      .use(_.fromDirectory("target/docs/mdoc").toDirectory("target/docs/site").transform)
      .flatMap(tree => IO.println(s"Rendered ${tree.allDocuments.size} document(s) to target/docs/site"))
