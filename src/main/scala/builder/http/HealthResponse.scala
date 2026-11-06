package builder.http

import io.circe.Codec
import sttp.tapir.Schema

/** JSON body of `GET /health`. */
final case class HealthResponse(status: String) derives Codec.AsObject, Schema
