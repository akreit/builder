package builder.http

import io.circe.Codec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*

/** Errors the JSON API can return; each case maps to one status code and JSON body. */
enum ApiError:
  case InvalidProjectId(projectId: String)
  case ProjectNotFound(projectId: String)
  case StorageFailed(code: String)

object ApiError:
  given Codec.AsObject[InvalidProjectId] = Codec.AsObject.derived
  given Codec.AsObject[ProjectNotFound] = Codec.AsObject.derived
  given Schema[InvalidProjectId] = Schema.derived
  given Schema[ProjectNotFound] = Schema.derived
  given Codec.AsObject[StorageFailed] = Codec.AsObject.derived
  given Schema[StorageFailed] = Schema.derived

  val output: EndpointOutput.OneOf[ApiError, ApiError] = oneOf[ApiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[InvalidProjectId]),
    oneOfVariant(StatusCode.NotFound, jsonBody[ProjectNotFound]),
    oneOfVariant(StatusCode.InternalServerError, jsonBody[StorageFailed])
  )
