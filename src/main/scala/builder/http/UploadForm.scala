package builder.http

import sttp.model.Part
import sttp.tapir.*

/** The multipart form of an upload: only the `datei` part is decoded, other parts are skipped. */
final case class UploadForm(datei: Option[Part[Array[Byte]]])

/** Multipart codecs for [[UploadForm]]. */
object UploadForm:
  /** Tapir's raw multipart codec, restricted to the `datei` part (no default part type, so other
    * parts are not read into memory). Hand-written: the derived codec warns under -Werror.
    */
  given MultipartCodec[UploadForm] =
    MultipartCodec(
      RawBodyType.MultipartBody(Map("datei" -> RawBodyType.ByteArrayBody), None),
      MultipartCodec.Default.codec.map(parts => UploadForm(parts.find(_.name == "datei")))(form =>
        form.datei.toList
      )
    )
