module CommentDecoder where

import CommentType (Comment)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Generic (class Generic)

derive instance genericComment :: Generic Comment

instance decodeJsonComment :: DecodeJson Comment where
  decodeJson = gDecodeJson
