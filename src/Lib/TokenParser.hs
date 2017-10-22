{-# LANGUAGE DeriveGeneric #-}

module TokenParser
 ( FireBaseAuthData (..)
 , AuthData (..)
 , TokenParser
 , parseToken
-- , extractToken
 , initTokenParser
 ) where

import Protolude
import qualified Network.Wreq as NW
import GHC.Base (String)
-- Operators such as (&) and (.~).
import Control.Lens
-- Conversion of Haskell values to JSON.
import qualified Data.Aeson as DA
import qualified Data.Text.Encoding as DTE
import Data.PEM (pemParseBS, pemContent)
import qualified Data.X509 as X509
import qualified Crypto.PubKey.RSA as RSA
import qualified Jose.Jws as JJ
import Jose.Jwt
import qualified Data.ByteString.Lazy as BStrL
import Control.Concurrent.STM.TVar

gCertURL :: String
gCertURL="https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"

type TokenParser = TVar (Maybe [RSA.PublicKey])

initTokenParser :: IO TokenParser
initTokenParser = atomically $ newTVar Nothing

getCerificates :: IO (Maybe [Text])
getCerificates = do
  resp <- NW.get gCertURL
  let respPayload = resp ^? NW.responseBody
  let jsonVal :: Maybe DA.Value
      jsonVal = join $ DA.decode <$> respPayload
  return $ join $ getCertFromJson <$> jsonVal

getCertFromJson :: DA.Value -> Maybe [Text]
getCertFromJson jsonVal =
    case jsonVal of
      (DA.Object obj) -> Just $ catMaybes (fmap jsonStr (toList obj))
      _ -> Nothing
  where
    jsonStr val = case val of
      (DA.String txt) -> Just txt
      _ -> Nothing

--decodeCert  txt =
  --let content = DTE.encodeUtf8 txt
  --in
  -- either error
  --(map (X509.decodeSignedObject . pemContent)) (T.unpack $ pemParseBS txt)

--content :: ByteString
--content = ""
pemCnt :: ByteString -> Either String [ByteString]
pemCnt content = fmap (fmap pemContent) (pemParseBS content)

decodeOneCert :: ByteString -> Either String (X509.SignedExact X509.Certificate)
decodeOneCert =  X509.decodeSignedObject

decodedCert :: ByteString -> Either String [Either String (X509.SignedExact X509.Certificate)]
decodedCert content = fmap (fmap decodeOneCert) (pemCnt content)

decodedCertFromText :: Text -> Either String [Either String (X509.SignedExact X509.Certificate)]
decodedCertFromText txt =
  decodedCert (DTE.encodeUtf8 txt)

decodeGoogleCertificates :: IO (Maybe [Either String [Either String (X509.SignedExact X509.Certificate)]])
decodeGoogleCertificates = do
  certTxts <- getCerificates
  return $ fmap (fmap decodedCertFromText) certTxts

decodeGoogleCertificates' :: IO (Maybe [X509.SignedExact X509.Certificate])
decodeGoogleCertificates' =
  fmap (fmap (rights . concat . rights)) decodeGoogleCertificates

googlePublicKeys :: IO (Maybe [RSA.PublicKey])
googlePublicKeys = fmap (fmap (catMaybes . fmap getPublicKey)) decodeGoogleCertificates'

getPublicKey :: X509.SignedCertificate -> Maybe RSA.PublicKey
getPublicKey signedCert =
    case X509.certPubKey cert of
      X509.PubKeyRSA pubkey -> Just pubkey --Just $ RSA.public_n pubkey
      _ -> Nothing
  where
    signed  = X509.getSigned signedCert
    cert    = X509.signedObject signed


decodeTokenUsingKeys :: ByteString -> [RSA.PublicKey] -> Maybe Jws
decodeTokenUsingKeys token publicKeys =
  let decodeRes ::  [Either JwtError Jws]
      decodeRes = (\key-> JJ.rsaDecode key token) <$> publicKeys
  in case rights decodeRes of
    (jws:_) -> Just jws
    [] -> Nothing

decodeGoogleToken :: ByteString -> TokenParser -> IO (Maybe Jws)
decodeGoogleToken token tp = do
  keysMaybe <- atomically $ readTVar tp
  let firstAttemptMaybe = keysMaybe >>= decodeTokenUsingKeys token
  case firstAttemptMaybe of
    Just jws -> return (Just jws)
    _ -> do
        newKeys <- googlePublicKeys
        join <$> mapM (\keys ->  do
          atomically $ writeTVar tp (Just keys) --refresh keys
          return $  decodeTokenUsingKeys token keys) newKeys

extractToken :: Text -> TokenParser -> IO (Maybe Jws)
extractToken tokenText = decodeGoogleToken (DTE.encodeUtf8 tokenText) -- tp

data FireBaseAuthData = FireBaseAuthData
  { identities :: Maybe DA.Value
  , sign_in_provider :: Text
  } deriving (Show, Eq, Generic)

instance DA.FromJSON FireBaseAuthData
instance DA.ToJSON FireBaseAuthData

data AuthData = AuthData
  { iss::Text
  , name :: Maybe Text
  , picture :: Maybe Text
  , aud :: Text
  , user_id :: Text
  , iat :: Int
  , exp ::Int
  , email :: Maybe Text -- missing for twitter
  , email_verified :: Maybe Bool
  , firebase :: Maybe FireBaseAuthData
  } deriving (Show, Eq, Generic)

instance DA.FromJSON AuthData
instance DA.ToJSON AuthData

parseToken :: Text -> TokenParser -> IO (Maybe AuthData)
parseToken tokenText tp = do
  jwsMaybe <- extractToken tokenText tp
  return $ do
    jwsPayload <- snd <$> jwsMaybe
    let jwsPayloadL = BStrL.fromStrict jwsPayload
     -- DA.eitherDecode jwsPayloadL
    DA.decode jwsPayloadL
