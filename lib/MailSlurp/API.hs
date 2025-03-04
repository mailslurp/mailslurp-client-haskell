{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module MailSlurp.API
  -- * Client and Server
  ( Config(..)
  , MailSlurpBackend(..)
  , createMailSlurpClient
  , runMailSlurpServer
  , runMailSlurpMiddlewareServer
  , runMailSlurpClient
  , runMailSlurpClientWithManager
  , callMailSlurp
  , MailSlurpClient
  , MailSlurpClientError(..)
  -- ** Servant
  , MailSlurpAPI
  ) where

import           MailSlurp.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for MailSlurp.
type MailSlurpAPI
    =    "aliases" :> ReqBody '[JSON] CreateAliasOptions :> Verb 'POST 200 '[JSON] AliasDto -- 'createAlias' route
    :<|> "aliases" :> Capture "aliasId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteAlias' route
    :<|> "aliases" :> Capture "aliasId" UUID :> Verb 'GET 200 '[JSON] AliasDto -- 'getAlias' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getAliasEmails' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "threads" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageAliasThreadProjection -- 'getAliasThreads' route
    :<|> "aliases" :> QueryParam "search" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageAlias -- 'getAliases' route
    :<|> "aliases" :> "threads" :> Capture "threadId" UUID :> Verb 'GET 200 '[JSON] AliasThreadProjection -- 'getThread' route
    :<|> "aliases" :> "threads" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageAliasThreadProjection -- 'getThreadsPaginated' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToAliasEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> ReqBody '[JSON] UpdateAliasOptions :> Verb 'PUT 200 '[JSON] AliasDto -- 'updateAlias' route
    :<|> "attachments" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllAttachments' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentAsBase64Encoded' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "bytes" :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachmentAsBytes' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'GET 200 '[JSON] AttachmentEntityDto -- 'getAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentInfo' route
    :<|> "attachments" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "fileNameFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PageAttachmentEntity -- 'getAttachments' route
    :<|> "attachments" :> ReqBody '[JSON] UploadAttachmentOptions :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachment' route
    :<|> "attachments" :> "bytes" :> QueryParam "contentType" Text :> QueryParam "contentId" Text :> QueryParam "filename" Text :> QueryParam "fileSize" Integer :> Header "contentType" Text :> Header "filename" Text :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachmentBytes' route
    :<|> "attachments" :> "multipart" :> QueryParam "contentId" Text :> QueryParam "contentType" Text :> QueryParam "filename" Text :> QueryParam "contentTypeHeader" Text :> QueryParam "x-filename" Text :> QueryParam "x-filename-raw" Text :> QueryParam "x-filesize" Integer :> ReqBody '[JSON] InlineObject :> Verb 'POST 200 '[JSON] [Text] -- 'uploadMultipartForm' route
    :<|> "bounce" :> "filter-recipients" :> ReqBody '[JSON] FilterBouncedRecipientsOptions :> Verb 'POST 200 '[JSON] FilterBouncedRecipientsResult -- 'filterBouncedRecipient' route
    :<|> "bounce" :> "account-block" :> Verb 'GET 200 '[JSON] AccountBounceBlockDto -- 'getAccountBounceBlockStatus' route
    :<|> "bounce" :> "emails" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedEmailDto -- 'getBouncedEmail' route
    :<|> "bounce" :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedEmail -- 'getBouncedEmails' route
    :<|> "bounce" :> "recipients" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedRecipientDto -- 'getBouncedRecipient' route
    :<|> "bounce" :> "recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedRecipients -- 'getBouncedRecipients' route
    :<|> "bounce" :> "complaints" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] Complaint -- 'getComplaint' route
    :<|> "bounce" :> "complaints" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageComplaint -- 'getComplaints' route
    :<|> "bounce" :> "list-unsubscribe-recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageListUnsubscribeRecipients -- 'getListUnsubscribeRecipients' route
    :<|> "bulk" :> "inboxes" :> QueryParam "count" Int :> Verb 'POST 200 '[JSON] [InboxDto] -- 'bulkCreateInboxes' route
    :<|> "bulk" :> "inboxes" :> ReqBody '[JSON] [UUID] :> Verb 'DELETE 200 '[JSON] () -- 'bulkDeleteInboxes' route
    :<|> "bulk" :> "send" :> ReqBody '[JSON] BulkSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'bulkSendEmails' route
    :<|> "newEmailAddress" :> QueryParam "allowTeamAccess" Bool :> QueryParam "useDomainPool" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "emailAddress" Text :> QueryParam "inboxType" Text :> QueryParam "description" Text :> QueryParam "name" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "favourite" Bool :> QueryParam "virtualInbox" Bool :> QueryParam "useShortAddress" Bool :> QueryParam "domainName" Text :> QueryParam "domainId" UUID :> QueryParam "prefix" Text :> Verb 'POST 200 '[JSON] InboxDto -- 'createNewEmailAddress' route
    :<|> "createInbox" :> QueryParam "allowTeamAccess" Bool :> QueryParam "useDomainPool" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "emailAddress" Text :> QueryParam "inboxType" Text :> QueryParam "description" Text :> QueryParam "name" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "favourite" Bool :> QueryParam "virtualInbox" Bool :> QueryParam "useShortAddress" Bool :> QueryParam "domainName" Text :> QueryParam "domainId" UUID :> QueryParam "prefix" Text :> Verb 'POST 200 '[JSON] InboxDto -- 'createRandomInbox' route
    :<|> "deleteEmailAddress" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteEmailAddress' route
    :<|> "emptyInbox" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'emptyInbox' route
    :<|> "sendEmailQuery" :> QueryParam "senderId" UUID :> QueryParam "to" Text :> QueryParam "body" Text :> QueryParam "subject" Text :> Verb 'POST 200 '[JSON] () -- 'sendEmailQuery' route
    :<|> "sendEmail" :> ReqBody '[JSON] SimpleSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailSimple' route
    :<|> "connectors" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateConnectorOptions :> Verb 'POST 200 '[JSON] ConnectorDto -- 'createConnector' route
    :<|> "connectors" :> Capture "id" UUID :> "imap" :> ReqBody '[JSON] CreateConnectorImapConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorImapConnectionDto -- 'createConnectorImapConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "smtp" :> ReqBody '[JSON] CreateConnectorSmtpConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorSmtpConnectionDto -- 'createConnectorSmtpConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "sync-settings" :> ReqBody '[JSON] CreateConnectorSyncSettingsOptions :> Verb 'POST 200 '[JSON] ConnectorSyncSettingsDto -- 'createConnectorSyncSettings' route
    :<|> "connectors" :> "withOptions" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateConnectorWithOptions :> Verb 'POST 200 '[JSON] ConnectorDto -- 'createConnectorWithOptions' route
    :<|> "connectors" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllConnector' route
    :<|> "connectors" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteConnector' route
    :<|> "connectors" :> Capture "id" UUID :> "imap" :> Verb 'DELETE 200 '[JSON] () -- 'deleteConnectorImapConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "smtp" :> Verb 'DELETE 200 '[JSON] () -- 'deleteConnectorSmtpConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "sync-settings" :> Verb 'DELETE 200 '[JSON] () -- 'deleteConnectorSyncSettings' route
    :<|> "connectors" :> "events" :> QueryParam "id" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> Verb 'GET 200 '[JSON] PageConnectorEvents -- 'getAllConnectorEvents' route
    :<|> "connectors" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] ConnectorDto -- 'getConnector' route
    :<|> "connectors" :> "by-email-address" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] OptionalConnectorDto -- 'getConnectorByEmailAddress' route
    :<|> "connectors" :> "by-inbox-id" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] OptionalConnectorDto -- 'getConnectorByInboxId' route
    :<|> "connectors" :> "by-name" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] OptionalConnectorDto -- 'getConnectorByName' route
    :<|> "connectors" :> "events" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] ConnectorEventDto -- 'getConnectorEvent' route
    :<|> "connectors" :> Capture "id" UUID :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> Verb 'GET 200 '[JSON] PageConnectorEvents -- 'getConnectorEvents' route
    :<|> "connectors" :> Capture "id" UUID :> "imap" :> Verb 'GET 200 '[JSON] OptionalConnectorImapConnectionDto -- 'getConnectorImapConnection' route
    :<|> "connectors" :> "provider-settings" :> Verb 'GET 200 '[JSON] ConnectorProviderSettingsDto -- 'getConnectorProviderSettings' route
    :<|> "connectors" :> Capture "id" UUID :> "smtp" :> Verb 'GET 200 '[JSON] OptionalConnectorSmtpConnectionDto -- 'getConnectorSmtpConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "sync-settings" :> Verb 'GET 200 '[JSON] OptionalConnectorSyncSettingsDto -- 'getConnectorSyncSettings' route
    :<|> "connectors" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageConnector -- 'getConnectors' route
    :<|> "connectors" :> Capture "id" UUID :> "send" :> QueryParam "useFallback" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendEmailFromConnector' route
    :<|> "connectors" :> Capture "id" UUID :> "sync" :> QueryParam "since" UTCTime :> QueryParam "folder" Text :> QueryParam "logging" Bool :> Verb 'POST 200 '[JSON] ConnectorSyncRequestResult -- 'syncConnector' route
    :<|> "connectors" :> Capture "id" UUID :> "imap" :> "test" :> ReqBody '[JSON] CreateConnectorImapConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorImapConnectionTestResult -- 'testConnectorImapConnection' route
    :<|> "connectors" :> "connections" :> "imap" :> "test" :> ReqBody '[JSON] CreateConnectorImapConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorImapConnectionTestResult -- 'testConnectorImapConnectionOptions' route
    :<|> "connectors" :> Capture "id" UUID :> "smtp" :> "test" :> ReqBody '[JSON] CreateConnectorSmtpConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorSmtpConnectionTestResult -- 'testConnectorSmtpConnection' route
    :<|> "connectors" :> "connections" :> "smtp" :> "test" :> ReqBody '[JSON] CreateConnectorSmtpConnectionOptions :> Verb 'POST 200 '[JSON] ConnectorSmtpConnectionTestResult -- 'testConnectorSmtpConnectionOptions' route
    :<|> "connectors" :> Capture "id" UUID :> ReqBody '[JSON] CreateConnectorOptions :> Verb 'PUT 200 '[JSON] ConnectorDto -- 'updateConnector' route
    :<|> "connectors" :> Capture "id" UUID :> "imap" :> ReqBody '[JSON] CreateConnectorImapConnectionOptions :> Verb 'PATCH 200 '[JSON] ConnectorImapConnectionDto -- 'updateConnectorImapConnection' route
    :<|> "connectors" :> Capture "id" UUID :> "smtp" :> ReqBody '[JSON] CreateConnectorSmtpConnectionOptions :> Verb 'PATCH 200 '[JSON] ConnectorSmtpConnectionDto -- 'updateConnectorSmtpConnection' route
    :<|> "consent" :> "opt-in" :> "sending-consent" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] OptInSendingConsentDto -- 'checkSendingConsentForEmailAddress' route
    :<|> "consent" :> "opt-in" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageOptInIdentityProjection -- 'getOptInIdentities' route
    :<|> "consent" :> "opt-in" :> QueryParam "emailAddress" Text :> Verb 'DELETE 200 '[JSON] OptInSendingConsentDto -- 'revokeOptInConsentForEmailAddress' route
    :<|> "consent" :> "opt-in" :> "send" :> ReqBody '[JSON] OptInConsentOptions :> Verb 'POST 200 '[JSON] OptInConsentSendResult -- 'sendOptInConsentForEmailAddress' route
    :<|> "contacts" :> ReqBody '[JSON] CreateContactOptions :> Verb 'POST 200 '[JSON] ContactDto -- 'createContact' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteContact' route
    :<|> "contacts" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageContactProjection -- 'getAllContacts' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'GET 200 '[JSON] ContactDto -- 'getContact' route
    :<|> "contacts" :> Capture "contactId" UUID :> "download" :> Verb 'GET 200 '[JSON] () -- 'getContactVCard' route
    :<|> "contacts" :> Verb 'GET 200 '[JSON] [ContactProjection] -- 'getContacts' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'POST 200 '[JSON] DomainDto -- 'addDomainWildcardCatchAll' route
    :<|> "domains" :> ReqBody '[JSON] CreateDomainOptions :> Verb 'POST 200 '[JSON] DomainDto -- 'createDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] [Text] -- 'deleteDomain' route
    :<|> "domains" :> "available-domains" :> QueryParam "inboxType" Text :> Verb 'GET 200 '[JSON] DomainGroupsDto -- 'getAvailableDomains' route
    :<|> "domains" :> Capture "id" UUID :> QueryParam "checkForErrors" Bool :> Verb 'GET 200 '[JSON] DomainDto -- 'getDomain' route
    :<|> "domains" :> "issues" :> Verb 'GET 200 '[JSON] DomainIssuesDto -- 'getDomainIssues' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'GET 200 '[JSON] InboxDto -- 'getDomainWildcardCatchAllInbox' route
    :<|> "domains" :> Verb 'GET 200 '[JSON] [DomainPreview] -- 'getDomains' route
    :<|> "domains" :> "mailslurp-domains" :> QueryParam "inboxType" Text :> Verb 'GET 200 '[JSON] DomainGroupsDto -- 'getMailSlurpDomains' route
    :<|> "domains" :> Capture "id" UUID :> ReqBody '[JSON] UpdateDomainOptions :> Verb 'PUT 200 '[JSON] DomainDto -- 'updateDomain' route
    :<|> "emails" :> Capture "emailId" UUID :> "imap-flag-operation" :> ReqBody '[JSON] ImapFlagOperationOptions :> Verb 'POST 200 '[JSON] EmailPreview -- 'applyImapFlagOperation' route
    :<|> "emails" :> "can-send" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] CanSendEmailResults -- 'canSend' route
    :<|> "emails" :> Capture "emailId" UUID :> "check-email-body" :> Verb 'POST 200 '[JSON] CheckEmailBodyResults -- 'checkEmailBody' route
    :<|> "emails" :> Capture "emailId" UUID :> "check-email-body-feature-support" :> Verb 'POST 200 '[JSON] CheckEmailBodyFeatureSupportResults -- 'checkEmailBodyFeatureSupport' route
    :<|> "emails" :> "check-email-client-support" :> ReqBody '[JSON] CheckEmailClientSupportOptions :> Verb 'POST 200 '[JSON] CheckEmailClientSupportResults -- 'checkEmailClientSupport' route
    :<|> "emails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllEmails' route
    :<|> "emails" :> Capture "emailId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> QueryParam "apiKey" Text :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachment' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentBase64' route
    :<|> "emails" :> Capture "emailId" UUID :> "body" :> Verb 'GET 200 '[JSON] Text -- 'downloadBody' route
    :<|> "emails" :> Capture "emailId" UUID :> "body-bytes" :> Verb 'GET 200 '[JSON] Text -- 'downloadBodyBytes' route
    :<|> "emails" :> Capture "emailId" UUID :> "forward" :> ReqBody '[JSON] ForwardEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'forwardEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentMetaData' route
    :<|> "emails" :> Capture "emailId" UUID :> Verb 'GET 200 '[JSON] Email -- 'getEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Verb 'GET 200 '[JSON] [AttachmentMetaData] -- 'getEmailAttachments' route
    :<|> "emails" :> Capture "emailId" UUID :> "contentMatch" :> ReqBody '[JSON] ContentMatchOptions :> Verb 'POST 200 '[JSON] EmailContentMatchResult -- 'getEmailContentMatch' route
    :<|> "emails" :> Capture "emailId" UUID :> "contentPart" :> QueryParam "contentType" Text :> QueryParam "strict" Bool :> QueryParam "index" Int :> Verb 'GET 200 '[JSON] EmailContentPartResult -- 'getEmailContentPart' route
    :<|> "emails" :> Capture "emailId" UUID :> "contentPart" :> "raw" :> QueryParam "contentType" Text :> QueryParam "strict" Bool :> QueryParam "index" Int :> Verb 'GET 200 '[JSON] Text -- 'getEmailContentPartContent' route
    :<|> "emails" :> "emails" :> "count" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] CountDto -- 'getEmailCount' route
    :<|> "emails" :> Capture "emailId" UUID :> "html" :> QueryParam "replaceCidImages" Bool :> Verb 'GET 200 '[JSON] Text -- 'getEmailHTML' route
    :<|> "emails" :> Capture "emailId" UUID :> "html" :> "json" :> QueryParam "replaceCidImages" Bool :> Verb 'GET 200 '[JSON] EmailHtmlDto -- 'getEmailHTMLJson' route
    :<|> "emails" :> Capture "emailId" UUID :> "htmlQuery" :> QueryParam "htmlSelector" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailHTMLQuery' route
    :<|> "emails" :> Capture "emailId" UUID :> "links" :> QueryParam "selector" Text :> Verb 'GET 200 '[JSON] EmailLinksResult -- 'getEmailLinks' route
    :<|> "emails" :> Capture "emailId" UUID :> "urls" :> Verb 'GET 200 '[JSON] EmailPreviewUrls -- 'getEmailPreviewURLs' route
    :<|> "emails" :> Capture "emailId" UUID :> "screenshot" :> "base64" :> ReqBody '[JSON] GetEmailScreenshotOptions :> Verb 'POST 200 '[JSON] EmailScreenshotResult -- 'getEmailScreenshotAsBase64' route
    :<|> "emails" :> Capture "emailId" UUID :> "screenshot" :> "binary" :> ReqBody '[JSON] GetEmailScreenshotOptions :> Verb 'POST 200 '[JSON] () -- 'getEmailScreenshotAsBinary' route
    :<|> "emails" :> Capture "emailId" UUID :> "summary" :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] EmailPreview -- 'getEmailSummary' route
    :<|> "emails" :> Capture "emailId" UUID :> "textLines" :> QueryParam "decodeHtmlEntities" Bool :> QueryParam "lineSeparator" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailTextLines' route
    :<|> "emails" :> "threads" :> Capture "threadId" UUID :> Verb 'GET 200 '[JSON] EmailThreadDto -- 'getEmailThread' route
    :<|> "emails" :> "threads" :> Capture "threadId" UUID :> "items" :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] EmailThreadItemsDto -- 'getEmailThreadItems' route
    :<|> "emails" :> "threads" :> QueryParam "htmlSelector" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailThreadProjection -- 'getEmailThreads' route
    :<|> "emails" :> "offset-paginated" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "favourited" Bool :> QueryParam "syncConnectors" Bool :> QueryParam "plusAddressId" UUID :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getEmailsOffsetPaginated' route
    :<|> "emails" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "syncConnectors" Bool :> QueryParam "plusAddressId" UUID :> QueryParam "favourited" Bool :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getEmailsPaginated' route
    :<|> "emails" :> "gravatarFor" :> QueryParam "emailAddress" Text :> QueryParam "size" Text :> Verb 'GET 200 '[JSON] GravatarUrl -- 'getGravatarUrlForEmailAddress' route
    :<|> "emails" :> "latest" :> QueryParam "inboxIds" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmail' route
    :<|> "emails" :> "latestIn" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmailInInbox1' route
    :<|> "emails" :> "organization" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "syncConnectors" Bool :> QueryParam "favourited" Bool :> QueryParam "plusAddressId" UUID :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getOrganizationEmailsPaginated' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> Verb 'GET 200 '[JSON] () -- 'getRawEmailContents' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> "json" :> Verb 'GET 200 '[JSON] RawEmailJson -- 'getRawEmailJson' route
    :<|> "emails" :> "unreadCount" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] UnreadCount -- 'getUnreadEmailCount' route
    :<|> "emails" :> "read" :> QueryParam "read" Bool :> QueryParam "inboxId" UUID :> Verb 'PATCH 200 '[JSON] () -- 'markAllAsRead' route
    :<|> "emails" :> Capture "emailId" UUID :> "read" :> QueryParam "read" Bool :> Verb 'PATCH 200 '[JSON] EmailPreview -- 'markAsRead' route
    :<|> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToEmail' route
    :<|> "emails" :> "search" :> QueryParam "syncConnectors" Bool :> QueryParam "favourited" Bool :> QueryParam "plusAddressId" UUID :> ReqBody '[JSON] SearchEmailsOptions :> Verb 'POST 200 '[JSON] PageEmailProjection -- 'searchEmails' route
    :<|> "emails" :> QueryParam "inboxId" UUID :> QueryParam "useDomainPool" Bool :> QueryParam "virtualSend" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailSourceOptional' route
    :<|> "emails" :> Capture "emailId" UUID :> "favourite" :> QueryParam "favourited" Bool :> Verb 'PUT 200 '[JSON] () -- 'setEmailFavourited' route
    :<|> "emails" :> Capture "emailId" UUID :> "validate" :> Verb 'POST 200 '[JSON] ValidationDto -- 'validateEmail' route
    :<|> "email-verification" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllValidationRequests' route
    :<|> "email-verification" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteValidationRequest' route
    :<|> "email-verification" :> "validation-requests" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "isValid" Bool :> Verb 'GET 200 '[JSON] PageEmailValidationRequest -- 'getValidationRequests' route
    :<|> "email-verification" :> "email-address-list" :> ReqBody '[JSON] ValidateEmailAddressListOptions :> Verb 'POST 200 '[JSON] ValidateEmailAddressListResult -- 'validateEmailAddressList' route
    :<|> "expired" :> "defaults" :> Verb 'GET 200 '[JSON] ExpirationDefaults -- 'getExpirationDefaults' route
    :<|> "expired" :> "inbox" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxByInboxId' route
    :<|> "expired" :> Capture "expiredId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxRecord' route
    :<|> "expired" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PageExpiredInboxRecordProjection -- 'getExpiredInboxes' route
    :<|> "export" :> QueryParam "exportType" Text :> QueryParam "apiKey" Text :> QueryParam "outputFormat" Text :> QueryParam "filter" Text :> QueryParam "listSeparatorToken" Text :> QueryParam "excludePreviouslyExported" Bool :> QueryParam "createdEarliestTime" UTCTime :> QueryParam "createdOldestTime" UTCTime :> Verb 'GET 200 '[JSON] Text -- 'exportEntities' route
    :<|> "export" :> QueryParam "exportType" Text :> QueryParam "apiKey" Text :> ReqBody '[JSON] ExportOptions :> Verb 'POST 200 '[JSON] ExportLink -- 'getExportLink' route
    :<|> "forms" :> QueryParam "_to" Text :> QueryParam "_subject" Text :> QueryParam "_redirectTo" Text :> QueryParam "_emailAddress" Text :> QueryParam "_successMessage" Text :> QueryParam "_spamCheck" Text :> QueryParam "otherParameters" Text :> Verb 'POST 200 '[JSON] Text -- 'submitForm' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> ReqBody '[JSON] UpdateGroupContacts :> Verb 'PUT 200 '[JSON] GroupContactsDto -- 'addContactsToGroup' route
    :<|> "groups" :> ReqBody '[JSON] CreateGroupOptions :> Verb 'POST 200 '[JSON] GroupDto -- 'createGroup' route
    :<|> "groups" :> Capture "groupId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteGroup' route
    :<|> "groups" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageGroupProjection -- 'getAllGroups' route
    :<|> "groups" :> Capture "groupId" UUID :> Verb 'GET 200 '[JSON] GroupDto -- 'getGroup' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> Verb 'GET 200 '[JSON] GroupContactsDto -- 'getGroupWithContacts' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts-paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageContactProjection -- 'getGroupWithContactsPaginated' route
    :<|> "groups" :> Verb 'GET 200 '[JSON] [GroupProjection] -- 'getGroups' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> ReqBody '[JSON] UpdateGroupContacts :> Verb 'DELETE 200 '[JSON] GroupContactsDto -- 'removeContactsFromGroup' route
    :<|> "guest-portal" :> ReqBody '[JSON] CreatePortalOptions :> Verb 'POST 200 '[JSON] GuestPortalDto -- 'createGuestPortal' route
    :<|> "guest-portal" :> Capture "portalId" UUID :> "user" :> ReqBody '[JSON] CreatePortalUserOptions :> Verb 'POST 200 '[JSON] GuestPortalUserCreateDto -- 'createGuestPortalUser' route
    :<|> "guest-portal" :> "user" :> QueryParam "portalId" UUID :> QueryParam "search" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageGuestPortalUsers -- 'getAllGuestPortalUsers' route
    :<|> "guest-portal" :> Capture "portalId" UUID :> Verb 'GET 200 '[JSON] GuestPortalDto -- 'getGuestPortal' route
    :<|> "guest-portal" :> Capture "portalId" UUID :> "user" :> Capture "guestId" UUID :> Verb 'GET 200 '[JSON] GuestPortalUserDto -- 'getGuestPortalUser' route
    :<|> "guest-portal" :> "user" :> Capture "guestId" UUID :> Verb 'GET 200 '[JSON] GuestPortalUserDto -- 'getGuestPortalUserById' route
    :<|> "guest-portal" :> Capture "portalId" UUID :> "user" :> QueryParam "search" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageGuestPortalUsers -- 'getGuestPortalUsers' route
    :<|> "guest-portal" :> Verb 'GET 200 '[JSON] [GuestPortalDto] -- 'getGuestPortals' route
    :<|> "imap" :> "server" :> "fetch" :> QueryParam "inboxId" UUID :> QueryParam "seqNum" Integer :> Verb 'POST 200 '[JSON] ImapServerFetchResult -- 'imapServerFetch' route
    :<|> "imap" :> "server" :> "get" :> QueryParam "emailId" UUID :> QueryParam "inboxId" UUID :> Verb 'POST 200 '[JSON] ImapServerGetResult -- 'imapServerGet' route
    :<|> "imap" :> "server" :> "list" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] ImapServerListOptions :> Verb 'POST 200 '[JSON] ImapServerListResult -- 'imapServerList' route
    :<|> "imap" :> "server" :> "mailbox" :> QueryParam "name" Text :> Verb 'POST 200 '[JSON] ImapServerMailboxResult -- 'imapServerMailbox' route
    :<|> "imap" :> "server" :> "search" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] ImapServerSearchOptions :> Verb 'POST 200 '[JSON] ImapServerSearchResult -- 'imapServerSearch' route
    :<|> "imap" :> "server" :> "status" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] ImapServerStatusOptions :> Verb 'POST 200 '[JSON] ImapServerStatusResult -- 'imapServerStatus' route
    :<|> "imap" :> "server" :> "update-flags" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] ImapUpdateFlagsOptions :> Verb 'POST 200 '[JSON] () -- 'imapServerUpdateFlags' route
    :<|> "inboxes" :> "scheduled-jobs" :> Capture "jobId" UUID :> Verb 'DELETE 200 '[JSON] ScheduledJobDto -- 'cancelScheduledJob' route
    :<|> "inboxes" :> QueryParam "emailAddress" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "name" Text :> QueryParam "description" Text :> QueryParam "useDomainPool" Bool :> QueryParam "favourite" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "allowTeamAccess" Bool :> QueryParam "inboxType" Text :> QueryParam "virtualInbox" Bool :> QueryParam "useShortAddress" Bool :> QueryParam "domainId" UUID :> QueryParam "domainName" Text :> QueryParam "prefix" Text :> Verb 'POST 200 '[JSON] InboxDto -- 'createInbox' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> ReqBody '[JSON] CreateInboxRulesetOptions :> Verb 'POST 200 '[JSON] InboxRulesetDto -- 'createInboxRuleset' route
    :<|> "inboxes" :> "withDefaults" :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithDefaults' route
    :<|> "inboxes" :> "withOptions" :> ReqBody '[JSON] CreateInboxDto :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithOptions' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "deleteAllInboxEmails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxEmails' route
    :<|> "inboxes" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxes' route
    :<|> "inboxes" :> "by-description" :> QueryParam "description" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByDescription' route
    :<|> "inboxes" :> "by-name" :> QueryParam "name" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByName' route
    :<|> "inboxes" :> "by-tag" :> QueryParam "tag" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByTag' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInbox' route
    :<|> "inboxes" :> "exists" :> QueryParam "emailAddress" Text :> QueryParam "allowCatchAll" Bool :> Verb 'GET 200 '[JSON] InboxExistsDto -- 'doesInboxExist' route
    :<|> "inboxes" :> "expired" :> QueryParam "before" UTCTime :> Verb 'DELETE 200 '[JSON] FlushExpiredInboxesResult -- 'flushExpired' route
    :<|> "inboxes" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "favourite" Bool :> QueryParam "search" Text :> QueryParam "tag" Text :> QueryParam "teamAccess" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxType" Text :> QueryParam "inboxFunction" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getAllInboxes' route
    :<|> "inboxes" :> "offset-paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "favourite" Bool :> QueryParam "search" Text :> QueryParam "tag" Text :> QueryParam "teamAccess" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxType" Text :> QueryParam "inboxFunction" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getAllInboxesOffsetPaginated' route
    :<|> "inboxes" :> "plus-addresses" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PagePlusAddressProjection -- 'getAllPlusAddresses' route
    :<|> "inboxes" :> "scheduled-jobs" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PageScheduledJobs -- 'getAllScheduledJobs' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "delivery-status" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageDeliveryStatus -- 'getDeliveryStatusesByInboxId' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> QueryParam "size" Int :> QueryParam "limit" Int :> QueryParam "sort" Text :> QueryParam "retryTimeout" Integer :> QueryParam "delayTimeout" Integer :> QueryParam "minCount" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "before" UTCTime :> QueryParam "since" UTCTime :> Verb 'GET 200 '[JSON] [EmailPreview] -- 'getEmails' route
    :<|> "inboxes" :> "imap-access" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] ImapAccessDetails -- 'getImapAccess' route
    :<|> "inboxes" :> "imap-smtp-access" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] ImapSmtpAccessDetails -- 'getImapSmtpAccess' route
    :<|> "inboxes" :> "imap-smtp-access" :> "env" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] Text -- 'getImapSmtpAccessEnv' route
    :<|> "inboxes" :> "imap-smtp-access" :> "servers" :> Verb 'GET 200 '[JSON] ImapSmtpAccessServers -- 'getImapSmtpAccessServers' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] InboxDto -- 'getInbox' route
    :<|> "inboxes" :> "byEmailAddress" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] InboxByEmailAddressResult -- 'getInboxByEmailAddress' route
    :<|> "inboxes" :> "byName" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] InboxByNameResult -- 'getInboxByName' route
    :<|> "inboxes" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getInboxCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getInboxEmailCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "syncConnectors" Bool :> Verb 'GET 200 '[JSON] PageEmailPreview -- 'getInboxEmailsPaginated' route
    :<|> "inboxes" :> "ids" :> Verb 'GET 200 '[JSON] InboxIdsResult -- 'getInboxIds' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "plus-addresses" :> Capture "plusAddressId" UUID :> Verb 'GET 200 '[JSON] PlusAddressDto -- 'getInboxPlusAddress' route
    :<|> "inboxes" :> "plus-addresses" :> Capture "plusAddressId" UUID :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PlusAddressDto -- 'getInboxPlusAddressById' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "plus-addresses" :> "emails" :> QueryParam "plusAddress" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailPreview -- 'getInboxPlusAddressEmails' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "plus-addresses" :> Capture "plusAddressId" UUID :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailPreview -- 'getInboxPlusAddressEmailsForPlusAddressId' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "plus-addresses" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PagePlusAddressProjection -- 'getInboxPlusAddresses' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "sent" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getInboxSentCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "sent" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getInboxSentEmails' route
    :<|> "inboxes" :> "tags" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> Verb 'GET 200 '[JSON] [Text] -- 'getInboxTags' route
    :<|> "inboxes" :> "tags" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> Verb 'GET 200 '[JSON] PageInboxTags -- 'getInboxTagsPaginated' route
    :<|> "inboxes" :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "excludeCatchAllInboxes" Bool :> QueryParam "before" UTCTime :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] [InboxDto] -- 'getInboxes' route
    :<|> "inboxes" :> "tags" :> "inboxes" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "tag" Text :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getInboxesByTag' route
    :<|> "inboxes" :> "getLatestEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeoutMillis" Integer :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmailInInbox' route
    :<|> "inboxes" :> "organization" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageOrganizationInboxProjection -- 'getOrganizationInboxes' route
    :<|> "inboxes" :> "outboxes" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getOutboxes' route
    :<|> "inboxes" :> "scheduled-jobs" :> Capture "jobId" UUID :> Verb 'GET 200 '[JSON] ScheduledJobDto -- 'getScheduledJob' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "scheduled-jobs" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageScheduledJobs -- 'getScheduledJobsByInboxId' route
    :<|> "inboxes" :> "smtp-access" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] SmtpAccessDetails -- 'getSmtpAccess' route
    :<|> "inboxes" :> "available" :> QueryParam "emailAddress" Text :> Verb 'POST 200 '[JSON] EmailAvailableResult -- 'isEmailAddressAvailable' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxRulesetDto -- 'listInboxRulesets' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "tracking-pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'listInboxTrackingPixels' route
    :<|> "inboxes" :> "search" :> ReqBody '[JSON] SearchInboxesOptions :> Verb 'POST 200 '[JSON] PageInboxProjection -- 'searchInboxes' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "confirm" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendEmailAndConfirm' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "with-queue" :> QueryParam "validateBeforeEnqueue" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailWithQueue' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "smtp-envelope" :> ReqBody '[JSON] SendSMTPEnvelopeOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendSmtpEnvelope' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "send-test-email" :> Verb 'POST 200 '[JSON] () -- 'sendTestEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "with-schedule" :> QueryParam "sendAtTimestamp" UTCTime :> QueryParam "sendAtNowPlusSeconds" Integer :> QueryParam "validateBeforeEnqueue" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] ScheduledJobDto -- 'sendWithSchedule' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "favourite" :> ReqBody '[JSON] SetInboxFavouritedOptions :> Verb 'PUT 200 '[JSON] InboxDto -- 'setInboxFavourited' route
    :<|> "inboxes" :> "imap-access" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] UpdateImapAccessOptions :> Verb 'PATCH 200 '[JSON] () -- 'updateImapAccess' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] UpdateInboxOptions :> Verb 'PATCH 200 '[JSON] InboxDto -- 'updateInbox' route
    :<|> "inboxes" :> "smtp-access" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] UpdateSmtpAccessOptions :> Verb 'PATCH 200 '[JSON] () -- 'updateSmtpAccess' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateInboxForwarderOptions :> Verb 'POST 200 '[JSON] InboxForwarderDto -- 'createNewInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxForwarder' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxForwarders' route
    :<|> "forwarders" :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "inboxId" UUID :> QueryParam "emailId" UUID :> QueryParam "sentId" UUID :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxForwarderEvents -- 'getAllInboxForwarderEvents' route
    :<|> "forwarders" :> "events" :> Capture "eventId" UUID :> Verb 'GET 200 '[JSON] InboxForwarderEventDto -- 'getForwarderEvent' route
    :<|> "forwarders" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] InboxForwarderDto -- 'getInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> "events" :> Capture "eventId" UUID :> Verb 'GET 200 '[JSON] InboxForwarderEventDto -- 'getInboxForwarderEvent' route
    :<|> "forwarders" :> Capture "id" UUID :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxForwarderEvents -- 'getInboxForwarderEvents' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxForwarderDto -- 'getInboxForwarders' route
    :<|> "forwarders" :> Capture "id" UUID :> "test" :> ReqBody '[JSON] InboxForwarderTestOptions :> Verb 'POST 200 '[JSON] InboxForwarderTestResult -- 'testInboxForwarder' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] InboxForwarderTestOptions :> Verb 'PUT 200 '[JSON] InboxForwarderTestResult -- 'testInboxForwardersForInbox' route
    :<|> "forwarders" :> ReqBody '[JSON] TestNewInboxForwarderOptions :> Verb 'PATCH 200 '[JSON] InboxForwarderTestResult -- 'testNewInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> ReqBody '[JSON] CreateInboxForwarderOptions :> Verb 'PUT 200 '[JSON] InboxForwarderDto -- 'updateInboxForwarder' route
    :<|> "repliers" :> ReqBody '[JSON] CreateInboxReplierOptions :> Verb 'POST 200 '[JSON] InboxReplierDto -- 'createNewInboxReplier' route
    :<|> "repliers" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxReplier' route
    :<|> "repliers" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxRepliers' route
    :<|> "repliers" :> "events" :> QueryParam "inboxReplierId" UUID :> QueryParam "inboxId" UUID :> QueryParam "emailId" UUID :> QueryParam "sentId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxReplierEvents -- 'getAllInboxReplierEvents' route
    :<|> "repliers" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] InboxReplierDto -- 'getInboxReplier' route
    :<|> "repliers" :> Capture "id" UUID :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxReplierEvents -- 'getInboxReplierEvents' route
    :<|> "repliers" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxReplierDto -- 'getInboxRepliers' route
    :<|> "repliers" :> Capture "id" UUID :> ReqBody '[JSON] UpdateInboxReplierOptions :> Verb 'PUT 200 '[JSON] InboxReplierDto -- 'updateInboxReplier' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateInboxRulesetOptions :> Verb 'POST 200 '[JSON] InboxRulesetDto -- 'createNewInboxRuleset' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxRulesets' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] InboxRulesetDto -- 'getInboxRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxRulesetDto -- 'getInboxRulesets' route
    :<|> "rulesets" :> Capture "id" UUID :> "test" :> ReqBody '[JSON] InboxRulesetTestOptions :> Verb 'POST 200 '[JSON] InboxRulesetTestResult -- 'testInboxRuleset' route
    :<|> "rulesets" :> "test-receiving" :> ReqBody '[JSON] TestInboxRulesetReceivingOptions :> Verb 'POST 200 '[JSON] TestInboxRulesetReceivingResult -- 'testInboxRulesetReceiving' route
    :<|> "rulesets" :> "test-sending" :> ReqBody '[JSON] TestInboxRulesetSendingOptions :> Verb 'POST 200 '[JSON] TestInboxRulesetSendingResult -- 'testInboxRulesetSending' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] InboxRulesetTestOptions :> Verb 'PUT 200 '[JSON] InboxRulesetTestResult -- 'testInboxRulesetsForInbox' route
    :<|> "rulesets" :> ReqBody '[JSON] TestNewInboxRulesetOptions :> Verb 'PATCH 200 '[JSON] InboxRulesetTestResult -- 'testNewInboxRuleset' route
    :<|> "mail-server" :> "describe" :> "domain" :> ReqBody '[JSON] DescribeDomainOptions :> Verb 'POST 200 '[JSON] DescribeMailServerDomainResult -- 'describeMailServerDomain' route
    :<|> "mail-server" :> "describe" :> "dns-lookup" :> ReqBody '[JSON] DNSLookupOptions :> Verb 'POST 200 '[JSON] DNSLookupResults -- 'getDnsLookup' route
    :<|> "mail-server" :> "describe" :> "dns-lookups" :> ReqBody '[JSON] DNSLookupsOptions :> Verb 'POST 200 '[JSON] DNSLookupResults -- 'getDnsLookups' route
    :<|> "mail-server" :> "describe" :> "ip-address" :> QueryParam "name" Text :> Verb 'POST 200 '[JSON] IPAddressResult -- 'getIpAddress' route
    :<|> "mail-server" :> "verify" :> "email-address" :> ReqBody '[JSON] VerifyEmailAddressOptions :> Verb 'POST 200 '[JSON] EmailVerificationResult -- 'verifyEmailAddress' route
    :<|> "missed-emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PageMissedEmailProjection -- 'getAllMissedEmails' route
    :<|> "missed-emails" :> "unknown" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] PageUnknownMissedEmailProjection -- 'getAllUnknownMissedEmails' route
    :<|> "missed-emails" :> Capture "missedEmailId" UUID :> Verb 'GET 200 '[JSON] MissedEmailDto -- 'getMissedEmail' route
    :<|> "missed-emails" :> "restore" :> Verb 'POST 200 '[JSON] () -- 'restoreMissedEmails' route
    :<|> "missed-emails" :> "waitForNthMissedEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "index" Int :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] MissedEmailDto -- 'waitForNthMissedEmail' route
    :<|> "phone" :> "emergency-addresses" :> ReqBody '[JSON] CreateEmergencyAddressOptions :> Verb 'POST 200 '[JSON] EmergencyAddress -- 'createEmergencyAddress' route
    :<|> "phone" :> "emergency-addresses" :> Capture "addressId" UUID :> Verb 'DELETE 200 '[JSON] EmptyResponseDto -- 'deleteEmergencyAddress' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deletePhoneNumber' route
    :<|> "phone" :> "consent" :> Verb 'GET 200 '[JSON] ConsentStatusDto -- 'getConsentStatus' route
    :<|> "phone" :> "emergency-addresses" :> Capture "addressId" UUID :> Verb 'GET 200 '[JSON] EmergencyAddress -- 'getEmergencyAddress' route
    :<|> "phone" :> "emergency-addresses" :> Verb 'GET 200 '[JSON] [EmergencyAddressDto] -- 'getEmergencyAddresses' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'getPhoneNumber' route
    :<|> "phone" :> "numbers" :> QueryParam "phoneCountry" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> QueryParam "favourite" Bool :> Verb 'GET 200 '[JSON] PagePhoneNumberProjection -- 'getPhoneNumbers' route
    :<|> "phone" :> "plans" :> Verb 'GET 200 '[JSON] [PhonePlanDto] -- 'getPhonePlans' route
    :<|> "phone" :> "plans" :> "availability" :> Verb 'GET 200 '[JSON] PhonePlanAvailability -- 'getPhonePlansAvailability' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms-sent" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageSentSmsProjection -- 'getSentSmsByPhoneNumber' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "favourite" Bool :> Verb 'GET 200 '[JSON] PageSmsProjection -- 'getSmsByPhoneNumber' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms" :> ReqBody '[JSON] SmsSendOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'sendSmsFromPhoneNumber' route
    :<|> "phone" :> "consent" :> QueryParam "agree" Bool :> Verb 'POST 200 '[JSON] ConsentStatusDto -- 'setConsentStatus' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "favourite" :> ReqBody '[JSON] SetPhoneFavouritedOptions :> Verb 'PUT 200 '[JSON] PhoneNumberDto -- 'setPhoneFavourited' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "test" :> ReqBody '[JSON] TestPhoneNumberOptions :> Header "x-test-id" Text :> Verb 'POST 200 '[JSON] () -- 'testPhoneNumberSendSms' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> ReqBody '[JSON] UpdatePhoneNumberOptions :> Verb 'PUT 200 '[JSON] PhoneNumberDto -- 'updatePhoneNumber' route
    :<|> "phone" :> "validate" :> ReqBody '[JSON] ValidatePhoneNumberOptions :> Verb 'POST 200 '[JSON] PhoneNumberValidationDto -- 'validatePhoneNumber' route
    :<|> "sent" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllSentEmails' route
    :<|> "sent" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSentEmail' route
    :<|> "sent" :> "tracking-pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getAllSentTrackingPixels' route
    :<|> "sent" :> Capture "emailId" UUID :> "raw" :> Verb 'GET 200 '[JSON] () -- 'getRawSentEmailContents' route
    :<|> "sent" :> Capture "emailId" UUID :> "raw" :> "json" :> Verb 'GET 200 '[JSON] RawEmailJson -- 'getRawSentEmailJson' route
    :<|> "sent" :> "delivery-status" :> Capture "deliveryId" UUID :> Verb 'GET 200 '[JSON] DeliveryStatusDto -- 'getSentDeliveryStatus' route
    :<|> "sent" :> "delivery-status" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageDeliveryStatus -- 'getSentDeliveryStatuses' route
    :<|> "sent" :> Capture "sentId" UUID :> "delivery-status" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageDeliveryStatus -- 'getSentDeliveryStatusesBySentId' route
    :<|> "sent" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] SentEmailDto -- 'getSentEmail' route
    :<|> "sent" :> Capture "id" UUID :> "html" :> Verb 'GET 200 '[JSON] Text -- 'getSentEmailHTMLContent' route
    :<|> "sent" :> Capture "id" UUID :> "urls" :> Verb 'GET 200 '[JSON] EmailPreviewUrls -- 'getSentEmailPreviewURLs' route
    :<|> "sent" :> Capture "id" UUID :> "tracking-pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getSentEmailTrackingPixels' route
    :<|> "sent" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getSentEmails' route
    :<|> "sent" :> "queue-results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSentEmailWithQueueProjection -- 'getSentEmailsWithQueueResults' route
    :<|> "sent" :> "organization" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getSentOrganizationEmails' route
    :<|> "sent" :> "delivery-status" :> "wait-for" :> QueryParam "sentId" UUID :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "index" Int :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] DeliveryStatusDto -- 'waitForDeliveryStatuses' route
    :<|> "sms" :> "sent" :> Capture "sentSmsId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSentSmsMessage' route
    :<|> "sms" :> "sent" :> QueryParam "phoneNumberId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSentSmsMessages' route
    :<|> "sms" :> Capture "smsId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSmsMessage' route
    :<|> "sms" :> QueryParam "phoneNumberId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSmsMessages' route
    :<|> "sms" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "favourite" Bool :> Verb 'GET 200 '[JSON] PageSmsProjection -- 'getAllSmsMessages' route
    :<|> "sms" :> Capture "smsId" UUID :> "reply" :> Verb 'GET 200 '[JSON] ReplyForSms -- 'getReplyForSmsMessage' route
    :<|> "sms" :> "sent" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getSentSmsCount' route
    :<|> "sms" :> "sent" :> Capture "sentSmsId" UUID :> Verb 'GET 200 '[JSON] SentSmsDto -- 'getSentSmsMessage' route
    :<|> "sms" :> "sent" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageSentSmsProjection -- 'getSentSmsMessagesPaginated' route
    :<|> "sms" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getSmsCount' route
    :<|> "sms" :> Capture "smsId" UUID :> Verb 'GET 200 '[JSON] SmsDto -- 'getSmsMessage' route
    :<|> "sms" :> "unreadCount" :> Verb 'GET 200 '[JSON] UnreadCount -- 'getUnreadSmsCount' route
    :<|> "sms" :> Capture "smsId" UUID :> "reply" :> ReqBody '[JSON] SmsReplyOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'replyToSmsMessage' route
    :<|> "sms" :> "send" :> QueryParam "fromPhoneNumber" Text :> QueryParam "fromPhoneId" UUID :> ReqBody '[JSON] SmsSendOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'sendSms' route
    :<|> "sms" :> Capture "smsId" UUID :> "favourite" :> QueryParam "favourited" Bool :> Verb 'PUT 200 '[JSON] SmsDto -- 'setSmsFavourited' route
    :<|> "sse" :> Header "x-api-key" Text :> Verb 'GET 200 '[JSON] [Text] -- 'streamEvents' route
    :<|> "templates" :> ReqBody '[JSON] CreateTemplateOptions :> Verb 'POST 200 '[JSON] TemplateDto -- 'createTemplate' route
    :<|> "templates" :> Capture "templateId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteTemplate' route
    :<|> "templates" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTemplateProjection -- 'getAllTemplates' route
    :<|> "templates" :> Capture "templateId" UUID :> Verb 'GET 200 '[JSON] TemplateDto -- 'getTemplate' route
    :<|> "templates" :> Capture "templateId" UUID :> "preview" :> "html" :> Verb 'GET 200 '[JSON] Text -- 'getTemplatePreviewHtml' route
    :<|> "templates" :> Capture "templateId" UUID :> "preview" :> "json" :> Verb 'GET 200 '[JSON] TemplatePreview -- 'getTemplatePreviewJson' route
    :<|> "templates" :> Verb 'GET 200 '[JSON] [TemplateProjection] -- 'getTemplates' route
    :<|> "templates" :> Capture "templateId" UUID :> ReqBody '[JSON] CreateTemplateOptions :> Verb 'PUT 200 '[JSON] TemplateDto -- 'updateTemplate' route
    :<|> "tools" :> "check-email-features-client-support" :> ReqBody '[JSON] CheckEmailFeaturesClientSupportOptions :> Verb 'POST 200 '[JSON] CheckEmailFeaturesClientSupportResults -- 'checkEmailFeaturesClientSupport' route
    :<|> "tools" :> "fake-email" :> Verb 'POST 200 '[JSON] NewFakeEmailAddressResult -- 'createNewFakeEmailAddress' route
    :<|> "tools" :> "fake-email" :> QueryParam "emailAddress" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteNewFakeEmailAddress' route
    :<|> "tools" :> "generate-bimi-record" :> ReqBody '[JSON] GenerateBimiRecordOptions :> Verb 'POST 200 '[JSON] GenerateBimiRecordResults -- 'generateBimiRecord' route
    :<|> "tools" :> "generate-dmarc-record" :> ReqBody '[JSON] GenerateDmarcRecordOptions :> Verb 'POST 200 '[JSON] GenerateDmarcRecordResults -- 'generateDmarcRecord' route
    :<|> "tools" :> "generate-mta-sts-record" :> ReqBody '[JSON] GenerateMtaStsRecordOptions :> Verb 'POST 200 '[JSON] GenerateMtaStsRecordResults -- 'generateMtaStsRecord' route
    :<|> "tools" :> "generate-tls-reporting-record" :> ReqBody '[JSON] GenerateTlsReportingRecordOptions :> Verb 'POST 200 '[JSON] GenerateTlsReportingRecordResults -- 'generateTlsReportingRecord' route
    :<|> "tools" :> "fake-email" :> "byEmailAddress" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] FakeEmailResult -- 'getFakeEmailByEmailAddress' route
    :<|> "tools" :> "fake-email" :> QueryParam "id" UUID :> Verb 'GET 200 '[JSON] FakeEmailResult -- 'getFakeEmailById' route
    :<|> "tools" :> "fake-email" :> "html" :> QueryParam "id" UUID :> Verb 'GET 200 '[JSON] Text -- 'getFakeEmailRaw' route
    :<|> "tools" :> "fake-emails" :> QueryParam "page" Int :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] [FakeEmailPreview] -- 'getFakeEmailsForAddress' route
    :<|> "tools" :> "lookup-bimi-domain" :> ReqBody '[JSON] LookupBimiDomainOptions :> Verb 'POST 200 '[JSON] LookupBimiDomainResults -- 'lookupBimiDomain' route
    :<|> "tools" :> "lookup-dmarc-domain" :> ReqBody '[JSON] LookupDmarcDomainOptions :> Verb 'POST 200 '[JSON] LookupDmarcDomainResults -- 'lookupDmarcDomain' route
    :<|> "tools" :> "lookup-mta-sts-domain" :> ReqBody '[JSON] LookupMtaStsDomainOptions :> Verb 'POST 200 '[JSON] LookupMtaStsDomainResults -- 'lookupMtaStsDomain' route
    :<|> "tools" :> "lookup-tls-reporting-domain" :> ReqBody '[JSON] LookupTlsReportingDomainOptions :> Verb 'POST 200 '[JSON] LookupTlsReportingDomainResults -- 'lookupTlsReportingDomain' route
    :<|> "tracking" :> "pixels" :> ReqBody '[JSON] CreateTrackingPixelOptions :> Verb 'POST 200 '[JSON] TrackingPixelDto -- 'createTrackingPixel' route
    :<|> "tracking" :> "pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getAllTrackingPixels' route
    :<|> "tracking" :> "pixels" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] TrackingPixelDto -- 'getTrackingPixel' route
    :<|> "user" :> "json" :> "pluck" :> QueryParam "property" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] Text -- 'getJsonPropertyAsString' route
    :<|> "user" :> "info" :> Verb 'GET 200 '[JSON] UserInfoDto -- 'getUserInfo' route
    :<|> "waitFor" :> ReqBody '[JSON] WaitForConditions :> Verb 'POST 200 '[JSON] [EmailPreview] -- 'waitFor' route
    :<|> "waitForEmailCount" :> QueryParam "inboxId" UUID :> QueryParam "count" Int :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "before" UTCTime :> QueryParam "since" UTCTime :> QueryParam "sort" Text :> QueryParam "delay" Integer :> Verb 'GET 200 '[JSON] [EmailPreview] -- 'waitForEmailCount' route
    :<|> "waitForLatestEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "before" UTCTime :> QueryParam "since" UTCTime :> QueryParam "sort" Text :> QueryParam "delay" Integer :> Verb 'GET 200 '[JSON] Email -- 'waitForLatestEmail' route
    :<|> "waitForLatestSms" :> ReqBody '[JSON] WaitForSingleSmsOptions :> Verb 'POST 200 '[JSON] SmsDto -- 'waitForLatestSms' route
    :<|> "waitForMatchingEmails" :> QueryParam "inboxId" UUID :> QueryParam "count" Int :> QueryParam "before" UTCTime :> QueryParam "since" UTCTime :> QueryParam "sort" Text :> QueryParam "delay" Integer :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> ReqBody '[JSON] MatchOptions :> Verb 'POST 200 '[JSON] [EmailPreview] -- 'waitForMatchingEmails' route
    :<|> "waitForMatchingFirstEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "sort" Text :> QueryParam "delay" Integer :> ReqBody '[JSON] MatchOptions :> Verb 'POST 200 '[JSON] Email -- 'waitForMatchingFirstEmail' route
    :<|> "waitForNthEmail" :> QueryParam "inboxId" UUID :> QueryParam "index" Int :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "sort" Text :> QueryParam "delay" Integer :> Verb 'GET 200 '[JSON] Email -- 'waitForNthEmail' route
    :<|> "waitForSms" :> ReqBody '[JSON] WaitForSmsConditions :> Verb 'POST 200 '[JSON] [SmsPreview] -- 'waitForSms' route
    :<|> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createAccountWebhook' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createWebhook' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createWebhookForPhoneNumber' route
    :<|> "webhooks" :> QueryParam "before" UTCTime :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllWebhooks' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> Capture "webhookId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteWebhook' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteWebhookById' route
    :<|> "webhooks" :> "account" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> QueryParam "health" Text :> QueryParam "searchFilter" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllAccountWebhooks' route
    :<|> "webhooks" :> "endpoints" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> Verb 'GET 200 '[JSON] PageWebhookEndpointProjection -- 'getAllWebhookEndpoints' route
    :<|> "webhooks" :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> QueryParam "resultType" Text :> QueryParam "eventName" Text :> QueryParam "minStatusCode" Int :> QueryParam "maxStatusCode" Int :> QueryParam "inboxId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" UUID :> QueryParam "emailId" UUID :> QueryParam "phoneId" UUID :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getAllWebhookResults' route
    :<|> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> QueryParam "url" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllWebhooks' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getInboxWebhooksPaginated' route
    :<|> "webhooks" :> "schema" :> QueryParam "event" Text :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookEvent' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "schema" :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookPayload' route
    :<|> "phone" :> "numbers" :> Capture "phoneId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> QueryParam "searchFilter" Text :> QueryParam "health" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getPhoneNumberWebhooksPaginated' route
    :<|> "webhooks" :> "test" :> QueryParam "eventName" Text :> Verb 'GET 200 '[JSON] AbstractWebhookPayload -- 'getTestWebhookPayload' route
    :<|> "webhooks" :> "test" :> "email-bounce-payload" :> Verb 'GET 200 '[JSON] WebhookBouncePayload -- 'getTestWebhookPayloadBounce' route
    :<|> "webhooks" :> "test" :> "email-bounce-recipient-payload" :> Verb 'GET 200 '[JSON] WebhookBounceRecipientPayload -- 'getTestWebhookPayloadBounceRecipient' route
    :<|> "webhooks" :> "test" :> "delivery-status-payload" :> Verb 'GET 200 '[JSON] WebhookDeliveryStatusPayload -- 'getTestWebhookPayloadDeliveryStatus' route
    :<|> "webhooks" :> "test" :> "email-opened-payload" :> Verb 'GET 200 '[JSON] WebhookEmailOpenedPayload -- 'getTestWebhookPayloadEmailOpened' route
    :<|> "webhooks" :> "test" :> "email-read-payload" :> Verb 'GET 200 '[JSON] WebhookEmailReadPayload -- 'getTestWebhookPayloadEmailRead' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "example" :> Verb 'POST 200 '[JSON] AbstractWebhookPayload -- 'getTestWebhookPayloadForWebhook' route
    :<|> "webhooks" :> "test" :> "new-attachment-payload" :> Verb 'GET 200 '[JSON] WebhookNewAttachmentPayload -- 'getTestWebhookPayloadNewAttachment' route
    :<|> "webhooks" :> "test" :> "new-contact-payload" :> Verb 'GET 200 '[JSON] WebhookNewContactPayload -- 'getTestWebhookPayloadNewContact' route
    :<|> "webhooks" :> "test" :> "new-email-payload" :> Verb 'GET 200 '[JSON] WebhookNewEmailPayload -- 'getTestWebhookPayloadNewEmail' route
    :<|> "webhooks" :> "test" :> "new-sms-payload" :> Verb 'GET 200 '[JSON] WebhookNewSmsPayload -- 'getTestWebhookPayloadNewSms' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> Verb 'GET 200 '[JSON] WebhookDto -- 'getWebhook' route
    :<|> "webhooks" :> "results" :> Capture "webhookResultId" UUID :> Verb 'GET 200 '[JSON] WebhookResultDto -- 'getWebhookResult' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> QueryParam "resultType" Text :> QueryParam "eventName" Text :> QueryParam "minStatusCode" Int :> QueryParam "maxStatusCode" Int :> QueryParam "inboxId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" UUID :> QueryParam "emailId" UUID :> QueryParam "phoneId" UUID :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getWebhookResults' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "results" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getWebhookResultsCount' route
    :<|> "webhooks" :> "results" :> "unseen-count" :> Verb 'GET 200 '[JSON] UnseenErrorCountDto -- 'getWebhookResultsUnseenErrorCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] [WebhookProjection] -- 'getWebhooks' route
    :<|> "webhooks" :> "results" :> "redrive" :> Verb 'POST 200 '[JSON] WebhookRedriveAllResult -- 'redriveAllWebhookResults' route
    :<|> "webhooks" :> "results" :> Capture "webhookResultId" UUID :> "redrive" :> Verb 'POST 200 '[JSON] WebhookRedriveResult -- 'redriveWebhookResult' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "test" :> Verb 'POST 200 '[JSON] WebhookTestResult -- 'sendTestData' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> QueryParam "inboxId" UUID :> QueryParam "phoneNumberId" UUID :> QueryParam "overrideAuth" Bool :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'PATCH 200 '[JSON] WebhookDto -- 'updateWebhook' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "headers" :> ReqBody '[JSON] WebhookHeaders :> Verb 'PUT 200 '[JSON] WebhookDto -- 'updateWebhookHeaders' route
    :<|> "webhooks" :> "verify" :> ReqBody '[JSON] VerifyWebhookSignatureOptions :> Verb 'POST 200 '[JSON] VerifyWebhookSignatureResults -- 'verifyWebhookSignature' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "wait" :> QueryParam "expectedCount" Int :> QueryParam "timeout" Int :> Verb 'GET 200 '[JSON] [WebhookResultDto] -- 'waitForWebhookResults' route
    :<|> Raw 


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype MailSlurpClientError = MailSlurpClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for MailSlurp.
-- The backend can be used both for the client and the server. The client generated from the MailSlurp OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createMailSlurpClient@). Alternatively, provided
-- a backend, the API can be served using @runMailSlurpMiddlewareServer@.
data MailSlurpBackend m = MailSlurpBackend
  { createAlias :: CreateAliasOptions -> m AliasDto{- ^ Email aliases use a MailSlurp randomly generated email address (or a custom domain inbox that you provide) to mask or proxy a real email address. Emails sent to the alias address will be forwarded to the hidden email address it was created for. If you want to send a reply use the threadId attached -}
  , deleteAlias :: UUID -> m (){- ^  -}
  , getAlias :: UUID -> m AliasDto{- ^ Get an email alias by ID -}
  , getAliasEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailProjection{- ^ Get paginated emails for an alias by ID -}
  , getAliasThreads :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageAliasThreadProjection{- ^ Returns threads created for an email alias in paginated form -}
  , getAliases :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageAlias{- ^ Get all email aliases in paginated form -}
  , getThread :: UUID -> m AliasThreadProjection{- ^ Return a thread associated with an alias -}
  , getThreadsPaginated :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageAliasThreadProjection{- ^ Returns threads created for all aliases in paginated form -}
  , replyToAliasEmail :: UUID -> UUID -> ReplyToAliasEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , sendAliasEmail :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Send an email from an alias. Replies to the email will be forwarded to the alias masked email address -}
  , updateAlias :: UUID -> UpdateAliasOptions -> m AliasDto{- ^  -}
  , deleteAllAttachments :: m (){- ^ Delete all attachments -}
  , deleteAttachment :: Text -> m (){- ^ Delete an attachment -}
  , downloadAttachmentAsBase64Encoded :: Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , downloadAttachmentAsBytes :: Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , getAttachment :: Text -> m AttachmentEntityDto{- ^  -}
  , getAttachmentInfo :: Text -> m AttachmentMetaData{- ^ Returns the metadata for an attachment. It is saved separately to the content of the attachment. Contains properties `name` and `content-type` and `content-length` in bytes for a given attachment. -}
  , getAttachments :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageAttachmentEntity{- ^ Get all attachments in paginated response. Each entity contains meta data for the attachment such as `name` and `content-type`. Use the `attachmentId` and the download endpoints to get the file contents. -}
  , uploadAttachment :: UploadAttachmentOptions -> m [Text]{- ^  -}
  , uploadAttachmentBytes :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Text -> Maybe Text -> m [Text]{- ^  -}
  , uploadMultipartForm :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> InlineObject -> m [Text]{- ^  -}
  , filterBouncedRecipient :: FilterBouncedRecipientsOptions -> m FilterBouncedRecipientsResult{- ^ Prevent email sending errors by remove recipients who have resulted in past email bounces or complaints -}
  , getAccountBounceBlockStatus :: m AccountBounceBlockDto{- ^ Check if account block status prevents sending -}
  , getBouncedEmail :: UUID -> m BouncedEmailDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedEmail{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipient :: UUID -> m BouncedRecipientDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedRecipients{- ^ Bounced recipients are email addresses that you have sent emails to that did not accept the sent email. Once a recipient is bounced you cannot send emails to that address. -}
  , getComplaint :: UUID -> m Complaint{- ^ Get complaint -}
  , getComplaints :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageComplaint{- ^ SMTP complaints made against your account -}
  , getListUnsubscribeRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UUID -> m PageListUnsubscribeRecipients{- ^ Unsubscribed recipient have unsubscribed from a mailing list for a user or domain and cannot be contacted again. -}
  , bulkCreateInboxes :: Maybe Int -> m [InboxDto]{- ^  -}
  , bulkDeleteInboxes :: [UUID] -> m (){- ^  -}
  , bulkSendEmails :: BulkSendEmailOptions -> m (){- ^  -}
  , createNewEmailAddress :: Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe UUID -> Maybe Text -> m InboxDto{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , createRandomInbox :: Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe UUID -> Maybe Text -> m InboxDto{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , deleteEmailAddress :: Maybe UUID -> m (){- ^ Deletes inbox email address -}
  , emptyInbox :: Maybe UUID -> m (){- ^ Deletes all emails -}
  , sendEmailQuery :: Maybe UUID -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ If no senderId or inboxId provided a random email address will be used to send from. Ensure your parameters are URL encoded. -}
  , sendEmailSimple :: SimpleSendEmailOptions -> m (){- ^ If no senderId or inboxId provided a random email address will be used to send from. -}
  , createConnector :: Maybe UUID -> CreateConnectorOptions -> m ConnectorDto{- ^ Sync emails between external mailboxes and MailSlurp inboxes -}
  , createConnectorImapConnection :: UUID -> CreateConnectorImapConnectionOptions -> m ConnectorImapConnectionDto{- ^ Allows the reading of emails in an external mailbox and syncing to a MailSlurp inbox -}
  , createConnectorSmtpConnection :: UUID -> CreateConnectorSmtpConnectionOptions -> m ConnectorSmtpConnectionDto{- ^ Allows sending via connector and email is routed to connected inbox and sent via SMTP -}
  , createConnectorSyncSettings :: UUID -> CreateConnectorSyncSettingsOptions -> m ConnectorSyncSettingsDto{- ^ Configure automatic pull or emails from external inboxes using an interval or schedule -}
  , createConnectorWithOptions :: Maybe UUID -> CreateConnectorWithOptions -> m ConnectorDto{- ^ Sync emails between external mailboxes and MailSlurp inboxes -}
  , deleteAllConnector :: m (){- ^  -}
  , deleteConnector :: UUID -> m (){- ^  -}
  , deleteConnectorImapConnection :: UUID -> m (){- ^ Delete IMAP connection for external inbox -}
  , deleteConnectorSmtpConnection :: UUID -> m (){- ^ Delete SMTP connection for external inbox -}
  , deleteConnectorSyncSettings :: UUID -> m (){- ^ Configure automatic pull or emails from external inboxes using an interval or schedule -}
  , getAllConnectorEvents :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageConnectorEvents{- ^  -}
  , getConnector :: UUID -> m ConnectorDto{- ^  -}
  , getConnectorByEmailAddress :: Maybe Text -> m OptionalConnectorDto{- ^ Find an inbox connector by email address -}
  , getConnectorByInboxId :: Maybe UUID -> m OptionalConnectorDto{- ^ Find an inbox connector by inbox ID -}
  , getConnectorByName :: Maybe Text -> m OptionalConnectorDto{- ^ Find an inbox connector by name -}
  , getConnectorEvent :: UUID -> m ConnectorEventDto{- ^  -}
  , getConnectorEvents :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageConnectorEvents{- ^  -}
  , getConnectorImapConnection :: UUID -> m OptionalConnectorImapConnectionDto{- ^ Get IMAP connection for external inbox -}
  , getConnectorProviderSettings :: m ConnectorProviderSettingsDto{- ^ Get common mail provider SMTP and IMAP connection settings -}
  , getConnectorSmtpConnection :: UUID -> m OptionalConnectorSmtpConnectionDto{- ^ Get SMTP connection for external inbox -}
  , getConnectorSyncSettings :: UUID -> m OptionalConnectorSyncSettingsDto{- ^ Get sync settings for connection with external inbox -}
  , getConnectors :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageConnector{- ^ List inbox connectors that sync external emails to MailSlurp inboxes -}
  , sendEmailFromConnector :: UUID -> Maybe Bool -> SendEmailOptions -> m SentEmailDto{- ^  -}
  , syncConnector :: UUID -> Maybe UTCTime -> Maybe Text -> Maybe Bool -> m ConnectorSyncRequestResult{- ^  -}
  , testConnectorImapConnection :: UUID -> CreateConnectorImapConnectionOptions -> m ConnectorImapConnectionTestResult{- ^ Test the IMAP connection for a connector -}
  , testConnectorImapConnectionOptions :: CreateConnectorImapConnectionOptions -> m ConnectorImapConnectionTestResult{- ^ Test the IMAP connection options for a connector -}
  , testConnectorSmtpConnection :: UUID -> CreateConnectorSmtpConnectionOptions -> m ConnectorSmtpConnectionTestResult{- ^ Test the SMTP connection for a connector -}
  , testConnectorSmtpConnectionOptions :: CreateConnectorSmtpConnectionOptions -> m ConnectorSmtpConnectionTestResult{- ^ Test the SMTP connection options for a connector -}
  , updateConnector :: UUID -> CreateConnectorOptions -> m ConnectorDto{- ^  -}
  , updateConnectorImapConnection :: UUID -> CreateConnectorImapConnectionOptions -> m ConnectorImapConnectionDto{- ^ Update IMAP connection for external inbox -}
  , updateConnectorSmtpConnection :: UUID -> CreateConnectorSmtpConnectionOptions -> m ConnectorSmtpConnectionDto{- ^ Update SMTP connection for external inbox -}
  , checkSendingConsentForEmailAddress :: Maybe Text -> m OptInSendingConsentDto{- ^  -}
  , getOptInIdentities :: Maybe Int -> Maybe Int -> Maybe Text -> m PageOptInIdentityProjection{- ^  -}
  , revokeOptInConsentForEmailAddress :: Maybe Text -> m OptInSendingConsentDto{- ^  -}
  , sendOptInConsentForEmailAddress :: OptInConsentOptions -> m OptInConsentSendResult{- ^ Send double-opt in consent for an email address -}
  , createContact :: CreateContactOptions -> m ContactDto{- ^  -}
  , deleteContact :: UUID -> m (){- ^  -}
  , getAllContacts :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageContactProjection{- ^  -}
  , getContact :: UUID -> m ContactDto{- ^  -}
  , getContactVCard :: UUID -> m (){- ^  -}
  , getContacts :: m [ContactProjection]{- ^  -}
  , addDomainWildcardCatchAll :: UUID -> m DomainDto{- ^ Add a catch all inbox to a domain so that any emails sent to it that cannot be matched will be sent to the catch all inbox generated -}
  , createDomain :: CreateDomainOptions -> m DomainDto{- ^ Link a domain that you own with MailSlurp so you can create email addresses using it. Endpoint returns DNS records used for validation. You must add these verification records to your host provider's DNS setup to verify the domain. -}
  , deleteDomain :: UUID -> m [Text]{- ^ Delete a domain. This will disable any existing inboxes that use this domain. -}
  , getAvailableDomains :: Maybe Text -> m DomainGroupsDto{- ^ List all domains available for use with email address creation -}
  , getDomain :: UUID -> Maybe Bool -> m DomainDto{- ^ Returns domain verification status and tokens for a given domain -}
  , getDomainIssues :: m DomainIssuesDto{- ^ List domain issues for domains you have created -}
  , getDomainWildcardCatchAllInbox :: UUID -> m InboxDto{- ^ Get the catch all inbox for a domain for missed emails -}
  , getDomains :: m [DomainPreview]{- ^ List all custom domains you have created -}
  , getMailSlurpDomains :: Maybe Text -> m DomainGroupsDto{- ^ List all MailSlurp domains used with non-custom email addresses -}
  , updateDomain :: UUID -> UpdateDomainOptions -> m DomainDto{- ^ Update values on a domain. Note you cannot change the domain name as it is immutable. Recreate the domain if you need to alter this. -}
  , applyImapFlagOperation :: UUID -> ImapFlagOperationOptions -> m EmailPreview{- ^ Apply RFC3501 section-2.3.2 IMAP flag operations on an email -}
  , canSend :: Maybe UUID -> SendEmailOptions -> m CanSendEmailResults{- ^ Can user send email to given recipient or is the recipient blocked -}
  , checkEmailBody :: UUID -> m CheckEmailBodyResults{- ^ Find dead links, broken images, and spelling mistakes in email body. Will call included links via HTTP so do not invoke if your links are sensitive or stateful. Any resource that returns a 4xx or 5xx response or is not reachable via HEAD or GET HTTP operations will be considered unhealthy. -}
  , checkEmailBodyFeatureSupport :: UUID -> m CheckEmailBodyFeatureSupportResults{- ^ Detect HTML and CSS features inside an email body and return a report of email client support across different platforms and versions. -}
  , checkEmailClientSupport :: CheckEmailClientSupportOptions -> m CheckEmailClientSupportResults{- ^ Evaluate the features used in an email body and return a report of email client support across different platforms and versions. -}
  , deleteAllEmails :: m (){- ^ Deletes all emails in your account. Be careful as emails cannot be recovered -}
  , deleteEmail :: UUID -> m (){- ^ Deletes an email and removes it from the inbox. Deleted emails cannot be recovered. -}
  , downloadAttachment :: UUID -> Text -> Maybe Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , downloadAttachmentBase64 :: UUID -> Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , downloadBody :: UUID -> m Text{- ^ Returns the specified email body for a given email as a string -}
  , downloadBodyBytes :: UUID -> m Text{- ^ Returns the specified email body for a given email as a stream / array of bytes. -}
  , forwardEmail :: UUID -> ForwardEmailOptions -> m SentEmailDto{- ^ Forward an existing email to new recipients. The sender of the email will be the inbox that received the email you are forwarding. You can override the sender with the `from` option. Note you must have access to the from address in MailSlurp to use the override. For more control consider fetching the email and sending it a new using the send email endpoints. -}
  , getAttachmentMetaData :: UUID -> Text -> m AttachmentMetaData{- ^ Returns the metadata such as name and content-type for a given attachment and email. -}
  , getEmail :: UUID -> m Email{- ^ Returns a email summary object with headers and content. To retrieve the raw unparsed email use the getRawEmail endpoints -}
  , getEmailAttachments :: UUID -> m [AttachmentMetaData]{- ^ Returns an array of attachment metadata such as name and content-type for a given email if present. -}
  , getEmailContentMatch :: UUID -> ContentMatchOptions -> m EmailContentMatchResult{- ^ Return the matches for a given Java style regex pattern. Do not include the typical `/` at start or end of regex in some languages. Given an example `your code is: 12345` the pattern to extract match looks like `code is: (\\d{6})`. This will return an array of matches with the first matching the entire pattern and the subsequent matching the groups: `['code is: 123456', '123456']` See https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html for more information of available patterns.  -}
  , getEmailContentPart :: UUID -> Maybe Text -> Maybe Bool -> Maybe Int -> m EmailContentPartResult{- ^ Get email body content parts from a multipart email message for a given content type -}
  , getEmailContentPartContent :: UUID -> Maybe Text -> Maybe Bool -> Maybe Int -> m Text{- ^ Get email body content parts from a multipart email message for a given content type and return as response -}
  , getEmailCount :: Maybe UUID -> m CountDto{- ^  -}
  , getEmailHTML :: UUID -> Maybe Bool -> m Text{- ^ Retrieve email content as HTML response for viewing in browsers. Decodes quoted-printable entities and converts charset to UTF-8. Pass your API KEY as a request parameter when viewing in a browser: `?apiKey=xxx`. Returns content-type `text/html;charset=utf-8` so you must call expecting that content response not JSON. For JSON response see the `getEmailHTMLJson` method. -}
  , getEmailHTMLJson :: UUID -> Maybe Bool -> m EmailHtmlDto{- ^ Retrieve email content as HTML response. Decodes quoted-printable entities and converts charset to UTF-8. Returns content-type `application/json;charset=utf-8` so you must call expecting that content response not JSON. -}
  , getEmailHTMLQuery :: UUID -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of text. HTML parsing uses JSoup which supports JQuery/CSS style selectors -}
  , getEmailLinks :: UUID -> Maybe Text -> m EmailLinksResult{- ^ HTML parsing uses JSoup and UNIX line separators. Searches content for href attributes -}
  , getEmailPreviewURLs :: UUID -> m EmailPreviewUrls{- ^ Get a list of URLs for email content as text/html or raw SMTP message for viewing the message in a browser. -}
  , getEmailScreenshotAsBase64 :: UUID -> GetEmailScreenshotOptions -> m EmailScreenshotResult{- ^ Capture image of email screenshot and return as base64 encoded string. Useful for embedding in HTML. Be careful as this may contain sensitive information. -}
  , getEmailScreenshotAsBinary :: UUID -> GetEmailScreenshotOptions -> m (){- ^ Returns binary octet-stream of screenshot of the given email -}
  , getEmailSummary :: UUID -> Maybe Bool -> m EmailPreview{- ^ Returns a email summary object with headers. To retrieve the body see getEmail and to get raw unparsed email use the getRawEmail endpoints -}
  , getEmailTextLines :: UUID -> Maybe Bool -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of strings. HTML parsing uses JSoup and UNIX line separators. -}
  , getEmailThread :: UUID -> m EmailThreadDto{- ^ Return email message thread summary from Message-ID, In-Reply-To, and References header. Get messages using items endpoint -}
  , getEmailThreadItems :: UUID -> Maybe Text -> m EmailThreadItemsDto{- ^ Return email thread messages based on Message-ID, In-Reply-To, and References header -}
  , getEmailThreads :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailThreadProjection{- ^ Return email message chains built from Message-ID, In-Reply-To, and References header. -}
  , getEmailsOffsetPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Bool -> Maybe UUID -> m PageEmailProjection{- ^ By default returns all emails across all inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe UUID -> Maybe Bool -> m PageEmailProjection{- ^ By default returns all emails across all inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getGravatarUrlForEmailAddress :: Maybe Text -> Maybe Text -> m GravatarUrl{- ^ Get gravatar url for email address -}
  , getLatestEmail :: Maybe [UUID] -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getLatestEmailInInbox1 :: Maybe UUID -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getOrganizationEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Bool -> Maybe UUID -> m PageEmailProjection{- ^ By default returns all emails across all team inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getRawEmailContents :: UUID -> m (){- ^ Returns a raw, unparsed, and unprocessed email. If your client has issues processing the response it is likely due to the response content-type which is text/plain. If you need a JSON response content-type use the getRawEmailJson endpoint -}
  , getRawEmailJson :: UUID -> m RawEmailJson{- ^ Returns a raw, unparsed, and unprocessed email wrapped in a JSON response object for easier handling when compared with the getRawEmail text/plain response -}
  , getUnreadEmailCount :: Maybe UUID -> m UnreadCount{- ^ Get number of emails unread. Unread means has not been viewed in dashboard or returned in an email API response -}
  , markAllAsRead :: Maybe Bool -> Maybe UUID -> m (){- ^ Marks all emails as read or unread. Pass boolean read flag to set value. This is useful if you want to read an email but keep it as unread -}
  , markAsRead :: UUID -> Maybe Bool -> m EmailPreview{- ^ Marks an email as read or unread. Pass boolean read flag to set value. This is useful if you want to read an email but keep it as unread -}
  , replyToEmail :: UUID -> ReplyToEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , searchEmails :: Maybe Bool -> Maybe Bool -> Maybe UUID -> SearchEmailsOptions -> m PageEmailProjection{- ^ Search emails by given criteria return matches in paginated format. Searches against email recipients, sender, subject, email address and ID. Does not search email body -}
  , sendEmailSourceOptional :: Maybe UUID -> Maybe Bool -> Maybe Bool -> SendEmailOptions -> m (){- ^ Alias for `InboxController.sendEmail` method - see original method for full details. Sends an email from a given inbox that you have created. If no inbox is supplied a random inbox will be created for you and used to send the email. -}
  , setEmailFavourited :: UUID -> Maybe Bool -> m (){- ^ Set and return new favorite state for an email -}
  , validateEmail :: UUID -> m ValidationDto{- ^ Validate the HTML content of email if HTML is found. Considered valid if no HTML is present. -}
  , deleteAllValidationRequests :: m (){- ^ Remove validation requests -}
  , deleteValidationRequest :: UUID -> m (){- ^ Delete a validation record -}
  , getValidationRequests :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> m PageEmailValidationRequest{- ^ List email verification requests -}
  , validateEmailAddressList :: ValidateEmailAddressListOptions -> m ValidateEmailAddressListResult{- ^ Verify a list of email addresses is valid and can be contacted. -}
  , getExpirationDefaults :: m ExpirationDefaults{- ^ Return default times used for inbox expiration -}
  , getExpiredInboxByInboxId :: UUID -> m ExpiredInboxDto{- ^ Use the inboxId to return an ExpiredInboxRecord if an inbox has expired. Inboxes expire and are disabled if an expiration date is set or plan requires. Returns 404 if no expired inbox is found for the inboxId -}
  , getExpiredInboxRecord :: UUID -> m ExpiredInboxDto{- ^ Inboxes created with an expiration date will expire after the given date and be moved to an ExpiredInbox entity. You can still read emails in the inbox but it can no longer send or receive emails. Fetch the expired inboxes to view the old inboxes properties -}
  , getExpiredInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageExpiredInboxRecordProjection{- ^ Inboxes created with an expiration date will expire after the given date. An ExpiredInboxRecord is created that records the inboxes old ID and email address. You can still read emails in the inbox (using the inboxes old ID) but the email address associated with the inbox can no longer send or receive emails. Fetch expired inbox records to view the old inboxes properties -}
  , exportEntities :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m Text{- ^  -}
  , getExportLink :: Maybe Text -> Maybe Text -> ExportOptions -> m ExportLink{- ^  -}
  , submitForm :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m Text{- ^ This endpoint allows you to submit HTML forms and receive the field values and files via email.   #### Parameters The endpoint looks for special meta parameters in the form fields OR in the URL request parameters. The meta parameters can be used to specify the behaviour of the email.   You must provide at-least a `_to` email address to tell the endpoint where the form should be emailed. These can be submitted as hidden HTML input fields with the corresponding `name` attributes or as URL query parameters such as `?_to=test@example.com`  The endpoint takes all other form fields that are named and includes them in the message body of the email. Files are sent as attachments.  #### Submitting This endpoint accepts form submission via POST method. It accepts `application/x-www-form-urlencoded`, and `multipart/form-data` content-types.  #### HTML Example ```html <form    action=\"https://api.mailslurp.com/forms\"   method=\"post\" >   <input name=\"_to\" type=\"hidden\" value=\"test@example.com\"/>   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```  #### URL Example ```html <form    action=\"https://api.mailslurp.com/forms?_to=test@example.com\"   method=\"post\" >   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```    The email address is specified by a `_to` field OR is extracted from an email alias specified by a `_toAlias` field (see the alias controller for more information).  Endpoint accepts .  You can specify a content type in HTML forms using the `enctype` attribute, for instance: `<form enctype=\"multipart/form-data\">`.   -}
  , addContactsToGroup :: UUID -> UpdateGroupContacts -> m GroupContactsDto{- ^  -}
  , createGroup :: CreateGroupOptions -> m GroupDto{- ^  -}
  , deleteGroup :: UUID -> m (){- ^  -}
  , getAllGroups :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageGroupProjection{- ^  -}
  , getGroup :: UUID -> m GroupDto{- ^  -}
  , getGroupWithContacts :: UUID -> m GroupContactsDto{- ^  -}
  , getGroupWithContactsPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageContactProjection{- ^ Get group and paginated contacts belonging to it -}
  , getGroups :: m [GroupProjection]{- ^  -}
  , removeContactsFromGroup :: UUID -> UpdateGroupContacts -> m GroupContactsDto{- ^  -}
  , createGuestPortal :: CreatePortalOptions -> m GuestPortalDto{- ^ Create a guest login page for customers or clients to access assigned email addresses -}
  , createGuestPortalUser :: UUID -> CreatePortalUserOptions -> m GuestPortalUserCreateDto{- ^ Add customer to portal -}
  , getAllGuestPortalUsers :: Maybe UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageGuestPortalUsers{- ^ Get all customers for a portal -}
  , getGuestPortal :: UUID -> m GuestPortalDto{- ^ Fetch a customer guest portal -}
  , getGuestPortalUser :: UUID -> UUID -> m GuestPortalUserDto{- ^ Get customer for portal -}
  , getGuestPortalUserById :: UUID -> m GuestPortalUserDto{- ^ Get customer by ID -}
  , getGuestPortalUsers :: UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageGuestPortalUsers{- ^ Get customers for a portal -}
  , getGuestPortals :: m [GuestPortalDto]{- ^ Get portals -}
  , imapServerFetch :: Maybe UUID -> Maybe Integer -> m ImapServerFetchResult{- ^  -}
  , imapServerGet :: Maybe UUID -> Maybe UUID -> m ImapServerGetResult{- ^  -}
  , imapServerList :: Maybe UUID -> ImapServerListOptions -> m ImapServerListResult{- ^  -}
  , imapServerMailbox :: Maybe Text -> m ImapServerMailboxResult{- ^  -}
  , imapServerSearch :: Maybe UUID -> ImapServerSearchOptions -> m ImapServerSearchResult{- ^  -}
  , imapServerStatus :: Maybe UUID -> ImapServerStatusOptions -> m ImapServerStatusResult{- ^  -}
  , imapServerUpdateFlags :: Maybe UUID -> ImapUpdateFlagsOptions -> m (){- ^ Update message flags -}
  , cancelScheduledJob :: UUID -> m ScheduledJobDto{- ^ Get a scheduled email job and cancel it. Will fail if status of job is already cancelled, failed, or complete. -}
  , createInbox :: Maybe Text -> Maybe [Text] -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe UUID -> Maybe Text -> Maybe Text -> m InboxDto{- ^ Create a new inbox and with a randomized email address to send and receive from. Pass emailAddress parameter if you wish to use a specific email address. Creating an inbox is required before sending or receiving emails. If writing tests it is recommended that you create a new inbox during each test method so that it is unique and empty.  -}
  , createInboxRuleset :: UUID -> CreateInboxRulesetOptions -> m InboxRulesetDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , createInboxWithDefaults :: m InboxDto{- ^  -}
  , createInboxWithOptions :: CreateInboxDto -> m InboxDto{- ^ Additional endpoint that allows inbox creation with request body options. Can be more flexible that other methods for some clients. -}
  , deleteAllInboxEmails :: UUID -> m (){- ^ Deletes all emails in an inbox. Be careful as emails cannot be recovered -}
  , deleteAllInboxes :: m (){- ^ Permanently delete all inboxes and associated email addresses. This will also delete all emails within the inboxes. Be careful as inboxes cannot be recovered once deleted. Note: deleting inboxes will not impact your usage limits. Monthly inbox creation limits are based on how many inboxes were created in the last 30 days, not how many inboxes you currently have. -}
  , deleteAllInboxesByDescription :: Maybe Text -> m (){- ^ Permanently delete all inboxes by description -}
  , deleteAllInboxesByName :: Maybe Text -> m (){- ^ Permanently delete all inboxes by name -}
  , deleteAllInboxesByTag :: Maybe Text -> m (){- ^ Permanently delete all inboxes by tag -}
  , deleteInbox :: UUID -> m (){- ^ Permanently delete an inbox and associated email address as well as all emails within the given inbox. This action cannot be undone. Note: deleting an inbox will not affect your account usage. Monthly inbox usage is based on how many inboxes you create within 30 days, not how many exist at time of request. -}
  , doesInboxExist :: Maybe Text -> Maybe Bool -> m InboxExistsDto{- ^ Check if inboxes exist by email address. Useful if you are sending emails to mailslurp addresses -}
  , flushExpired :: Maybe UTCTime -> m FlushExpiredInboxesResult{- ^ Remove any expired inboxes for your account (instead of waiting for scheduled removal on server) -}
  , getAllInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe UUID -> m PageInboxProjection{- ^ List inboxes in paginated form. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time). You Can also filter by whether an inbox is favorited or use email address pattern. This method is the recommended way to query inboxes. The alternative `getInboxes` method returns a full list of inboxes but is limited to 100 results. -}
  , getAllInboxesOffsetPaginated :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe UUID -> m PageInboxProjection{- ^ List inboxes in paginated form. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time). You Can also filter by whether an inbox is favorited or use email address pattern. This method is the recommended way to query inboxes. The alternative `getInboxes` method returns a full list of inboxes but is limited to 100 results. -}
  , getAllPlusAddresses :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UUID -> m PagePlusAddressProjection{- ^ Returns paginated list of all plus alias addresses found for in account based on received emails that used the inbox address with a +xyz alias. -}
  , getAllScheduledJobs :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageScheduledJobs{- ^ Schedule sending of emails using scheduled jobs. These can be inbox or account level. -}
  , getDeliveryStatusesByInboxId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageDeliveryStatus{- ^ Get all email delivery statuses for an inbox -}
  , getEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m [EmailPreview]{- ^ List emails that an inbox has received. Only emails that are sent to the inbox's email address will appear in the inbox. It may take several seconds for any email you send to an inbox's email address to appear in the inbox. To make this endpoint wait for a minimum number of emails use the `minCount` parameter. The server will retry the inbox database until the `minCount` is satisfied or the `retryTimeout` is reached -}
  , getImapAccess :: Maybe UUID -> m ImapAccessDetails{- ^ Get IMAP access usernames and passwords -}
  , getImapSmtpAccess :: Maybe UUID -> m ImapSmtpAccessDetails{- ^ Get IMAP and SMTP access usernames and passwords -}
  , getImapSmtpAccessEnv :: Maybe UUID -> m Text{- ^ Get IMAP and SMTP access details in .env format -}
  , getImapSmtpAccessServers :: m ImapSmtpAccessServers{- ^ Get IMAP and SMTP server hosts -}
  , getInbox :: UUID -> m InboxDto{- ^ Returns an inbox's properties, including its email address and ID. -}
  , getInboxByEmailAddress :: Maybe Text -> m InboxByEmailAddressResult{- ^ Get a inbox result by email address -}
  , getInboxByName :: Maybe Text -> m InboxByNameResult{- ^ Get a inbox result by name -}
  , getInboxCount :: m CountDto{- ^  -}
  , getInboxEmailCount :: UUID -> m CountDto{- ^  -}
  , getInboxEmailsPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> m PageEmailPreview{- ^ Get a paginated list of emails in an inbox. Does not hold connections open. -}
  , getInboxIds :: m InboxIdsResult{- ^ Get list of inbox IDs -}
  , getInboxPlusAddress :: UUID -> UUID -> m PlusAddressDto{- ^ Returns a plus address object based on emails that used the inbox address with a +xyz alias. -}
  , getInboxPlusAddressById :: UUID -> Maybe UUID -> m PlusAddressDto{- ^ Returns a plus address object based on emails that used the inbox address with a +xyz alias. -}
  , getInboxPlusAddressEmails :: UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailPreview{- ^ Returns paginated list of all emails for a given plus alias addresses found for an inbox based on received emails that used the inbox address with a +xyz alias. -}
  , getInboxPlusAddressEmailsForPlusAddressId :: UUID -> UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailPreview{- ^ Returns paginated list of all emails for a given plus alias addresses found for an inbox based on received emails that used the inbox address with a +xyz alias. -}
  , getInboxPlusAddresses :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PagePlusAddressProjection{- ^ Returns paginated list of all plus alias addresses found for an inbox based on received emails that used the inbox address with a +xyz alias. -}
  , getInboxSentCount :: UUID -> m CountDto{- ^  -}
  , getInboxSentEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageSentEmailProjection{- ^ Returns an inbox's sent email receipts. Call individual sent email endpoints for more details. Note for privacy reasons the full body of sent emails is never stored. An MD5 hash hex is available for comparison instead. -}
  , getInboxTags :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> m [Text]{- ^ Get all inbox tags -}
  , getInboxTagsPaginated :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> m PageInboxTags{- ^ Get all inbox tags paginated -}
  , getInboxes :: Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe Bool -> Maybe UTCTime -> Maybe [UUID] -> m [InboxDto]{- ^ List the inboxes you have created. Note use of the more advanced `getAllInboxes` is recommended and allows paginated access using a limit and sort parameter. -}
  , getInboxesByTag :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> m PageInboxProjection{- ^ Get all inboxes for a given inbox tag -}
  , getLatestEmailInInbox :: Maybe UUID -> Maybe Integer -> m Email{- ^ Get the newest email in an inbox or wait for one to arrive -}
  , getOrganizationInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageOrganizationInboxProjection{- ^ List organization inboxes in paginated form. These are inboxes created with `allowTeamAccess` flag enabled. Organization inboxes are `readOnly` for non-admin users. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time).  -}
  , getOutboxes :: Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxProjection{- ^ List inboxes that have sent emails -}
  , getScheduledJob :: UUID -> m ScheduledJobDto{- ^ Get a scheduled email job details. -}
  , getScheduledJobsByInboxId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageScheduledJobs{- ^ Schedule sending of emails using scheduled jobs. -}
  , getSmtpAccess :: Maybe UUID -> m SmtpAccessDetails{- ^ Get SMTP access usernames and passwords -}
  , isEmailAddressAvailable :: Maybe Text -> m EmailAvailableResult{- ^ Returns whether an email address is available -}
  , listInboxRulesets :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxRulesetDto{- ^ List all rulesets attached to an inbox -}
  , listInboxTrackingPixels :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ List all tracking pixels sent from an inbox -}
  , searchInboxes :: SearchInboxesOptions -> m PageInboxProjection{- ^ Search inboxes and return in paginated form. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time). You Can also filter by whether an inbox is favorited or use email address pattern. This method is the recommended way to query inboxes. The alternative `getInboxes` method returns a full list of inboxes but is limited to 100 results. -}
  , sendEmail :: UUID -> SendEmailOptions -> m (){- ^ Send an email from an inbox's email address.  The request body should contain the `SendEmailOptions` that include recipients, attachments, body etc. See `SendEmailOptions` for all available properties. Note the `inboxId` refers to the inbox's id not the inbox's email address. See https://www.mailslurp.com/guides/ for more information on how to send emails. This method does not return a sent email entity due to legacy reasons. To send and get a sent email as returned response use the sister method `sendEmailAndConfirm`. -}
  , sendEmailAndConfirm :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Sister method for standard `sendEmail` method with the benefit of returning a `SentEmail` entity confirming the successful sending of the email with a link to the sent object created for it. -}
  , sendEmailWithQueue :: UUID -> Maybe Bool -> SendEmailOptions -> m (){- ^ Send an email using a queue. Will place the email onto a queue that will then be processed and sent. Use this queue method to enable any failed email sending to be recovered. This will prevent lost emails when sending if your account encounters a block or payment issue. -}
  , sendSmtpEnvelope :: UUID -> SendSMTPEnvelopeOptions -> m SentEmailDto{- ^ Send email using an SMTP envelope containing RCPT TO, MAIL FROM, and a SMTP BODY. -}
  , sendTestEmail :: UUID -> m (){- ^ Send an inbox a test email to test email receiving is working -}
  , sendWithSchedule :: UUID -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> SendEmailOptions -> m ScheduledJobDto{- ^ Send an email using a delay. Will place the email onto a scheduler that will then be processed and sent. Use delays to schedule email sending. -}
  , setInboxFavourited :: UUID -> SetInboxFavouritedOptions -> m InboxDto{- ^ Set and return new favorite state for an inbox -}
  , updateImapAccess :: Maybe UUID -> UpdateImapAccessOptions -> m (){- ^ Update IMAP access usernames and passwords -}
  , updateInbox :: UUID -> UpdateInboxOptions -> m InboxDto{- ^ Update editable fields on an inbox -}
  , updateSmtpAccess :: Maybe UUID -> UpdateSmtpAccessOptions -> m (){- ^ Update SMTP access usernames and passwords -}
  , createNewInboxForwarder :: Maybe UUID -> CreateInboxForwarderOptions -> m InboxForwarderDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , deleteInboxForwarder :: UUID -> m (){- ^ Delete inbox forwarder -}
  , deleteInboxForwarders :: Maybe UUID -> m (){- ^ Delete inbox forwarders. Accepts optional inboxId filter. -}
  , getAllInboxForwarderEvents :: Maybe Int -> Maybe Int -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> m PageInboxForwarderEvents{- ^ Get all inbox forwarder events -}
  , getForwarderEvent :: UUID -> m InboxForwarderEventDto{- ^ Get forwarder event -}
  , getInboxForwarder :: UUID -> m InboxForwarderDto{- ^ Get inbox forwarder -}
  , getInboxForwarderEvent :: UUID -> UUID -> m InboxForwarderEventDto{- ^ Get inbox forwarder event -}
  , getInboxForwarderEvents :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxForwarderEvents{- ^ Get inbox forwarder events -}
  , getInboxForwarders :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxForwarderDto{- ^ List all forwarders attached to an inbox -}
  , testInboxForwarder :: UUID -> InboxForwarderTestOptions -> m InboxForwarderTestResult{- ^ Test an inbox forwarder -}
  , testInboxForwardersForInbox :: Maybe UUID -> InboxForwarderTestOptions -> m InboxForwarderTestResult{- ^ Test inbox forwarders for inbox -}
  , testNewInboxForwarder :: TestNewInboxForwarderOptions -> m InboxForwarderTestResult{- ^ Test new inbox forwarder -}
  , updateInboxForwarder :: UUID -> CreateInboxForwarderOptions -> m InboxForwarderDto{- ^ Update inbox forwarder -}
  , createNewInboxReplier :: CreateInboxReplierOptions -> m InboxReplierDto{- ^ Create a new inbox rule for reply toing, blocking, and allowing emails when sending and receiving -}
  , deleteInboxReplier :: UUID -> m (){- ^ Delete inbox replier -}
  , deleteInboxRepliers :: Maybe UUID -> m (){- ^ Delete inbox repliers. Accepts optional inboxId filter. -}
  , getAllInboxReplierEvents :: Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxReplierEvents{- ^ Get all inbox ruleset events -}
  , getInboxReplier :: UUID -> m InboxReplierDto{- ^ Get inbox ruleset -}
  , getInboxReplierEvents :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxReplierEvents{- ^ Get inbox ruleset events -}
  , getInboxRepliers :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxReplierDto{- ^ List all repliers attached to an inbox -}
  , updateInboxReplier :: UUID -> UpdateInboxReplierOptions -> m InboxReplierDto{- ^ Update inbox ruleset -}
  , createNewInboxRuleset :: Maybe UUID -> CreateInboxRulesetOptions -> m InboxRulesetDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , deleteInboxRuleset :: UUID -> m (){- ^ Delete inbox ruleset -}
  , deleteInboxRulesets :: Maybe UUID -> m (){- ^ Delete inbox rulesets. Accepts optional inboxId filter. -}
  , getInboxRuleset :: UUID -> m InboxRulesetDto{- ^ Get inbox ruleset -}
  , getInboxRulesets :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxRulesetDto{- ^ List all rulesets attached to an inbox -}
  , testInboxRuleset :: UUID -> InboxRulesetTestOptions -> m InboxRulesetTestResult{- ^ Test an inbox ruleset -}
  , testInboxRulesetReceiving :: TestInboxRulesetReceivingOptions -> m TestInboxRulesetReceivingResult{- ^ Test whether inbound emails from an email address would be blocked or allowed by inbox rulesets -}
  , testInboxRulesetSending :: TestInboxRulesetSendingOptions -> m TestInboxRulesetSendingResult{- ^ Test whether outbound emails to an email address would be blocked or allowed by inbox rulesets -}
  , testInboxRulesetsForInbox :: Maybe UUID -> InboxRulesetTestOptions -> m InboxRulesetTestResult{- ^ Test inbox rulesets for inbox -}
  , testNewInboxRuleset :: TestNewInboxRulesetOptions -> m InboxRulesetTestResult{- ^ Test new inbox ruleset -}
  , describeMailServerDomain :: DescribeDomainOptions -> m DescribeMailServerDomainResult{- ^  -}
  , getDnsLookup :: DNSLookupOptions -> m DNSLookupResults{- ^  -}
  , getDnsLookups :: DNSLookupsOptions -> m DNSLookupResults{- ^  -}
  , getIpAddress :: Maybe Text -> m IPAddressResult{- ^  -}
  , verifyEmailAddress :: VerifyEmailAddressOptions -> m EmailVerificationResult{- ^  -}
  , getAllMissedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageMissedEmailProjection{- ^  -}
  , getAllUnknownMissedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageUnknownMissedEmailProjection{- ^ Unknown missed emails are emails that were sent to MailSlurp but could not be assigned to an existing inbox. -}
  , getMissedEmail :: UUID -> m MissedEmailDto{- ^ List emails that were missed due to plan limits. -}
  , restoreMissedEmails :: m (){- ^ If emails were missed due to a plan limit they are saved as missed emails. If support team enables the canRestore flag these emails can be reload into your account using this method. -}
  , waitForNthMissedEmail :: Maybe UUID -> Maybe Integer -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> m MissedEmailDto{- ^ Wait for 0 based index missed email -}
  , createEmergencyAddress :: CreateEmergencyAddressOptions -> m EmergencyAddress{- ^ Add an emergency address to a phone number -}
  , deleteEmergencyAddress :: UUID -> m EmptyResponseDto{- ^ Delete an emergency address -}
  , deletePhoneNumber :: UUID -> m (){- ^ Remove phone number from account -}
  , getConsentStatus :: m ConsentStatusDto{- ^ Get the status of phone usage consent -}
  , getEmergencyAddress :: UUID -> m EmergencyAddress{- ^ Fetch an emergency address by ID -}
  , getEmergencyAddresses :: m [EmergencyAddressDto]{- ^ List emergency addresses -}
  , getPhoneNumber :: UUID -> m PhoneNumberDto{- ^ Get a phone number by ID -}
  , getPhoneNumbers :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe [UUID] -> Maybe Bool -> m PagePhoneNumberProjection{- ^ List phone numbers for account -}
  , getPhonePlans :: m [PhonePlanDto]{- ^ Get phone number plans -}
  , getPhonePlansAvailability :: m PhonePlanAvailability{- ^  -}
  , getSentSmsByPhoneNumber :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageSentSmsProjection{- ^ Get sent SMS messages for a phone number -}
  , getSmsByPhoneNumber :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Bool -> m PageSmsProjection{- ^ Get SMS messages for a phone number -}
  , sendSmsFromPhoneNumber :: UUID -> SmsSendOptions -> m SentSmsDto{- ^ Send SMS from a phone number -}
  , setConsentStatus :: Maybe Bool -> m ConsentStatusDto{- ^ Give or revoke consent for phone usage -}
  , setPhoneFavourited :: UUID -> SetPhoneFavouritedOptions -> m PhoneNumberDto{- ^ Set and return new favorite state for a phone -}
  , testPhoneNumberSendSms :: UUID -> TestPhoneNumberOptions -> Maybe Text -> m (){- ^ Test a phone number by sending an SMS to it -}
  , updatePhoneNumber :: UUID -> UpdatePhoneNumberOptions -> m PhoneNumberDto{- ^ Set field for phone number -}
  , validatePhoneNumber :: ValidatePhoneNumberOptions -> m PhoneNumberValidationDto{- ^ Validate a phone number -}
  , deleteAllSentEmails :: m (){- ^  -}
  , deleteSentEmail :: UUID -> m (){- ^  -}
  , getAllSentTrackingPixels :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ Get all sent email tracking pixels in paginated form -}
  , getRawSentEmailContents :: UUID -> m (){- ^ Returns a raw, unparsed, and unprocessed sent email. If your client has issues processing the response it is likely due to the response content-type which is text/plain. If you need a JSON response content-type use the getRawSentEmailJson endpoint -}
  , getRawSentEmailJson :: UUID -> m RawEmailJson{- ^ Returns a raw, unparsed, and unprocessed sent email wrapped in a JSON response object for easier handling when compared with the getRawSentEmail text/plain response -}
  , getSentDeliveryStatus :: UUID -> m DeliveryStatusDto{- ^ Get a sent email delivery status -}
  , getSentDeliveryStatuses :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageDeliveryStatus{- ^ Get all sent email delivery statuses -}
  , getSentDeliveryStatusesBySentId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageDeliveryStatus{- ^ Get all sent email delivery statuses -}
  , getSentEmail :: UUID -> m SentEmailDto{- ^  -}
  , getSentEmailHTMLContent :: UUID -> m Text{- ^  -}
  , getSentEmailPreviewURLs :: UUID -> m EmailPreviewUrls{- ^ Get a list of URLs for sent email content as text/html or raw SMTP message for viewing the message in a browser. -}
  , getSentEmailTrackingPixels :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ Get all tracking pixels for a sent email in paginated form -}
  , getSentEmails :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageSentEmailProjection{- ^  -}
  , getSentEmailsWithQueueResults :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageSentEmailWithQueueProjection{- ^  -}
  , getSentOrganizationEmails :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageSentEmailProjection{- ^ Get all sent organization emails in paginated form -}
  , waitForDeliveryStatuses :: Maybe UUID -> Maybe UUID -> Maybe Integer -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> m DeliveryStatusDto{- ^ Wait for delivery statuses -}
  , deleteSentSmsMessage :: UUID -> m (){- ^ Delete a sent SMS message -}
  , deleteSentSmsMessages :: Maybe UUID -> m (){- ^ Delete all sent SMS messages or all messages for a given phone number -}
  , deleteSmsMessage :: UUID -> m (){- ^ Delete an SMS message -}
  , deleteSmsMessages :: Maybe UUID -> m (){- ^ Delete all SMS messages or all messages for a given phone number -}
  , getAllSmsMessages :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Bool -> m PageSmsProjection{- ^  -}
  , getReplyForSmsMessage :: UUID -> m ReplyForSms{- ^ Get reply for an SMS message. -}
  , getSentSmsCount :: m CountDto{- ^ Get number of sent SMS -}
  , getSentSmsMessage :: UUID -> m SentSmsDto{- ^ Returns an SMS summary object with content. -}
  , getSentSmsMessagesPaginated :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageSentSmsProjection{- ^ By default returns all SMS messages across all phone numbers sorted by ascending created at date. Responses are paginated. You can restrict results to a list of phone number IDs. You can also filter out read messages -}
  , getSmsCount :: m CountDto{- ^ Get number of SMS -}
  , getSmsMessage :: UUID -> m SmsDto{- ^ Returns a SMS summary object with content. -}
  , getUnreadSmsCount :: m UnreadCount{- ^ Get number of SMS unread. Unread means has not been viewed in dashboard or returned in an email API response -}
  , replyToSmsMessage :: UUID -> SmsReplyOptions -> m SentSmsDto{- ^ Reply to an SMS message. -}
  , sendSms :: Maybe Text -> Maybe UUID -> SmsSendOptions -> m SentSmsDto{- ^  -}
  , setSmsFavourited :: UUID -> Maybe Bool -> m SmsDto{- ^  -}
  , streamEvents :: Maybe Text -> m [Text]{- ^  -}
  , createTemplate :: CreateTemplateOptions -> m TemplateDto{- ^ Create an email template with variables for use with templated transactional emails. -}
  , deleteTemplate :: UUID -> m (){- ^ Delete template -}
  , getAllTemplates :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTemplateProjection{- ^ Get all templates in paginated format -}
  , getTemplate :: UUID -> m TemplateDto{- ^ Get email template -}
  , getTemplatePreviewHtml :: UUID -> m Text{- ^ Get email template preview with passed template variables in HTML format for browsers. Pass template variables as query params. -}
  , getTemplatePreviewJson :: UUID -> m TemplatePreview{- ^ Get email template preview with passed template variables in JSON format. Pass template variables as query params. -}
  , getTemplates :: m [TemplateProjection]{- ^ Get all templates -}
  , updateTemplate :: UUID -> CreateTemplateOptions -> m TemplateDto{- ^ Update email template -}
  , checkEmailFeaturesClientSupport :: CheckEmailFeaturesClientSupportOptions -> m CheckEmailFeaturesClientSupportResults{- ^  -}
  , createNewFakeEmailAddress :: m NewFakeEmailAddressResult{- ^  -}
  , deleteNewFakeEmailAddress :: Maybe Text -> m (){- ^ Delete a fake email address using the fake email domains -}
  , generateBimiRecord :: GenerateBimiRecordOptions -> m GenerateBimiRecordResults{- ^  -}
  , generateDmarcRecord :: GenerateDmarcRecordOptions -> m GenerateDmarcRecordResults{- ^  -}
  , generateMtaStsRecord :: GenerateMtaStsRecordOptions -> m GenerateMtaStsRecordResults{- ^  -}
  , generateTlsReportingRecord :: GenerateTlsReportingRecordOptions -> m GenerateTlsReportingRecordResults{- ^  -}
  , getFakeEmailByEmailAddress :: Maybe Text -> m FakeEmailResult{- ^  -}
  , getFakeEmailById :: Maybe UUID -> m FakeEmailResult{- ^ Get a fake email by its ID -}
  , getFakeEmailRaw :: Maybe UUID -> m Text{- ^ Retrieve the raw content of a fake email by its ID -}
  , getFakeEmailsForAddress :: Maybe Int -> Maybe Text -> m [FakeEmailPreview]{- ^ Get fake emails for an address -}
  , lookupBimiDomain :: LookupBimiDomainOptions -> m LookupBimiDomainResults{- ^  -}
  , lookupDmarcDomain :: LookupDmarcDomainOptions -> m LookupDmarcDomainResults{- ^  -}
  , lookupMtaStsDomain :: LookupMtaStsDomainOptions -> m LookupMtaStsDomainResults{- ^  -}
  , lookupTlsReportingDomain :: LookupTlsReportingDomainOptions -> m LookupTlsReportingDomainResults{- ^  -}
  , createTrackingPixel :: CreateTrackingPixelOptions -> m TrackingPixelDto{- ^ Create a tracking pixel. A tracking pixel is an image that can be embedded in an email. When the email is viewed and the image is seen MailSlurp will mark the pixel as seen. Use tracking pixels to monitor email open events. You can receive open notifications via webhook or by fetching the pixel. -}
  , getAllTrackingPixels :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ List tracking pixels in paginated form -}
  , getTrackingPixel :: UUID -> m TrackingPixelDto{- ^  -}
  , getJsonPropertyAsString :: Maybe Text -> Value -> m Text{- ^ Utility function to extract properties from JSON objects in language where this is cumbersome. -}
  , getUserInfo :: m UserInfoDto{- ^ Get account information for your user -}
  , waitFor :: WaitForConditions -> m [EmailPreview]{- ^ Generic waitFor method that will wait until an inbox meets given conditions or return immediately if already met -}
  , waitForEmailCount :: Maybe UUID -> Maybe Int -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Integer -> m [EmailPreview]{- ^ If inbox contains count or more emails at time of request then return count worth of emails. If not wait until the count is reached and return those or return an error if timeout is exceeded. -}
  , waitForLatestEmail :: Maybe UUID -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Integer -> m Email{- ^ Will return either the last received email or wait for an email to arrive and return that. If you need to wait for an email for a non-empty inbox set `unreadOnly=true` or see the other receive methods such as `waitForNthEmail` or `waitForEmailCount`. -}
  , waitForLatestSms :: WaitForSingleSmsOptions -> m SmsDto{- ^ Wait until a phone number meets given conditions or return immediately if already met -}
  , waitForMatchingEmails :: Maybe UUID -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> MatchOptions -> m [EmailPreview]{- ^ Perform a search of emails in an inbox with the given patterns. If results match expected count then return or else retry the search until results are found or timeout is reached. Match options allow simple CONTAINS or EQUALS filtering on SUBJECT, TO, BCC, CC, and FROM. See the `MatchOptions` object for options. An example payload is `{ matches: [{field: 'SUBJECT',should:'CONTAIN',value:'needle'}] }`. You can use an array of matches and they will be applied sequentially to filter out emails. If you want to perform matches and extractions of content using Regex patterns see the EmailController `getEmailContentMatch` method. -}
  , waitForMatchingFirstEmail :: Maybe UUID -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Integer -> MatchOptions -> m Email{- ^ Perform a search of emails in an inbox with the given patterns. If a result if found then return or else retry the search until a result is found or timeout is reached. Match options allow simple CONTAINS or EQUALS filtering on SUBJECT, TO, BCC, CC, and FROM. See the `MatchOptions` object for options. An example payload is `{ matches: [{field: 'SUBJECT',should:'CONTAIN',value:'needle'}] }`. You can use an array of matches and they will be applied sequentially to filter out emails. If you want to perform matches and extractions of content using Regex patterns see the EmailController `getEmailContentMatch` method. -}
  , waitForNthEmail :: Maybe UUID -> Maybe Int -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Integer -> m Email{- ^ If nth email is already present in inbox then return it. If not hold the connection open until timeout expires or the nth email is received and returned. -}
  , waitForSms :: WaitForSmsConditions -> m [SmsPreview]{- ^ Generic waitFor method that will wait until a phone number meets given conditions or return immediately if already met -}
  , createAccountWebhook :: CreateWebhookOptions -> m WebhookDto{- ^ Get notified of account level events such as bounce and bounce recipient. -}
  , createWebhook :: UUID -> CreateWebhookOptions -> m WebhookDto{- ^ Get notified whenever an inbox receives an email via a WebHook URL. An emailID will be posted to this URL every time an email is received for this inbox. The URL must be publicly reachable by the MailSlurp server. You can provide basicAuth values if you wish to secure this endpoint. -}
  , createWebhookForPhoneNumber :: UUID -> CreateWebhookOptions -> m WebhookDto{- ^ Get notified whenever a phone number receives an SMS via a WebHook URL. -}
  , deleteAllWebhooks :: Maybe UTCTime -> m (){- ^  -}
  , deleteWebhook :: UUID -> UUID -> m (){- ^  -}
  , deleteWebhookById :: UUID -> m (){- ^  -}
  , getAllAccountWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> m PageWebhookProjection{- ^ List account webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getAllWebhookEndpoints :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> Maybe Text -> Maybe Text -> m PageWebhookEndpointProjection{- ^ List webhooks URL in paginated form. Allows for page index, page size, and sort direction. -}
  , getAllWebhookResults :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> m PageWebhookResult{- ^  -}
  , getAllWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> m PageWebhookProjection{- ^ List webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getInboxWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> m PageWebhookProjection{- ^  -}
  , getJsonSchemaForWebhookEvent :: Maybe Text -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload by event -}
  , getJsonSchemaForWebhookPayload :: UUID -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload -}
  , getPhoneNumberWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> m PageWebhookProjection{- ^  -}
  , getTestWebhookPayload :: Maybe Text -> m AbstractWebhookPayload{- ^ Get test webhook payload example. Response content depends on eventName passed. Uses `EMAIL_RECEIVED` as default. -}
  , getTestWebhookPayloadBounce :: m WebhookBouncePayload{- ^ Get webhook test payload for bounce -}
  , getTestWebhookPayloadBounceRecipient :: m WebhookBounceRecipientPayload{- ^ Get webhook test payload for bounce recipient -}
  , getTestWebhookPayloadDeliveryStatus :: m WebhookDeliveryStatusPayload{- ^  -}
  , getTestWebhookPayloadEmailOpened :: m WebhookEmailOpenedPayload{- ^ Get webhook test payload for email opened event -}
  , getTestWebhookPayloadEmailRead :: m WebhookEmailReadPayload{- ^ Get webhook test payload for email opened event -}
  , getTestWebhookPayloadForWebhook :: UUID -> m AbstractWebhookPayload{- ^ Get example payload for webhook -}
  , getTestWebhookPayloadNewAttachment :: m WebhookNewAttachmentPayload{- ^  -}
  , getTestWebhookPayloadNewContact :: m WebhookNewContactPayload{- ^  -}
  , getTestWebhookPayloadNewEmail :: m WebhookNewEmailPayload{- ^  -}
  , getTestWebhookPayloadNewSms :: m WebhookNewSmsPayload{- ^  -}
  , getWebhook :: UUID -> m WebhookDto{- ^  -}
  , getWebhookResult :: UUID -> m WebhookResultDto{- ^  -}
  , getWebhookResults :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> m PageWebhookResult{- ^  -}
  , getWebhookResultsCount :: UUID -> m CountDto{- ^  -}
  , getWebhookResultsUnseenErrorCount :: m UnseenErrorCountDto{- ^  -}
  , getWebhooks :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m [WebhookProjection]{- ^  -}
  , redriveAllWebhookResults :: m WebhookRedriveAllResult{- ^ Allows you to resend webhook payloads for any recorded webhook result that failed to deliver the payload. -}
  , redriveWebhookResult :: UUID -> m WebhookRedriveResult{- ^ Allows you to resend a webhook payload that was already sent. Webhooks that fail are retried automatically for 24 hours and then put in a dead letter queue. You can retry results manually using this method. -}
  , sendTestData :: UUID -> m WebhookTestResult{- ^  -}
  , updateWebhook :: UUID -> Maybe UUID -> Maybe UUID -> Maybe Bool -> CreateWebhookOptions -> m WebhookDto{- ^  -}
  , updateWebhookHeaders :: UUID -> WebhookHeaders -> m WebhookDto{- ^  -}
  , verifyWebhookSignature :: VerifyWebhookSignatureOptions -> m VerifyWebhookSignatureResults{- ^ Verify a webhook payload using the messageId and signature. This allows you to be sure that MailSlurp sent the payload and not another server. -}
  , waitForWebhookResults :: UUID -> Maybe Int -> Maybe Int -> m [WebhookResultDto]{- ^  -}
  }

newtype MailSlurpClient a = MailSlurpClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative MailSlurpClient where
  pure x = MailSlurpClient (\_ -> pure x)
  (MailSlurpClient f) <*> (MailSlurpClient x) =
    MailSlurpClient (\env -> f env <*> x env)

instance Monad MailSlurpClient where
  (MailSlurpClient a) >>= f =
    MailSlurpClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO MailSlurpClient where
  liftIO io = MailSlurpClient (\_ -> liftIO io)

createMailSlurpClient :: MailSlurpBackend MailSlurpClient
createMailSlurpClient = MailSlurpBackend{..}
  where
    ((coerce -> createAlias) :<|>
     (coerce -> deleteAlias) :<|>
     (coerce -> getAlias) :<|>
     (coerce -> getAliasEmails) :<|>
     (coerce -> getAliasThreads) :<|>
     (coerce -> getAliases) :<|>
     (coerce -> getThread) :<|>
     (coerce -> getThreadsPaginated) :<|>
     (coerce -> replyToAliasEmail) :<|>
     (coerce -> sendAliasEmail) :<|>
     (coerce -> updateAlias) :<|>
     (coerce -> deleteAllAttachments) :<|>
     (coerce -> deleteAttachment) :<|>
     (coerce -> downloadAttachmentAsBase64Encoded) :<|>
     (coerce -> downloadAttachmentAsBytes) :<|>
     (coerce -> getAttachment) :<|>
     (coerce -> getAttachmentInfo) :<|>
     (coerce -> getAttachments) :<|>
     (coerce -> uploadAttachment) :<|>
     (coerce -> uploadAttachmentBytes) :<|>
     (coerce -> uploadMultipartForm) :<|>
     (coerce -> filterBouncedRecipient) :<|>
     (coerce -> getAccountBounceBlockStatus) :<|>
     (coerce -> getBouncedEmail) :<|>
     (coerce -> getBouncedEmails) :<|>
     (coerce -> getBouncedRecipient) :<|>
     (coerce -> getBouncedRecipients) :<|>
     (coerce -> getComplaint) :<|>
     (coerce -> getComplaints) :<|>
     (coerce -> getListUnsubscribeRecipients) :<|>
     (coerce -> bulkCreateInboxes) :<|>
     (coerce -> bulkDeleteInboxes) :<|>
     (coerce -> bulkSendEmails) :<|>
     (coerce -> createNewEmailAddress) :<|>
     (coerce -> createRandomInbox) :<|>
     (coerce -> deleteEmailAddress) :<|>
     (coerce -> emptyInbox) :<|>
     (coerce -> sendEmailQuery) :<|>
     (coerce -> sendEmailSimple) :<|>
     (coerce -> createConnector) :<|>
     (coerce -> createConnectorImapConnection) :<|>
     (coerce -> createConnectorSmtpConnection) :<|>
     (coerce -> createConnectorSyncSettings) :<|>
     (coerce -> createConnectorWithOptions) :<|>
     (coerce -> deleteAllConnector) :<|>
     (coerce -> deleteConnector) :<|>
     (coerce -> deleteConnectorImapConnection) :<|>
     (coerce -> deleteConnectorSmtpConnection) :<|>
     (coerce -> deleteConnectorSyncSettings) :<|>
     (coerce -> getAllConnectorEvents) :<|>
     (coerce -> getConnector) :<|>
     (coerce -> getConnectorByEmailAddress) :<|>
     (coerce -> getConnectorByInboxId) :<|>
     (coerce -> getConnectorByName) :<|>
     (coerce -> getConnectorEvent) :<|>
     (coerce -> getConnectorEvents) :<|>
     (coerce -> getConnectorImapConnection) :<|>
     (coerce -> getConnectorProviderSettings) :<|>
     (coerce -> getConnectorSmtpConnection) :<|>
     (coerce -> getConnectorSyncSettings) :<|>
     (coerce -> getConnectors) :<|>
     (coerce -> sendEmailFromConnector) :<|>
     (coerce -> syncConnector) :<|>
     (coerce -> testConnectorImapConnection) :<|>
     (coerce -> testConnectorImapConnectionOptions) :<|>
     (coerce -> testConnectorSmtpConnection) :<|>
     (coerce -> testConnectorSmtpConnectionOptions) :<|>
     (coerce -> updateConnector) :<|>
     (coerce -> updateConnectorImapConnection) :<|>
     (coerce -> updateConnectorSmtpConnection) :<|>
     (coerce -> checkSendingConsentForEmailAddress) :<|>
     (coerce -> getOptInIdentities) :<|>
     (coerce -> revokeOptInConsentForEmailAddress) :<|>
     (coerce -> sendOptInConsentForEmailAddress) :<|>
     (coerce -> createContact) :<|>
     (coerce -> deleteContact) :<|>
     (coerce -> getAllContacts) :<|>
     (coerce -> getContact) :<|>
     (coerce -> getContactVCard) :<|>
     (coerce -> getContacts) :<|>
     (coerce -> addDomainWildcardCatchAll) :<|>
     (coerce -> createDomain) :<|>
     (coerce -> deleteDomain) :<|>
     (coerce -> getAvailableDomains) :<|>
     (coerce -> getDomain) :<|>
     (coerce -> getDomainIssues) :<|>
     (coerce -> getDomainWildcardCatchAllInbox) :<|>
     (coerce -> getDomains) :<|>
     (coerce -> getMailSlurpDomains) :<|>
     (coerce -> updateDomain) :<|>
     (coerce -> applyImapFlagOperation) :<|>
     (coerce -> canSend) :<|>
     (coerce -> checkEmailBody) :<|>
     (coerce -> checkEmailBodyFeatureSupport) :<|>
     (coerce -> checkEmailClientSupport) :<|>
     (coerce -> deleteAllEmails) :<|>
     (coerce -> deleteEmail) :<|>
     (coerce -> downloadAttachment) :<|>
     (coerce -> downloadAttachmentBase64) :<|>
     (coerce -> downloadBody) :<|>
     (coerce -> downloadBodyBytes) :<|>
     (coerce -> forwardEmail) :<|>
     (coerce -> getAttachmentMetaData) :<|>
     (coerce -> getEmail) :<|>
     (coerce -> getEmailAttachments) :<|>
     (coerce -> getEmailContentMatch) :<|>
     (coerce -> getEmailContentPart) :<|>
     (coerce -> getEmailContentPartContent) :<|>
     (coerce -> getEmailCount) :<|>
     (coerce -> getEmailHTML) :<|>
     (coerce -> getEmailHTMLJson) :<|>
     (coerce -> getEmailHTMLQuery) :<|>
     (coerce -> getEmailLinks) :<|>
     (coerce -> getEmailPreviewURLs) :<|>
     (coerce -> getEmailScreenshotAsBase64) :<|>
     (coerce -> getEmailScreenshotAsBinary) :<|>
     (coerce -> getEmailSummary) :<|>
     (coerce -> getEmailTextLines) :<|>
     (coerce -> getEmailThread) :<|>
     (coerce -> getEmailThreadItems) :<|>
     (coerce -> getEmailThreads) :<|>
     (coerce -> getEmailsOffsetPaginated) :<|>
     (coerce -> getEmailsPaginated) :<|>
     (coerce -> getGravatarUrlForEmailAddress) :<|>
     (coerce -> getLatestEmail) :<|>
     (coerce -> getLatestEmailInInbox1) :<|>
     (coerce -> getOrganizationEmailsPaginated) :<|>
     (coerce -> getRawEmailContents) :<|>
     (coerce -> getRawEmailJson) :<|>
     (coerce -> getUnreadEmailCount) :<|>
     (coerce -> markAllAsRead) :<|>
     (coerce -> markAsRead) :<|>
     (coerce -> replyToEmail) :<|>
     (coerce -> searchEmails) :<|>
     (coerce -> sendEmailSourceOptional) :<|>
     (coerce -> setEmailFavourited) :<|>
     (coerce -> validateEmail) :<|>
     (coerce -> deleteAllValidationRequests) :<|>
     (coerce -> deleteValidationRequest) :<|>
     (coerce -> getValidationRequests) :<|>
     (coerce -> validateEmailAddressList) :<|>
     (coerce -> getExpirationDefaults) :<|>
     (coerce -> getExpiredInboxByInboxId) :<|>
     (coerce -> getExpiredInboxRecord) :<|>
     (coerce -> getExpiredInboxes) :<|>
     (coerce -> exportEntities) :<|>
     (coerce -> getExportLink) :<|>
     (coerce -> submitForm) :<|>
     (coerce -> addContactsToGroup) :<|>
     (coerce -> createGroup) :<|>
     (coerce -> deleteGroup) :<|>
     (coerce -> getAllGroups) :<|>
     (coerce -> getGroup) :<|>
     (coerce -> getGroupWithContacts) :<|>
     (coerce -> getGroupWithContactsPaginated) :<|>
     (coerce -> getGroups) :<|>
     (coerce -> removeContactsFromGroup) :<|>
     (coerce -> createGuestPortal) :<|>
     (coerce -> createGuestPortalUser) :<|>
     (coerce -> getAllGuestPortalUsers) :<|>
     (coerce -> getGuestPortal) :<|>
     (coerce -> getGuestPortalUser) :<|>
     (coerce -> getGuestPortalUserById) :<|>
     (coerce -> getGuestPortalUsers) :<|>
     (coerce -> getGuestPortals) :<|>
     (coerce -> imapServerFetch) :<|>
     (coerce -> imapServerGet) :<|>
     (coerce -> imapServerList) :<|>
     (coerce -> imapServerMailbox) :<|>
     (coerce -> imapServerSearch) :<|>
     (coerce -> imapServerStatus) :<|>
     (coerce -> imapServerUpdateFlags) :<|>
     (coerce -> cancelScheduledJob) :<|>
     (coerce -> createInbox) :<|>
     (coerce -> createInboxRuleset) :<|>
     (coerce -> createInboxWithDefaults) :<|>
     (coerce -> createInboxWithOptions) :<|>
     (coerce -> deleteAllInboxEmails) :<|>
     (coerce -> deleteAllInboxes) :<|>
     (coerce -> deleteAllInboxesByDescription) :<|>
     (coerce -> deleteAllInboxesByName) :<|>
     (coerce -> deleteAllInboxesByTag) :<|>
     (coerce -> deleteInbox) :<|>
     (coerce -> doesInboxExist) :<|>
     (coerce -> flushExpired) :<|>
     (coerce -> getAllInboxes) :<|>
     (coerce -> getAllInboxesOffsetPaginated) :<|>
     (coerce -> getAllPlusAddresses) :<|>
     (coerce -> getAllScheduledJobs) :<|>
     (coerce -> getDeliveryStatusesByInboxId) :<|>
     (coerce -> getEmails) :<|>
     (coerce -> getImapAccess) :<|>
     (coerce -> getImapSmtpAccess) :<|>
     (coerce -> getImapSmtpAccessEnv) :<|>
     (coerce -> getImapSmtpAccessServers) :<|>
     (coerce -> getInbox) :<|>
     (coerce -> getInboxByEmailAddress) :<|>
     (coerce -> getInboxByName) :<|>
     (coerce -> getInboxCount) :<|>
     (coerce -> getInboxEmailCount) :<|>
     (coerce -> getInboxEmailsPaginated) :<|>
     (coerce -> getInboxIds) :<|>
     (coerce -> getInboxPlusAddress) :<|>
     (coerce -> getInboxPlusAddressById) :<|>
     (coerce -> getInboxPlusAddressEmails) :<|>
     (coerce -> getInboxPlusAddressEmailsForPlusAddressId) :<|>
     (coerce -> getInboxPlusAddresses) :<|>
     (coerce -> getInboxSentCount) :<|>
     (coerce -> getInboxSentEmails) :<|>
     (coerce -> getInboxTags) :<|>
     (coerce -> getInboxTagsPaginated) :<|>
     (coerce -> getInboxes) :<|>
     (coerce -> getInboxesByTag) :<|>
     (coerce -> getLatestEmailInInbox) :<|>
     (coerce -> getOrganizationInboxes) :<|>
     (coerce -> getOutboxes) :<|>
     (coerce -> getScheduledJob) :<|>
     (coerce -> getScheduledJobsByInboxId) :<|>
     (coerce -> getSmtpAccess) :<|>
     (coerce -> isEmailAddressAvailable) :<|>
     (coerce -> listInboxRulesets) :<|>
     (coerce -> listInboxTrackingPixels) :<|>
     (coerce -> searchInboxes) :<|>
     (coerce -> sendEmail) :<|>
     (coerce -> sendEmailAndConfirm) :<|>
     (coerce -> sendEmailWithQueue) :<|>
     (coerce -> sendSmtpEnvelope) :<|>
     (coerce -> sendTestEmail) :<|>
     (coerce -> sendWithSchedule) :<|>
     (coerce -> setInboxFavourited) :<|>
     (coerce -> updateImapAccess) :<|>
     (coerce -> updateInbox) :<|>
     (coerce -> updateSmtpAccess) :<|>
     (coerce -> createNewInboxForwarder) :<|>
     (coerce -> deleteInboxForwarder) :<|>
     (coerce -> deleteInboxForwarders) :<|>
     (coerce -> getAllInboxForwarderEvents) :<|>
     (coerce -> getForwarderEvent) :<|>
     (coerce -> getInboxForwarder) :<|>
     (coerce -> getInboxForwarderEvent) :<|>
     (coerce -> getInboxForwarderEvents) :<|>
     (coerce -> getInboxForwarders) :<|>
     (coerce -> testInboxForwarder) :<|>
     (coerce -> testInboxForwardersForInbox) :<|>
     (coerce -> testNewInboxForwarder) :<|>
     (coerce -> updateInboxForwarder) :<|>
     (coerce -> createNewInboxReplier) :<|>
     (coerce -> deleteInboxReplier) :<|>
     (coerce -> deleteInboxRepliers) :<|>
     (coerce -> getAllInboxReplierEvents) :<|>
     (coerce -> getInboxReplier) :<|>
     (coerce -> getInboxReplierEvents) :<|>
     (coerce -> getInboxRepliers) :<|>
     (coerce -> updateInboxReplier) :<|>
     (coerce -> createNewInboxRuleset) :<|>
     (coerce -> deleteInboxRuleset) :<|>
     (coerce -> deleteInboxRulesets) :<|>
     (coerce -> getInboxRuleset) :<|>
     (coerce -> getInboxRulesets) :<|>
     (coerce -> testInboxRuleset) :<|>
     (coerce -> testInboxRulesetReceiving) :<|>
     (coerce -> testInboxRulesetSending) :<|>
     (coerce -> testInboxRulesetsForInbox) :<|>
     (coerce -> testNewInboxRuleset) :<|>
     (coerce -> describeMailServerDomain) :<|>
     (coerce -> getDnsLookup) :<|>
     (coerce -> getDnsLookups) :<|>
     (coerce -> getIpAddress) :<|>
     (coerce -> verifyEmailAddress) :<|>
     (coerce -> getAllMissedEmails) :<|>
     (coerce -> getAllUnknownMissedEmails) :<|>
     (coerce -> getMissedEmail) :<|>
     (coerce -> restoreMissedEmails) :<|>
     (coerce -> waitForNthMissedEmail) :<|>
     (coerce -> createEmergencyAddress) :<|>
     (coerce -> deleteEmergencyAddress) :<|>
     (coerce -> deletePhoneNumber) :<|>
     (coerce -> getConsentStatus) :<|>
     (coerce -> getEmergencyAddress) :<|>
     (coerce -> getEmergencyAddresses) :<|>
     (coerce -> getPhoneNumber) :<|>
     (coerce -> getPhoneNumbers) :<|>
     (coerce -> getPhonePlans) :<|>
     (coerce -> getPhonePlansAvailability) :<|>
     (coerce -> getSentSmsByPhoneNumber) :<|>
     (coerce -> getSmsByPhoneNumber) :<|>
     (coerce -> sendSmsFromPhoneNumber) :<|>
     (coerce -> setConsentStatus) :<|>
     (coerce -> setPhoneFavourited) :<|>
     (coerce -> testPhoneNumberSendSms) :<|>
     (coerce -> updatePhoneNumber) :<|>
     (coerce -> validatePhoneNumber) :<|>
     (coerce -> deleteAllSentEmails) :<|>
     (coerce -> deleteSentEmail) :<|>
     (coerce -> getAllSentTrackingPixels) :<|>
     (coerce -> getRawSentEmailContents) :<|>
     (coerce -> getRawSentEmailJson) :<|>
     (coerce -> getSentDeliveryStatus) :<|>
     (coerce -> getSentDeliveryStatuses) :<|>
     (coerce -> getSentDeliveryStatusesBySentId) :<|>
     (coerce -> getSentEmail) :<|>
     (coerce -> getSentEmailHTMLContent) :<|>
     (coerce -> getSentEmailPreviewURLs) :<|>
     (coerce -> getSentEmailTrackingPixels) :<|>
     (coerce -> getSentEmails) :<|>
     (coerce -> getSentEmailsWithQueueResults) :<|>
     (coerce -> getSentOrganizationEmails) :<|>
     (coerce -> waitForDeliveryStatuses) :<|>
     (coerce -> deleteSentSmsMessage) :<|>
     (coerce -> deleteSentSmsMessages) :<|>
     (coerce -> deleteSmsMessage) :<|>
     (coerce -> deleteSmsMessages) :<|>
     (coerce -> getAllSmsMessages) :<|>
     (coerce -> getReplyForSmsMessage) :<|>
     (coerce -> getSentSmsCount) :<|>
     (coerce -> getSentSmsMessage) :<|>
     (coerce -> getSentSmsMessagesPaginated) :<|>
     (coerce -> getSmsCount) :<|>
     (coerce -> getSmsMessage) :<|>
     (coerce -> getUnreadSmsCount) :<|>
     (coerce -> replyToSmsMessage) :<|>
     (coerce -> sendSms) :<|>
     (coerce -> setSmsFavourited) :<|>
     (coerce -> streamEvents) :<|>
     (coerce -> createTemplate) :<|>
     (coerce -> deleteTemplate) :<|>
     (coerce -> getAllTemplates) :<|>
     (coerce -> getTemplate) :<|>
     (coerce -> getTemplatePreviewHtml) :<|>
     (coerce -> getTemplatePreviewJson) :<|>
     (coerce -> getTemplates) :<|>
     (coerce -> updateTemplate) :<|>
     (coerce -> checkEmailFeaturesClientSupport) :<|>
     (coerce -> createNewFakeEmailAddress) :<|>
     (coerce -> deleteNewFakeEmailAddress) :<|>
     (coerce -> generateBimiRecord) :<|>
     (coerce -> generateDmarcRecord) :<|>
     (coerce -> generateMtaStsRecord) :<|>
     (coerce -> generateTlsReportingRecord) :<|>
     (coerce -> getFakeEmailByEmailAddress) :<|>
     (coerce -> getFakeEmailById) :<|>
     (coerce -> getFakeEmailRaw) :<|>
     (coerce -> getFakeEmailsForAddress) :<|>
     (coerce -> lookupBimiDomain) :<|>
     (coerce -> lookupDmarcDomain) :<|>
     (coerce -> lookupMtaStsDomain) :<|>
     (coerce -> lookupTlsReportingDomain) :<|>
     (coerce -> createTrackingPixel) :<|>
     (coerce -> getAllTrackingPixels) :<|>
     (coerce -> getTrackingPixel) :<|>
     (coerce -> getJsonPropertyAsString) :<|>
     (coerce -> getUserInfo) :<|>
     (coerce -> waitFor) :<|>
     (coerce -> waitForEmailCount) :<|>
     (coerce -> waitForLatestEmail) :<|>
     (coerce -> waitForLatestSms) :<|>
     (coerce -> waitForMatchingEmails) :<|>
     (coerce -> waitForMatchingFirstEmail) :<|>
     (coerce -> waitForNthEmail) :<|>
     (coerce -> waitForSms) :<|>
     (coerce -> createAccountWebhook) :<|>
     (coerce -> createWebhook) :<|>
     (coerce -> createWebhookForPhoneNumber) :<|>
     (coerce -> deleteAllWebhooks) :<|>
     (coerce -> deleteWebhook) :<|>
     (coerce -> deleteWebhookById) :<|>
     (coerce -> getAllAccountWebhooks) :<|>
     (coerce -> getAllWebhookEndpoints) :<|>
     (coerce -> getAllWebhookResults) :<|>
     (coerce -> getAllWebhooks) :<|>
     (coerce -> getInboxWebhooksPaginated) :<|>
     (coerce -> getJsonSchemaForWebhookEvent) :<|>
     (coerce -> getJsonSchemaForWebhookPayload) :<|>
     (coerce -> getPhoneNumberWebhooksPaginated) :<|>
     (coerce -> getTestWebhookPayload) :<|>
     (coerce -> getTestWebhookPayloadBounce) :<|>
     (coerce -> getTestWebhookPayloadBounceRecipient) :<|>
     (coerce -> getTestWebhookPayloadDeliveryStatus) :<|>
     (coerce -> getTestWebhookPayloadEmailOpened) :<|>
     (coerce -> getTestWebhookPayloadEmailRead) :<|>
     (coerce -> getTestWebhookPayloadForWebhook) :<|>
     (coerce -> getTestWebhookPayloadNewAttachment) :<|>
     (coerce -> getTestWebhookPayloadNewContact) :<|>
     (coerce -> getTestWebhookPayloadNewEmail) :<|>
     (coerce -> getTestWebhookPayloadNewSms) :<|>
     (coerce -> getWebhook) :<|>
     (coerce -> getWebhookResult) :<|>
     (coerce -> getWebhookResults) :<|>
     (coerce -> getWebhookResultsCount) :<|>
     (coerce -> getWebhookResultsUnseenErrorCount) :<|>
     (coerce -> getWebhooks) :<|>
     (coerce -> redriveAllWebhookResults) :<|>
     (coerce -> redriveWebhookResult) :<|>
     (coerce -> sendTestData) :<|>
     (coerce -> updateWebhook) :<|>
     (coerce -> updateWebhookHeaders) :<|>
     (coerce -> verifyWebhookSignature) :<|>
     (coerce -> waitForWebhookResults) :<|>
     _) = client (Proxy :: Proxy MailSlurpAPI)

-- | Run requests in the MailSlurpClient monad.
runMailSlurpClient :: Config -> MailSlurpClient a -> ExceptT ClientError IO a
runMailSlurpClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runMailSlurpClientWithManager manager clientConfig cl

-- | Run requests in the MailSlurpClient monad using a custom manager.
runMailSlurpClientWithManager :: Manager -> Config -> MailSlurpClient a -> ExceptT ClientError IO a
runMailSlurpClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a MailSlurpClientError
callMailSlurp
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> MailSlurpClient a -> m a
callMailSlurp env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (MailSlurpClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the MailSlurp server at the provided host and port.
runMailSlurpServer
  :: (MonadIO m, MonadThrow m)
  => Config -> MailSlurpBackend (ExceptT ServerError IO) -> m ()
runMailSlurpServer config backend = runMailSlurpMiddlewareServer config requestMiddlewareId backend

-- | Run the MailSlurp server at the provided host and port.
runMailSlurpMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> MailSlurpBackend (ExceptT ServerError IO) -> m ()
runMailSlurpMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serve (Proxy :: Proxy MailSlurpAPI) (serverFromBackend backend)
  where
    serverFromBackend MailSlurpBackend{..} =
      (coerce createAlias :<|>
       coerce deleteAlias :<|>
       coerce getAlias :<|>
       coerce getAliasEmails :<|>
       coerce getAliasThreads :<|>
       coerce getAliases :<|>
       coerce getThread :<|>
       coerce getThreadsPaginated :<|>
       coerce replyToAliasEmail :<|>
       coerce sendAliasEmail :<|>
       coerce updateAlias :<|>
       coerce deleteAllAttachments :<|>
       coerce deleteAttachment :<|>
       coerce downloadAttachmentAsBase64Encoded :<|>
       coerce downloadAttachmentAsBytes :<|>
       coerce getAttachment :<|>
       coerce getAttachmentInfo :<|>
       coerce getAttachments :<|>
       coerce uploadAttachment :<|>
       coerce uploadAttachmentBytes :<|>
       coerce uploadMultipartForm :<|>
       coerce filterBouncedRecipient :<|>
       coerce getAccountBounceBlockStatus :<|>
       coerce getBouncedEmail :<|>
       coerce getBouncedEmails :<|>
       coerce getBouncedRecipient :<|>
       coerce getBouncedRecipients :<|>
       coerce getComplaint :<|>
       coerce getComplaints :<|>
       coerce getListUnsubscribeRecipients :<|>
       coerce bulkCreateInboxes :<|>
       coerce bulkDeleteInboxes :<|>
       coerce bulkSendEmails :<|>
       coerce createNewEmailAddress :<|>
       coerce createRandomInbox :<|>
       coerce deleteEmailAddress :<|>
       coerce emptyInbox :<|>
       coerce sendEmailQuery :<|>
       coerce sendEmailSimple :<|>
       coerce createConnector :<|>
       coerce createConnectorImapConnection :<|>
       coerce createConnectorSmtpConnection :<|>
       coerce createConnectorSyncSettings :<|>
       coerce createConnectorWithOptions :<|>
       coerce deleteAllConnector :<|>
       coerce deleteConnector :<|>
       coerce deleteConnectorImapConnection :<|>
       coerce deleteConnectorSmtpConnection :<|>
       coerce deleteConnectorSyncSettings :<|>
       coerce getAllConnectorEvents :<|>
       coerce getConnector :<|>
       coerce getConnectorByEmailAddress :<|>
       coerce getConnectorByInboxId :<|>
       coerce getConnectorByName :<|>
       coerce getConnectorEvent :<|>
       coerce getConnectorEvents :<|>
       coerce getConnectorImapConnection :<|>
       coerce getConnectorProviderSettings :<|>
       coerce getConnectorSmtpConnection :<|>
       coerce getConnectorSyncSettings :<|>
       coerce getConnectors :<|>
       coerce sendEmailFromConnector :<|>
       coerce syncConnector :<|>
       coerce testConnectorImapConnection :<|>
       coerce testConnectorImapConnectionOptions :<|>
       coerce testConnectorSmtpConnection :<|>
       coerce testConnectorSmtpConnectionOptions :<|>
       coerce updateConnector :<|>
       coerce updateConnectorImapConnection :<|>
       coerce updateConnectorSmtpConnection :<|>
       coerce checkSendingConsentForEmailAddress :<|>
       coerce getOptInIdentities :<|>
       coerce revokeOptInConsentForEmailAddress :<|>
       coerce sendOptInConsentForEmailAddress :<|>
       coerce createContact :<|>
       coerce deleteContact :<|>
       coerce getAllContacts :<|>
       coerce getContact :<|>
       coerce getContactVCard :<|>
       coerce getContacts :<|>
       coerce addDomainWildcardCatchAll :<|>
       coerce createDomain :<|>
       coerce deleteDomain :<|>
       coerce getAvailableDomains :<|>
       coerce getDomain :<|>
       coerce getDomainIssues :<|>
       coerce getDomainWildcardCatchAllInbox :<|>
       coerce getDomains :<|>
       coerce getMailSlurpDomains :<|>
       coerce updateDomain :<|>
       coerce applyImapFlagOperation :<|>
       coerce canSend :<|>
       coerce checkEmailBody :<|>
       coerce checkEmailBodyFeatureSupport :<|>
       coerce checkEmailClientSupport :<|>
       coerce deleteAllEmails :<|>
       coerce deleteEmail :<|>
       coerce downloadAttachment :<|>
       coerce downloadAttachmentBase64 :<|>
       coerce downloadBody :<|>
       coerce downloadBodyBytes :<|>
       coerce forwardEmail :<|>
       coerce getAttachmentMetaData :<|>
       coerce getEmail :<|>
       coerce getEmailAttachments :<|>
       coerce getEmailContentMatch :<|>
       coerce getEmailContentPart :<|>
       coerce getEmailContentPartContent :<|>
       coerce getEmailCount :<|>
       coerce getEmailHTML :<|>
       coerce getEmailHTMLJson :<|>
       coerce getEmailHTMLQuery :<|>
       coerce getEmailLinks :<|>
       coerce getEmailPreviewURLs :<|>
       coerce getEmailScreenshotAsBase64 :<|>
       coerce getEmailScreenshotAsBinary :<|>
       coerce getEmailSummary :<|>
       coerce getEmailTextLines :<|>
       coerce getEmailThread :<|>
       coerce getEmailThreadItems :<|>
       coerce getEmailThreads :<|>
       coerce getEmailsOffsetPaginated :<|>
       coerce getEmailsPaginated :<|>
       coerce getGravatarUrlForEmailAddress :<|>
       coerce getLatestEmail :<|>
       coerce getLatestEmailInInbox1 :<|>
       coerce getOrganizationEmailsPaginated :<|>
       coerce getRawEmailContents :<|>
       coerce getRawEmailJson :<|>
       coerce getUnreadEmailCount :<|>
       coerce markAllAsRead :<|>
       coerce markAsRead :<|>
       coerce replyToEmail :<|>
       coerce searchEmails :<|>
       coerce sendEmailSourceOptional :<|>
       coerce setEmailFavourited :<|>
       coerce validateEmail :<|>
       coerce deleteAllValidationRequests :<|>
       coerce deleteValidationRequest :<|>
       coerce getValidationRequests :<|>
       coerce validateEmailAddressList :<|>
       coerce getExpirationDefaults :<|>
       coerce getExpiredInboxByInboxId :<|>
       coerce getExpiredInboxRecord :<|>
       coerce getExpiredInboxes :<|>
       coerce exportEntities :<|>
       coerce getExportLink :<|>
       coerce submitForm :<|>
       coerce addContactsToGroup :<|>
       coerce createGroup :<|>
       coerce deleteGroup :<|>
       coerce getAllGroups :<|>
       coerce getGroup :<|>
       coerce getGroupWithContacts :<|>
       coerce getGroupWithContactsPaginated :<|>
       coerce getGroups :<|>
       coerce removeContactsFromGroup :<|>
       coerce createGuestPortal :<|>
       coerce createGuestPortalUser :<|>
       coerce getAllGuestPortalUsers :<|>
       coerce getGuestPortal :<|>
       coerce getGuestPortalUser :<|>
       coerce getGuestPortalUserById :<|>
       coerce getGuestPortalUsers :<|>
       coerce getGuestPortals :<|>
       coerce imapServerFetch :<|>
       coerce imapServerGet :<|>
       coerce imapServerList :<|>
       coerce imapServerMailbox :<|>
       coerce imapServerSearch :<|>
       coerce imapServerStatus :<|>
       coerce imapServerUpdateFlags :<|>
       coerce cancelScheduledJob :<|>
       coerce createInbox :<|>
       coerce createInboxRuleset :<|>
       coerce createInboxWithDefaults :<|>
       coerce createInboxWithOptions :<|>
       coerce deleteAllInboxEmails :<|>
       coerce deleteAllInboxes :<|>
       coerce deleteAllInboxesByDescription :<|>
       coerce deleteAllInboxesByName :<|>
       coerce deleteAllInboxesByTag :<|>
       coerce deleteInbox :<|>
       coerce doesInboxExist :<|>
       coerce flushExpired :<|>
       coerce getAllInboxes :<|>
       coerce getAllInboxesOffsetPaginated :<|>
       coerce getAllPlusAddresses :<|>
       coerce getAllScheduledJobs :<|>
       coerce getDeliveryStatusesByInboxId :<|>
       coerce getEmails :<|>
       coerce getImapAccess :<|>
       coerce getImapSmtpAccess :<|>
       coerce getImapSmtpAccessEnv :<|>
       coerce getImapSmtpAccessServers :<|>
       coerce getInbox :<|>
       coerce getInboxByEmailAddress :<|>
       coerce getInboxByName :<|>
       coerce getInboxCount :<|>
       coerce getInboxEmailCount :<|>
       coerce getInboxEmailsPaginated :<|>
       coerce getInboxIds :<|>
       coerce getInboxPlusAddress :<|>
       coerce getInboxPlusAddressById :<|>
       coerce getInboxPlusAddressEmails :<|>
       coerce getInboxPlusAddressEmailsForPlusAddressId :<|>
       coerce getInboxPlusAddresses :<|>
       coerce getInboxSentCount :<|>
       coerce getInboxSentEmails :<|>
       coerce getInboxTags :<|>
       coerce getInboxTagsPaginated :<|>
       coerce getInboxes :<|>
       coerce getInboxesByTag :<|>
       coerce getLatestEmailInInbox :<|>
       coerce getOrganizationInboxes :<|>
       coerce getOutboxes :<|>
       coerce getScheduledJob :<|>
       coerce getScheduledJobsByInboxId :<|>
       coerce getSmtpAccess :<|>
       coerce isEmailAddressAvailable :<|>
       coerce listInboxRulesets :<|>
       coerce listInboxTrackingPixels :<|>
       coerce searchInboxes :<|>
       coerce sendEmail :<|>
       coerce sendEmailAndConfirm :<|>
       coerce sendEmailWithQueue :<|>
       coerce sendSmtpEnvelope :<|>
       coerce sendTestEmail :<|>
       coerce sendWithSchedule :<|>
       coerce setInboxFavourited :<|>
       coerce updateImapAccess :<|>
       coerce updateInbox :<|>
       coerce updateSmtpAccess :<|>
       coerce createNewInboxForwarder :<|>
       coerce deleteInboxForwarder :<|>
       coerce deleteInboxForwarders :<|>
       coerce getAllInboxForwarderEvents :<|>
       coerce getForwarderEvent :<|>
       coerce getInboxForwarder :<|>
       coerce getInboxForwarderEvent :<|>
       coerce getInboxForwarderEvents :<|>
       coerce getInboxForwarders :<|>
       coerce testInboxForwarder :<|>
       coerce testInboxForwardersForInbox :<|>
       coerce testNewInboxForwarder :<|>
       coerce updateInboxForwarder :<|>
       coerce createNewInboxReplier :<|>
       coerce deleteInboxReplier :<|>
       coerce deleteInboxRepliers :<|>
       coerce getAllInboxReplierEvents :<|>
       coerce getInboxReplier :<|>
       coerce getInboxReplierEvents :<|>
       coerce getInboxRepliers :<|>
       coerce updateInboxReplier :<|>
       coerce createNewInboxRuleset :<|>
       coerce deleteInboxRuleset :<|>
       coerce deleteInboxRulesets :<|>
       coerce getInboxRuleset :<|>
       coerce getInboxRulesets :<|>
       coerce testInboxRuleset :<|>
       coerce testInboxRulesetReceiving :<|>
       coerce testInboxRulesetSending :<|>
       coerce testInboxRulesetsForInbox :<|>
       coerce testNewInboxRuleset :<|>
       coerce describeMailServerDomain :<|>
       coerce getDnsLookup :<|>
       coerce getDnsLookups :<|>
       coerce getIpAddress :<|>
       coerce verifyEmailAddress :<|>
       coerce getAllMissedEmails :<|>
       coerce getAllUnknownMissedEmails :<|>
       coerce getMissedEmail :<|>
       coerce restoreMissedEmails :<|>
       coerce waitForNthMissedEmail :<|>
       coerce createEmergencyAddress :<|>
       coerce deleteEmergencyAddress :<|>
       coerce deletePhoneNumber :<|>
       coerce getConsentStatus :<|>
       coerce getEmergencyAddress :<|>
       coerce getEmergencyAddresses :<|>
       coerce getPhoneNumber :<|>
       coerce getPhoneNumbers :<|>
       coerce getPhonePlans :<|>
       coerce getPhonePlansAvailability :<|>
       coerce getSentSmsByPhoneNumber :<|>
       coerce getSmsByPhoneNumber :<|>
       coerce sendSmsFromPhoneNumber :<|>
       coerce setConsentStatus :<|>
       coerce setPhoneFavourited :<|>
       coerce testPhoneNumberSendSms :<|>
       coerce updatePhoneNumber :<|>
       coerce validatePhoneNumber :<|>
       coerce deleteAllSentEmails :<|>
       coerce deleteSentEmail :<|>
       coerce getAllSentTrackingPixels :<|>
       coerce getRawSentEmailContents :<|>
       coerce getRawSentEmailJson :<|>
       coerce getSentDeliveryStatus :<|>
       coerce getSentDeliveryStatuses :<|>
       coerce getSentDeliveryStatusesBySentId :<|>
       coerce getSentEmail :<|>
       coerce getSentEmailHTMLContent :<|>
       coerce getSentEmailPreviewURLs :<|>
       coerce getSentEmailTrackingPixels :<|>
       coerce getSentEmails :<|>
       coerce getSentEmailsWithQueueResults :<|>
       coerce getSentOrganizationEmails :<|>
       coerce waitForDeliveryStatuses :<|>
       coerce deleteSentSmsMessage :<|>
       coerce deleteSentSmsMessages :<|>
       coerce deleteSmsMessage :<|>
       coerce deleteSmsMessages :<|>
       coerce getAllSmsMessages :<|>
       coerce getReplyForSmsMessage :<|>
       coerce getSentSmsCount :<|>
       coerce getSentSmsMessage :<|>
       coerce getSentSmsMessagesPaginated :<|>
       coerce getSmsCount :<|>
       coerce getSmsMessage :<|>
       coerce getUnreadSmsCount :<|>
       coerce replyToSmsMessage :<|>
       coerce sendSms :<|>
       coerce setSmsFavourited :<|>
       coerce streamEvents :<|>
       coerce createTemplate :<|>
       coerce deleteTemplate :<|>
       coerce getAllTemplates :<|>
       coerce getTemplate :<|>
       coerce getTemplatePreviewHtml :<|>
       coerce getTemplatePreviewJson :<|>
       coerce getTemplates :<|>
       coerce updateTemplate :<|>
       coerce checkEmailFeaturesClientSupport :<|>
       coerce createNewFakeEmailAddress :<|>
       coerce deleteNewFakeEmailAddress :<|>
       coerce generateBimiRecord :<|>
       coerce generateDmarcRecord :<|>
       coerce generateMtaStsRecord :<|>
       coerce generateTlsReportingRecord :<|>
       coerce getFakeEmailByEmailAddress :<|>
       coerce getFakeEmailById :<|>
       coerce getFakeEmailRaw :<|>
       coerce getFakeEmailsForAddress :<|>
       coerce lookupBimiDomain :<|>
       coerce lookupDmarcDomain :<|>
       coerce lookupMtaStsDomain :<|>
       coerce lookupTlsReportingDomain :<|>
       coerce createTrackingPixel :<|>
       coerce getAllTrackingPixels :<|>
       coerce getTrackingPixel :<|>
       coerce getJsonPropertyAsString :<|>
       coerce getUserInfo :<|>
       coerce waitFor :<|>
       coerce waitForEmailCount :<|>
       coerce waitForLatestEmail :<|>
       coerce waitForLatestSms :<|>
       coerce waitForMatchingEmails :<|>
       coerce waitForMatchingFirstEmail :<|>
       coerce waitForNthEmail :<|>
       coerce waitForSms :<|>
       coerce createAccountWebhook :<|>
       coerce createWebhook :<|>
       coerce createWebhookForPhoneNumber :<|>
       coerce deleteAllWebhooks :<|>
       coerce deleteWebhook :<|>
       coerce deleteWebhookById :<|>
       coerce getAllAccountWebhooks :<|>
       coerce getAllWebhookEndpoints :<|>
       coerce getAllWebhookResults :<|>
       coerce getAllWebhooks :<|>
       coerce getInboxWebhooksPaginated :<|>
       coerce getJsonSchemaForWebhookEvent :<|>
       coerce getJsonSchemaForWebhookPayload :<|>
       coerce getPhoneNumberWebhooksPaginated :<|>
       coerce getTestWebhookPayload :<|>
       coerce getTestWebhookPayloadBounce :<|>
       coerce getTestWebhookPayloadBounceRecipient :<|>
       coerce getTestWebhookPayloadDeliveryStatus :<|>
       coerce getTestWebhookPayloadEmailOpened :<|>
       coerce getTestWebhookPayloadEmailRead :<|>
       coerce getTestWebhookPayloadForWebhook :<|>
       coerce getTestWebhookPayloadNewAttachment :<|>
       coerce getTestWebhookPayloadNewContact :<|>
       coerce getTestWebhookPayloadNewEmail :<|>
       coerce getTestWebhookPayloadNewSms :<|>
       coerce getWebhook :<|>
       coerce getWebhookResult :<|>
       coerce getWebhookResults :<|>
       coerce getWebhookResultsCount :<|>
       coerce getWebhookResultsUnseenErrorCount :<|>
       coerce getWebhooks :<|>
       coerce redriveAllWebhookResults :<|>
       coerce redriveWebhookResult :<|>
       coerce sendTestData :<|>
       coerce updateWebhook :<|>
       coerce updateWebhookHeaders :<|>
       coerce verifyWebhookSignature :<|>
       coerce waitForWebhookResults :<|>
       serveDirectoryFileServer "static")
