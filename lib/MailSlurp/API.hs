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



data FormUploadMultipartForm = FormUploadMultipartForm
  { uploadMultipartFormFile :: FilePath
  } deriving (Show, Eq, Generic, Data)

instance FromForm FormUploadMultipartForm
instance ToForm FormUploadMultipartForm

data FormSubmitForm = FormSubmitForm
  { submitFormEmailAddress :: Text
  , submitFormRedirectTo :: Text
  , submitFormSpamCheck :: Text
  , submitFormSubject :: Text
  , submitFormSuccessMessage :: Text
  , submitFormTo :: Text
  , submitFormOtherParameters :: Text
  } deriving (Show, Eq, Generic, Data)

instance FromForm FormSubmitForm
instance ToForm FormSubmitForm


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
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getAliasEmails' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "threads" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageThreadProjection -- 'getAliasThreads' route
    :<|> "aliases" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageAlias -- 'getAliases' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToAliasEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> ReqBody '[JSON] UpdateAliasOptions :> Verb 'PUT 200 '[JSON] () -- 'updateAlias' route
    :<|> "attachments" :> ReqBody '[JSON] UploadAttachmentOptions :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachment' route
    :<|> "attachments" :> "bytes" :> QueryParam "String" Text :> QueryParam "filename" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachmentBytes' route
    :<|> "attachments" :> "multipart" :> QueryParam "contentType" Text :> QueryParam "filename" Text :> QueryParam "x-filename" Text :> ReqBody '[FormUrlEncoded] FormUploadMultipartForm :> Verb 'POST 200 '[JSON] [Text] -- 'uploadMultipartForm' route
    :<|> "bulk" :> "inboxes" :> QueryParam "count" Int :> Verb 'POST 200 '[JSON] [Inbox] -- 'bulkCreateInboxes' route
    :<|> "bulk" :> "inboxes" :> ReqBody '[JSON] [UUID] :> Verb 'DELETE 200 '[JSON] () -- 'bulkDeleteInboxes' route
    :<|> "bulk" :> "send" :> ReqBody '[JSON] BulkSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'bulkSendEmails' route
    :<|> "createInbox" :> QueryParam "allowTeamAccess" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "useDomainPool" Bool :> Verb 'POST 200 '[JSON] Inbox -- 'createNewEmailAddress' route
    :<|> "newEmailAddress" :> QueryParam "allowTeamAccess" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "useDomainPool" Bool :> Verb 'POST 200 '[JSON] Inbox -- 'createNewEmailAddress1' route
    :<|> "emptyInbox" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'emptyInbox' route
    :<|> "sendEmail" :> ReqBody '[JSON] SimpleSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailSimple' route
    :<|> "contacts" :> ReqBody '[JSON] CreateContactOptions :> Verb 'POST 200 '[JSON] ContactDto -- 'createContact' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteContact' route
    :<|> "contacts" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageContactProjection -- 'getAllContacts' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'GET 200 '[JSON] ContactDto -- 'getContact' route
    :<|> "contacts" :> Verb 'GET 200 '[JSON] [ContactProjection] -- 'getContacts' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'POST 200 '[JSON] DomainDto -- 'addDomainWildcardCatchAll' route
    :<|> "domains" :> ReqBody '[JSON] CreateDomainOptions :> Verb 'POST 200 '[JSON] DomainDto -- 'createDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] [Text] -- 'deleteDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] DomainDto -- 'getDomain' route
    :<|> "domains" :> Verb 'GET 200 '[JSON] [DomainPreview] -- 'getDomains' route
    :<|> "domains" :> Capture "id" UUID :> ReqBody '[JSON] UpdateDomainOptions :> Verb 'PUT 200 '[JSON] DomainDto -- 'updateDomain' route
    :<|> "emails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllEmails' route
    :<|> "emails" :> Capture "emailId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> QueryParam "apiKey" Text :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachment' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentBase64' route
    :<|> "emails" :> Capture "emailId" UUID :> "forward" :> ReqBody '[JSON] ForwardEmailOptions :> Verb 'POST 200 '[JSON] () -- 'forwardEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentMetaData' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Verb 'GET 200 '[JSON] [AttachmentMetaData] -- 'getAttachments' route
    :<|> "emails" :> Capture "emailId" UUID :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] Email -- 'getEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "contentMatch" :> ReqBody '[JSON] ContentMatchOptions :> Verb 'POST 200 '[JSON] EmailContentMatchResult -- 'getEmailContentMatch' route
    :<|> "emails" :> Capture "emailId" UUID :> "html" :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] Text -- 'getEmailHTML' route
    :<|> "emails" :> Capture "emailId" UUID :> "htmlQuery" :> QueryParam "htmlSelector" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailHTMLQuery' route
    :<|> "emails" :> Capture "emailId" UUID :> "textLines" :> QueryParam "decodeHtmlEntities" Bool :> QueryParam "lineSeparator" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailTextLines' route
    :<|> "emails" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getEmailsPaginated' route
    :<|> "emails" :> "latest" :> QueryParam "inboxIds" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmail' route
    :<|> "emails" :> "latestIn" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmailInInbox' route
    :<|> "emails" :> "organization" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getOrganizationEmailsPaginated' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> Verb 'GET 200 '[JSON] Text -- 'getRawEmailContents' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> "json" :> Verb 'GET 200 '[JSON] RawEmailJson -- 'getRawEmailJson' route
    :<|> "emails" :> "unreadCount" :> Verb 'GET 200 '[JSON] UnreadCount -- 'getUnreadEmailCount' route
    :<|> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "validate" :> Verb 'POST 200 '[JSON] ValidationDto -- 'validateEmail' route
    :<|> "expired" :> "defaults" :> Verb 'GET 200 '[JSON] ExpirationDefaults -- 'getExpirationDefaults' route
    :<|> "expired" :> "inbox" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxByInboxId' route
    :<|> "expired" :> Capture "expiredId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxRecord' route
    :<|> "expired" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageExpiredInboxRecordProjection -- 'getExpiredInboxes' route
    :<|> "forms" :> ReqBody '[FormUrlEncoded] FormSubmitForm :> Verb 'POST 200 '[JSON] Text -- 'submitForm' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> ReqBody '[JSON] UpdateGroupContacts :> Verb 'PUT 200 '[JSON] GroupContactsDto -- 'addContactsToGroup' route
    :<|> "groups" :> ReqBody '[JSON] CreateGroupOptions :> Verb 'POST 200 '[JSON] GroupDto -- 'createGroup' route
    :<|> "groups" :> Capture "groupId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteGroup' route
    :<|> "groups" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageGroupProjection -- 'getAllGroups' route
    :<|> "groups" :> Capture "groupId" UUID :> Verb 'GET 200 '[JSON] GroupDto -- 'getGroup' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> Verb 'GET 200 '[JSON] GroupContactsDto -- 'getGroupWithContacts' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts-paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageContactProjection -- 'getGroupWithContactsPaginated' route
    :<|> "groups" :> Verb 'GET 200 '[JSON] [GroupProjection] -- 'getGroups' route
    :<|> "groups" :> Capture "groupId" UUID :> "contacts" :> ReqBody '[JSON] UpdateGroupContacts :> Verb 'DELETE 200 '[JSON] GroupContactsDto -- 'removeContactsFromGroup' route
    :<|> "inboxes" :> QueryParam "allowTeamAccess" Bool :> QueryParam "description" Text :> QueryParam "emailAddress" Text :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "favourite" Bool :> QueryParam "name" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "useDomainPool" Bool :> Verb 'POST 200 '[JSON] Inbox -- 'createInbox' route
    :<|> "inboxes" :> "withOptions" :> ReqBody '[JSON] CreateInboxDto :> Verb 'POST 200 '[JSON] Inbox -- 'createInboxWithOptions' route
    :<|> "inboxes" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxes' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInbox' route
    :<|> "inboxes" :> "paginated" :> QueryParam "favourite" Bool :> QueryParam "page" Int :> QueryParam "search" Text :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "tag" Text :> QueryParam "teamAccess" Bool :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getAllInboxes' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> QueryParam "limit" Int :> QueryParam "minCount" Integer :> QueryParam "retryTimeout" Integer :> QueryParam "since" UTCTime :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] [EmailPreview] -- 'getEmails' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] Inbox -- 'getInbox' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageEmailPreview -- 'getInboxEmailsPaginated' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "sent" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getInboxSentEmails' route
    :<|> "inboxes" :> "tags" :> Verb 'GET 200 '[JSON] [Text] -- 'getInboxTags' route
    :<|> "inboxes" :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] [Inbox] -- 'getInboxes' route
    :<|> "inboxes" :> "organization" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageOrganizationInboxProjection -- 'getOrganizationInboxes' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "confirm" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendEmailAndConfirm' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "favourite" :> ReqBody '[JSON] SetInboxFavouritedOptions :> Verb 'PUT 200 '[JSON] Inbox -- 'setInboxFavourited' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] UpdateInboxOptions :> Verb 'PATCH 200 '[JSON] Inbox -- 'updateInbox' route
    :<|> "mail-server" :> "describe" :> "domain" :> ReqBody '[JSON] DescribeDomainOptions :> Verb 'POST 200 '[JSON] DescribeMailServerDomainResult -- 'describeMailServerDomain' route
    :<|> "mail-server" :> "describe" :> "dns-lookup" :> ReqBody '[JSON] DNSLookupOptions :> Verb 'POST 200 '[JSON] DNSLookupResults -- 'getDnsLookup' route
    :<|> "mail-server" :> "describe" :> "ip-address" :> QueryParam "name" Text :> Verb 'POST 200 '[JSON] IPAddressResult -- 'getIpAddress' route
    :<|> "mail-server" :> "verify" :> "email-address" :> ReqBody '[JSON] VerifyEmailAddressOptions :> Verb 'POST 200 '[JSON] EmailVerificationResult -- 'verifyEmailAddress' route
    :<|> "missed-emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageMissedEmailProjection -- 'getAllMissedEmails' route
    :<|> "missed-emails" :> Capture "MissedEmailId" UUID :> Verb 'GET 200 '[JSON] MissedEmail -- 'getMissedEmail' route
    :<|> "sent" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] SentEmailDto -- 'getSentEmail' route
    :<|> "sent" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getSentEmails' route
    :<|> "sent" :> "organization" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getSentOrganizationEmails' route
    :<|> "templates" :> ReqBody '[JSON] CreateTemplateOptions :> Verb 'POST 200 '[JSON] TemplateDto -- 'createTemplate' route
    :<|> "templates" :> Capture "TemplateId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteTemplate' route
    :<|> "templates" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageTemplateProjection -- 'getAllTemplates' route
    :<|> "templates" :> Capture "TemplateId" UUID :> Verb 'GET 200 '[JSON] TemplateDto -- 'getTemplate' route
    :<|> "templates" :> Verb 'GET 200 '[JSON] [TemplateProjection] -- 'getTemplates' route
    :<|> "waitFor" :> ReqBody '[JSON] WaitForConditions :> Verb 'POST 200 '[JSON] [EmailPreview] -- 'waitFor' route
    :<|> "waitForEmailCount" :> QueryParam "count" Int :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> Verb 'GET 200 '[JSON] [EmailPreview] -- 'waitForEmailCount' route
    :<|> "waitForLatestEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> Verb 'GET 200 '[JSON] Email -- 'waitForLatestEmail' route
    :<|> "waitForMatchingEmails" :> QueryParam "count" Int :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> ReqBody '[JSON] MatchOptions :> Verb 'POST 200 '[JSON] [EmailPreview] -- 'waitForMatchingEmail' route
    :<|> "waitForMatchingFirstEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> ReqBody '[JSON] MatchOptions :> Verb 'POST 200 '[JSON] Email -- 'waitForMatchingFirstEmail' route
    :<|> "waitForNthEmail" :> QueryParam "inboxId" UUID :> QueryParam "index" Int :> QueryParam "timeout" Integer :> QueryParam "unreadOnly" Bool :> Verb 'GET 200 '[JSON] Email -- 'waitForNthEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createWebhook' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> Capture "webhookId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteWebhook' route
    :<|> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllWebhooks' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> Verb 'GET 200 '[JSON] WebhookDto -- 'getWebhook' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> Verb 'GET 200 '[JSON] [WebhookDto] -- 'getWebhooks' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "test" :> Verb 'POST 200 '[JSON] WebhookTestResult -- 'sendTestData' route
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
  , getAliasEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageEmailProjection{- ^ Get paginated emails for an alias by ID -}
  , getAliasThreads :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageThreadProjection{- ^ Returns threads created for an email alias in paginated form -}
  , getAliases :: Maybe Int -> Maybe Int -> Maybe Text -> m PageAlias{- ^ Get all email aliases in paginated form -}
  , replyToAliasEmail :: UUID -> UUID -> ReplyToAliasEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , sendAliasEmail :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Send an email from an alias. Replies to the email will be forwarded to the alias masked email address -}
  , updateAlias :: UUID -> UpdateAliasOptions -> m (){- ^  -}
  , uploadAttachment :: UploadAttachmentOptions -> m [Text]{- ^ Email attachments are essentially files with meta data. Files are byte arrays and the meta data is a content type and a filename. These properties allow email clients to display the filename and icon etc. When sending emails with attachments first upload each attachment with an upload endpoint. Record the returned attachment ID and use it with subsequent email sending. For legacy reasons the ID is returned as the first element in an array. Only a single ID is ever returned. To send the attachments pass a list of attachment IDs with `SendEmailOptions` when sending an email. Using the upload endpoints prior to sending mean attachments can easily be reused. -}
  , uploadAttachmentBytes :: Maybe Text -> Maybe Text -> Text -> m [Text]{- ^ Email attachments are essentially files with meta data. Files are byte arrays and the meta data is a content type and a filename. These properties allow email clients to display the filename and icon etc. When sending emails with attachments first upload each attachment with an upload endpoint. Record the returned attachment ID and use it with subsequent email sending. For legacy reasons the ID is returned as the first element in an array. Only a single ID is ever returned. To send the attachments pass a list of attachment IDs with `SendEmailOptions` when sending an email. Using the upload endpoints prior to sending mean attachments can easily be reused. -}
  , uploadMultipartForm :: Maybe Text -> Maybe Text -> Maybe Text -> FormUploadMultipartForm -> m [Text]{- ^ Email attachments are essentially files with meta data. Files are byte arrays and the meta data is a content type and a filename. These properties allow email clients to display the filename and icon etc. When sending emails with attachments first upload each attachment with an upload endpoint. Record the returned attachment ID and use it with subsequent email sending. For legacy reasons the ID is returned as the first element in an array. Only a single ID is ever returned. To send the attachments pass a list of attachment IDs with `SendEmailOptions` when sending an email. Using the upload endpoints prior to sending mean attachments can easily be reused. -}
  , bulkCreateInboxes :: Maybe Int -> m [Inbox]{- ^  -}
  , bulkDeleteInboxes :: [UUID] -> m (){- ^  -}
  , bulkSendEmails :: BulkSendEmailOptions -> m (){- ^  -}
  , createNewEmailAddress :: Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> m Inbox{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , createNewEmailAddress1 :: Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> m Inbox{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , emptyInbox :: Maybe UUID -> m (){- ^ Deletes all emails -}
  , sendEmailSimple :: SimpleSendEmailOptions -> m (){- ^ If no senderId or inboxId provided a random email address will be used to send from. -}
  , createContact :: CreateContactOptions -> m ContactDto{- ^  -}
  , deleteContact :: UUID -> m (){- ^  -}
  , getAllContacts :: Maybe Int -> Maybe Int -> Maybe Text -> m PageContactProjection{- ^  -}
  , getContact :: UUID -> m ContactDto{- ^  -}
  , getContacts :: m [ContactProjection]{- ^  -}
  , addDomainWildcardCatchAll :: UUID -> m DomainDto{- ^ Add a catch all inbox to a domain so that any emails sent to it that cannot be matched will be sent to the catch all inbox generated -}
  , createDomain :: CreateDomainOptions -> m DomainDto{- ^ Link a domain that you own with MailSlurp so you can create email addresses using it. Endpoint returns DNS records used for validation. You must add these verification records to your host provider's DNS setup to verify the domain. -}
  , deleteDomain :: UUID -> m [Text]{- ^ Delete a domain. This will disable any existing inboxes that use this domain. -}
  , getDomain :: UUID -> m DomainDto{- ^ Returns domain verification status and tokens for a given domain -}
  , getDomains :: m [DomainPreview]{- ^ List all custom domains you have created -}
  , updateDomain :: UUID -> UpdateDomainOptions -> m DomainDto{- ^ Update values on a domain. Note you cannot change the domain name as it is immutable. Recreate the domain if you need to alter this. -}
  , deleteAllEmails :: m (){- ^ Deletes all emails in your account. Be careful as emails cannot be recovered -}
  , deleteEmail :: UUID -> m (){- ^ Deletes an email and removes it from the inbox. Deleted emails cannot be recovered. -}
  , downloadAttachment :: UUID -> Text -> Maybe Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , downloadAttachmentBase64 :: UUID -> Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , forwardEmail :: UUID -> ForwardEmailOptions -> m (){- ^ Forward an existing email to new recipients. -}
  , getAttachmentMetaData :: UUID -> Text -> m AttachmentMetaData{- ^ Returns the metadata such as name and content-type for a given attachment and email. -}
  , getAttachments :: UUID -> m [AttachmentMetaData]{- ^ Returns an array of attachment metadata such as name and content-type for a given email if present. -}
  , getEmail :: UUID -> Maybe Bool -> m Email{- ^ Returns a email summary object with headers and content. To retrieve the raw unparsed email use the getRawEmail endpoints -}
  , getEmailContentMatch :: UUID -> ContentMatchOptions -> m EmailContentMatchResult{- ^ Return the matches for a given Java style regex pattern. Do not include the typical `/` at start or end of regex in some languages. Given an example `your code is: 12345` the pattern to extract match looks like `code is: (\\d{6})`. This will return an array of matches with the first matching the entire pattern and the subsequent matching the groups: `['code is: 123456', '123456']` See https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html for more information of available patterns.  -}
  , getEmailHTML :: UUID -> Maybe Bool -> m Text{- ^ Retrieve email content as HTML response for viewing in browsers. Decodes quoted-printable entities and converts charset to UTF-8. Pass your API KEY as a request parameter when viewing in a browser: `?apiKey=xxx` -}
  , getEmailHTMLQuery :: UUID -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of text. HTML parsing uses JSoup which supports JQuery/CSS style selectors -}
  , getEmailTextLines :: UUID -> Maybe Bool -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of strings. HTML parsing uses JSoup and UNIX line separators. -}
  , getEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> m PageEmailProjection{- ^ By default returns all emails across all inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getLatestEmail :: Maybe [UUID] -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getLatestEmailInInbox :: Maybe UUID -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getOrganizationEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> m PageEmailProjection{- ^ By default returns all emails across all team inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getRawEmailContents :: UUID -> m Text{- ^ Returns a raw, unparsed, and unprocessed email. If your client has issues processing the response it is likely due to the response content-type which is text/plain. If you need a JSON response content-type use the getRawEmailJson endpoint -}
  , getRawEmailJson :: UUID -> m RawEmailJson{- ^ Returns a raw, unparsed, and unprocessed email wrapped in a JSON response object for easier handling when compared with the getRawEmail text/plain response -}
  , getUnreadEmailCount :: m UnreadCount{- ^ Get number of emails unread -}
  , replyToEmail :: UUID -> ReplyToEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , validateEmail :: UUID -> m ValidationDto{- ^ Validate the HTML content of email if HTML is found. Considered valid if no HTML. -}
  , getExpirationDefaults :: m ExpirationDefaults{- ^ Return default times used for inbox expiration -}
  , getExpiredInboxByInboxId :: UUID -> m ExpiredInboxDto{- ^ Use the inboxId to return an ExpiredInboxRecord if an inbox has expired. Inboxes expire and are disabled if an expiration date is set or plan requires. Returns 404 if no expired inbox is found for the inboxId -}
  , getExpiredInboxRecord :: UUID -> m ExpiredInboxDto{- ^ Inboxes created with an expiration date will expire after the given date and be moved to an ExpiredInbox entity. You can still read emails in the inbox but it can no longer send or receive emails. Fetch the expired inboxes to view the old inboxes properties -}
  , getExpiredInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> m PageExpiredInboxRecordProjection{- ^ Inboxes created with an expiration date will expire after the given date. An ExpiredInboxRecord is created that records the inboxes old ID and email address. You can still read emails in the inbox (using the inboxes old ID) but the email address associated with the inbox can no longer send or receive emails. Fetch expired inbox records to view the old inboxes properties -}
  , submitForm :: FormSubmitForm -> m Text{- ^ This endpoint allows you to submit HTML forms and receive the field values and files via email.   #### Parameters The endpoint looks for special meta parameters in the form fields OR in the URL request parameters. The meta parameters can be used to specify the behaviour of the email.   You must provide at-least a `_to` email address to tell the endpoint where the form should be emailed. These can be submitted as hidden HTML input fields with the corresponding `name` attributes or as URL query parameters such as `?_to=test@example.com`  The endpoint takes all other form fields that are named and includes them in the message body of the email. Files are sent as attachments.  #### Submitting This endpoint accepts form submission via POST method. It accepts `application/x-www-form-urlencoded`, and `multipart/form-data` content-types.  #### HTML Example ```html <form    action=\"https://api.mailslurp.com/forms\"   method=\"post\" >   <input name=\"_to\" type=\"hidden\" value=\"test@example.com\"/>   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```  #### URL Example ```html <form    action=\"https://api.mailslurp.com/forms?_to=test@example.com\"   method=\"post\" >   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```    The email address is specified by a `_to` field OR is extracted from an email alias specified by a `_toAlias` field (see the alias controller for more information).  Endpoint accepts .  You can specify a content type in HTML forms using the `enctype` attribute, for instance: `<form enctype=\"multipart/form-data\">`.   -}
  , addContactsToGroup :: UUID -> UpdateGroupContacts -> m GroupContactsDto{- ^  -}
  , createGroup :: CreateGroupOptions -> m GroupDto{- ^  -}
  , deleteGroup :: UUID -> m (){- ^  -}
  , getAllGroups :: Maybe Int -> Maybe Int -> Maybe Text -> m PageGroupProjection{- ^  -}
  , getGroup :: UUID -> m GroupDto{- ^  -}
  , getGroupWithContacts :: UUID -> m GroupContactsDto{- ^  -}
  , getGroupWithContactsPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageContactProjection{- ^  -}
  , getGroups :: m [GroupProjection]{- ^  -}
  , removeContactsFromGroup :: UUID -> UpdateGroupContacts -> m GroupContactsDto{- ^  -}
  , createInbox :: Maybe Bool -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> Maybe Text -> Maybe [Text] -> Maybe Bool -> m Inbox{- ^ Create a new inbox and with a randomized email address to send and receive from. Pass emailAddress parameter if you wish to use a specific email address. Creating an inbox is required before sending or receiving emails. If writing tests it is recommended that you create a new inbox during each test method so that it is unique and empty.  -}
  , createInboxWithOptions :: CreateInboxDto -> m Inbox{- ^ Additional endpoint that allows inbox creation with request body options. Can be more flexible that other methods for some clients. -}
  , deleteAllInboxes :: m (){- ^ Permanently delete all inboxes and associated email addresses. This will also delete all emails within the inboxes. Be careful as inboxes cannot be recovered once deleted. Note: deleting inboxes will not impact your usage limits. Monthly inbox creation limits are based on how many inboxes were created in the last 30 days, not how many inboxes you currently have. -}
  , deleteInbox :: UUID -> m (){- ^ Permanently delete an inbox and associated email address as well as all emails within the given inbox. This action cannot be undone. Note: deleting an inbox will not affect your account usage. Monthly inbox usage is based on how many inboxes you create within 30 days, not how many exist at time of request. -}
  , getAllInboxes :: Maybe Bool -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> m PageInboxProjection{- ^ List inboxes in paginated form. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time). You Can also filter by whether an inbox is favorited or use email address pattern. This method is the recommended way to query inboxes. The alternative `getInboxes` method returns a full list of inboxes but is limited to 100 results. Results do not include team access inboxes by default. Use organization method to list team inboxes or set `teamAccess` to true. -}
  , getEmails :: UUID -> Maybe Int -> Maybe Integer -> Maybe Integer -> Maybe UTCTime -> Maybe Int -> Maybe Text -> m [EmailPreview]{- ^ List emails that an inbox has received. Only emails that are sent to the inbox's email address will appear in the inbox. It may take several seconds for any email you send to an inbox's email address to appear in the inbox. To make this endpoint wait for a minimum number of emails use the `minCount` parameter. The server will retry the inbox database until the `minCount` is satisfied or the `retryTimeout` is reached -}
  , getInbox :: UUID -> m Inbox{- ^ Returns an inbox's properties, including its email address and ID. -}
  , getInboxEmailsPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageEmailPreview{- ^ Get a paginated list of emails in an inbox. Does not hold connections open. -}
  , getInboxSentEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageSentEmailProjection{- ^ Returns an inbox's sent email receipts. Call individual sent email endpoints for more details. Note for privacy reasons the full body of sent emails is never stored. An MD5 hash hex is available for comparison instead. -}
  , getInboxTags :: m [Text]{- ^ Get all inbox tags -}
  , getInboxes :: Maybe Int -> Maybe Text -> m [Inbox]{- ^ List the inboxes you have created. Note use of the more advanced `getAllEmails` is recommended. You can provide a limit and sort parameter. -}
  , getOrganizationInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> m PageOrganizationInboxProjection{- ^ List organization inboxes in paginated form. These are inboxes created with `allowTeamAccess` flag enabled. Organization inboxes are `readOnly` for non-admin users. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time).  -}
  , sendEmail :: UUID -> SendEmailOptions -> m (){- ^ Send an email from an inbox's email address.  The request body should contain the `SendEmailOptions` that include recipients, attachments, body etc. See `SendEmailOptions` for all available properties. Note the `inboxId` refers to the inbox's id not the inbox's email address. See https://www.mailslurp.com/guides/ for more information on how to send emails. This method does not return a sent email entity due to legacy reasons. To send and get a sent email as returned response use the sister method `sendEmailAndConfirm`. -}
  , sendEmailAndConfirm :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Sister method for standard `sendEmail` method with the benefit of returning a `SentEmail` entity confirming the successful sending of the email with link the the sent object created for it. -}
  , setInboxFavourited :: UUID -> SetInboxFavouritedOptions -> m Inbox{- ^ Set and return new favourite state for an inbox -}
  , updateInbox :: UUID -> UpdateInboxOptions -> m Inbox{- ^ Update editable fields on an inbox -}
  , describeMailServerDomain :: DescribeDomainOptions -> m DescribeMailServerDomainResult{- ^  -}
  , getDnsLookup :: DNSLookupOptions -> m DNSLookupResults{- ^  -}
  , getIpAddress :: Maybe Text -> m IPAddressResult{- ^  -}
  , verifyEmailAddress :: VerifyEmailAddressOptions -> m EmailVerificationResult{- ^  -}
  , getAllMissedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> m PageMissedEmailProjection{- ^  -}
  , getMissedEmail :: UUID -> m MissedEmail{- ^  -}
  , getSentEmail :: UUID -> m SentEmailDto{- ^  -}
  , getSentEmails :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageSentEmailProjection{- ^  -}
  , getSentOrganizationEmails :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageSentEmailProjection{- ^  -}
  , createTemplate :: CreateTemplateOptions -> m TemplateDto{- ^  -}
  , deleteTemplate :: UUID -> m (){- ^  -}
  , getAllTemplates :: Maybe Int -> Maybe Int -> Maybe Text -> m PageTemplateProjection{- ^  -}
  , getTemplate :: UUID -> m TemplateDto{- ^  -}
  , getTemplates :: m [TemplateProjection]{- ^  -}
  , waitFor :: WaitForConditions -> m [EmailPreview]{- ^ Generic waitFor method that will wait until an inbox meets given conditions or return immediately if already met -}
  , waitForEmailCount :: Maybe Int -> Maybe UUID -> Maybe Integer -> Maybe Bool -> m [EmailPreview]{- ^ If inbox contains count or more emails at time of request then return count worth of emails. If not wait until the count is reached and return those or return an error if timeout is exceeded. -}
  , waitForLatestEmail :: Maybe UUID -> Maybe Integer -> Maybe Bool -> m Email{- ^ Will return either the last received email or wait for an email to arrive and return that. If you need to wait for an email for a non-empty inbox set `unreadOnly=true` or see the other receive methods such as `waitForNthEmail` or `waitForEmailCount`. -}
  , waitForMatchingEmail :: Maybe Int -> Maybe UUID -> Maybe Integer -> Maybe Bool -> MatchOptions -> m [EmailPreview]{- ^ Perform a search of emails in an inbox with the given patterns. If results match expected count then return or else retry the search until results are found or timeout is reached. Match options allow simple CONTAINS or EQUALS filtering on SUBJECT, TO, BCC, CC, and FROM. See the `MatchOptions` object for options. An example payload is `{ matches: [{field: 'SUBJECT',should:'CONTAIN',value:'needle'}] }`. You can use an array of matches and they will be applied sequentially to filter out emails. If you want to perform matches and extractions of content using Regex patterns see the EmailController `getEmailContentMatch` method. -}
  , waitForMatchingFirstEmail :: Maybe UUID -> Maybe Integer -> Maybe Bool -> MatchOptions -> m Email{- ^ Perform a search of emails in an inbox with the given patterns. If a result if found then return or else retry the search until a result is found or timeout is reached. Match options allow simple CONTAINS or EQUALS filtering on SUBJECT, TO, BCC, CC, and FROM. See the `MatchOptions` object for options. An example payload is `{ matches: [{field: 'SUBJECT',should:'CONTAIN',value:'needle'}] }`. You can use an array of matches and they will be applied sequentially to filter out emails. If you want to perform matches and extractions of content using Regex patterns see the EmailController `getEmailContentMatch` method. -}
  , waitForNthEmail :: Maybe UUID -> Maybe Int -> Maybe Integer -> Maybe Bool -> m Email{- ^ If nth email is already present in inbox then return it. If not hold the connection open until timeout expires or the nth email is received and returned. -}
  , createWebhook :: UUID -> CreateWebhookOptions -> m WebhookDto{- ^ Get notified whenever an inbox receives an email via a WebHook URL. An emailID will be posted to this URL every time an email is received for this inbox. The URL must be publicly reachable by the MailSlurp server. You can provide basicAuth values if you wish to secure this endpoint. -}
  , deleteWebhook :: UUID -> UUID -> m (){- ^  -}
  , getAllWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> m PageWebhookProjection{- ^ List webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getWebhook :: UUID -> m WebhookDto{- ^  -}
  , getWebhooks :: UUID -> m [WebhookDto]{- ^  -}
  , sendTestData :: UUID -> m WebhookTestResult{- ^  -}
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
     (coerce -> replyToAliasEmail) :<|>
     (coerce -> sendAliasEmail) :<|>
     (coerce -> updateAlias) :<|>
     (coerce -> uploadAttachment) :<|>
     (coerce -> uploadAttachmentBytes) :<|>
     (coerce -> uploadMultipartForm) :<|>
     (coerce -> bulkCreateInboxes) :<|>
     (coerce -> bulkDeleteInboxes) :<|>
     (coerce -> bulkSendEmails) :<|>
     (coerce -> createNewEmailAddress) :<|>
     (coerce -> createNewEmailAddress1) :<|>
     (coerce -> emptyInbox) :<|>
     (coerce -> sendEmailSimple) :<|>
     (coerce -> createContact) :<|>
     (coerce -> deleteContact) :<|>
     (coerce -> getAllContacts) :<|>
     (coerce -> getContact) :<|>
     (coerce -> getContacts) :<|>
     (coerce -> addDomainWildcardCatchAll) :<|>
     (coerce -> createDomain) :<|>
     (coerce -> deleteDomain) :<|>
     (coerce -> getDomain) :<|>
     (coerce -> getDomains) :<|>
     (coerce -> updateDomain) :<|>
     (coerce -> deleteAllEmails) :<|>
     (coerce -> deleteEmail) :<|>
     (coerce -> downloadAttachment) :<|>
     (coerce -> downloadAttachmentBase64) :<|>
     (coerce -> forwardEmail) :<|>
     (coerce -> getAttachmentMetaData) :<|>
     (coerce -> getAttachments) :<|>
     (coerce -> getEmail) :<|>
     (coerce -> getEmailContentMatch) :<|>
     (coerce -> getEmailHTML) :<|>
     (coerce -> getEmailHTMLQuery) :<|>
     (coerce -> getEmailTextLines) :<|>
     (coerce -> getEmailsPaginated) :<|>
     (coerce -> getLatestEmail) :<|>
     (coerce -> getLatestEmailInInbox) :<|>
     (coerce -> getOrganizationEmailsPaginated) :<|>
     (coerce -> getRawEmailContents) :<|>
     (coerce -> getRawEmailJson) :<|>
     (coerce -> getUnreadEmailCount) :<|>
     (coerce -> replyToEmail) :<|>
     (coerce -> validateEmail) :<|>
     (coerce -> getExpirationDefaults) :<|>
     (coerce -> getExpiredInboxByInboxId) :<|>
     (coerce -> getExpiredInboxRecord) :<|>
     (coerce -> getExpiredInboxes) :<|>
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
     (coerce -> createInbox) :<|>
     (coerce -> createInboxWithOptions) :<|>
     (coerce -> deleteAllInboxes) :<|>
     (coerce -> deleteInbox) :<|>
     (coerce -> getAllInboxes) :<|>
     (coerce -> getEmails) :<|>
     (coerce -> getInbox) :<|>
     (coerce -> getInboxEmailsPaginated) :<|>
     (coerce -> getInboxSentEmails) :<|>
     (coerce -> getInboxTags) :<|>
     (coerce -> getInboxes) :<|>
     (coerce -> getOrganizationInboxes) :<|>
     (coerce -> sendEmail) :<|>
     (coerce -> sendEmailAndConfirm) :<|>
     (coerce -> setInboxFavourited) :<|>
     (coerce -> updateInbox) :<|>
     (coerce -> describeMailServerDomain) :<|>
     (coerce -> getDnsLookup) :<|>
     (coerce -> getIpAddress) :<|>
     (coerce -> verifyEmailAddress) :<|>
     (coerce -> getAllMissedEmails) :<|>
     (coerce -> getMissedEmail) :<|>
     (coerce -> getSentEmail) :<|>
     (coerce -> getSentEmails) :<|>
     (coerce -> getSentOrganizationEmails) :<|>
     (coerce -> createTemplate) :<|>
     (coerce -> deleteTemplate) :<|>
     (coerce -> getAllTemplates) :<|>
     (coerce -> getTemplate) :<|>
     (coerce -> getTemplates) :<|>
     (coerce -> waitFor) :<|>
     (coerce -> waitForEmailCount) :<|>
     (coerce -> waitForLatestEmail) :<|>
     (coerce -> waitForMatchingEmail) :<|>
     (coerce -> waitForMatchingFirstEmail) :<|>
     (coerce -> waitForNthEmail) :<|>
     (coerce -> createWebhook) :<|>
     (coerce -> deleteWebhook) :<|>
     (coerce -> getAllWebhooks) :<|>
     (coerce -> getWebhook) :<|>
     (coerce -> getWebhooks) :<|>
     (coerce -> sendTestData) :<|>
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
       coerce replyToAliasEmail :<|>
       coerce sendAliasEmail :<|>
       coerce updateAlias :<|>
       coerce uploadAttachment :<|>
       coerce uploadAttachmentBytes :<|>
       coerce uploadMultipartForm :<|>
       coerce bulkCreateInboxes :<|>
       coerce bulkDeleteInboxes :<|>
       coerce bulkSendEmails :<|>
       coerce createNewEmailAddress :<|>
       coerce createNewEmailAddress1 :<|>
       coerce emptyInbox :<|>
       coerce sendEmailSimple :<|>
       coerce createContact :<|>
       coerce deleteContact :<|>
       coerce getAllContacts :<|>
       coerce getContact :<|>
       coerce getContacts :<|>
       coerce addDomainWildcardCatchAll :<|>
       coerce createDomain :<|>
       coerce deleteDomain :<|>
       coerce getDomain :<|>
       coerce getDomains :<|>
       coerce updateDomain :<|>
       coerce deleteAllEmails :<|>
       coerce deleteEmail :<|>
       coerce downloadAttachment :<|>
       coerce downloadAttachmentBase64 :<|>
       coerce forwardEmail :<|>
       coerce getAttachmentMetaData :<|>
       coerce getAttachments :<|>
       coerce getEmail :<|>
       coerce getEmailContentMatch :<|>
       coerce getEmailHTML :<|>
       coerce getEmailHTMLQuery :<|>
       coerce getEmailTextLines :<|>
       coerce getEmailsPaginated :<|>
       coerce getLatestEmail :<|>
       coerce getLatestEmailInInbox :<|>
       coerce getOrganizationEmailsPaginated :<|>
       coerce getRawEmailContents :<|>
       coerce getRawEmailJson :<|>
       coerce getUnreadEmailCount :<|>
       coerce replyToEmail :<|>
       coerce validateEmail :<|>
       coerce getExpirationDefaults :<|>
       coerce getExpiredInboxByInboxId :<|>
       coerce getExpiredInboxRecord :<|>
       coerce getExpiredInboxes :<|>
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
       coerce createInbox :<|>
       coerce createInboxWithOptions :<|>
       coerce deleteAllInboxes :<|>
       coerce deleteInbox :<|>
       coerce getAllInboxes :<|>
       coerce getEmails :<|>
       coerce getInbox :<|>
       coerce getInboxEmailsPaginated :<|>
       coerce getInboxSentEmails :<|>
       coerce getInboxTags :<|>
       coerce getInboxes :<|>
       coerce getOrganizationInboxes :<|>
       coerce sendEmail :<|>
       coerce sendEmailAndConfirm :<|>
       coerce setInboxFavourited :<|>
       coerce updateInbox :<|>
       coerce describeMailServerDomain :<|>
       coerce getDnsLookup :<|>
       coerce getIpAddress :<|>
       coerce verifyEmailAddress :<|>
       coerce getAllMissedEmails :<|>
       coerce getMissedEmail :<|>
       coerce getSentEmail :<|>
       coerce getSentEmails :<|>
       coerce getSentOrganizationEmails :<|>
       coerce createTemplate :<|>
       coerce deleteTemplate :<|>
       coerce getAllTemplates :<|>
       coerce getTemplate :<|>
       coerce getTemplates :<|>
       coerce waitFor :<|>
       coerce waitForEmailCount :<|>
       coerce waitForLatestEmail :<|>
       coerce waitForMatchingEmail :<|>
       coerce waitForMatchingFirstEmail :<|>
       coerce waitForNthEmail :<|>
       coerce createWebhook :<|>
       coerce deleteWebhook :<|>
       coerce getAllWebhooks :<|>
       coerce getWebhook :<|>
       coerce getWebhooks :<|>
       coerce sendTestData :<|>
       serveDirectoryFileServer "static")