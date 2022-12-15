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
    :<|> "aliases" :> Capture "aliasId" UUID :> "threads" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageThreadProjection -- 'getAliasThreads' route
    :<|> "aliases" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageAlias -- 'getAliases' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToAliasEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> "emails" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendAliasEmail' route
    :<|> "aliases" :> Capture "aliasId" UUID :> ReqBody '[JSON] UpdateAliasOptions :> Verb 'PUT 200 '[JSON] AliasDto -- 'updateAlias' route
    :<|> "user" :> "json" :> "pluck" :> QueryParam "property" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] Text -- 'getJsonPropertyAsString' route
    :<|> "user" :> "info" :> Verb 'GET 200 '[JSON] UserInfoDto -- 'getUserInfo' route
    :<|> "attachments" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllAttachments' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentAsBase64Encoded' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "bytes" :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachmentAsBytes' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'GET 200 '[JSON] AttachmentEntity -- 'getAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentInfo' route
    :<|> "attachments" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "fileNameFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageAttachmentEntity -- 'getAttachments' route
    :<|> "attachments" :> ReqBody '[JSON] UploadAttachmentOptions :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachment' route
    :<|> "attachments" :> "bytes" :> QueryParam "filename" Text :> ReqBody '[JSON] InlineObject1 :> Header "contentType" Text :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachmentBytes' route
    :<|> "attachments" :> "multipart" :> QueryParam "contentType" Text :> QueryParam "filename" Text :> QueryParam "x-filename" Text :> ReqBody '[JSON] InlineObject :> Verb 'POST 200 '[JSON] [Text] -- 'uploadMultipartForm' route
    :<|> "bounce" :> "filter-recipients" :> ReqBody '[JSON] FilterBouncedRecipientsOptions :> Verb 'POST 200 '[JSON] FilterBouncedRecipientsResult -- 'filterBouncedRecipient' route
    :<|> "bounce" :> "emails" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedEmailDto -- 'getBouncedEmail' route
    :<|> "bounce" :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedEmail -- 'getBouncedEmails' route
    :<|> "bounce" :> "recipients" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedRecipientDto -- 'getBouncedRecipient' route
    :<|> "bounce" :> "recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedRecipients -- 'getBouncedRecipients' route
    :<|> "bounce" :> "complaints" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageComplaint -- 'getComplaints' route
    :<|> "bounce" :> "list-unsubscribe-recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageListUnsubscribeRecipients -- 'getListUnsubscribeRecipients' route
    :<|> "bulk" :> "inboxes" :> QueryParam "count" Int :> Verb 'POST 200 '[JSON] [InboxDto] -- 'bulkCreateInboxes' route
    :<|> "bulk" :> "inboxes" :> ReqBody '[JSON] [UUID] :> Verb 'DELETE 200 '[JSON] () -- 'bulkDeleteInboxes' route
    :<|> "bulk" :> "send" :> ReqBody '[JSON] BulkSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'bulkSendEmails' route
    :<|> "newEmailAddress" :> QueryParam "allowTeamAccess" Bool :> QueryParam "useDomainPool" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "emailAddress" Text :> QueryParam "inboxType" Text :> QueryParam "description" Text :> QueryParam "name" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "favourite" Bool :> QueryParam "virtualInbox" Bool :> Verb 'POST 200 '[JSON] InboxDto -- 'createNewEmailAddress' route
    :<|> "createInbox" :> QueryParam "allowTeamAccess" Bool :> QueryParam "useDomainPool" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "emailAddress" Text :> QueryParam "inboxType" Text :> QueryParam "description" Text :> QueryParam "name" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "favourite" Bool :> QueryParam "virtualInbox" Bool :> Verb 'POST 200 '[JSON] InboxDto -- 'createRandomInbox' route
    :<|> "deleteEmailAddress" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteEmailAddress' route
    :<|> "emptyInbox" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'emptyInbox' route
    :<|> "sendEmail" :> ReqBody '[JSON] SimpleSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailSimple' route
    :<|> "contacts" :> ReqBody '[JSON] CreateContactOptions :> Verb 'POST 200 '[JSON] ContactDto -- 'createContact' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteContact' route
    :<|> "contacts" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageContactProjection -- 'getAllContacts' route
    :<|> "contacts" :> Capture "contactId" UUID :> Verb 'GET 200 '[JSON] ContactDto -- 'getContact' route
    :<|> "contacts" :> Capture "contactId" UUID :> "download" :> Verb 'GET 200 '[JSON] [Text] -- 'getContactVCard' route
    :<|> "contacts" :> Verb 'GET 200 '[JSON] [ContactProjection] -- 'getContacts' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'POST 200 '[JSON] DomainDto -- 'addDomainWildcardCatchAll' route
    :<|> "domains" :> ReqBody '[JSON] CreateDomainOptions :> Verb 'POST 200 '[JSON] DomainDto -- 'createDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] [Text] -- 'deleteDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] DomainDto -- 'getDomain' route
    :<|> "domains" :> Verb 'GET 200 '[JSON] [DomainPreview] -- 'getDomains' route
    :<|> "domains" :> Capture "id" UUID :> ReqBody '[JSON] UpdateDomainOptions :> Verb 'PUT 200 '[JSON] DomainDto -- 'updateDomain' route
    :<|> "emails" :> Capture "emailId" UUID :> "imap-flag-operation" :> ReqBody '[JSON] ImapFlagOperationOptions :> Verb 'POST 200 '[JSON] EmailPreview -- 'applyImapFlagOperation' route
    :<|> "emails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllEmails' route
    :<|> "emails" :> Capture "emailId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> QueryParam "apiKey" Text :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachment' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentBase64' route
    :<|> "emails" :> Capture "emailId" UUID :> "body" :> Verb 'GET 200 '[JSON] Text -- 'downloadBody' route
    :<|> "emails" :> Capture "emailId" UUID :> "body-bytes" :> Verb 'GET 200 '[JSON] Text -- 'downloadBodyBytes' route
    :<|> "emails" :> Capture "emailId" UUID :> "forward" :> ReqBody '[JSON] ForwardEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'forwardEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentMetaData' route
    :<|> "emails" :> Capture "emailId" UUID :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] Email -- 'getEmail' route
    :<|> "emails" :> Capture "emailId" UUID :> "attachments" :> Verb 'GET 200 '[JSON] [AttachmentMetaData] -- 'getEmailAttachments' route
    :<|> "emails" :> Capture "emailId" UUID :> "contentMatch" :> ReqBody '[JSON] ContentMatchOptions :> Verb 'POST 200 '[JSON] EmailContentMatchResult -- 'getEmailContentMatch' route
    :<|> "emails" :> "emails" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getEmailCount' route
    :<|> "emails" :> Capture "emailId" UUID :> "html" :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] Text -- 'getEmailHTML' route
    :<|> "emails" :> Capture "emailId" UUID :> "html" :> "json" :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] EmailHtmlDto -- 'getEmailHTMLJson' route
    :<|> "emails" :> Capture "emailId" UUID :> "htmlQuery" :> QueryParam "htmlSelector" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailHTMLQuery' route
    :<|> "emails" :> Capture "emailId" UUID :> "links" :> Verb 'GET 200 '[JSON] EmailLinksResult -- 'getEmailLinks' route
    :<|> "emails" :> Capture "emailId" UUID :> "urls" :> Verb 'GET 200 '[JSON] EmailPreviewUrls -- 'getEmailPreviewURLs' route
    :<|> "emails" :> Capture "emailId" UUID :> "textLines" :> QueryParam "decodeHtmlEntities" Bool :> QueryParam "lineSeparator" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailTextLines' route
    :<|> "emails" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getEmailsPaginated' route
    :<|> "emails" :> "gravatarFor" :> QueryParam "emailAddress" Text :> QueryParam "size" Text :> Verb 'GET 200 '[JSON] GravatarUrl -- 'getGravatarUrlForEmailAddress' route
    :<|> "emails" :> "latest" :> QueryParam "inboxIds" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmail' route
    :<|> "emails" :> "latestIn" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmailInInbox1' route
    :<|> "emails" :> "organization" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getOrganizationEmailsPaginated' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> Verb 'GET 200 '[JSON] Text -- 'getRawEmailContents' route
    :<|> "emails" :> Capture "emailId" UUID :> "raw" :> "json" :> Verb 'GET 200 '[JSON] RawEmailJson -- 'getRawEmailJson' route
    :<|> "emails" :> "unreadCount" :> Verb 'GET 200 '[JSON] UnreadCount -- 'getUnreadEmailCount' route
    :<|> "emails" :> Capture "emailId" UUID :> "read" :> QueryParam "read" Bool :> Verb 'PATCH 200 '[JSON] EmailPreview -- 'markAsRead' route
    :<|> "emails" :> Capture "emailId" UUID :> ReqBody '[JSON] ReplyToEmailOptions :> Verb 'PUT 200 '[JSON] SentEmailDto -- 'replyToEmail' route
    :<|> "emails" :> QueryParam "inboxId" UUID :> QueryParam "useDomainPool" Bool :> QueryParam "virtualSend" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailSourceOptional' route
    :<|> "emails" :> Capture "emailId" UUID :> "validate" :> Verb 'POST 200 '[JSON] ValidationDto -- 'validateEmail' route
    :<|> "email-verification" :> "validation-requests" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "isValid" Bool :> Verb 'GET 200 '[JSON] PageEmailValidationRequest -- 'getValidationRequests' route
    :<|> "email-verification" :> "email-address-list" :> ReqBody '[JSON] ValidateEmailAddressListOptions :> Verb 'POST 200 '[JSON] ValidateEmailAddressListResult -- 'validateEmailAddressList' route
    :<|> "expired" :> "defaults" :> Verb 'GET 200 '[JSON] ExpirationDefaults -- 'getExpirationDefaults' route
    :<|> "expired" :> "inbox" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxByInboxId' route
    :<|> "expired" :> Capture "expiredId" UUID :> Verb 'GET 200 '[JSON] ExpiredInboxDto -- 'getExpiredInboxRecord' route
    :<|> "expired" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageExpiredInboxRecordProjection -- 'getExpiredInboxes' route
    :<|> "export" :> QueryParam "exportType" Text :> QueryParam "apiKey" Text :> QueryParam "outputFormat" Text :> QueryParam "filter" Text :> QueryParam "listSeparatorToken" Text :> QueryParam "excludePreviouslyExported" Bool :> QueryParam "createdEarliestTime" UTCTime :> QueryParam "createdOldestTime" UTCTime :> Verb 'GET 200 '[JSON] [Text] -- 'exportEntities' route
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
    :<|> "inboxes" :> "scheduled-jobs" :> Capture "jobId" UUID :> Verb 'DELETE 200 '[JSON] ScheduledJobDto -- 'cancelScheduledJob' route
    :<|> "inboxes" :> QueryParam "emailAddress" Text :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> QueryParam "name" Text :> QueryParam "description" Text :> QueryParam "useDomainPool" Bool :> QueryParam "favourite" Bool :> QueryParam "expiresAt" UTCTime :> QueryParam "expiresIn" Integer :> QueryParam "allowTeamAccess" Bool :> QueryParam "inboxType" Text :> QueryParam "virtualInbox" Bool :> Verb 'POST 200 '[JSON] InboxDto -- 'createInbox' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> ReqBody '[JSON] CreateInboxRulesetOptions :> Verb 'POST 200 '[JSON] InboxRulesetDto -- 'createInboxRuleset' route
    :<|> "inboxes" :> "withDefaults" :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithDefaults' route
    :<|> "inboxes" :> "withOptions" :> ReqBody '[JSON] CreateInboxDto :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithOptions' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "deleteAllInboxEmails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxEmails' route
    :<|> "inboxes" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxes' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInbox' route
    :<|> "inboxes" :> "exists" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] InboxExistsDto -- 'doesInboxExist' route
    :<|> "inboxes" :> "expired" :> QueryParam "before" UTCTime :> Verb 'DELETE 200 '[JSON] FlushExpiredInboxesResult -- 'flushExpired' route
    :<|> "inboxes" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "favourite" Bool :> QueryParam "search" Text :> QueryParam "tag" Text :> QueryParam "teamAccess" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxType" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getAllInboxes' route
    :<|> "inboxes" :> "scheduled-jobs" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageScheduledJobs -- 'getAllScheduledJobs' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "delivery-status" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageDeliveryStatus -- 'getDeliveryStatusesByInboxId' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> QueryParam "size" Int :> QueryParam "limit" Int :> QueryParam "sort" Text :> QueryParam "retryTimeout" Integer :> QueryParam "delayTimeout" Integer :> QueryParam "minCount" Integer :> QueryParam "unreadOnly" Bool :> QueryParam "before" UTCTime :> QueryParam "since" UTCTime :> Verb 'GET 200 '[JSON] [EmailPreview] -- 'getEmails' route
    :<|> "inboxes" :> "imap-smtp-access" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] ImapSmtpAccessDetails -- 'getImapSmtpAccess' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'GET 200 '[JSON] InboxDto -- 'getInbox' route
    :<|> "inboxes" :> "byEmailAddress" :> QueryParam "emailAddress" Text :> Verb 'GET 200 '[JSON] InboxByEmailAddressResult -- 'getInboxByEmailAddress' route
    :<|> "inboxes" :> "byName" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] InboxByNameResult -- 'getInboxByName' route
    :<|> "inboxes" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getInboxCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getInboxEmailCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "emails" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailPreview -- 'getInboxEmailsPaginated' route
    :<|> "inboxes" :> "ids" :> Verb 'GET 200 '[JSON] InboxIdsResult -- 'getInboxIds' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "sent" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSentEmailProjection -- 'getInboxSentEmails' route
    :<|> "inboxes" :> "tags" :> Verb 'GET 200 '[JSON] [Text] -- 'getInboxTags' route
    :<|> "inboxes" :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] [InboxDto] -- 'getInboxes' route
    :<|> "inboxes" :> "getLatestEmail" :> QueryParam "inboxId" UUID :> QueryParam "timeoutMillis" Integer :> Verb 'GET 200 '[JSON] Email -- 'getLatestEmailInInbox' route
    :<|> "inboxes" :> "organization" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageOrganizationInboxProjection -- 'getOrganizationInboxes' route
    :<|> "inboxes" :> "scheduled-jobs" :> Capture "jobId" UUID :> Verb 'GET 200 '[JSON] ScheduledJobDto -- 'getScheduledJob' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "scheduled-jobs" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageScheduledJobs -- 'getScheduledJobsByInboxId' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxRulesetDto -- 'listInboxRulesets' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "tracking-pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'listInboxTrackingPixels' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "confirm" :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendEmailAndConfirm' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "with-queue" :> QueryParam "validateBeforeEnqueue" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'sendEmailWithQueue' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "smtp-envelope" :> ReqBody '[JSON] SendSMTPEnvelopeOptions :> Verb 'POST 200 '[JSON] SentEmailDto -- 'sendSmtpEnvelope' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "send-test-email" :> Verb 'POST 200 '[JSON] () -- 'sendTestEmail' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "with-schedule" :> QueryParam "sendAtTimestamp" UTCTime :> QueryParam "sendAtNowPlusSeconds" Integer :> QueryParam "validateBeforeEnqueue" Bool :> ReqBody '[JSON] SendEmailOptions :> Verb 'POST 200 '[JSON] ScheduledJobDto -- 'sendWithSchedule' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "favourite" :> ReqBody '[JSON] SetInboxFavouritedOptions :> Verb 'PUT 200 '[JSON] InboxDto -- 'setInboxFavourited' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> ReqBody '[JSON] UpdateInboxOptions :> Verb 'PATCH 200 '[JSON] InboxDto -- 'updateInbox' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateInboxForwarderOptions :> Verb 'POST 200 '[JSON] InboxForwarderDto -- 'createNewInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxForwarder' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxForwarders' route
    :<|> "forwarders" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] InboxForwarderDto -- 'getInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxForwarderEvents -- 'getInboxForwarderEvents' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxForwarderDto -- 'getInboxForwarders' route
    :<|> "forwarders" :> Capture "id" UUID :> "test" :> ReqBody '[JSON] InboxForwarderTestOptions :> Verb 'POST 200 '[JSON] InboxForwarderTestResult -- 'testInboxForwarder' route
    :<|> "forwarders" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] InboxForwarderTestOptions :> Verb 'PUT 200 '[JSON] InboxForwarderTestResult -- 'testInboxForwardersForInbox' route
    :<|> "forwarders" :> ReqBody '[JSON] TestNewInboxForwarderOptions :> Verb 'PATCH 200 '[JSON] InboxForwarderTestResult -- 'testNewInboxForwarder' route
    :<|> "forwarders" :> Capture "id" UUID :> ReqBody '[JSON] CreateInboxForwarderOptions :> Verb 'PUT 200 '[JSON] InboxForwarderDto -- 'updateInboxForwarder' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] CreateInboxRulesetOptions :> Verb 'POST 200 '[JSON] InboxRulesetDto -- 'createNewInboxRuleset' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInboxRulesets' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] InboxRulesetDto -- 'getInboxRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageInboxRulesetDto -- 'getInboxRulesets' route
    :<|> "rulesets" :> Capture "id" UUID :> "test" :> ReqBody '[JSON] InboxRulesetTestOptions :> Verb 'POST 200 '[JSON] InboxRulesetTestResult -- 'testInboxRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] InboxRulesetTestOptions :> Verb 'PUT 200 '[JSON] InboxRulesetTestResult -- 'testInboxRulesetsForInbox' route
    :<|> "rulesets" :> ReqBody '[JSON] TestNewInboxRulesetOptions :> Verb 'PATCH 200 '[JSON] InboxRulesetTestResult -- 'testNewInboxRuleset' route
    :<|> "mail-server" :> "describe" :> "domain" :> ReqBody '[JSON] DescribeDomainOptions :> Verb 'POST 200 '[JSON] DescribeMailServerDomainResult -- 'describeMailServerDomain' route
    :<|> "mail-server" :> "describe" :> "dns-lookup" :> ReqBody '[JSON] DNSLookupOptions :> Verb 'POST 200 '[JSON] DNSLookupResults -- 'getDnsLookup' route
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
    :<|> "phone" :> "emergency-addresses" :> Capture "addressId" UUID :> Verb 'GET 200 '[JSON] EmergencyAddress -- 'getEmergencyAddress' route
    :<|> "phone" :> "emergency-addresses" :> Verb 'GET 200 '[JSON] [EmergencyAddressDto] -- 'getEmergencyAddresses' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'getPhoneNumber' route
    :<|> "phone" :> "numbers" :> QueryParam "phoneCountry" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PagePhoneNumberProjection -- 'getPhoneNumbers' route
    :<|> "phone" :> "plans" :> Verb 'GET 200 '[JSON] [PhonePlanDto] -- 'getPhonePlans' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "test" :> ReqBody '[JSON] TestPhoneNumberOptions :> Header "x-test-id" Text :> Verb 'POST 200 '[JSON] () -- 'testPhoneNumberSendSms' route
    :<|> "sent" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllSentEmails' route
    :<|> "sent" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSentEmail' route
    :<|> "sent" :> "tracking-pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getAllSentTrackingPixels' route
    :<|> "sent" :> Capture "emailId" UUID :> "raw" :> Verb 'GET 200 '[JSON] Text -- 'getRawSentEmailContents' route
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
    :<|> "sms" :> Capture "smsId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSmsMessage' route
    :<|> "sms" :> QueryParam "phoneNumberId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteSmsMessages' route
    :<|> "sms" :> Capture "smsId" UUID :> Verb 'GET 200 '[JSON] SmsDto -- 'getSmsMessage' route
    :<|> "sms" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageSmsProjection -- 'getSmsMessagesPaginated' route
    :<|> "templates" :> ReqBody '[JSON] CreateTemplateOptions :> Verb 'POST 200 '[JSON] TemplateDto -- 'createTemplate' route
    :<|> "templates" :> Capture "templateId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteTemplate' route
    :<|> "templates" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTemplateProjection -- 'getAllTemplates' route
    :<|> "templates" :> Capture "templateId" UUID :> Verb 'GET 200 '[JSON] TemplateDto -- 'getTemplate' route
    :<|> "templates" :> Capture "templateId" UUID :> "preview" :> "html" :> Verb 'GET 200 '[JSON] Text -- 'getTemplatePreviewHtml' route
    :<|> "templates" :> Capture "templateId" UUID :> "preview" :> "json" :> Verb 'GET 200 '[JSON] TemplatePreview -- 'getTemplatePreviewJson' route
    :<|> "templates" :> Verb 'GET 200 '[JSON] [TemplateProjection] -- 'getTemplates' route
    :<|> "templates" :> Capture "templateId" UUID :> ReqBody '[JSON] CreateTemplateOptions :> Verb 'PUT 200 '[JSON] TemplateDto -- 'updateTemplate' route
    :<|> "tracking" :> "pixels" :> ReqBody '[JSON] CreateTrackingPixelOptions :> Verb 'POST 200 '[JSON] TrackingPixelDto -- 'createTrackingPixel' route
    :<|> "tracking" :> "pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getAllTrackingPixels' route
    :<|> "tracking" :> "pixels" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] TrackingPixelDto -- 'getTrackingPixel' route
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
    :<|> "webhooks" :> "account" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "eventType" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllAccountWebhooks' route
    :<|> "webhooks" :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getAllWebhookResults' route
    :<|> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllWebhooks' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getInboxWebhooksPaginated' route
    :<|> "webhooks" :> "schema" :> QueryParam "event" Text :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookEvent' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "schema" :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookPayload' route
    :<|> "phone" :> "numbers" :> Capture "phoneId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getPhoneNumberWebhooksPaginated' route
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
    :<|> "webhooks" :> Capture "webhookId" UUID :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getWebhookResults' route
    :<|> "webhooks" :> "results" :> "unseen-count" :> Verb 'GET 200 '[JSON] UnseenErrorCountDto -- 'getWebhookResultsUnseenErrorCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> Verb 'GET 200 '[JSON] [WebhookDto] -- 'getWebhooks' route
    :<|> "webhooks" :> "results" :> Capture "webhookResultId" UUID :> "redrive" :> Verb 'POST 200 '[JSON] WebhookRedriveResult -- 'redriveWebhookResult' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "test" :> Verb 'POST 200 '[JSON] WebhookTestResult -- 'sendTestData' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "headers" :> ReqBody '[JSON] WebhookHeaders :> Verb 'PUT 200 '[JSON] WebhookDto -- 'updateWebhookHeaders' route
    :<|> "webhooks" :> "verify" :> ReqBody '[JSON] VerifyWebhookSignatureOptions :> Verb 'POST 200 '[JSON] VerifyWebhookSignatureResults -- 'verifyWebhookSignature' route
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
  , getAliasThreads :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageThreadProjection{- ^ Returns threads created for an email alias in paginated form -}
  , getAliases :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageAlias{- ^ Get all email aliases in paginated form -}
  , replyToAliasEmail :: UUID -> UUID -> ReplyToAliasEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , sendAliasEmail :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Send an email from an alias. Replies to the email will be forwarded to the alias masked email address -}
  , updateAlias :: UUID -> UpdateAliasOptions -> m AliasDto{- ^  -}
  , getJsonPropertyAsString :: Maybe Text -> Value -> m Text{- ^ Utility function to extract properties from JSON objects in language where this is cumbersome. -}
  , getUserInfo :: m UserInfoDto{- ^  -}
  , deleteAllAttachments :: m (){- ^  -}
  , deleteAttachment :: Text -> m (){- ^  -}
  , downloadAttachmentAsBase64Encoded :: Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , downloadAttachmentAsBytes :: Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , getAttachment :: Text -> m AttachmentEntity{- ^  -}
  , getAttachmentInfo :: Text -> m AttachmentMetaData{- ^ Returns the metadata for an attachment. It is saved separately to the content of the attachment. Contains properties `name` and `content-type` and `content-length` in bytes for a given attachment. -}
  , getAttachments :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageAttachmentEntity{- ^ Get all attachments in paginated response. Each entity contains meta data for the attachment such as `name` and `content-type`. Use the `attachmentId` and the download endpoints to get the file contents. -}
  , uploadAttachment :: UploadAttachmentOptions -> m [Text]{- ^  -}
  , uploadAttachmentBytes :: Maybe Text -> InlineObject1 -> Maybe Text -> m [Text]{- ^  -}
  , uploadMultipartForm :: Maybe Text -> Maybe Text -> Maybe Text -> InlineObject -> m [Text]{- ^  -}
  , filterBouncedRecipient :: FilterBouncedRecipientsOptions -> m FilterBouncedRecipientsResult{- ^ Prevent email sending errors by remove recipients who have resulted in past email bounces or complaints -}
  , getBouncedEmail :: UUID -> m BouncedEmailDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedEmail{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipient :: UUID -> m BouncedRecipientDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedRecipients{- ^ Bounced recipients are email addresses that you have sent emails to that did not accept the sent email. Once a recipient is bounced you cannot send emails to that address. -}
  , getComplaints :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageComplaint{- ^ SMTP complaints made against your account -}
  , getListUnsubscribeRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UUID -> m PageListUnsubscribeRecipients{- ^ Unsubscribed recipient have unsubscribed from a mailing list for a user or domain and cannot be contacted again. -}
  , bulkCreateInboxes :: Maybe Int -> m [InboxDto]{- ^  -}
  , bulkDeleteInboxes :: [UUID] -> m (){- ^  -}
  , bulkSendEmails :: BulkSendEmailOptions -> m (){- ^  -}
  , createNewEmailAddress :: Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> m InboxDto{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , createRandomInbox :: Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Text] -> Maybe Bool -> Maybe Bool -> m InboxDto{- ^ Returns an Inbox with an `id` and an `emailAddress` -}
  , deleteEmailAddress :: Maybe UUID -> m (){- ^ Deletes inbox email address -}
  , emptyInbox :: Maybe UUID -> m (){- ^ Deletes all emails -}
  , sendEmailSimple :: SimpleSendEmailOptions -> m (){- ^ If no senderId or inboxId provided a random email address will be used to send from. -}
  , createContact :: CreateContactOptions -> m ContactDto{- ^  -}
  , deleteContact :: UUID -> m (){- ^  -}
  , getAllContacts :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageContactProjection{- ^  -}
  , getContact :: UUID -> m ContactDto{- ^  -}
  , getContactVCard :: UUID -> m [Text]{- ^  -}
  , getContacts :: m [ContactProjection]{- ^  -}
  , addDomainWildcardCatchAll :: UUID -> m DomainDto{- ^ Add a catch all inbox to a domain so that any emails sent to it that cannot be matched will be sent to the catch all inbox generated -}
  , createDomain :: CreateDomainOptions -> m DomainDto{- ^ Link a domain that you own with MailSlurp so you can create email addresses using it. Endpoint returns DNS records used for validation. You must add these verification records to your host provider's DNS setup to verify the domain. -}
  , deleteDomain :: UUID -> m [Text]{- ^ Delete a domain. This will disable any existing inboxes that use this domain. -}
  , getDomain :: UUID -> m DomainDto{- ^ Returns domain verification status and tokens for a given domain -}
  , getDomains :: m [DomainPreview]{- ^ List all custom domains you have created -}
  , updateDomain :: UUID -> UpdateDomainOptions -> m DomainDto{- ^ Update values on a domain. Note you cannot change the domain name as it is immutable. Recreate the domain if you need to alter this. -}
  , applyImapFlagOperation :: UUID -> ImapFlagOperationOptions -> m EmailPreview{- ^ Apply RFC3501 section-2.3.2 IMAP flag operations on an email -}
  , deleteAllEmails :: m (){- ^ Deletes all emails in your account. Be careful as emails cannot be recovered -}
  , deleteEmail :: UUID -> m (){- ^ Deletes an email and removes it from the inbox. Deleted emails cannot be recovered. -}
  , downloadAttachment :: UUID -> Text -> Maybe Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , downloadAttachmentBase64 :: UUID -> Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , downloadBody :: UUID -> m Text{- ^ Returns the specified email body for a given email as a string -}
  , downloadBodyBytes :: UUID -> m Text{- ^ Returns the specified email body for a given email as a stream / array of bytes. -}
  , forwardEmail :: UUID -> ForwardEmailOptions -> m SentEmailDto{- ^ Forward an existing email to new recipients. The sender of the email will be the inbox that received the email you are forwarding. You can override the sender with the `from` option. Note you must have access to the from address in MailSlurp to use the override. For more control consider fetching the email and sending it a new using the send email endpoints. -}
  , getAttachmentMetaData :: UUID -> Text -> m AttachmentMetaData{- ^ Returns the metadata such as name and content-type for a given attachment and email. -}
  , getEmail :: UUID -> Maybe Bool -> m Email{- ^ Returns a email summary object with headers and content. To retrieve the raw unparsed email use the getRawEmail endpoints -}
  , getEmailAttachments :: UUID -> m [AttachmentMetaData]{- ^ Returns an array of attachment metadata such as name and content-type for a given email if present. -}
  , getEmailContentMatch :: UUID -> ContentMatchOptions -> m EmailContentMatchResult{- ^ Return the matches for a given Java style regex pattern. Do not include the typical `/` at start or end of regex in some languages. Given an example `your code is: 12345` the pattern to extract match looks like `code is: (\\d{6})`. This will return an array of matches with the first matching the entire pattern and the subsequent matching the groups: `['code is: 123456', '123456']` See https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html for more information of available patterns.  -}
  , getEmailCount :: m CountDto{- ^  -}
  , getEmailHTML :: UUID -> Maybe Bool -> m Text{- ^ Retrieve email content as HTML response for viewing in browsers. Decodes quoted-printable entities and converts charset to UTF-8. Pass your API KEY as a request parameter when viewing in a browser: `?apiKey=xxx`. Returns content-type `text/html;charset=utf-8` so you must call expecting that content response not JSON. For JSON response see the `getEmailHTMLJson` method. -}
  , getEmailHTMLJson :: UUID -> Maybe Bool -> m EmailHtmlDto{- ^ Retrieve email content as HTML response. Decodes quoted-printable entities and converts charset to UTF-8. Returns content-type `application/json;charset=utf-8` so you must call expecting that content response not JSON. -}
  , getEmailHTMLQuery :: UUID -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of text. HTML parsing uses JSoup which supports JQuery/CSS style selectors -}
  , getEmailLinks :: UUID -> m EmailLinksResult{- ^ HTML parsing uses JSoup and UNIX line separators. Searches content for href attributes -}
  , getEmailPreviewURLs :: UUID -> m EmailPreviewUrls{- ^ Get a list of URLs for email content as text/html or raw SMTP message for viewing the message in a browser. -}
  , getEmailTextLines :: UUID -> Maybe Bool -> Maybe Text -> m EmailTextLinesResult{- ^ Parse an email body and return the content as an array of strings. HTML parsing uses JSoup and UNIX line separators. -}
  , getEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailProjection{- ^ By default returns all emails across all inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getGravatarUrlForEmailAddress :: Maybe Text -> Maybe Text -> m GravatarUrl{- ^ Get gravatar url for email address -}
  , getLatestEmail :: Maybe [UUID] -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getLatestEmailInInbox1 :: Maybe UUID -> m Email{- ^ Get the newest email in all inboxes or in a passed set of inbox IDs -}
  , getOrganizationEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailProjection{- ^ By default returns all emails across all team inboxes sorted by ascending created at date. Responses are paginated. You can restrict results to a list of inbox IDs. You can also filter out read messages -}
  , getRawEmailContents :: UUID -> m Text{- ^ Returns a raw, unparsed, and unprocessed email. If your client has issues processing the response it is likely due to the response content-type which is text/plain. If you need a JSON response content-type use the getRawEmailJson endpoint -}
  , getRawEmailJson :: UUID -> m RawEmailJson{- ^ Returns a raw, unparsed, and unprocessed email wrapped in a JSON response object for easier handling when compared with the getRawEmail text/plain response -}
  , getUnreadEmailCount :: m UnreadCount{- ^ Get number of emails unread. Unread means has not been viewed in dashboard or returned in an email API response -}
  , markAsRead :: UUID -> Maybe Bool -> m EmailPreview{- ^ Marks an email as read or unread. Pass boolean read flag to set value. This is useful if you want to read an email but keep it as unread -}
  , replyToEmail :: UUID -> ReplyToEmailOptions -> m SentEmailDto{- ^ Send the reply to the email sender or reply-to and include same subject cc bcc etc. Reply to an email and the contents will be sent with the existing subject to the emails `to`, `cc`, and `bcc`. -}
  , sendEmailSourceOptional :: Maybe UUID -> Maybe Bool -> Maybe Bool -> SendEmailOptions -> m (){- ^ Alias for `InboxController.sendEmail` method - see original method for full details. Sends an email from a given inbox that you have created. If no inbox is supplied a random inbox will be created for you and used to send the email. -}
  , validateEmail :: UUID -> m ValidationDto{- ^ Validate the HTML content of email if HTML is found. Considered valid if no HTML is present. -}
  , getValidationRequests :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> m PageEmailValidationRequest{- ^  -}
  , validateEmailAddressList :: ValidateEmailAddressListOptions -> m ValidateEmailAddressListResult{- ^  -}
  , getExpirationDefaults :: m ExpirationDefaults{- ^ Return default times used for inbox expiration -}
  , getExpiredInboxByInboxId :: UUID -> m ExpiredInboxDto{- ^ Use the inboxId to return an ExpiredInboxRecord if an inbox has expired. Inboxes expire and are disabled if an expiration date is set or plan requires. Returns 404 if no expired inbox is found for the inboxId -}
  , getExpiredInboxRecord :: UUID -> m ExpiredInboxDto{- ^ Inboxes created with an expiration date will expire after the given date and be moved to an ExpiredInbox entity. You can still read emails in the inbox but it can no longer send or receive emails. Fetch the expired inboxes to view the old inboxes properties -}
  , getExpiredInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageExpiredInboxRecordProjection{- ^ Inboxes created with an expiration date will expire after the given date. An ExpiredInboxRecord is created that records the inboxes old ID and email address. You can still read emails in the inbox (using the inboxes old ID) but the email address associated with the inbox can no longer send or receive emails. Fetch expired inbox records to view the old inboxes properties -}
  , exportEntities :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m [Text]{- ^  -}
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
  , cancelScheduledJob :: UUID -> m ScheduledJobDto{- ^ Get a scheduled email job and cancel it. Will fail if status of job is already cancelled, failed, or complete. -}
  , createInbox :: Maybe Text -> Maybe [Text] -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> Maybe Text -> Maybe Bool -> m InboxDto{- ^ Create a new inbox and with a randomized email address to send and receive from. Pass emailAddress parameter if you wish to use a specific email address. Creating an inbox is required before sending or receiving emails. If writing tests it is recommended that you create a new inbox during each test method so that it is unique and empty.  -}
  , createInboxRuleset :: UUID -> CreateInboxRulesetOptions -> m InboxRulesetDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , createInboxWithDefaults :: m InboxDto{- ^  -}
  , createInboxWithOptions :: CreateInboxDto -> m InboxDto{- ^ Additional endpoint that allows inbox creation with request body options. Can be more flexible that other methods for some clients. -}
  , deleteAllInboxEmails :: UUID -> m (){- ^ Deletes all emails in an inbox. Be careful as emails cannot be recovered -}
  , deleteAllInboxes :: m (){- ^ Permanently delete all inboxes and associated email addresses. This will also delete all emails within the inboxes. Be careful as inboxes cannot be recovered once deleted. Note: deleting inboxes will not impact your usage limits. Monthly inbox creation limits are based on how many inboxes were created in the last 30 days, not how many inboxes you currently have. -}
  , deleteInbox :: UUID -> m (){- ^ Permanently delete an inbox and associated email address as well as all emails within the given inbox. This action cannot be undone. Note: deleting an inbox will not affect your account usage. Monthly inbox usage is based on how many inboxes you create within 30 days, not how many exist at time of request. -}
  , doesInboxExist :: Maybe Text -> m InboxExistsDto{- ^ Check if inboxes exist by email address. Useful if you are sending emails to mailslurp addresses -}
  , flushExpired :: Maybe UTCTime -> m FlushExpiredInboxesResult{- ^ Remove any expired inboxes for your account (instead of waiting for scheduled removal on server) -}
  , getAllInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe UUID -> m PageInboxProjection{- ^ List inboxes in paginated form. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time). You Can also filter by whether an inbox is favorited or use email address pattern. This method is the recommended way to query inboxes. The alternative `getInboxes` method returns a full list of inboxes but is limited to 100 results. -}
  , getAllScheduledJobs :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageScheduledJobs{- ^ Schedule sending of emails using scheduled jobs. These can be inbox or account level. -}
  , getDeliveryStatusesByInboxId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageDeliveryStatus{- ^ Get all email delivery statuses for an inbox -}
  , getEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m [EmailPreview]{- ^ List emails that an inbox has received. Only emails that are sent to the inbox's email address will appear in the inbox. It may take several seconds for any email you send to an inbox's email address to appear in the inbox. To make this endpoint wait for a minimum number of emails use the `minCount` parameter. The server will retry the inbox database until the `minCount` is satisfied or the `retryTimeout` is reached -}
  , getImapSmtpAccess :: Maybe UUID -> m ImapSmtpAccessDetails{- ^ Get IMAP and SMTP access usernames and passwords -}
  , getInbox :: UUID -> m InboxDto{- ^ Returns an inbox's properties, including its email address and ID. -}
  , getInboxByEmailAddress :: Maybe Text -> m InboxByEmailAddressResult{- ^ Get a inbox result by email address -}
  , getInboxByName :: Maybe Text -> m InboxByNameResult{- ^ Get a inbox result by name -}
  , getInboxCount :: m CountDto{- ^  -}
  , getInboxEmailCount :: UUID -> m CountDto{- ^  -}
  , getInboxEmailsPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailPreview{- ^ Get a paginated list of emails in an inbox. Does not hold connections open. -}
  , getInboxIds :: m InboxIdsResult{- ^ Get list of inbox IDs -}
  , getInboxSentEmails :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageSentEmailProjection{- ^ Returns an inbox's sent email receipts. Call individual sent email endpoints for more details. Note for privacy reasons the full body of sent emails is never stored. An MD5 hash hex is available for comparison instead. -}
  , getInboxTags :: m [Text]{- ^ Get all inbox tags -}
  , getInboxes :: Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m [InboxDto]{- ^ List the inboxes you have created. Note use of the more advanced `getAllInboxes` is recommended and allows paginated access using a limit and sort parameter. -}
  , getLatestEmailInInbox :: Maybe UUID -> Maybe Integer -> m Email{- ^ Get the newest email in an inbox or wait for one to arrive -}
  , getOrganizationInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageOrganizationInboxProjection{- ^ List organization inboxes in paginated form. These are inboxes created with `allowTeamAccess` flag enabled. Organization inboxes are `readOnly` for non-admin users. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time).  -}
  , getScheduledJob :: UUID -> m ScheduledJobDto{- ^ Get a scheduled email job details. -}
  , getScheduledJobsByInboxId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageScheduledJobs{- ^ Schedule sending of emails using scheduled jobs. -}
  , listInboxRulesets :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxRulesetDto{- ^ List all rulesets attached to an inbox -}
  , listInboxTrackingPixels :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ List all tracking pixels sent from an inbox -}
  , sendEmail :: UUID -> SendEmailOptions -> m (){- ^ Send an email from an inbox's email address.  The request body should contain the `SendEmailOptions` that include recipients, attachments, body etc. See `SendEmailOptions` for all available properties. Note the `inboxId` refers to the inbox's id not the inbox's email address. See https://www.mailslurp.com/guides/ for more information on how to send emails. This method does not return a sent email entity due to legacy reasons. To send and get a sent email as returned response use the sister method `sendEmailAndConfirm`. -}
  , sendEmailAndConfirm :: UUID -> SendEmailOptions -> m SentEmailDto{- ^ Sister method for standard `sendEmail` method with the benefit of returning a `SentEmail` entity confirming the successful sending of the email with a link to the sent object created for it. -}
  , sendEmailWithQueue :: UUID -> Maybe Bool -> SendEmailOptions -> m (){- ^ Send an email using a queue. Will place the email onto a queue that will then be processed and sent. Use this queue method to enable any failed email sending to be recovered. This will prevent lost emails when sending if your account encounters a block or payment issue. -}
  , sendSmtpEnvelope :: UUID -> SendSMTPEnvelopeOptions -> m SentEmailDto{- ^ Send email using an SMTP envelope containing RCPT TO, MAIL FROM, and a SMTP BODY. -}
  , sendTestEmail :: UUID -> m (){- ^ Send an inbox a test email to test email receiving is working -}
  , sendWithSchedule :: UUID -> Maybe UTCTime -> Maybe Integer -> Maybe Bool -> SendEmailOptions -> m ScheduledJobDto{- ^ Send an email using a delay. Will place the email onto a scheduler that will then be processed and sent. Use delays to schedule email sending. -}
  , setInboxFavourited :: UUID -> SetInboxFavouritedOptions -> m InboxDto{- ^ Set and return new favourite state for an inbox -}
  , updateInbox :: UUID -> UpdateInboxOptions -> m InboxDto{- ^ Update editable fields on an inbox -}
  , createNewInboxForwarder :: Maybe UUID -> CreateInboxForwarderOptions -> m InboxForwarderDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , deleteInboxForwarder :: UUID -> m (){- ^ Delete inbox forwarder -}
  , deleteInboxForwarders :: Maybe UUID -> m (){- ^ Delete inbox forwarders. Accepts optional inboxId filter. -}
  , getInboxForwarder :: UUID -> m InboxForwarderDto{- ^ Get inbox ruleset -}
  , getInboxForwarderEvents :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxForwarderEvents{- ^ Get inbox ruleset events -}
  , getInboxForwarders :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxForwarderDto{- ^ List all forwarders attached to an inbox -}
  , testInboxForwarder :: UUID -> InboxForwarderTestOptions -> m InboxForwarderTestResult{- ^ Test an inbox forwarder -}
  , testInboxForwardersForInbox :: Maybe UUID -> InboxForwarderTestOptions -> m InboxForwarderTestResult{- ^ Test inbox forwarders for inbox -}
  , testNewInboxForwarder :: TestNewInboxForwarderOptions -> m InboxForwarderTestResult{- ^ Test new inbox forwarder -}
  , updateInboxForwarder :: UUID -> CreateInboxForwarderOptions -> m InboxForwarderDto{- ^ Update inbox ruleset -}
  , createNewInboxRuleset :: Maybe UUID -> CreateInboxRulesetOptions -> m InboxRulesetDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , deleteInboxRuleset :: UUID -> m (){- ^ Delete inbox ruleset -}
  , deleteInboxRulesets :: Maybe UUID -> m (){- ^ Delete inbox rulesets. Accepts optional inboxId filter. -}
  , getInboxRuleset :: UUID -> m InboxRulesetDto{- ^ Get inbox ruleset -}
  , getInboxRulesets :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageInboxRulesetDto{- ^ List all rulesets attached to an inbox -}
  , testInboxRuleset :: UUID -> InboxRulesetTestOptions -> m InboxRulesetTestResult{- ^ Test an inbox ruleset -}
  , testInboxRulesetsForInbox :: Maybe UUID -> InboxRulesetTestOptions -> m InboxRulesetTestResult{- ^ Test inbox rulesets for inbox -}
  , testNewInboxRuleset :: TestNewInboxRulesetOptions -> m InboxRulesetTestResult{- ^ Test new inbox ruleset -}
  , describeMailServerDomain :: DescribeDomainOptions -> m DescribeMailServerDomainResult{- ^  -}
  , getDnsLookup :: DNSLookupOptions -> m DNSLookupResults{- ^  -}
  , getIpAddress :: Maybe Text -> m IPAddressResult{- ^  -}
  , verifyEmailAddress :: VerifyEmailAddressOptions -> m EmailVerificationResult{- ^  -}
  , getAllMissedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageMissedEmailProjection{- ^  -}
  , getAllUnknownMissedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> m PageUnknownMissedEmailProjection{- ^ Unknown missed emails are emails that were sent to MailSlurp but could not be assigned to an existing inbox. -}
  , getMissedEmail :: UUID -> m MissedEmailDto{- ^ List emails that were missed due to plan limits. -}
  , restoreMissedEmails :: m (){- ^ If emails were missed due to a plan limit they are saved as missed emails. If support team enables the canRestore flag these emails can be reload into your account using this method. -}
  , waitForNthMissedEmail :: Maybe UUID -> Maybe Integer -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> m MissedEmailDto{- ^ Wait for 0 based index missed email -}
  , createEmergencyAddress :: CreateEmergencyAddressOptions -> m EmergencyAddress{- ^  -}
  , deleteEmergencyAddress :: UUID -> m EmptyResponseDto{- ^  -}
  , deletePhoneNumber :: UUID -> m (){- ^  -}
  , getEmergencyAddress :: UUID -> m EmergencyAddress{- ^  -}
  , getEmergencyAddresses :: m [EmergencyAddressDto]{- ^  -}
  , getPhoneNumber :: UUID -> m PhoneNumberDto{- ^  -}
  , getPhoneNumbers :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PagePhoneNumberProjection{- ^  -}
  , getPhonePlans :: m [PhonePlanDto]{- ^  -}
  , testPhoneNumberSendSms :: UUID -> TestPhoneNumberOptions -> Maybe Text -> m (){- ^  -}
  , deleteAllSentEmails :: m (){- ^  -}
  , deleteSentEmail :: UUID -> m (){- ^  -}
  , getAllSentTrackingPixels :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ Get all sent email tracking pixels in paginated form -}
  , getRawSentEmailContents :: UUID -> m Text{- ^ Returns a raw, unparsed, and unprocessed sent email. If your client has issues processing the response it is likely due to the response content-type which is text/plain. If you need a JSON response content-type use the getRawSentEmailJson endpoint -}
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
  , deleteSmsMessage :: UUID -> m (){- ^ Delete an SMS message -}
  , deleteSmsMessages :: Maybe UUID -> m (){- ^ Delete all SMS messages or all messages for a given phone number -}
  , getSmsMessage :: UUID -> m SmsDto{- ^ Returns a SMS summary object with content. -}
  , getSmsMessagesPaginated :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> m PageSmsProjection{- ^ By default returns all SMS messages across all phone numbers sorted by ascending created at date. Responses are paginated. You can restrict results to a list of phone number IDs. You can also filter out read messages -}
  , createTemplate :: CreateTemplateOptions -> m TemplateDto{- ^ Create an email template with variables for use with templated transactional emails. -}
  , deleteTemplate :: UUID -> m (){- ^ Delete template -}
  , getAllTemplates :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTemplateProjection{- ^ Get all templates in paginated format -}
  , getTemplate :: UUID -> m TemplateDto{- ^ Get email template -}
  , getTemplatePreviewHtml :: UUID -> m Text{- ^ Get email template preview with passed template variables in HTML format for browsers. Pass template variables as query params. -}
  , getTemplatePreviewJson :: UUID -> m TemplatePreview{- ^ Get email template preview with passed template variables in JSON format. Pass template variables as query params. -}
  , getTemplates :: m [TemplateProjection]{- ^ Get all templates -}
  , updateTemplate :: UUID -> CreateTemplateOptions -> m TemplateDto{- ^ Update email template -}
  , createTrackingPixel :: CreateTrackingPixelOptions -> m TrackingPixelDto{- ^ Create a tracking pixel. A tracking pixel is an image that can be embedded in an email. When the email is viewed and the image is seen MailSlurp will mark the pixel as seen. Use tracking pixels to monitor email open events. You can receive open notifications via webhook or by fetching the pixel. -}
  , getAllTrackingPixels :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ List tracking pixels in paginated form -}
  , getTrackingPixel :: UUID -> m TrackingPixelDto{- ^  -}
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
  , getAllAccountWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageWebhookProjection{- ^ List account webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getAllWebhookResults :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> m PageWebhookResult{- ^  -}
  , getAllWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> m PageWebhookProjection{- ^ List webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getInboxWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageWebhookProjection{- ^  -}
  , getJsonSchemaForWebhookEvent :: Maybe Text -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload by event -}
  , getJsonSchemaForWebhookPayload :: UUID -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload -}
  , getPhoneNumberWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageWebhookProjection{- ^  -}
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
  , getWebhookResults :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> m PageWebhookResult{- ^  -}
  , getWebhookResultsUnseenErrorCount :: m UnseenErrorCountDto{- ^  -}
  , getWebhooks :: UUID -> m [WebhookDto]{- ^  -}
  , redriveWebhookResult :: UUID -> m WebhookRedriveResult{- ^ Allows you to resend a webhook payload that was already sent. Webhooks that fail are retried automatically for 24 hours and then put in a dead letter queue. You can retry results manually using this method. -}
  , sendTestData :: UUID -> m WebhookTestResult{- ^  -}
  , updateWebhookHeaders :: UUID -> WebhookHeaders -> m WebhookDto{- ^  -}
  , verifyWebhookSignature :: VerifyWebhookSignatureOptions -> m VerifyWebhookSignatureResults{- ^ Verify a webhook payload using the messageId and signature. This allows you to be sure that MailSlurp sent the payload and not another server. -}
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
     (coerce -> getJsonPropertyAsString) :<|>
     (coerce -> getUserInfo) :<|>
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
     (coerce -> getBouncedEmail) :<|>
     (coerce -> getBouncedEmails) :<|>
     (coerce -> getBouncedRecipient) :<|>
     (coerce -> getBouncedRecipients) :<|>
     (coerce -> getComplaints) :<|>
     (coerce -> getListUnsubscribeRecipients) :<|>
     (coerce -> bulkCreateInboxes) :<|>
     (coerce -> bulkDeleteInboxes) :<|>
     (coerce -> bulkSendEmails) :<|>
     (coerce -> createNewEmailAddress) :<|>
     (coerce -> createRandomInbox) :<|>
     (coerce -> deleteEmailAddress) :<|>
     (coerce -> emptyInbox) :<|>
     (coerce -> sendEmailSimple) :<|>
     (coerce -> createContact) :<|>
     (coerce -> deleteContact) :<|>
     (coerce -> getAllContacts) :<|>
     (coerce -> getContact) :<|>
     (coerce -> getContactVCard) :<|>
     (coerce -> getContacts) :<|>
     (coerce -> addDomainWildcardCatchAll) :<|>
     (coerce -> createDomain) :<|>
     (coerce -> deleteDomain) :<|>
     (coerce -> getDomain) :<|>
     (coerce -> getDomains) :<|>
     (coerce -> updateDomain) :<|>
     (coerce -> applyImapFlagOperation) :<|>
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
     (coerce -> getEmailCount) :<|>
     (coerce -> getEmailHTML) :<|>
     (coerce -> getEmailHTMLJson) :<|>
     (coerce -> getEmailHTMLQuery) :<|>
     (coerce -> getEmailLinks) :<|>
     (coerce -> getEmailPreviewURLs) :<|>
     (coerce -> getEmailTextLines) :<|>
     (coerce -> getEmailsPaginated) :<|>
     (coerce -> getGravatarUrlForEmailAddress) :<|>
     (coerce -> getLatestEmail) :<|>
     (coerce -> getLatestEmailInInbox1) :<|>
     (coerce -> getOrganizationEmailsPaginated) :<|>
     (coerce -> getRawEmailContents) :<|>
     (coerce -> getRawEmailJson) :<|>
     (coerce -> getUnreadEmailCount) :<|>
     (coerce -> markAsRead) :<|>
     (coerce -> replyToEmail) :<|>
     (coerce -> sendEmailSourceOptional) :<|>
     (coerce -> validateEmail) :<|>
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
     (coerce -> cancelScheduledJob) :<|>
     (coerce -> createInbox) :<|>
     (coerce -> createInboxRuleset) :<|>
     (coerce -> createInboxWithDefaults) :<|>
     (coerce -> createInboxWithOptions) :<|>
     (coerce -> deleteAllInboxEmails) :<|>
     (coerce -> deleteAllInboxes) :<|>
     (coerce -> deleteInbox) :<|>
     (coerce -> doesInboxExist) :<|>
     (coerce -> flushExpired) :<|>
     (coerce -> getAllInboxes) :<|>
     (coerce -> getAllScheduledJobs) :<|>
     (coerce -> getDeliveryStatusesByInboxId) :<|>
     (coerce -> getEmails) :<|>
     (coerce -> getImapSmtpAccess) :<|>
     (coerce -> getInbox) :<|>
     (coerce -> getInboxByEmailAddress) :<|>
     (coerce -> getInboxByName) :<|>
     (coerce -> getInboxCount) :<|>
     (coerce -> getInboxEmailCount) :<|>
     (coerce -> getInboxEmailsPaginated) :<|>
     (coerce -> getInboxIds) :<|>
     (coerce -> getInboxSentEmails) :<|>
     (coerce -> getInboxTags) :<|>
     (coerce -> getInboxes) :<|>
     (coerce -> getLatestEmailInInbox) :<|>
     (coerce -> getOrganizationInboxes) :<|>
     (coerce -> getScheduledJob) :<|>
     (coerce -> getScheduledJobsByInboxId) :<|>
     (coerce -> listInboxRulesets) :<|>
     (coerce -> listInboxTrackingPixels) :<|>
     (coerce -> sendEmail) :<|>
     (coerce -> sendEmailAndConfirm) :<|>
     (coerce -> sendEmailWithQueue) :<|>
     (coerce -> sendSmtpEnvelope) :<|>
     (coerce -> sendTestEmail) :<|>
     (coerce -> sendWithSchedule) :<|>
     (coerce -> setInboxFavourited) :<|>
     (coerce -> updateInbox) :<|>
     (coerce -> createNewInboxForwarder) :<|>
     (coerce -> deleteInboxForwarder) :<|>
     (coerce -> deleteInboxForwarders) :<|>
     (coerce -> getInboxForwarder) :<|>
     (coerce -> getInboxForwarderEvents) :<|>
     (coerce -> getInboxForwarders) :<|>
     (coerce -> testInboxForwarder) :<|>
     (coerce -> testInboxForwardersForInbox) :<|>
     (coerce -> testNewInboxForwarder) :<|>
     (coerce -> updateInboxForwarder) :<|>
     (coerce -> createNewInboxRuleset) :<|>
     (coerce -> deleteInboxRuleset) :<|>
     (coerce -> deleteInboxRulesets) :<|>
     (coerce -> getInboxRuleset) :<|>
     (coerce -> getInboxRulesets) :<|>
     (coerce -> testInboxRuleset) :<|>
     (coerce -> testInboxRulesetsForInbox) :<|>
     (coerce -> testNewInboxRuleset) :<|>
     (coerce -> describeMailServerDomain) :<|>
     (coerce -> getDnsLookup) :<|>
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
     (coerce -> getEmergencyAddress) :<|>
     (coerce -> getEmergencyAddresses) :<|>
     (coerce -> getPhoneNumber) :<|>
     (coerce -> getPhoneNumbers) :<|>
     (coerce -> getPhonePlans) :<|>
     (coerce -> testPhoneNumberSendSms) :<|>
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
     (coerce -> deleteSmsMessage) :<|>
     (coerce -> deleteSmsMessages) :<|>
     (coerce -> getSmsMessage) :<|>
     (coerce -> getSmsMessagesPaginated) :<|>
     (coerce -> createTemplate) :<|>
     (coerce -> deleteTemplate) :<|>
     (coerce -> getAllTemplates) :<|>
     (coerce -> getTemplate) :<|>
     (coerce -> getTemplatePreviewHtml) :<|>
     (coerce -> getTemplatePreviewJson) :<|>
     (coerce -> getTemplates) :<|>
     (coerce -> updateTemplate) :<|>
     (coerce -> createTrackingPixel) :<|>
     (coerce -> getAllTrackingPixels) :<|>
     (coerce -> getTrackingPixel) :<|>
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
     (coerce -> getWebhookResultsUnseenErrorCount) :<|>
     (coerce -> getWebhooks) :<|>
     (coerce -> redriveWebhookResult) :<|>
     (coerce -> sendTestData) :<|>
     (coerce -> updateWebhookHeaders) :<|>
     (coerce -> verifyWebhookSignature) :<|>
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
       coerce getJsonPropertyAsString :<|>
       coerce getUserInfo :<|>
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
       coerce getBouncedEmail :<|>
       coerce getBouncedEmails :<|>
       coerce getBouncedRecipient :<|>
       coerce getBouncedRecipients :<|>
       coerce getComplaints :<|>
       coerce getListUnsubscribeRecipients :<|>
       coerce bulkCreateInboxes :<|>
       coerce bulkDeleteInboxes :<|>
       coerce bulkSendEmails :<|>
       coerce createNewEmailAddress :<|>
       coerce createRandomInbox :<|>
       coerce deleteEmailAddress :<|>
       coerce emptyInbox :<|>
       coerce sendEmailSimple :<|>
       coerce createContact :<|>
       coerce deleteContact :<|>
       coerce getAllContacts :<|>
       coerce getContact :<|>
       coerce getContactVCard :<|>
       coerce getContacts :<|>
       coerce addDomainWildcardCatchAll :<|>
       coerce createDomain :<|>
       coerce deleteDomain :<|>
       coerce getDomain :<|>
       coerce getDomains :<|>
       coerce updateDomain :<|>
       coerce applyImapFlagOperation :<|>
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
       coerce getEmailCount :<|>
       coerce getEmailHTML :<|>
       coerce getEmailHTMLJson :<|>
       coerce getEmailHTMLQuery :<|>
       coerce getEmailLinks :<|>
       coerce getEmailPreviewURLs :<|>
       coerce getEmailTextLines :<|>
       coerce getEmailsPaginated :<|>
       coerce getGravatarUrlForEmailAddress :<|>
       coerce getLatestEmail :<|>
       coerce getLatestEmailInInbox1 :<|>
       coerce getOrganizationEmailsPaginated :<|>
       coerce getRawEmailContents :<|>
       coerce getRawEmailJson :<|>
       coerce getUnreadEmailCount :<|>
       coerce markAsRead :<|>
       coerce replyToEmail :<|>
       coerce sendEmailSourceOptional :<|>
       coerce validateEmail :<|>
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
       coerce cancelScheduledJob :<|>
       coerce createInbox :<|>
       coerce createInboxRuleset :<|>
       coerce createInboxWithDefaults :<|>
       coerce createInboxWithOptions :<|>
       coerce deleteAllInboxEmails :<|>
       coerce deleteAllInboxes :<|>
       coerce deleteInbox :<|>
       coerce doesInboxExist :<|>
       coerce flushExpired :<|>
       coerce getAllInboxes :<|>
       coerce getAllScheduledJobs :<|>
       coerce getDeliveryStatusesByInboxId :<|>
       coerce getEmails :<|>
       coerce getImapSmtpAccess :<|>
       coerce getInbox :<|>
       coerce getInboxByEmailAddress :<|>
       coerce getInboxByName :<|>
       coerce getInboxCount :<|>
       coerce getInboxEmailCount :<|>
       coerce getInboxEmailsPaginated :<|>
       coerce getInboxIds :<|>
       coerce getInboxSentEmails :<|>
       coerce getInboxTags :<|>
       coerce getInboxes :<|>
       coerce getLatestEmailInInbox :<|>
       coerce getOrganizationInboxes :<|>
       coerce getScheduledJob :<|>
       coerce getScheduledJobsByInboxId :<|>
       coerce listInboxRulesets :<|>
       coerce listInboxTrackingPixels :<|>
       coerce sendEmail :<|>
       coerce sendEmailAndConfirm :<|>
       coerce sendEmailWithQueue :<|>
       coerce sendSmtpEnvelope :<|>
       coerce sendTestEmail :<|>
       coerce sendWithSchedule :<|>
       coerce setInboxFavourited :<|>
       coerce updateInbox :<|>
       coerce createNewInboxForwarder :<|>
       coerce deleteInboxForwarder :<|>
       coerce deleteInboxForwarders :<|>
       coerce getInboxForwarder :<|>
       coerce getInboxForwarderEvents :<|>
       coerce getInboxForwarders :<|>
       coerce testInboxForwarder :<|>
       coerce testInboxForwardersForInbox :<|>
       coerce testNewInboxForwarder :<|>
       coerce updateInboxForwarder :<|>
       coerce createNewInboxRuleset :<|>
       coerce deleteInboxRuleset :<|>
       coerce deleteInboxRulesets :<|>
       coerce getInboxRuleset :<|>
       coerce getInboxRulesets :<|>
       coerce testInboxRuleset :<|>
       coerce testInboxRulesetsForInbox :<|>
       coerce testNewInboxRuleset :<|>
       coerce describeMailServerDomain :<|>
       coerce getDnsLookup :<|>
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
       coerce getEmergencyAddress :<|>
       coerce getEmergencyAddresses :<|>
       coerce getPhoneNumber :<|>
       coerce getPhoneNumbers :<|>
       coerce getPhonePlans :<|>
       coerce testPhoneNumberSendSms :<|>
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
       coerce deleteSmsMessage :<|>
       coerce deleteSmsMessages :<|>
       coerce getSmsMessage :<|>
       coerce getSmsMessagesPaginated :<|>
       coerce createTemplate :<|>
       coerce deleteTemplate :<|>
       coerce getAllTemplates :<|>
       coerce getTemplate :<|>
       coerce getTemplatePreviewHtml :<|>
       coerce getTemplatePreviewJson :<|>
       coerce getTemplates :<|>
       coerce updateTemplate :<|>
       coerce createTrackingPixel :<|>
       coerce getAllTrackingPixels :<|>
       coerce getTrackingPixel :<|>
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
       coerce getWebhookResultsUnseenErrorCount :<|>
       coerce getWebhooks :<|>
       coerce redriveWebhookResult :<|>
       coerce sendTestData :<|>
       coerce updateWebhookHeaders :<|>
       coerce verifyWebhookSignature :<|>
       serveDirectoryFileServer "static")
