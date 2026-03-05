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



data FormSubmitForm = FormSubmitForm
  { submitFormTo :: Text
  , submitFormSubject :: Text
  , submitFormEmailAddress :: Text
  , submitFormSuccessMessage :: Text
  , submitFormSpamCheck :: Text
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
    =    "ai" :> "transformer" :> ReqBody '[JSON] AITransformCreateOptions :> Verb 'POST 200 '[JSON] AITransformDto -- 'createTransformer' route
    :<|> "ai" :> "transformer" :> "mappings" :> ReqBody '[JSON] CreateAITransformerMappingOptions :> Verb 'POST 200 '[JSON] AITransformMappingDto -- 'createTransformerMappings' route
    :<|> "ai" :> "transformer" :> "mappings" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllTransformerMappings' route
    :<|> "ai" :> "transformer" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteTransformer' route
    :<|> "ai" :> "transformer" :> "mappings" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteTransformerMapping' route
    :<|> "ai" :> "transformer" :> Verb 'DELETE 200 '[JSON] () -- 'deleteTransformers' route
    :<|> "ai" :> "transformer" :> "results" :> "export" :> ReqBody '[JSON] ExportTransformerOptions :> Verb 'POST 200 '[JSON] ExportTransformerResponse -- 'exportTransformerResults' route
    :<|> "ai" :> "structured-content" :> "attachment" :> ReqBody '[JSON] GenerateStructuredContentAttachmentOptions :> Verb 'POST 200 '[JSON] StructuredContentResultDto -- 'generateStructuredContentFromAttachment' route
    :<|> "ai" :> "structured-content" :> "email" :> ReqBody '[JSON] GenerateStructuredContentEmailOptions :> Verb 'POST 200 '[JSON] StructuredContentResultDto -- 'generateStructuredContentFromEmail' route
    :<|> "ai" :> "structured-content" :> "sms" :> ReqBody '[JSON] GenerateStructuredContentSmsOptions :> Verb 'POST 200 '[JSON] StructuredContentResultDto -- 'generateStructuredContentFromSms' route
    :<|> "ai" :> "transformer" :> "results" :> "export" :> Capture "id" UUID :> Verb 'POST 200 '[JSON] ExportTransformerResultJobDto -- 'getExportTransformerResultsJob' route
    :<|> "ai" :> "transformer" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] AITransformDto -- 'getTransformer' route
    :<|> "ai" :> "transformer" :> "mappings" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] AITransformMappingDto -- 'getTransformerMapping' route
    :<|> "ai" :> "transformer" :> "mappings" :> QueryParam "aiTransformId" UUID :> QueryParam "entityId" UUID :> QueryParam "entityType" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageAITransformMappingProjection -- 'getTransformerMappings' route
    :<|> "ai" :> "transformer" :> "results" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] AITransformResultDto -- 'getTransformerResult' route
    :<|> "ai" :> "transformer" :> "results" :> QueryParam "emailId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" Text :> QueryParam "aiTransformId" UUID :> QueryParam "aiTransformMappingId" UUID :> QueryParam "entityId" UUID :> QueryParam "entityType" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageAITransformResultProjection -- 'getTransformerResults' route
    :<|> "ai" :> "transformer" :> "results" :> "table" :> QueryParam "emailId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" Text :> QueryParam "aiTransformId" UUID :> QueryParam "aiTransformMappingId" UUID :> QueryParam "entityId" UUID :> QueryParam "entityType" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "includeMetaData" Bool :> QueryParam "flattenArraysToRows" Bool :> Verb 'GET 200 '[JSON] PageTableData -- 'getTransformerResultsTable' route
    :<|> "ai" :> "transformer" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] PageAITransformProjection -- 'getTransformers' route
    :<|> "ai" :> "transformer" :> "invoke" :> ReqBody '[JSON] InvokeTransformerOptions :> Verb 'POST 200 '[JSON] ConditionalStructuredContentResult -- 'invokeTransformer' route
    :<|> "ai" :> "transformer" :> "mappings" :> Capture "id" UUID :> "match" :> QueryParam "emailId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" Text :> Verb 'POST 200 '[JSON] AITransformMappingMatchResult -- 'testTransformerMappingMatch' route
    :<|> "ai" :> "structured-content" :> "validate" :> ReqBody '[JSON] StructuredOutputSchema :> Verb 'POST 200 '[JSON] StructuredOutputSchemaValidation -- 'validateStructuredOutputSchema' route
    :<|> "aliases" :> ReqBody '[JSON] CreateAliasOptions :> Verb 'POST 200 '[JSON] AliasDto -- 'createAlias' route
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
    :<|> "audit-logs" :> Capture "eventId" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] AuditLogEventDto -- 'getAuditLogByEventId' route
    :<|> "audit-logs" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "action" Text :> QueryParam "userId" UUID :> QueryParam "actorUserId" UUID :> QueryParam "targetUserId" UUID :> QueryParam "resourceType" Text :> QueryParam "resourceId" Text :> QueryParam "outcome" Text :> QueryParam "requestId" Text :> QueryParam "ipAddress" Text :> QueryParam "pageSize" Int :> QueryParam "cursor" Text :> Verb 'GET 200 '[JSON] AuditLogPageDto -- 'getAuditLogs' route
    :<|> "audit-logs" :> "search" :> ReqBody '[JSON] AuditLogSearchOptions :> Verb 'POST 200 '[JSON] AuditLogPageDto -- 'searchAuditLogs' route
    :<|> "attachments" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllAttachments' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "base64" :> Verb 'GET 200 '[JSON] DownloadAttachmentDto -- 'downloadAttachmentAsBase64Encoded' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "bytes" :> Verb 'GET 200 '[JSON] Text -- 'downloadAttachmentAsBytes' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "text" :> ReqBody '[JSON] ExtractAttachmentTextOptions :> Verb 'POST 200 '[JSON] ExtractAttachmentTextResult -- 'extractAttachmentText' route
    :<|> "attachments" :> Capture "attachmentId" Text :> Verb 'GET 200 '[JSON] AttachmentEntityDto -- 'getAttachment' route
    :<|> "attachments" :> Capture "attachmentId" Text :> "metadata" :> Verb 'GET 200 '[JSON] AttachmentMetaData -- 'getAttachmentInfo' route
    :<|> "attachments" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "fileNameFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "emailId" UUID :> QueryParam "sentEmailId" UUID :> QueryParam "include" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] PageAttachmentEntity -- 'getAttachments' route
    :<|> "attachments" :> ReqBody '[JSON] UploadAttachmentOptions :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachment' route
    :<|> "attachments" :> "bytes" :> QueryParam "contentType" Text :> QueryParam "contentId" Text :> QueryParam "filename" Text :> QueryParam "fileSize" Integer :> Header "contentType" Text :> Header "filename" Text :> Verb 'POST 200 '[JSON] [Text] -- 'uploadAttachmentBytes' route
    :<|> "attachments" :> "multipart" :> QueryParam "contentId" Text :> QueryParam "contentType" Text :> QueryParam "filename" Text :> QueryParam "contentTypeHeader" Text :> QueryParam "x-filename" Text :> QueryParam "x-filename-raw" Text :> QueryParam "x-filesize" Integer :> ReqBody '[JSON] InlineObject1 :> Verb 'POST 200 '[JSON] [Text] -- 'uploadMultipartForm' route
    :<|> "bounce" :> "filter-recipients" :> ReqBody '[JSON] FilterBouncedRecipientsOptions :> Verb 'POST 200 '[JSON] FilterBouncedRecipientsResult -- 'filterBouncedRecipient' route
    :<|> "bounce" :> "account-block" :> Verb 'GET 200 '[JSON] AccountBounceBlockDto -- 'getAccountBounceBlockStatus' route
    :<|> "bounce" :> "emails" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedEmailDto -- 'getBouncedEmail' route
    :<|> "bounce" :> "emails" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedEmail -- 'getBouncedEmails' route
    :<|> "bounce" :> "recipients" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] BouncedRecipientDto -- 'getBouncedRecipient' route
    :<|> "bounce" :> "recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageBouncedRecipients -- 'getBouncedRecipients' route
    :<|> "bounce" :> "complaints" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] Complaint -- 'getComplaint' route
    :<|> "bounce" :> "complaints" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageComplaint -- 'getComplaints' route
    :<|> "bounce" :> "list-unsubscribe-recipients" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "domainId" UUID :> Verb 'GET 200 '[JSON] PageListUnsubscribeRecipients -- 'getListUnsubscribeRecipients' route
    :<|> "bounce" :> "reputation-items" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageReputationItems -- 'getReputationItems' route
    :<|> "bounce" :> "tenant-findings" :> QueryParam "accountRegion" Text :> Verb 'GET 200 '[JSON] TenantReputationFindingsDto -- 'getTenantReputationFindings' route
    :<|> "bounce" :> "tenant-status" :> QueryParam "accountRegion" Text :> Verb 'GET 200 '[JSON] TenantReputationStatusSummaryDto -- 'getTenantReputationStatusSummary' route
    :<|> "bulk" :> "inboxes" :> QueryParam "count" Int :> Verb 'POST 200 '[JSON] [InboxDto] -- 'bulkCreateInboxes' route
    :<|> "bulk" :> "inboxes" :> ReqBody '[JSON] [UUID] :> Verb 'DELETE 200 '[JSON] () -- 'bulkDeleteInboxes' route
    :<|> "bulk" :> "send" :> ReqBody '[JSON] BulkSendEmailOptions :> Verb 'POST 200 '[JSON] () -- 'bulkSendEmails' route
    :<|> "campaign-probe" :> "probes" :> ReqBody '[JSON] CreateCampaignProbeOptions :> Verb 'POST 200 '[JSON] CampaignProbeDto -- 'createCampaignProbe' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteCampaignProbe' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> Verb 'GET 200 '[JSON] CampaignProbeDto -- 'getCampaignProbe' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> "insights" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] CampaignProbeInsightsDto -- 'getCampaignProbeInsights' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> "runs" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "status" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [CampaignProbeRunDto] -- 'getCampaignProbeRuns' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> "series" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "bucket" Text :> Verb 'GET 200 '[JSON] CampaignProbeSeriesDto -- 'getCampaignProbeSeries' route
    :<|> "campaign-probe" :> "probes" :> Verb 'GET 200 '[JSON] [CampaignProbeDto] -- 'getCampaignProbes' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> "run-now" :> ReqBody '[JSON] CreateCampaignProbeRunOptions :> Verb 'POST 200 '[JSON] CampaignProbeRunNowResult -- 'runCampaignProbeNow' route
    :<|> "campaign-probe" :> "probes" :> "run-due" :> QueryParam "maxRuns" Int :> Verb 'POST 200 '[JSON] CampaignProbeRunDueResult -- 'runDueCampaignProbes' route
    :<|> "campaign-probe" :> "probes" :> Capture "probeId" UUID :> ReqBody '[JSON] UpdateCampaignProbeOptions :> Verb 'PUT 200 '[JSON] CampaignProbeDto -- 'updateCampaignProbe' route
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
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> Capture "jobId" UUID :> "cancel" :> Verb 'POST 200 '[JSON] DeliverabilitySimulationJobDto -- 'cancelDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> ReqBody '[JSON] CreateDeliverabilitySimulationJobOptions :> Verb 'POST 200 '[JSON] DeliverabilitySimulationJobDto -- 'createDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> ReqBody '[JSON] CreateDeliverabilityTestOptions :> Verb 'POST 200 '[JSON] DeliverabilityTestDto -- 'createDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> Verb 'DELETE 200 '[JSON] DeleteResult -- 'deleteDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "duplicate" :> Verb 'POST 200 '[JSON] DeliverabilityTestDto -- 'duplicateDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "report" :> "export" :> Verb 'GET 200 '[JSON] () -- 'exportDeliverabilityTestReport' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "results" :> "export" :> QueryParam "matched" Bool :> Verb 'GET 200 '[JSON] () -- 'exportDeliverabilityTestResults' route
    :<|> "test" :> "deliverability" :> "analytics" :> "series" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "scope" Text :> QueryParam "bucket" Text :> QueryParam "runLimit" Int :> Verb 'GET 200 '[JSON] DeliverabilityAnalyticsSeriesDto -- 'getDeliverabilityAnalyticsSeries' route
    :<|> "test" :> "deliverability" :> "analytics" :> "hotspots" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "scope" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] DeliverabilityFailureHotspotsDto -- 'getDeliverabilityFailureHotspots' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> Capture "jobId" UUID :> Verb 'GET 200 '[JSON] DeliverabilitySimulationJobDto -- 'getDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> Capture "jobId" UUID :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] DeliverabilitySimulationJobEventPageDto -- 'getDeliverabilitySimulationJobEvents' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> Verb 'GET 200 '[JSON] DeliverabilityTestDto -- 'getDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "results" :> QueryParam "matched" Bool :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] DeliverabilityEntityResultPageDto -- 'getDeliverabilityTestResults' route
    :<|> "test" :> "deliverability" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] DeliverabilityTestPageDto -- 'getDeliverabilityTests' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> "latest" :> Verb 'GET 200 '[JSON] DeliverabilitySimulationJobDto -- 'getLatestDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> Capture "jobId" UUID :> "pause" :> Verb 'POST 200 '[JSON] DeliverabilitySimulationJobDto -- 'pauseDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "pause" :> Verb 'POST 200 '[JSON] DeliverabilityTestDto -- 'pauseDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "status" :> Verb 'GET 200 '[JSON] DeliverabilityPollStatusResultDto -- 'pollDeliverabilityTestStatus' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "simulation-jobs" :> Capture "jobId" UUID :> "resume" :> Verb 'POST 200 '[JSON] DeliverabilitySimulationJobDto -- 'resumeDeliverabilitySimulationJob' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "start" :> Verb 'POST 200 '[JSON] DeliverabilityTestDto -- 'startDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> "stop" :> Verb 'POST 200 '[JSON] DeliverabilityTestDto -- 'stopDeliverabilityTest' route
    :<|> "test" :> "deliverability" :> Capture "testId" UUID :> ReqBody '[JSON] UpdateDeliverabilityTestOptions :> Verb 'PATCH 200 '[JSON] DeliverabilityTestDto -- 'updateDeliverabilityTest' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> "cancel" :> ReqBody '[JSON] CancelDevicePreviewRunOptions :> Verb 'POST 200 '[JSON] CancelDevicePreviewRunResult -- 'cancelDevicePreviewRun' route
    :<|> "emails" :> "device-previews" :> "feedback" :> ReqBody '[JSON] CreateDevicePreviewFeedbackOptions :> Verb 'POST 200 '[JSON] DevicePreviewFeedbackDto -- 'createDevicePreviewFeedback' route
    :<|> "emails" :> Capture "emailId" UUID :> "device-previews" :> ReqBody '[JSON] CreateDevicePreviewOptions :> Verb 'POST 200 '[JSON] CreateDevicePreviewRunResult -- 'createDevicePreviewRun' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> Verb 'DELETE 200 '[JSON] DeleteDevicePreviewRunResult -- 'deleteDevicePreviewRun' route
    :<|> "emails" :> Capture "emailId" UUID :> "device-previews" :> "latest" :> ReqBody '[JSON] CreateDevicePreviewOptions :> Verb 'PUT 200 '[JSON] CreateDevicePreviewRunResult -- 'ensureDevicePreviewRun' route
    :<|> "emails" :> "device-previews" :> "feedback" :> Capture "feedbackId" UUID :> Verb 'GET 200 '[JSON] DevicePreviewFeedbackDto -- 'getDevicePreviewFeedback' route
    :<|> "emails" :> "device-previews" :> "feedback" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "source" Text :> QueryParam "runId" UUID :> QueryParam "status" Text :> QueryParam "provider" Text :> QueryParam "category" Text :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] DevicePreviewFeedbackListDto -- 'getDevicePreviewFeedbackItems' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> Verb 'GET 200 '[JSON] DevicePreviewRunDto -- 'getDevicePreviewRun' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> "providers" :> Capture "provider" Text :> Verb 'GET 200 '[JSON] DevicePreviewProviderProgressDto -- 'getDevicePreviewRunProviderProgress' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> "results" :> Verb 'GET 200 '[JSON] DevicePreviewRunResultsDto -- 'getDevicePreviewRunResults' route
    :<|> "emails" :> "device-previews" :> Capture "runId" UUID :> "screenshots" :> Capture "screenshotId" UUID :> "image" :> Verb 'GET 200 '[JSON] Text -- 'getDevicePreviewRunScreenshot' route
    :<|> "emails" :> Capture "emailId" UUID :> "device-previews" :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [DevicePreviewRunDto] -- 'getDevicePreviewRuns' route
    :<|> "emails" :> "device-previews" :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [DevicePreviewRunDto] -- 'getDevicePreviewRunsForAccount' route
    :<|> "emails" :> Capture "emailId" UUID :> "device-previews" :> "offset-paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageDevicePreviewRunProjection -- 'getDevicePreviewRunsOffsetPaginated' route
    :<|> "emails" :> "device-previews" :> "feedback" :> Capture "feedbackId" UUID :> ReqBody '[JSON] UpdateDevicePreviewFeedbackOptions :> Verb 'PUT 200 '[JSON] DevicePreviewFeedbackDto -- 'updateDevicePreviewFeedback' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'POST 200 '[JSON] DomainDto -- 'addDomainWildcardCatchAll' route
    :<|> "domains" :> ReqBody '[JSON] CreateDomainOptions :> Verb 'POST 200 '[JSON] DomainDto -- 'createDomain' route
    :<|> "domains" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] [Text] -- 'deleteDomain' route
    :<|> "domains" :> "available-domain-regions" :> QueryParam "inboxType" Text :> Verb 'GET 200 '[JSON] DomainRegionGroupsDto -- 'getAvailableDomainRegions' route
    :<|> "domains" :> "available-domains" :> QueryParam "inboxType" Text :> Verb 'GET 200 '[JSON] DomainGroupsDto -- 'getAvailableDomains' route
    :<|> "domains" :> Capture "id" UUID :> QueryParam "checkForErrors" Bool :> Verb 'GET 200 '[JSON] DomainDto -- 'getDomain' route
    :<|> "domains" :> "issues" :> Verb 'GET 200 '[JSON] DomainIssuesDto -- 'getDomainIssues' route
    :<|> "domains" :> Capture "id" UUID :> "wildcard" :> Verb 'GET 200 '[JSON] InboxDto -- 'getDomainWildcardCatchAllInbox' route
    :<|> "domains" :> Verb 'GET 200 '[JSON] [DomainPreview] -- 'getDomains' route
    :<|> "domains" :> "mailslurp-domains" :> QueryParam "inboxType" Text :> Verb 'GET 200 '[JSON] DomainGroupsDto -- 'getMailSlurpDomains' route
    :<|> "domains" :> Capture "id" UUID :> ReqBody '[JSON] UpdateDomainOptions :> Verb 'PUT 200 '[JSON] DomainDto -- 'updateDomain' route
    :<|> "domain-monitor" :> "monitors" :> ReqBody '[JSON] CreateDomainMonitorOptions :> Verb 'POST 200 '[JSON] DomainMonitorDto -- 'createDomainMonitor' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "alert-sinks" :> ReqBody '[JSON] CreateDomainMonitorAlertSinkOptions :> Verb 'POST 200 '[JSON] DomainMonitorAlertSinkDto -- 'createDomainMonitorAlertSink' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteDomainMonitor' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "alert-sinks" :> Capture "sinkId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteDomainMonitorAlertSink' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> Verb 'GET 200 '[JSON] DomainMonitorDto -- 'getDomainMonitor' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "alert-sinks" :> Verb 'GET 200 '[JSON] [DomainMonitorAlertSinkDto] -- 'getDomainMonitorAlertSinks' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "insights" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] DomainMonitorInsightsDto -- 'getDomainMonitorInsights' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "runs" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "status" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [DomainMonitorRunDto] -- 'getDomainMonitorRuns' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "series" :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "bucket" Text :> Verb 'GET 200 '[JSON] DomainMonitorSeriesDto -- 'getDomainMonitorSeries' route
    :<|> "domain-monitor" :> "monitors" :> Verb 'GET 200 '[JSON] [DomainMonitorDto] -- 'getDomainMonitors' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> "run-now" :> Verb 'POST 200 '[JSON] DomainMonitorRunNowResult -- 'runDomainMonitorNow' route
    :<|> "domain-monitor" :> "monitors" :> "run-due" :> QueryParam "maxRuns" Int :> Verb 'POST 200 '[JSON] DomainMonitorRunDueResult -- 'runDueDomainMonitors' route
    :<|> "domain-monitor" :> "monitors" :> Capture "monitorId" UUID :> ReqBody '[JSON] UpdateDomainMonitorOptions :> Verb 'PUT 200 '[JSON] DomainMonitorDto -- 'updateDomainMonitor' route
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
    :<|> "emails" :> Capture "emailId" UUID :> "codes" :> ReqBody '[JSON] ExtractCodesOptions :> Verb 'POST 200 '[JSON] ExtractCodesResult -- 'getEmailCodes' route
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
    :<|> "emails" :> Capture "emailId" UUID :> "signature" :> Verb 'GET 200 '[JSON] EmailSignatureParseResult -- 'getEmailSignature' route
    :<|> "emails" :> Capture "emailId" UUID :> "summary" :> QueryParam "decode" Bool :> Verb 'GET 200 '[JSON] EmailPreview -- 'getEmailSummary' route
    :<|> "emails" :> Capture "emailId" UUID :> "textLines" :> QueryParam "decodeHtmlEntities" Bool :> QueryParam "lineSeparator" Text :> Verb 'GET 200 '[JSON] EmailTextLinesResult -- 'getEmailTextLines' route
    :<|> "emails" :> "threads" :> Capture "threadId" UUID :> Verb 'GET 200 '[JSON] EmailThreadDto -- 'getEmailThread' route
    :<|> "emails" :> "threads" :> Capture "threadId" UUID :> "items" :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] EmailThreadItemsDto -- 'getEmailThreadItems' route
    :<|> "emails" :> "threads" :> QueryParam "htmlSelector" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageEmailThreadProjection -- 'getEmailThreads' route
    :<|> "emails" :> "offset-paginated" :> QueryParam "inboxId" (QueryList 'MultiParamArray (UUID)) :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "favourited" Bool :> QueryParam "syncConnectors" Bool :> QueryParam "plusAddressId" UUID :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] PageEmailProjection -- 'getEmailsOffsetPaginated' route
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
    :<|> "forms" :> ReqBody '[FormUrlEncoded] FormSubmitForm :> Verb 'POST 200 '[JSON] Text -- 'submitForm' route
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
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> ReqBody '[JSON] CreateRulesetOptions :> Verb 'POST 200 '[JSON] RulesetDto -- 'createInboxRuleset' route
    :<|> "inboxes" :> "withDefaults" :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithDefaults' route
    :<|> "inboxes" :> "withOptions" :> ReqBody '[JSON] CreateInboxDto :> Verb 'POST 200 '[JSON] InboxDto -- 'createInboxWithOptions' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "deleteAllInboxEmails" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxEmails' route
    :<|> "inboxes" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxes' route
    :<|> "inboxes" :> "by-description" :> QueryParam "description" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByDescription' route
    :<|> "inboxes" :> "by-name" :> QueryParam "name" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByName' route
    :<|> "inboxes" :> "by-tag" :> QueryParam "tag" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllInboxesByTag' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteInbox' route
    :<|> "inboxes" :> "exists" :> QueryParam "emailAddress" Text :> QueryParam "allowCatchAll" Bool :> QueryParam "ipAddress" Text :> QueryParam "sender" Text :> Verb 'GET 200 '[JSON] InboxExistsDto -- 'doesInboxExist' route
    :<|> "inboxes" :> "automations" :> Verb 'GET 200 '[JSON] () -- 'doesInboxHaveAutomations' route
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
    :<|> "inboxes" :> Capture "inboxId" UUID :> "plus-addresses" :> "get-or-create" :> QueryParam "fullAddress" Text :> Verb 'POST 200 '[JSON] PlusAddressDto -- 'getOrCreateInboxPlusAddress' route
    :<|> "inboxes" :> "get-or-create-plus-address" :> QueryParam "fullAddress" Text :> Verb 'POST 200 '[JSON] PlusAddressDto -- 'getOrCreatePlusAddressByFullAddress' route
    :<|> "inboxes" :> "organization" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageOrganizationInboxProjection -- 'getOrganizationInboxes' route
    :<|> "inboxes" :> "outboxes" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PageInboxProjection -- 'getOutboxes' route
    :<|> "inboxes" :> "scheduled-jobs" :> Capture "jobId" UUID :> Verb 'GET 200 '[JSON] ScheduledJobDto -- 'getScheduledJob' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "scheduled-jobs" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageScheduledJobs -- 'getScheduledJobsByInboxId' route
    :<|> "inboxes" :> "smtp-access" :> QueryParam "inboxId" UUID :> Verb 'GET 200 '[JSON] SmtpAccessDetails -- 'getSmtpAccess' route
    :<|> "inboxes" :> "available" :> QueryParam "emailAddress" Text :> Verb 'POST 200 '[JSON] EmailAvailableResult -- 'isEmailAddressAvailable' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "rulesets" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageRulesetDto -- 'listInboxRulesets' route
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
    :<|> "mfa" :> "totp" :> "device" :> "base32SecretKey" :> ReqBody '[JSON] CreateTotpDeviceBase32SecretKeyOptions :> Verb 'POST 200 '[JSON] TotpDeviceDto -- 'createTotpDeviceForBase32SecretKey' route
    :<|> "mfa" :> "totp" :> "device" :> "custom" :> ReqBody '[JSON] CreateTotpDeviceCustomOptions :> Verb 'POST 200 '[JSON] TotpDeviceDto -- 'createTotpDeviceForCustom' route
    :<|> "mfa" :> "totp" :> "device" :> "otpAuthUrl" :> ReqBody '[JSON] CreateTotpDeviceOtpAuthUrlOptions :> Verb 'POST 200 '[JSON] TotpDeviceDto -- 'createTotpDeviceForOtpAuthUrl' route
    :<|> "mfa" :> "totp" :> "device" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] TotpDeviceDto -- 'getTotpDevice' route
    :<|> "mfa" :> "totp" :> "device" :> "by" :> QueryParam "name" Text :> QueryParam "issuer" Text :> QueryParam "username" Text :> Verb 'GET 200 '[JSON] TotpDeviceOptionalDto -- 'getTotpDeviceBy' route
    :<|> "mfa" :> "totp" :> "device" :> Capture "id" UUID :> "code" :> QueryParam "at" UTCTime :> QueryParam "minSecondsUntilExpire" Int :> Verb 'GET 200 '[JSON] TotpDeviceCodeDto -- 'getTotpDeviceCode' route
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
    :<|> "missed-sms" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageMissedSmsProjection -- 'getAllMissedSmsMessages' route
    :<|> "missed-sms" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getMissedSmsCount' route
    :<|> "missed-sms" :> Capture "missedSmsId" UUID :> Verb 'GET 200 '[JSON] MissedSmsDto -- 'getMissedSmsMessage' route
    :<|> "phone" :> "emergency-addresses" :> ReqBody '[JSON] CreateEmergencyAddressOptions :> Verb 'POST 200 '[JSON] EmergencyAddress -- 'createEmergencyAddress' route
    :<|> "phone" :> ReqBody '[JSON] CreatePhoneNumberOptions :> Verb 'POST 200 '[JSON] PhoneNumberDto -- 'createPhoneNumber' route
    :<|> "phone" :> "numbers" :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllPhoneNumber' route
    :<|> "phone" :> "emergency-addresses" :> Capture "addressId" UUID :> Verb 'DELETE 200 '[JSON] EmptyResponseDto -- 'deleteEmergencyAddress' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "message-threads" :> Capture "otherNumber" Text :> Verb 'DELETE 200 '[JSON] EmptyResponseDto -- 'deletePhoneMessageThreadItems' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deletePhoneNumber' route
    :<|> "phone" :> "numbers" :> "message-threads" :> QueryParam "page" Int :> QueryParam "size" Int :> Verb 'GET 200 '[JSON] PagePhoneMessageThreadProjection -- 'getAllPhoneMessageThreads' route
    :<|> "phone" :> "releases" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] PagePhoneNumberReleaseProjection -- 'getAllPhoneNumberReleases' route
    :<|> "phone" :> "consent" :> Verb 'GET 200 '[JSON] ConsentStatusDto -- 'getConsentStatus' route
    :<|> "phone" :> "emergency-addresses" :> Capture "addressId" UUID :> Verb 'GET 200 '[JSON] EmergencyAddress -- 'getEmergencyAddress' route
    :<|> "phone" :> "emergency-addresses" :> Verb 'GET 200 '[JSON] [EmergencyAddressDto] -- 'getEmergencyAddresses' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "message-threads" :> Capture "otherNumber" Text :> QueryParam "page" Int :> QueryParam "size" Int :> Verb 'GET 200 '[JSON] PagePhoneMessageThreadItemProjection -- 'getPhoneMessageThreadItems' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "message-threads" :> QueryParam "page" Int :> QueryParam "size" Int :> Verb 'GET 200 '[JSON] PagePhoneMessageThreadProjection -- 'getPhoneMessageThreads' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'getPhoneNumber' route
    :<|> "phone" :> "numbers" :> "by-name" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'getPhoneNumberByName' route
    :<|> "phone" :> "numbers" :> "by-phone-number" :> QueryParam "phoneNumber" Text :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'getPhoneNumberByPhoneNumber' route
    :<|> "phone" :> "releases" :> Capture "releaseId" UUID :> Verb 'GET 200 '[JSON] PhoneNumberReleaseProjection -- 'getPhoneNumberRelease' route
    :<|> "phone" :> "numbers" :> QueryParam "phoneCountry" Text :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> QueryParam "favourite" Bool :> Verb 'GET 200 '[JSON] PagePhoneNumberProjection -- 'getPhoneNumbers' route
    :<|> "phone" :> "plans" :> Verb 'GET 200 '[JSON] [PhonePlanDto] -- 'getPhonePlans' route
    :<|> "phone" :> "plans" :> "availability" :> Verb 'GET 200 '[JSON] PhonePlanAvailability -- 'getPhonePlansAvailability' route
    :<|> "phone" :> "summary" :> Verb 'GET 200 '[JSON] PhoneSummaryDto -- 'getPhoneSummary' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms-sent" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageSentSmsProjection -- 'getSentSmsByPhoneNumber' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "unreadOnly" Bool :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "favourite" Bool :> Verb 'GET 200 '[JSON] PageSmsProjection -- 'getSmsByPhoneNumber' route
    :<|> "phone" :> "releases" :> Capture "releaseId" UUID :> "reassign" :> Verb 'GET 200 '[JSON] PhoneNumberDto -- 'reassignPhoneNumberRelease' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "sms" :> ReqBody '[JSON] SmsSendOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'sendSmsFromPhoneNumber' route
    :<|> "phone" :> "consent" :> QueryParam "agree" Bool :> Verb 'POST 200 '[JSON] ConsentStatusDto -- 'setConsentStatus' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "favourite" :> ReqBody '[JSON] SetPhoneFavouritedOptions :> Verb 'PUT 200 '[JSON] PhoneNumberDto -- 'setPhoneFavourited' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "test" :> ReqBody '[JSON] TestPhoneNumberOptions :> Header "x-test-id" Text :> Verb 'POST 200 '[JSON] () -- 'testPhoneNumberSendSms' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> ReqBody '[JSON] UpdatePhoneNumberOptions :> Verb 'PUT 200 '[JSON] PhoneNumberDto -- 'updatePhoneNumber' route
    :<|> "phone" :> "validate" :> ReqBody '[JSON] ValidatePhoneNumberOptions :> Verb 'POST 200 '[JSON] PhoneNumberValidationDto -- 'validatePhoneNumber' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> ReqBody '[JSON] CreateRulesetOptions :> Verb 'POST 200 '[JSON] RulesetDto -- 'createNewRuleset' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteRulesets' route
    :<|> "rulesets" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] RulesetDto -- 'getRuleset' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageRulesetDto -- 'getRulesets' route
    :<|> "rulesets" :> QueryParam "inboxId" UUID :> ReqBody '[JSON] RulesetTestOptions :> Verb 'PUT 200 '[JSON] InboxRulesetTestResult -- 'testInboxRulesetsForInbox' route
    :<|> "rulesets" :> ReqBody '[JSON] TestNewInboxRulesetOptions :> Verb 'PATCH 200 '[JSON] InboxRulesetTestResult -- 'testNewRuleset' route
    :<|> "rulesets" :> Capture "id" UUID :> "test" :> ReqBody '[JSON] RulesetTestOptions :> Verb 'POST 200 '[JSON] InboxRulesetTestResult -- 'testRuleset' route
    :<|> "rulesets" :> "test-receiving" :> ReqBody '[JSON] TestRulesetReceivingOptions :> Verb 'POST 200 '[JSON] TestRulesetReceivingResult -- 'testRulesetReceiving' route
    :<|> "rulesets" :> "test-sending" :> ReqBody '[JSON] TestInboxRulesetSendingOptions :> Verb 'POST 200 '[JSON] TestRulesetSendingResult -- 'testRulesetSending' route
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
    :<|> "sms" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> QueryParam "favourite" Bool :> QueryParam "include" (QueryList 'MultiParamArray (UUID)) :> Verb 'GET 200 '[JSON] PageSmsProjection -- 'getAllSmsMessages' route
    :<|> "sms" :> Capture "smsId" UUID :> "reply" :> Verb 'GET 200 '[JSON] ReplyForSms -- 'getReplyForSmsMessage' route
    :<|> "sms" :> "sent" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getSentSmsCount' route
    :<|> "sms" :> "sent" :> Capture "sentSmsId" UUID :> Verb 'GET 200 '[JSON] SentSmsDto -- 'getSentSmsMessage' route
    :<|> "sms" :> "sent" :> QueryParam "phoneNumber" UUID :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "search" Text :> Verb 'GET 200 '[JSON] PageSentSmsProjection -- 'getSentSmsMessagesPaginated' route
    :<|> "sms" :> Capture "smsId" UUID :> "codes" :> ReqBody '[JSON] ExtractCodesOptions :> Verb 'POST 200 '[JSON] ExtractCodesResult -- 'getSmsCodes' route
    :<|> "sms" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getSmsCount' route
    :<|> "sms" :> Capture "smsId" UUID :> Verb 'GET 200 '[JSON] SmsDto -- 'getSmsMessage' route
    :<|> "sms" :> "unreadCount" :> Verb 'GET 200 '[JSON] UnreadCount -- 'getUnreadSmsCount' route
    :<|> "sms" :> Capture "smsId" UUID :> "reply" :> ReqBody '[JSON] SmsReplyOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'replyToSmsMessage' route
    :<|> "sms" :> "send" :> QueryParam "fromPhoneNumber" Text :> QueryParam "fromPhoneId" UUID :> ReqBody '[JSON] SmsSendOptions :> Verb 'POST 200 '[JSON] SentSmsDto -- 'sendSms' route
    :<|> "sms" :> Capture "smsId" UUID :> "favourite" :> QueryParam "favourited" Bool :> Verb 'PUT 200 '[JSON] SmsDto -- 'setSmsFavourited' route
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
    :<|> "tools" :> "lookup-mx-records" :> ReqBody '[JSON] LookupMxRecordsOptions :> Verb 'POST 200 '[JSON] LookupMxRecordsResults -- 'lookupMxRecord' route
    :<|> "tools" :> "lookup-tls-reporting-domain" :> ReqBody '[JSON] LookupTlsReportingDomainOptions :> Verb 'POST 200 '[JSON] LookupTlsReportingDomainResults -- 'lookupTlsReportingDomain' route
    :<|> "tracking" :> "pixels" :> ReqBody '[JSON] CreateTrackingPixelOptions :> Verb 'POST 200 '[JSON] TrackingPixelDto -- 'createTrackingPixel' route
    :<|> "tracking" :> "pixels" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> Verb 'GET 200 '[JSON] PageTrackingPixelProjection -- 'getAllTrackingPixels' route
    :<|> "tracking" :> "pixels" :> Capture "id" UUID :> Verb 'GET 200 '[JSON] TrackingPixelDto -- 'getTrackingPixel' route
    :<|> "user" :> "inbox-retention-policies" :> "account" :> ReqBody '[JSON] CreateInboxRetentionPolicyForAccountOptions :> Verb 'POST 200 '[JSON] InboxRetentionPolicyDto -- 'createOrUpdateInboxRetentionPolicyForAccount' route
    :<|> "user" :> "inbox-retention-policies" :> "account" :> Verb 'DELETE 200 '[JSON] EmptyResponseDto -- 'deleteInboxRetentionPolicyForAccount' route
    :<|> "user" :> "automations" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "filter" Text :> Verb 'GET 200 '[JSON] PageEntityAutomationItems -- 'getEntityAutomations' route
    :<|> "user" :> "events" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "emailId" UUID :> QueryParam "phoneId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" UUID :> QueryParam "filter" Text :> Verb 'GET 200 '[JSON] PageEntityEventItems -- 'getEntityEvents' route
    :<|> "user" :> "favorites" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "filter" Text :> Verb 'GET 200 '[JSON] PageEntityFavouriteItems -- 'getEntityFavorites' route
    :<|> "user" :> "inbox-retention-policies" :> "account" :> Verb 'GET 200 '[JSON] InboxRetentionPolicyOptionalDto -- 'getInboxRetentionPolicyForAccount' route
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
    :<|> "ai-transformers" :> Capture "transformerId" UUID :> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createWebhookForAITransformer' route
    :<|> "phone" :> "numbers" :> Capture "phoneNumberId" UUID :> "webhooks" :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'POST 200 '[JSON] WebhookDto -- 'createWebhookForPhoneNumber' route
    :<|> "webhooks" :> QueryParam "before" UTCTime :> Verb 'DELETE 200 '[JSON] () -- 'deleteAllWebhooks' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> Capture "webhookId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteWebhook' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> Verb 'DELETE 200 '[JSON] () -- 'deleteWebhookById' route
    :<|> "webhooks" :> "account" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> QueryParam "health" Text :> QueryParam "searchFilter" Text :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllAccountWebhooks' route
    :<|> "webhooks" :> "endpoints" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "phoneId" UUID :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> Verb 'GET 200 '[JSON] PageWebhookEndpointProjection -- 'getAllWebhookEndpoints' route
    :<|> "webhooks" :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> QueryParam "resultType" Text :> QueryParam "eventName" Text :> QueryParam "minStatusCode" Int :> QueryParam "maxStatusCode" Int :> QueryParam "inboxId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" UUID :> QueryParam "emailId" UUID :> QueryParam "phoneId" UUID :> QueryParam "aiTransformerId" UUID :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getAllWebhookResults' route
    :<|> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "inboxId" UUID :> QueryParam "aiTransformerId" UUID :> QueryParam "phoneId" UUID :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> QueryParam "url" Text :> QueryParam "eventTypeSource" Text :> QueryParam "includeAccountWide" Bool :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getAllWebhooks' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "health" Text :> QueryParam "eventType" Text :> QueryParam "includeAccountWide" Bool :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getInboxWebhooksPaginated' route
    :<|> "webhooks" :> "schema" :> QueryParam "event" Text :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookEvent' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "schema" :> Verb 'POST 200 '[JSON] JSONSchemaDto -- 'getJsonSchemaForWebhookPayload' route
    :<|> "phone" :> "numbers" :> Capture "phoneId" UUID :> "webhooks" :> "paginated" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "eventType" Text :> QueryParam "searchFilter" Text :> QueryParam "health" Text :> QueryParam "includeAccountWide" Bool :> Verb 'GET 200 '[JSON] PageWebhookProjection -- 'getPhoneNumberWebhooksPaginated' route
    :<|> "webhooks" :> "test" :> QueryParam "eventName" Text :> Verb 'GET 200 '[JSON] AbstractWebhookPayload -- 'getTestWebhookPayload' route
    :<|> "webhooks" :> "test" :> "email-bounce-payload" :> Verb 'GET 200 '[JSON] WebhookBouncePayload -- 'getTestWebhookPayloadBounce' route
    :<|> "webhooks" :> "test" :> "email-bounce-recipient-payload" :> Verb 'GET 200 '[JSON] WebhookBounceRecipientPayload -- 'getTestWebhookPayloadBounceRecipient' route
    :<|> "webhooks" :> "test" :> "delivery-status-payload" :> Verb 'GET 200 '[JSON] WebhookDeliveryStatusPayload -- 'getTestWebhookPayloadDeliveryStatus' route
    :<|> "webhooks" :> "test" :> "email-opened-payload" :> Verb 'GET 200 '[JSON] WebhookEmailOpenedPayload -- 'getTestWebhookPayloadEmailOpened' route
    :<|> "webhooks" :> "test" :> "email-read-payload" :> Verb 'GET 200 '[JSON] WebhookEmailReadPayload -- 'getTestWebhookPayloadEmailRead' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "example" :> Verb 'POST 200 '[JSON] AbstractWebhookPayload -- 'getTestWebhookPayloadForWebhook' route
    :<|> "webhooks" :> "test" :> "new-ai-transform-result-payload" :> Verb 'GET 200 '[JSON] WebhookNewAITransformResultPayload -- 'getTestWebhookPayloadNewAITransformResult' route
    :<|> "webhooks" :> "test" :> "new-attachment-payload" :> Verb 'GET 200 '[JSON] WebhookNewAttachmentPayload -- 'getTestWebhookPayloadNewAttachment' route
    :<|> "webhooks" :> "test" :> "new-contact-payload" :> Verb 'GET 200 '[JSON] WebhookNewContactPayload -- 'getTestWebhookPayloadNewContact' route
    :<|> "webhooks" :> "test" :> "new-email-payload" :> Verb 'GET 200 '[JSON] WebhookNewEmailPayload -- 'getTestWebhookPayloadNewEmail' route
    :<|> "webhooks" :> "test" :> "new-sms-payload" :> Verb 'GET 200 '[JSON] WebhookNewSmsPayload -- 'getTestWebhookPayloadNewSms' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> Verb 'GET 200 '[JSON] WebhookDto -- 'getWebhook' route
    :<|> "webhooks" :> "results" :> Capture "webhookResultId" UUID :> Verb 'GET 200 '[JSON] WebhookResultDto -- 'getWebhookResult' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "results" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> QueryParam "searchFilter" Text :> QueryParam "since" UTCTime :> QueryParam "before" UTCTime :> QueryParam "unseenOnly" Bool :> QueryParam "resultType" Text :> QueryParam "eventName" Text :> QueryParam "minStatusCode" Int :> QueryParam "maxStatusCode" Int :> QueryParam "inboxId" UUID :> QueryParam "smsId" UUID :> QueryParam "attachmentId" UUID :> QueryParam "emailId" UUID :> QueryParam "phoneId" UUID :> QueryParam "aiTransformerId" UUID :> Verb 'GET 200 '[JSON] PageWebhookResult -- 'getWebhookResults' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "results" :> "count" :> Verb 'GET 200 '[JSON] CountDto -- 'getWebhookResultsCount' route
    :<|> "webhooks" :> "results" :> "unseen-count" :> Verb 'GET 200 '[JSON] UnseenErrorCountDto -- 'getWebhookResultsUnseenErrorCount' route
    :<|> "inboxes" :> Capture "inboxId" UUID :> "webhooks" :> QueryParam "page" Int :> QueryParam "size" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] [WebhookProjection] -- 'getWebhooks' route
    :<|> "webhooks" :> "results" :> "redrive" :> Verb 'POST 200 '[JSON] WebhookRedriveAllResult -- 'redriveAllWebhookResults' route
    :<|> "webhooks" :> "results" :> Capture "webhookResultId" UUID :> "redrive" :> Verb 'POST 200 '[JSON] WebhookRedriveResult -- 'redriveWebhookResult' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> "test" :> Verb 'POST 200 '[JSON] WebhookTestResult -- 'sendTestData' route
    :<|> "webhooks" :> Capture "webhookId" UUID :> QueryParam "inboxId" UUID :> QueryParam "aiTransformerId" UUID :> QueryParam "phoneNumberId" UUID :> QueryParam "overrideAuth" Bool :> ReqBody '[JSON] CreateWebhookOptions :> Verb 'PATCH 200 '[JSON] WebhookDto -- 'updateWebhook' route
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
  { createTransformer :: AITransformCreateOptions -> m AITransformDto{- ^ Save an AI transform instructions and schema for use with webhooks and automations -}
  , createTransformerMappings :: CreateAITransformerMappingOptions -> m AITransformMappingDto{- ^ Create AI transformer mappings to other entities -}
  , deleteAllTransformerMappings :: m (){- ^ Delete all AI transformer mappings -}
  , deleteTransformer :: UUID -> m (){- ^ Delete an AI transformer and schemas by ID -}
  , deleteTransformerMapping :: UUID -> m (){- ^ Delete an AI transformer mapping -}
  , deleteTransformers :: m (){- ^ Delete all AI transformers and schemas -}
  , exportTransformerResults :: ExportTransformerOptions -> m ExportTransformerResponse{- ^ Export AI transformer results in formats such as Excel, CSV, XML etc. -}
  , generateStructuredContentFromAttachment :: GenerateStructuredContentAttachmentOptions -> m StructuredContentResultDto{- ^ Use output schemas to extract data from an attachment using AI -}
  , generateStructuredContentFromEmail :: GenerateStructuredContentEmailOptions -> m StructuredContentResultDto{- ^ Use output schemas to extract data from an email using AI -}
  , generateStructuredContentFromSms :: GenerateStructuredContentSmsOptions -> m StructuredContentResultDto{- ^ Use output schemas to extract data from an SMS using AI -}
  , getExportTransformerResultsJob :: UUID -> m ExportTransformerResultJobDto{- ^ Get the job status for an export -}
  , getTransformer :: UUID -> m AITransformDto{- ^ Get AI transformer and schemas by ID -}
  , getTransformerMapping :: UUID -> m AITransformMappingDto{- ^ Get an AI transformer mapping -}
  , getTransformerMappings :: Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m PageAITransformMappingProjection{- ^ Get AI transformer mappings to other entities -}
  , getTransformerResult :: UUID -> m AITransformResultDto{- ^ Get AI transformer result -}
  , getTransformerResults :: Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m PageAITransformResultProjection{- ^ Get AI transformer results -}
  , getTransformerResultsTable :: Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Bool -> m PageTableData{- ^ Get AI transformer results in table format -}
  , getTransformers :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe [UUID] -> m PageAITransformProjection{- ^ List all AI transforms -}
  , invokeTransformer :: InvokeTransformerOptions -> m ConditionalStructuredContentResult{- ^ Execute an AI transformer to generate structured content -}
  , testTransformerMappingMatch :: UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> m AITransformMappingMatchResult{- ^ Evaluate transform mapping match conditions for given email, sms, or attachment -}
  , validateStructuredOutputSchema :: StructuredOutputSchema -> m StructuredOutputSchemaValidation{- ^ Check if a schema is valid and can be used to extract data using AI -}
  , createAlias :: CreateAliasOptions -> m AliasDto{- ^ Email aliases use a MailSlurp randomly generated email address (or a custom domain inbox that you provide) to mask or proxy a real email address. Emails sent to the alias address will be forwarded to the hidden email address it was created for. If you want to send a reply use the threadId attached -}
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
  , getAuditLogByEventId :: Text -> Maybe UTCTime -> Maybe UTCTime -> m AuditLogEventDto{- ^  -}
  , getAuditLogs :: Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> m AuditLogPageDto{- ^  -}
  , searchAuditLogs :: AuditLogSearchOptions -> m AuditLogPageDto{- ^  -}
  , deleteAllAttachments :: m (){- ^ Delete all attachments -}
  , deleteAttachment :: Text -> m (){- ^ Delete an attachment -}
  , downloadAttachmentAsBase64Encoded :: Text -> m DownloadAttachmentDto{- ^ Returns the specified attachment for a given email as a base 64 encoded string. The response type is application/json. This method is similar to the `downloadAttachment` method but allows some clients to get around issues with binary responses. -}
  , downloadAttachmentAsBytes :: Text -> m Text{- ^ Returns the specified attachment for a given email as a stream / array of bytes. You can find attachment ids in email responses endpoint responses. The response type is application/octet-stream. -}
  , extractAttachmentText :: Text -> ExtractAttachmentTextOptions -> m ExtractAttachmentTextResult{- ^ Extract text content from an attachment using the requested method. `NATIVE` decoding is available now for text-like files. OCR/LLM methods are wired for future use and may return not implemented unless fallback is enabled. -}
  , getAttachment :: Text -> m AttachmentEntityDto{- ^  -}
  , getAttachmentInfo :: Text -> m AttachmentMetaData{- ^ Returns the metadata for an attachment. It is saved separately to the content of the attachment. Contains properties `name` and `content-type` and `content-length` in bytes for a given attachment. -}
  , getAttachments :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe [Text] -> m PageAttachmentEntity{- ^ Get all attachments in paginated response. Each entity contains meta data for the attachment such as `name` and `content-type`. Use the `attachmentId` and the download endpoints to get the file contents. -}
  , uploadAttachment :: UploadAttachmentOptions -> m [Text]{- ^  -}
  , uploadAttachmentBytes :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Text -> Maybe Text -> m [Text]{- ^  -}
  , uploadMultipartForm :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> InlineObject1 -> m [Text]{- ^  -}
  , filterBouncedRecipient :: FilterBouncedRecipientsOptions -> m FilterBouncedRecipientsResult{- ^ Prevent email sending errors by remove recipients who have resulted in past email bounces or complaints -}
  , getAccountBounceBlockStatus :: m AccountBounceBlockDto{- ^ Check if account block status prevents sending -}
  , getBouncedEmail :: UUID -> m BouncedEmailDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedEmails :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedEmail{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipient :: UUID -> m BouncedRecipientDto{- ^ Bounced emails are email you have sent that were rejected by a recipient -}
  , getBouncedRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageBouncedRecipients{- ^ Bounced recipients are email addresses that you have sent emails to that did not accept the sent email. Once a recipient is bounced you cannot send emails to that address. -}
  , getComplaint :: UUID -> m Complaint{- ^ Get complaint -}
  , getComplaints :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageComplaint{- ^ SMTP complaints made against your account -}
  , getListUnsubscribeRecipients :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UUID -> m PageListUnsubscribeRecipients{- ^ Unsubscribed recipient have unsubscribed from a mailing list for a user or domain and cannot be contacted again. -}
  , getReputationItems :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageReputationItems{- ^ List of complaints and bounces -}
  , getTenantReputationFindings :: Maybe Text -> m TenantReputationFindingsDto{- ^ Get SES tenant reputation recommendations/findings for this user. -}
  , getTenantReputationStatusSummary :: Maybe Text -> m TenantReputationStatusSummaryDto{- ^ Get SES tenant sending and reputation status rows for this user. Includes complaint and bounce rates from CloudWatch. -}
  , bulkCreateInboxes :: Maybe Int -> m [InboxDto]{- ^  -}
  , bulkDeleteInboxes :: [UUID] -> m (){- ^  -}
  , bulkSendEmails :: BulkSendEmailOptions -> m (){- ^  -}
  , createCampaignProbe :: CreateCampaignProbeOptions -> m CampaignProbeDto{- ^  -}
  , deleteCampaignProbe :: UUID -> m (){- ^  -}
  , getCampaignProbe :: UUID -> m CampaignProbeDto{- ^  -}
  , getCampaignProbeInsights :: UUID -> Maybe UTCTime -> Maybe UTCTime -> m CampaignProbeInsightsDto{- ^  -}
  , getCampaignProbeRuns :: UUID -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Int -> m [CampaignProbeRunDto]{- ^  -}
  , getCampaignProbeSeries :: UUID -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m CampaignProbeSeriesDto{- ^  -}
  , getCampaignProbes :: m [CampaignProbeDto]{- ^  -}
  , runCampaignProbeNow :: UUID -> CreateCampaignProbeRunOptions -> m CampaignProbeRunNowResult{- ^  -}
  , runDueCampaignProbes :: Maybe Int -> m CampaignProbeRunDueResult{- ^  -}
  , updateCampaignProbe :: UUID -> UpdateCampaignProbeOptions -> m CampaignProbeDto{- ^  -}
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
  , cancelDeliverabilitySimulationJob :: UUID -> UUID -> m DeliverabilitySimulationJobDto{- ^ Cancel a running or paused simulation job. -}
  , createDeliverabilitySimulationJob :: UUID -> CreateDeliverabilitySimulationJobOptions -> m DeliverabilitySimulationJobDto{- ^ Create and start a simulation job for a running deliverability test. Only one active simulation job is allowed per user. -}
  , createDeliverabilityTest :: CreateDeliverabilityTestOptions -> m DeliverabilityTestDto{- ^ Create a deliverability test for inboxes or phone numbers using ALL, PATTERN, or EXPLICIT selector scope. -}
  , deleteDeliverabilityTest :: UUID -> m DeleteResult{- ^ Delete test and all persisted entity-level results. -}
  , duplicateDeliverabilityTest :: UUID -> m DeliverabilityTestDto{- ^ Create a fresh deliverability test using an existing test configuration, including selector scope, exclusions, and expectations. -}
  , exportDeliverabilityTestReport :: UUID -> m (){- ^ Export a PDF report for a terminal deliverability test (COMPLETE, FAILED, or STOPPED), including configuration, summary outcomes, and detailed entity-level results. -}
  , exportDeliverabilityTestResults :: UUID -> Maybe Bool -> m (){- ^ Export per-entity deliverability results including expectation-level pass/fail counts. The latest status is evaluated with the same polling safeguards before export. -}
  , getDeliverabilityAnalyticsSeries :: Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Int -> m DeliverabilityAnalyticsSeriesDto{- ^ Compare deliverability runs over a time range with bucketed chart metrics and run-level rows for table views. -}
  , getDeliverabilityFailureHotspots :: Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Int -> m DeliverabilityFailureHotspotsDto{- ^ Find commonly failing entities and phone country/variant dimensions across deliverability runs in a time range. -}
  , getDeliverabilitySimulationJob :: UUID -> UUID -> m DeliverabilitySimulationJobDto{- ^ Get simulation job status and progress counters. -}
  , getDeliverabilitySimulationJobEvents :: UUID -> UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m DeliverabilitySimulationJobEventPageDto{- ^ Get paged simulation events including send successes and failures. -}
  , getDeliverabilityTest :: UUID -> m DeliverabilityTestDto{- ^ Get deliverability test configuration and latest progress counters. -}
  , getDeliverabilityTestResults :: UUID -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> m DeliverabilityEntityResultPageDto{- ^ Get paged per-entity expectation results with optional matched/unmatched filtering. -}
  , getDeliverabilityTests :: Maybe Int -> Maybe Int -> Maybe Text -> m DeliverabilityTestPageDto{- ^ List deliverability tests for the authenticated account. -}
  , getLatestDeliverabilitySimulationJob :: UUID -> m DeliverabilitySimulationJobDto{- ^ Get the most recent simulation job for a deliverability test. -}
  , pauseDeliverabilitySimulationJob :: UUID -> UUID -> m DeliverabilitySimulationJobDto{- ^ Pause a running simulation job. -}
  , pauseDeliverabilityTest :: UUID -> m DeliverabilityTestDto{- ^ Pause a RUNNING or SCHEDULED deliverability test. -}
  , pollDeliverabilityTestStatus :: UUID -> m DeliverabilityPollStatusResultDto{- ^ Poll test progress. Evaluation is throttled with a 5-second cache window to protect backing data stores. -}
  , resumeDeliverabilitySimulationJob :: UUID -> UUID -> m DeliverabilitySimulationJobDto{- ^ Resume a paused simulation job. -}
  , startDeliverabilityTest :: UUID -> m DeliverabilityTestDto{- ^ Start a CREATED test or resume a PAUSED/SCHEDULED test. -}
  , stopDeliverabilityTest :: UUID -> m DeliverabilityTestDto{- ^ Stop a deliverability test and mark it terminal. -}
  , updateDeliverabilityTest :: UUID -> UpdateDeliverabilityTestOptions -> m DeliverabilityTestDto{- ^ Update metadata, timeout, and expectations for a non-running non-terminal test. -}
  , cancelDevicePreviewRun :: UUID -> CancelDevicePreviewRunOptions -> m CancelDevicePreviewRunResult{- ^  -}
  , createDevicePreviewFeedback :: CreateDevicePreviewFeedbackOptions -> m DevicePreviewFeedbackDto{- ^  -}
  , createDevicePreviewRun :: UUID -> CreateDevicePreviewOptions -> m CreateDevicePreviewRunResult{- ^  -}
  , deleteDevicePreviewRun :: UUID -> m DeleteDevicePreviewRunResult{- ^  -}
  , ensureDevicePreviewRun :: UUID -> CreateDevicePreviewOptions -> m CreateDevicePreviewRunResult{- ^  -}
  , getDevicePreviewFeedback :: UUID -> m DevicePreviewFeedbackDto{- ^  -}
  , getDevicePreviewFeedbackItems :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UUID -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m DevicePreviewFeedbackListDto{- ^  -}
  , getDevicePreviewRun :: UUID -> m DevicePreviewRunDto{- ^  -}
  , getDevicePreviewRunProviderProgress :: UUID -> Text -> m DevicePreviewProviderProgressDto{- ^  -}
  , getDevicePreviewRunResults :: UUID -> m DevicePreviewRunResultsDto{- ^  -}
  , getDevicePreviewRunScreenshot :: UUID -> UUID -> m Text{- ^  -}
  , getDevicePreviewRuns :: UUID -> Maybe Int -> m [DevicePreviewRunDto]{- ^  -}
  , getDevicePreviewRunsForAccount :: Maybe Int -> m [DevicePreviewRunDto]{- ^  -}
  , getDevicePreviewRunsOffsetPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m PageDevicePreviewRunProjection{- ^  -}
  , updateDevicePreviewFeedback :: UUID -> UpdateDevicePreviewFeedbackOptions -> m DevicePreviewFeedbackDto{- ^  -}
  , addDomainWildcardCatchAll :: UUID -> m DomainDto{- ^ Add a catch all inbox to a domain so that any emails sent to it that cannot be matched will be sent to the catch all inbox generated -}
  , createDomain :: CreateDomainOptions -> m DomainDto{- ^ Link a domain that you own with MailSlurp so you can create email addresses using it. Endpoint returns DNS records used for validation. You must add these verification records to your host provider's DNS setup to verify the domain. -}
  , deleteDomain :: UUID -> m [Text]{- ^ Delete a domain. This will disable any existing inboxes that use this domain. -}
  , getAvailableDomainRegions :: Maybe Text -> m DomainRegionGroupsDto{- ^ List all domains available for use with email address creation, including account-region and create/send enablement flags. -}
  , getAvailableDomains :: Maybe Text -> m DomainGroupsDto{- ^ List all domains available for use with email address creation -}
  , getDomain :: UUID -> Maybe Bool -> m DomainDto{- ^ Returns domain verification status and tokens for a given domain -}
  , getDomainIssues :: m DomainIssuesDto{- ^ List domain issues for domains you have created -}
  , getDomainWildcardCatchAllInbox :: UUID -> m InboxDto{- ^ Get the catch all inbox for a domain for missed emails -}
  , getDomains :: m [DomainPreview]{- ^ List all custom domains you have created -}
  , getMailSlurpDomains :: Maybe Text -> m DomainGroupsDto{- ^ List all MailSlurp domains used with non-custom email addresses -}
  , updateDomain :: UUID -> UpdateDomainOptions -> m DomainDto{- ^ Update values on a domain. Note you cannot change the domain name as it is immutable. Recreate the domain if you need to alter this. -}
  , createDomainMonitor :: CreateDomainMonitorOptions -> m DomainMonitorDto{- ^  -}
  , createDomainMonitorAlertSink :: UUID -> CreateDomainMonitorAlertSinkOptions -> m DomainMonitorAlertSinkDto{- ^  -}
  , deleteDomainMonitor :: UUID -> m (){- ^  -}
  , deleteDomainMonitorAlertSink :: UUID -> UUID -> m (){- ^  -}
  , getDomainMonitor :: UUID -> m DomainMonitorDto{- ^  -}
  , getDomainMonitorAlertSinks :: UUID -> m [DomainMonitorAlertSinkDto]{- ^  -}
  , getDomainMonitorInsights :: UUID -> Maybe UTCTime -> Maybe UTCTime -> m DomainMonitorInsightsDto{- ^  -}
  , getDomainMonitorRuns :: UUID -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Int -> m [DomainMonitorRunDto]{- ^  -}
  , getDomainMonitorSeries :: UUID -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m DomainMonitorSeriesDto{- ^  -}
  , getDomainMonitors :: m [DomainMonitorDto]{- ^  -}
  , runDomainMonitorNow :: UUID -> m DomainMonitorRunNowResult{- ^  -}
  , runDueDomainMonitors :: Maybe Int -> m DomainMonitorRunDueResult{- ^  -}
  , updateDomainMonitor :: UUID -> UpdateDomainMonitorOptions -> m DomainMonitorDto{- ^  -}
  , applyImapFlagOperation :: UUID -> ImapFlagOperationOptions -> m EmailPreview{- ^ Applies RFC3501 IMAP flag operations on a message. Current implementation supports read/unread semantics via the `\\\\Seen` flag only. -}
  , canSend :: Maybe UUID -> SendEmailOptions -> m CanSendEmailResults{- ^ Validates sender/inbox context and recipient eligibility before attempting a send. Useful for preflight checks in UI or test workflows. -}
  , checkEmailBody :: UUID -> m CheckEmailBodyResults{- ^ Runs content quality checks against hydrated email body content. This endpoint performs outbound HTTP checks for linked resources, so avoid use with sensitive or stateful URLs. -}
  , checkEmailBodyFeatureSupport :: UUID -> m CheckEmailBodyFeatureSupportResults{- ^ Detects HTML/CSS features in the target email body and reports compatibility across major email clients and devices. -}
  , checkEmailClientSupport :: CheckEmailClientSupportOptions -> m CheckEmailClientSupportResults{- ^ Evaluates HTML/CSS features in the supplied body and reports support coverage across major email clients and platforms. -}
  , deleteAllEmails :: m (){- ^ Deletes all emails for the authenticated account context. This operation is destructive and cannot be undone. -}
  , deleteEmail :: UUID -> m (){- ^ Deletes a single email from account scope. Operation is destructive and not reversible. -}
  , downloadAttachment :: UUID -> Text -> Maybe Text -> m Text{- ^ Returns attachment bytes by attachment ID. Use attachment IDs from email payloads or attachment listing endpoints. -}
  , downloadAttachmentBase64 :: UUID -> Text -> m DownloadAttachmentDto{- ^ Returns attachment payload as base64 in JSON. Useful for clients that cannot reliably consume binary streaming responses. -}
  , downloadBody :: UUID -> m Text{- ^ Returns hydrated email body text as a string with content type aligned to the underlying body format. -}
  , downloadBodyBytes :: UUID -> m Text{- ^ Streams hydrated email body bytes with content type derived from the message body format. -}
  , forwardEmail :: UUID -> ForwardEmailOptions -> m SentEmailDto{- ^ Forwards an existing email to new recipients. Uses the owning inbox context unless overridden by allowed sender options. -}
  , getAttachmentMetaData :: UUID -> Text -> m AttachmentMetaData{- ^ Returns metadata for a specific attachment ID (name, content type, and size fields). -}
  , getEmail :: UUID -> m Email{- ^ Returns parsed email content including headers and body fields. Accessing hydrated content may mark the email as read depending on read-state rules. -}
  , getEmailAttachments :: UUID -> m [AttachmentMetaData]{- ^ Returns metadata for all attachment IDs associated with the email (name, content type, size, and IDs). -}
  , getEmailCodes :: UUID -> ExtractCodesOptions -> m ExtractCodesResult{- ^ Extracts one-time passcodes and similar tokens from email content. Supports deterministic extraction now with method/fallback flags (`AUTO`, `PATTERN`, `LLM`, `OCR`, `OCR_THEN_LLM`) for QA and future advanced pipelines. -}
  , getEmailContentMatch :: UUID -> ContentMatchOptions -> m EmailContentMatchResult{- ^ Executes a Java regex pattern over hydrated email body text and returns the full match plus capture groups. Pattern syntax follows Java `Pattern` rules. -}
  , getEmailContentPart :: UUID -> Maybe Text -> Maybe Bool -> Maybe Int -> m EmailContentPartResult{- ^ Extracts one MIME body part by `contentType` and optional `index`, returned in a structured DTO with metadata. -}
  , getEmailContentPartContent :: UUID -> Maybe Text -> Maybe Bool -> Maybe Int -> m Text{- ^ Extracts one MIME body part by `contentType` and optional `index`, and returns raw content with matching response content type when valid. -}
  , getEmailCount :: Maybe UUID -> m CountDto{- ^ Returns total email count for the authenticated user, or count scoped to a specific inbox when `inboxId` is provided. -}
  , getEmailHTML :: UUID -> Maybe Bool -> m Text{- ^ Returns hydrated HTML body directly with `text/html` content type. Supports temporary access/browser usage and optional CID replacement for inline asset rendering. -}
  , getEmailHTMLJson :: UUID -> Maybe Bool -> m EmailHtmlDto{- ^ Returns hydrated HTML body and subject in a JSON DTO. Useful for API clients that need structured response payloads instead of raw HTML responses. -}
  , getEmailHTMLQuery :: UUID -> Maybe Text -> m EmailTextLinesResult{- ^ Applies a JSoup/CSS selector to hydrated HTML email body and returns matching text lines. -}
  , getEmailLinks :: UUID -> Maybe Text -> m EmailLinksResult{- ^ Parses HTML content and extracts link URLs (`href`). For non-HTML emails this endpoint returns a validation error. -}
  , getEmailPreviewURLs :: UUID -> m EmailPreviewUrls{- ^ Returns precomputed URLs for preview and raw message access for the specified email. -}
  , getEmailScreenshotAsBase64 :: UUID -> GetEmailScreenshotOptions -> m EmailScreenshotResult{- ^ Renders the email in a browser engine and returns PNG data as base64. Useful for APIs and dashboards that cannot easily stream binary responses. -}
  , getEmailScreenshotAsBinary :: UUID -> GetEmailScreenshotOptions -> m (){- ^ Renders the email in a browser engine and returns PNG bytes. Intended for visual QA and rendering regression checks. -}
  , getEmailSignature :: UUID -> m EmailSignatureParseResult{- ^ Attempts to parse a sender signature block from an email body. Uses raw MIME content parts when possible and falls back to hydrated body parsing. This is heuristic and may not be accurate for all email clients or formats. -}
  , getEmailSummary :: UUID -> Maybe Bool -> m EmailPreview{- ^ Returns lightweight metadata and headers for an email. Use `getEmail` for hydrated body content or `getRawEmail` for original SMTP content. -}
  , getEmailTextLines :: UUID -> Maybe Bool -> Maybe Text -> m EmailTextLinesResult{- ^ Converts email body content to line-based plain text with optional HTML entity decoding and custom line separator. -}
  , getEmailThread :: UUID -> m EmailThreadDto{- ^ Returns thread metadata built from RFC 5322 `Message-ID`, `In-Reply-To`, and `References` headers. Use `getEmailThreadItems` to fetch the thread messages. -}
  , getEmailThreadItems :: UUID -> Maybe Text -> m EmailThreadItemsDto{- ^ Returns all messages in a thread ordered by `createdAt` using the requested sort direction. -}
  , getEmailThreads :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageEmailThreadProjection{- ^ Lists conversation threads inferred from `Message-ID`, `In-Reply-To`, and `References`. Supports filtering by inbox, search text, and time range. -}
  , getEmailsOffsetPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Bool -> Maybe UUID -> Maybe [UUID] -> m PageEmailProjection{- ^ Offset-style pagination endpoint for listing emails across inboxes. Supports inbox filters, unread-only, search, date boundaries, favourites, connector sync, plus-address filtering, and explicit include IDs. -}
  , getEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe UUID -> Maybe Bool -> m PageEmailProjection{- ^ Primary paginated email listing endpoint. Returns emails across inboxes with support for inbox filters, unread-only, search, date boundaries, favourites, connector sync, and plus-address filtering. -}
  , getGravatarUrlForEmailAddress :: Maybe Text -> Maybe Text -> m GravatarUrl{- ^ Builds a Gravatar image URL from the provided email address and optional size. This endpoint does not fetch image bytes; it only returns the computed URL. -}
  , getLatestEmail :: Maybe [UUID] -> m Email{- ^ Returns the most recently received email across all inboxes or an optional subset of inbox IDs. -}
  , getLatestEmailInInbox1 :: Maybe UUID -> m Email{- ^ Returns the newest email for the specified inbox ID. For polling/wait semantics use wait endpoints. -}
  , getOrganizationEmailsPaginated :: Maybe [UUID] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Bool -> Maybe UUID -> m PageEmailProjection{- ^ Returns paginated emails visible through organization/team access. Supports inbox filtering, unread-only filtering, search, favourites, plus-address filtering, and optional connector sync. -}
  , getRawEmailContents :: UUID -> m (){- ^ Returns the original unparsed SMTP/MIME message as `text/plain`. Use JSON variant if your client expects JSON transport. -}
  , getRawEmailJson :: UUID -> m RawEmailJson{- ^ Returns the original unparsed SMTP/MIME message wrapped in a JSON DTO for API clients that avoid plain-text stream responses. -}
  , getUnreadEmailCount :: Maybe UUID -> m UnreadCount{- ^ Returns unread email count. An email is considered read after dashboard/API retrieval depending on your read workflow. -}
  , markAllAsRead :: Maybe Bool -> Maybe UUID -> m (){- ^ Sets read state for all emails, optionally scoped to one inbox. Use `read=false` to reset unread state in bulk. -}
  , markAsRead :: UUID -> Maybe Bool -> m EmailPreview{- ^ Sets read state for one email. Useful when implementing custom mailbox workflows that treat viewed messages as unread. -}
  , replyToEmail :: UUID -> ReplyToEmailOptions -> m SentEmailDto{- ^ Sends a reply using the original conversation context (subject/thread headers). Reply target resolution honors sender/reply-to semantics. -}
  , searchEmails :: Maybe Bool -> Maybe Bool -> Maybe UUID -> SearchEmailsOptions -> m PageEmailProjection{- ^ Searches emails by sender/recipient/address/subject/id fields and returns paginated matches. Does not perform full-text body search. -}
  , sendEmailSourceOptional :: Maybe UUID -> Maybe Bool -> Maybe Bool -> SendEmailOptions -> m (){- ^ Sends an email from an existing inbox, or creates a temporary inbox when `inboxId` is not provided. Supports `useDomainPool` and `virtualSend` inbox creation behavior for convenience sends. -}
  , setEmailFavourited :: UUID -> Maybe Bool -> m (){- ^ Sets favourite state for an email for dashboard/search workflows. -}
  , validateEmail :: UUID -> m ValidationDto{- ^ Runs HTML validation on the email body when HTML is present. Non-HTML emails are treated as valid for this check. -}
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
  , submitForm :: FormSubmitForm -> m Text{- ^ This endpoint allows you to submit HTML forms and receive the field values and files via email.   #### Parameters The endpoint looks for special meta parameters in the form fields OR in the URL request parameters. The meta parameters can be used to specify the behaviour of the email.   You must provide at-least a `_to` email address to tell the endpoint where the form should be emailed. These can be submitted as hidden HTML input fields with the corresponding `name` attributes or as URL query parameters such as `?_to=test@example.com`  The endpoint takes all other form fields that are named and includes them in the message body of the email. Files are sent as attachments.  #### Submitting This endpoint accepts form submission via POST method. It accepts `application/x-www-form-urlencoded`, and `multipart/form-data` content-types.  #### HTML Example ```html <form    action=\"https://api.mailslurp.com/forms\"   method=\"post\" >   <input name=\"_to\" type=\"hidden\" value=\"test@example.com\"/>   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```  #### URL Example ```html <form    action=\"https://api.mailslurp.com/forms?_to=test@example.com\"   method=\"post\" >   <textarea name=\"feedback\"></textarea>   <button type=\"submit\">Submit</button> </form> ```    The email address is specified by a `_to` field OR is extracted from an email alias specified by a `_toAlias` field (see the alias controller for more information).  Endpoint accepts .  You can specify a content type in HTML forms using the `enctype` attribute, for instance: `<form enctype=\"multipart/form-data\">`.   -}
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
  , createInboxRuleset :: UUID -> CreateRulesetOptions -> m RulesetDto{- ^ Create a new inbox rule for forwarding, blocking, and allowing emails when sending and receiving -}
  , createInboxWithDefaults :: m InboxDto{- ^  -}
  , createInboxWithOptions :: CreateInboxDto -> m InboxDto{- ^ Additional endpoint that allows inbox creation with request body options. Can be more flexible that other methods for some clients. -}
  , deleteAllInboxEmails :: UUID -> m (){- ^ Deletes all emails in an inbox. Be careful as emails cannot be recovered -}
  , deleteAllInboxes :: m (){- ^ Permanently delete all inboxes and associated email addresses. This will also delete all emails within the inboxes. Be careful as inboxes cannot be recovered once deleted. Note: deleting inboxes will not impact your usage limits. Monthly inbox creation limits are based on how many inboxes were created in the last 30 days, not how many inboxes you currently have. -}
  , deleteAllInboxesByDescription :: Maybe Text -> m (){- ^ Permanently delete all inboxes by description -}
  , deleteAllInboxesByName :: Maybe Text -> m (){- ^ Permanently delete all inboxes by name -}
  , deleteAllInboxesByTag :: Maybe Text -> m (){- ^ Permanently delete all inboxes by tag -}
  , deleteInbox :: UUID -> m (){- ^ Permanently delete an inbox and associated email address as well as all emails within the given inbox. This action cannot be undone. Note: deleting an inbox will not affect your account usage. Monthly inbox usage is based on how many inboxes you create within 30 days, not how many exist at time of request. -}
  , doesInboxExist :: Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> m InboxExistsDto{- ^ Check if inboxes exist by email address. Useful if you are sending emails to mailslurp addresses -}
  , doesInboxHaveAutomations :: m (){- ^ Check if an inbox has automations. -}
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
  , getOrCreateInboxPlusAddress :: UUID -> Maybe Text -> m PlusAddressDto{- ^ Looks up an inbox plus address using a full email address like `inbox+alias@domain.com`. Returns an existing plus address if found, otherwise creates one. -}
  , getOrCreatePlusAddressByFullAddress :: Maybe Text -> m PlusAddressDto{- ^ Looks up an inbox plus address using a full email address like `inbox+alias@domain.com`. Resolves the base inbox from the full address for the authenticated user, then returns an existing plus address if found, otherwise creates one. -}
  , getOrganizationInboxes :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageOrganizationInboxProjection{- ^ List organization inboxes in paginated form. These are inboxes created with `allowTeamAccess` flag enabled. Organization inboxes are `readOnly` for non-admin users. The results are available on the `content` property of the returned object. This method allows for page index (zero based), page size (how many results to return), and a sort direction (based on createdAt time).  -}
  , getOutboxes :: Maybe Int -> Maybe Int -> Maybe Text -> m PageInboxProjection{- ^ List inboxes that have sent emails -}
  , getScheduledJob :: UUID -> m ScheduledJobDto{- ^ Get a scheduled email job details. -}
  , getScheduledJobsByInboxId :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageScheduledJobs{- ^ Schedule sending of emails using scheduled jobs. -}
  , getSmtpAccess :: Maybe UUID -> m SmtpAccessDetails{- ^ Get SMTP access usernames and passwords -}
  , isEmailAddressAvailable :: Maybe Text -> m EmailAvailableResult{- ^ Returns whether an email address is available -}
  , listInboxRulesets :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageRulesetDto{- ^ List all rulesets attached to an inbox -}
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
  , createTotpDeviceForBase32SecretKey :: CreateTotpDeviceBase32SecretKeyOptions -> m TotpDeviceDto{- ^ Create a virtual TOTP device for a given secret key. This is usually present as an alternative login option when pairing OTP devices. -}
  , createTotpDeviceForCustom :: CreateTotpDeviceCustomOptions -> m TotpDeviceDto{- ^ Create a virtual TOTP device for custom options specifying all parameters of the TOTP device. -}
  , createTotpDeviceForOtpAuthUrl :: CreateTotpDeviceOtpAuthUrlOptions -> m TotpDeviceDto{- ^ Create a virtual TOTP device for a given OTP Auth URL such as otpauth://totp/MyApp:alice@example.com?secret=ABC123&issuer=MyApp&period=30&digits=6&algorithm=SHA1. You can provider overrides in the options for each component of the URL. -}
  , getTotpDevice :: UUID -> m TotpDeviceDto{- ^ Get Time-Based One-Time Password (TOTP) device by its ID. -}
  , getTotpDeviceBy :: Maybe Text -> Maybe Text -> Maybe Text -> m TotpDeviceOptionalDto{- ^ Get Time-Based One-Time Password (TOTP) device by its username and issuer mapping. -}
  , getTotpDeviceCode :: UUID -> Maybe UTCTime -> Maybe Int -> m TotpDeviceCodeDto{- ^ Get Time-Based One-Time Password for a device by its ID. -}
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
  , getAllMissedSmsMessages :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageMissedSmsProjection{- ^  -}
  , getMissedSmsCount :: m CountDto{- ^  -}
  , getMissedSmsMessage :: UUID -> m MissedSmsDto{- ^ Returns a missed SMS with full content. -}
  , createEmergencyAddress :: CreateEmergencyAddressOptions -> m EmergencyAddress{- ^ Add an emergency address to a phone number -}
  , createPhoneNumber :: CreatePhoneNumberOptions -> m PhoneNumberDto{- ^ Create new phone number -}
  , deleteAllPhoneNumber :: m (){- ^ Remove all phone number from account -}
  , deleteEmergencyAddress :: UUID -> m EmptyResponseDto{- ^ Delete an emergency address -}
  , deletePhoneMessageThreadItems :: UUID -> Text -> m EmptyResponseDto{- ^ Delete all messages in an SMS thread -}
  , deletePhoneNumber :: UUID -> m (){- ^ Remove phone number from account -}
  , getAllPhoneMessageThreads :: Maybe Int -> Maybe Int -> m PagePhoneMessageThreadProjection{- ^ List all message threads for all phones -}
  , getAllPhoneNumberReleases :: Maybe Int -> Maybe Int -> Maybe Text -> m PagePhoneNumberReleaseProjection{- ^ List released or deleted phone numbers -}
  , getConsentStatus :: m ConsentStatusDto{- ^ Get the status of phone usage consent -}
  , getEmergencyAddress :: UUID -> m EmergencyAddress{- ^ Fetch an emergency address by ID -}
  , getEmergencyAddresses :: m [EmergencyAddressDto]{- ^ List emergency addresses -}
  , getPhoneMessageThreadItems :: UUID -> Text -> Maybe Int -> Maybe Int -> m PagePhoneMessageThreadItemProjection{- ^ List message thread messages for a phone message thread -}
  , getPhoneMessageThreads :: UUID -> Maybe Int -> Maybe Int -> m PagePhoneMessageThreadProjection{- ^ List message threads for a phone -}
  , getPhoneNumber :: UUID -> m PhoneNumberDto{- ^ Get a phone number by ID -}
  , getPhoneNumberByName :: Maybe Text -> m PhoneNumberDto{- ^ Get a phone number by name -}
  , getPhoneNumberByPhoneNumber :: Maybe Text -> m PhoneNumberDto{- ^ Get a phone number by phone number -}
  , getPhoneNumberRelease :: UUID -> m PhoneNumberReleaseProjection{- ^ Get a released or deleted phone numbers -}
  , getPhoneNumbers :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe [UUID] -> Maybe Bool -> m PagePhoneNumberProjection{- ^ List phone numbers for account -}
  , getPhonePlans :: m [PhonePlanDto]{- ^ Get phone number plans -}
  , getPhonePlansAvailability :: m PhonePlanAvailability{- ^  -}
  , getPhoneSummary :: m PhoneSummaryDto{- ^ Get overview of assigned phones -}
  , getSentSmsByPhoneNumber :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageSentSmsProjection{- ^ Get sent SMS messages for a phone number -}
  , getSmsByPhoneNumber :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Bool -> m PageSmsProjection{- ^ Get SMS messages for a phone number -}
  , reassignPhoneNumberRelease :: UUID -> m PhoneNumberDto{- ^ Reassign phone number that was released or deleted -}
  , sendSmsFromPhoneNumber :: UUID -> SmsSendOptions -> m SentSmsDto{- ^ Send SMS from a phone number -}
  , setConsentStatus :: Maybe Bool -> m ConsentStatusDto{- ^ Give or revoke consent for phone usage -}
  , setPhoneFavourited :: UUID -> SetPhoneFavouritedOptions -> m PhoneNumberDto{- ^ Set and return new favorite state for a phone -}
  , testPhoneNumberSendSms :: UUID -> TestPhoneNumberOptions -> Maybe Text -> m (){- ^ Test a phone number by sending an SMS to it. NOTE this is only for internal use to check that a phone number is working. For end-to-end phone testing see https://docs.mailslurp.com/txt-sms/ -}
  , updatePhoneNumber :: UUID -> UpdatePhoneNumberOptions -> m PhoneNumberDto{- ^ Set field for phone number -}
  , validatePhoneNumber :: ValidatePhoneNumberOptions -> m PhoneNumberValidationDto{- ^ Validate a phone number -}
  , createNewRuleset :: Maybe UUID -> Maybe UUID -> CreateRulesetOptions -> m RulesetDto{- ^ Create a new inbox or phone number rule for forwarding, blocking, and allowing emails or SMS when sending and receiving -}
  , deleteRuleset :: UUID -> m (){- ^ Delete ruleset -}
  , deleteRulesets :: Maybe UUID -> Maybe UUID -> m (){- ^ Delete rulesets. Accepts optional inboxId or phoneId filters. -}
  , getRuleset :: UUID -> m RulesetDto{- ^ Get ruleset -}
  , getRulesets :: Maybe UUID -> Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageRulesetDto{- ^ List all rulesets attached to an inbox or phone or account -}
  , testInboxRulesetsForInbox :: Maybe UUID -> RulesetTestOptions -> m InboxRulesetTestResult{- ^ Test inbox rulesets for inbox -}
  , testNewRuleset :: TestNewInboxRulesetOptions -> m InboxRulesetTestResult{- ^ Test new ruleset -}
  , testRuleset :: UUID -> RulesetTestOptions -> m InboxRulesetTestResult{- ^ Test an inbox or phone ruleset -}
  , testRulesetReceiving :: TestRulesetReceivingOptions -> m TestRulesetReceivingResult{- ^ Test whether inbound emails from an email address would be blocked or allowed by inbox rulesets or test if phone number can receive SMS -}
  , testRulesetSending :: TestInboxRulesetSendingOptions -> m TestRulesetSendingResult{- ^ Test whether outbound emails to an email address would be blocked or allowed by inbox rulesets or whether a phone number can send SMS -}
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
  , getAllSmsMessages :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Bool -> Maybe [UUID] -> m PageSmsProjection{- ^  -}
  , getReplyForSmsMessage :: UUID -> m ReplyForSms{- ^ Get reply for an SMS message. -}
  , getSentSmsCount :: m CountDto{- ^ Get number of sent SMS -}
  , getSentSmsMessage :: UUID -> m SentSmsDto{- ^ Returns an SMS summary object with content. -}
  , getSentSmsMessagesPaginated :: Maybe UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageSentSmsProjection{- ^ By default returns all SMS messages across all phone numbers sorted by ascending created at date. Responses are paginated. You can restrict results to a list of phone number IDs. You can also filter out read messages -}
  , getSmsCodes :: UUID -> ExtractCodesOptions -> m ExtractCodesResult{- ^ Extract one-time passcodes and verification tokens from SMS body content. Deterministic `PATTERN` extraction is available now. Use method flags to control fallback behavior for QA. -}
  , getSmsCount :: m CountDto{- ^ Get number of SMS -}
  , getSmsMessage :: UUID -> m SmsDto{- ^ Returns a SMS summary object with content. -}
  , getUnreadSmsCount :: m UnreadCount{- ^ Get number of SMS unread. Unread means has not been viewed in dashboard or returned in an email API response -}
  , replyToSmsMessage :: UUID -> SmsReplyOptions -> m SentSmsDto{- ^ Reply to an SMS message. -}
  , sendSms :: Maybe Text -> Maybe UUID -> SmsSendOptions -> m SentSmsDto{- ^  -}
  , setSmsFavourited :: UUID -> Maybe Bool -> m SmsDto{- ^  -}
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
  , lookupMxRecord :: LookupMxRecordsOptions -> m LookupMxRecordsResults{- ^  -}
  , lookupTlsReportingDomain :: LookupTlsReportingDomainOptions -> m LookupTlsReportingDomainResults{- ^  -}
  , createTrackingPixel :: CreateTrackingPixelOptions -> m TrackingPixelDto{- ^ Create a tracking pixel. A tracking pixel is an image that can be embedded in an email. When the email is viewed and the image is seen MailSlurp will mark the pixel as seen. Use tracking pixels to monitor email open events. You can receive open notifications via webhook or by fetching the pixel. -}
  , getAllTrackingPixels :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> m PageTrackingPixelProjection{- ^ List tracking pixels in paginated form -}
  , getTrackingPixel :: UUID -> m TrackingPixelDto{- ^  -}
  , createOrUpdateInboxRetentionPolicyForAccount :: CreateInboxRetentionPolicyForAccountOptions -> m InboxRetentionPolicyDto{- ^ Create inbox retention policy for your global account -}
  , deleteInboxRetentionPolicyForAccount :: m EmptyResponseDto{- ^ Delete inbox retention policy for your global account -}
  , getEntityAutomations :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe Text -> m PageEntityAutomationItems{- ^  -}
  , getEntityEvents :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Text -> m PageEntityEventItems{- ^  -}
  , getEntityFavorites :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> m PageEntityFavouriteItems{- ^  -}
  , getInboxRetentionPolicyForAccount :: m InboxRetentionPolicyOptionalDto{- ^ Get inbox retention policy for your global account -}
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
  , createWebhookForAITransformer :: UUID -> CreateWebhookOptions -> m WebhookDto{- ^ Get notified whenever AI transformation pipeline converts and email or SMS into structured data via a WebHook URL. -}
  , createWebhookForPhoneNumber :: UUID -> CreateWebhookOptions -> m WebhookDto{- ^ Get notified whenever a phone number receives an SMS via a WebHook URL. -}
  , deleteAllWebhooks :: Maybe UTCTime -> m (){- ^  -}
  , deleteWebhook :: UUID -> UUID -> m (){- ^  -}
  , deleteWebhookById :: UUID -> m (){- ^  -}
  , getAllAccountWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> m PageWebhookProjection{- ^ List account webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getAllWebhookEndpoints :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> Maybe Text -> Maybe Text -> m PageWebhookEndpointProjection{- ^ List webhooks URL in paginated form. Allows for page index, page size, and sort direction. -}
  , getAllWebhookResults :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> m PageWebhookResult{- ^  -}
  , getAllWebhooks :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> m PageWebhookProjection{- ^ List webhooks in paginated form. Allows for page index, page size, and sort direction. -}
  , getInboxWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Bool -> m PageWebhookProjection{- ^  -}
  , getJsonSchemaForWebhookEvent :: Maybe Text -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload by event -}
  , getJsonSchemaForWebhookPayload :: UUID -> m JSONSchemaDto{- ^ Get JSON Schema definition for webhook payload -}
  , getPhoneNumberWebhooksPaginated :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> m PageWebhookProjection{- ^  -}
  , getTestWebhookPayload :: Maybe Text -> m AbstractWebhookPayload{- ^ Get test webhook payload example. Response content depends on eventName passed. Uses `EMAIL_RECEIVED` as default. -}
  , getTestWebhookPayloadBounce :: m WebhookBouncePayload{- ^ Get webhook test payload for bounce -}
  , getTestWebhookPayloadBounceRecipient :: m WebhookBounceRecipientPayload{- ^ Get webhook test payload for bounce recipient -}
  , getTestWebhookPayloadDeliveryStatus :: m WebhookDeliveryStatusPayload{- ^  -}
  , getTestWebhookPayloadEmailOpened :: m WebhookEmailOpenedPayload{- ^ Get webhook test payload for email opened event -}
  , getTestWebhookPayloadEmailRead :: m WebhookEmailReadPayload{- ^ Get webhook test payload for email opened event -}
  , getTestWebhookPayloadForWebhook :: UUID -> m AbstractWebhookPayload{- ^ Get example payload for webhook -}
  , getTestWebhookPayloadNewAITransformResult :: m WebhookNewAITransformResultPayload{- ^  -}
  , getTestWebhookPayloadNewAttachment :: m WebhookNewAttachmentPayload{- ^  -}
  , getTestWebhookPayloadNewContact :: m WebhookNewContactPayload{- ^  -}
  , getTestWebhookPayloadNewEmail :: m WebhookNewEmailPayload{- ^  -}
  , getTestWebhookPayloadNewSms :: m WebhookNewSmsPayload{- ^  -}
  , getWebhook :: UUID -> m WebhookDto{- ^  -}
  , getWebhookResult :: UUID -> m WebhookResultDto{- ^  -}
  , getWebhookResults :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> m PageWebhookResult{- ^  -}
  , getWebhookResultsCount :: UUID -> m CountDto{- ^  -}
  , getWebhookResultsUnseenErrorCount :: m UnseenErrorCountDto{- ^  -}
  , getWebhooks :: UUID -> Maybe Int -> Maybe Int -> Maybe Text -> m [WebhookProjection]{- ^  -}
  , redriveAllWebhookResults :: m WebhookRedriveAllResult{- ^ Allows you to resend webhook payloads for any recorded webhook result that failed to deliver the payload. -}
  , redriveWebhookResult :: UUID -> m WebhookRedriveResult{- ^ Allows you to resend a webhook payload that was already sent. Webhooks that fail are retried automatically for 24 hours and then put in a dead letter queue. You can retry results manually using this method. -}
  , sendTestData :: UUID -> m WebhookTestResult{- ^  -}
  , updateWebhook :: UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe Bool -> CreateWebhookOptions -> m WebhookDto{- ^  -}
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
    ((coerce -> createTransformer) :<|>
     (coerce -> createTransformerMappings) :<|>
     (coerce -> deleteAllTransformerMappings) :<|>
     (coerce -> deleteTransformer) :<|>
     (coerce -> deleteTransformerMapping) :<|>
     (coerce -> deleteTransformers) :<|>
     (coerce -> exportTransformerResults) :<|>
     (coerce -> generateStructuredContentFromAttachment) :<|>
     (coerce -> generateStructuredContentFromEmail) :<|>
     (coerce -> generateStructuredContentFromSms) :<|>
     (coerce -> getExportTransformerResultsJob) :<|>
     (coerce -> getTransformer) :<|>
     (coerce -> getTransformerMapping) :<|>
     (coerce -> getTransformerMappings) :<|>
     (coerce -> getTransformerResult) :<|>
     (coerce -> getTransformerResults) :<|>
     (coerce -> getTransformerResultsTable) :<|>
     (coerce -> getTransformers) :<|>
     (coerce -> invokeTransformer) :<|>
     (coerce -> testTransformerMappingMatch) :<|>
     (coerce -> validateStructuredOutputSchema) :<|>
     (coerce -> createAlias) :<|>
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
     (coerce -> getAuditLogByEventId) :<|>
     (coerce -> getAuditLogs) :<|>
     (coerce -> searchAuditLogs) :<|>
     (coerce -> deleteAllAttachments) :<|>
     (coerce -> deleteAttachment) :<|>
     (coerce -> downloadAttachmentAsBase64Encoded) :<|>
     (coerce -> downloadAttachmentAsBytes) :<|>
     (coerce -> extractAttachmentText) :<|>
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
     (coerce -> getReputationItems) :<|>
     (coerce -> getTenantReputationFindings) :<|>
     (coerce -> getTenantReputationStatusSummary) :<|>
     (coerce -> bulkCreateInboxes) :<|>
     (coerce -> bulkDeleteInboxes) :<|>
     (coerce -> bulkSendEmails) :<|>
     (coerce -> createCampaignProbe) :<|>
     (coerce -> deleteCampaignProbe) :<|>
     (coerce -> getCampaignProbe) :<|>
     (coerce -> getCampaignProbeInsights) :<|>
     (coerce -> getCampaignProbeRuns) :<|>
     (coerce -> getCampaignProbeSeries) :<|>
     (coerce -> getCampaignProbes) :<|>
     (coerce -> runCampaignProbeNow) :<|>
     (coerce -> runDueCampaignProbes) :<|>
     (coerce -> updateCampaignProbe) :<|>
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
     (coerce -> cancelDeliverabilitySimulationJob) :<|>
     (coerce -> createDeliverabilitySimulationJob) :<|>
     (coerce -> createDeliverabilityTest) :<|>
     (coerce -> deleteDeliverabilityTest) :<|>
     (coerce -> duplicateDeliverabilityTest) :<|>
     (coerce -> exportDeliverabilityTestReport) :<|>
     (coerce -> exportDeliverabilityTestResults) :<|>
     (coerce -> getDeliverabilityAnalyticsSeries) :<|>
     (coerce -> getDeliverabilityFailureHotspots) :<|>
     (coerce -> getDeliverabilitySimulationJob) :<|>
     (coerce -> getDeliverabilitySimulationJobEvents) :<|>
     (coerce -> getDeliverabilityTest) :<|>
     (coerce -> getDeliverabilityTestResults) :<|>
     (coerce -> getDeliverabilityTests) :<|>
     (coerce -> getLatestDeliverabilitySimulationJob) :<|>
     (coerce -> pauseDeliverabilitySimulationJob) :<|>
     (coerce -> pauseDeliverabilityTest) :<|>
     (coerce -> pollDeliverabilityTestStatus) :<|>
     (coerce -> resumeDeliverabilitySimulationJob) :<|>
     (coerce -> startDeliverabilityTest) :<|>
     (coerce -> stopDeliverabilityTest) :<|>
     (coerce -> updateDeliverabilityTest) :<|>
     (coerce -> cancelDevicePreviewRun) :<|>
     (coerce -> createDevicePreviewFeedback) :<|>
     (coerce -> createDevicePreviewRun) :<|>
     (coerce -> deleteDevicePreviewRun) :<|>
     (coerce -> ensureDevicePreviewRun) :<|>
     (coerce -> getDevicePreviewFeedback) :<|>
     (coerce -> getDevicePreviewFeedbackItems) :<|>
     (coerce -> getDevicePreviewRun) :<|>
     (coerce -> getDevicePreviewRunProviderProgress) :<|>
     (coerce -> getDevicePreviewRunResults) :<|>
     (coerce -> getDevicePreviewRunScreenshot) :<|>
     (coerce -> getDevicePreviewRuns) :<|>
     (coerce -> getDevicePreviewRunsForAccount) :<|>
     (coerce -> getDevicePreviewRunsOffsetPaginated) :<|>
     (coerce -> updateDevicePreviewFeedback) :<|>
     (coerce -> addDomainWildcardCatchAll) :<|>
     (coerce -> createDomain) :<|>
     (coerce -> deleteDomain) :<|>
     (coerce -> getAvailableDomainRegions) :<|>
     (coerce -> getAvailableDomains) :<|>
     (coerce -> getDomain) :<|>
     (coerce -> getDomainIssues) :<|>
     (coerce -> getDomainWildcardCatchAllInbox) :<|>
     (coerce -> getDomains) :<|>
     (coerce -> getMailSlurpDomains) :<|>
     (coerce -> updateDomain) :<|>
     (coerce -> createDomainMonitor) :<|>
     (coerce -> createDomainMonitorAlertSink) :<|>
     (coerce -> deleteDomainMonitor) :<|>
     (coerce -> deleteDomainMonitorAlertSink) :<|>
     (coerce -> getDomainMonitor) :<|>
     (coerce -> getDomainMonitorAlertSinks) :<|>
     (coerce -> getDomainMonitorInsights) :<|>
     (coerce -> getDomainMonitorRuns) :<|>
     (coerce -> getDomainMonitorSeries) :<|>
     (coerce -> getDomainMonitors) :<|>
     (coerce -> runDomainMonitorNow) :<|>
     (coerce -> runDueDomainMonitors) :<|>
     (coerce -> updateDomainMonitor) :<|>
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
     (coerce -> getEmailCodes) :<|>
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
     (coerce -> getEmailSignature) :<|>
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
     (coerce -> doesInboxHaveAutomations) :<|>
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
     (coerce -> getOrCreateInboxPlusAddress) :<|>
     (coerce -> getOrCreatePlusAddressByFullAddress) :<|>
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
     (coerce -> createTotpDeviceForBase32SecretKey) :<|>
     (coerce -> createTotpDeviceForCustom) :<|>
     (coerce -> createTotpDeviceForOtpAuthUrl) :<|>
     (coerce -> getTotpDevice) :<|>
     (coerce -> getTotpDeviceBy) :<|>
     (coerce -> getTotpDeviceCode) :<|>
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
     (coerce -> getAllMissedSmsMessages) :<|>
     (coerce -> getMissedSmsCount) :<|>
     (coerce -> getMissedSmsMessage) :<|>
     (coerce -> createEmergencyAddress) :<|>
     (coerce -> createPhoneNumber) :<|>
     (coerce -> deleteAllPhoneNumber) :<|>
     (coerce -> deleteEmergencyAddress) :<|>
     (coerce -> deletePhoneMessageThreadItems) :<|>
     (coerce -> deletePhoneNumber) :<|>
     (coerce -> getAllPhoneMessageThreads) :<|>
     (coerce -> getAllPhoneNumberReleases) :<|>
     (coerce -> getConsentStatus) :<|>
     (coerce -> getEmergencyAddress) :<|>
     (coerce -> getEmergencyAddresses) :<|>
     (coerce -> getPhoneMessageThreadItems) :<|>
     (coerce -> getPhoneMessageThreads) :<|>
     (coerce -> getPhoneNumber) :<|>
     (coerce -> getPhoneNumberByName) :<|>
     (coerce -> getPhoneNumberByPhoneNumber) :<|>
     (coerce -> getPhoneNumberRelease) :<|>
     (coerce -> getPhoneNumbers) :<|>
     (coerce -> getPhonePlans) :<|>
     (coerce -> getPhonePlansAvailability) :<|>
     (coerce -> getPhoneSummary) :<|>
     (coerce -> getSentSmsByPhoneNumber) :<|>
     (coerce -> getSmsByPhoneNumber) :<|>
     (coerce -> reassignPhoneNumberRelease) :<|>
     (coerce -> sendSmsFromPhoneNumber) :<|>
     (coerce -> setConsentStatus) :<|>
     (coerce -> setPhoneFavourited) :<|>
     (coerce -> testPhoneNumberSendSms) :<|>
     (coerce -> updatePhoneNumber) :<|>
     (coerce -> validatePhoneNumber) :<|>
     (coerce -> createNewRuleset) :<|>
     (coerce -> deleteRuleset) :<|>
     (coerce -> deleteRulesets) :<|>
     (coerce -> getRuleset) :<|>
     (coerce -> getRulesets) :<|>
     (coerce -> testInboxRulesetsForInbox) :<|>
     (coerce -> testNewRuleset) :<|>
     (coerce -> testRuleset) :<|>
     (coerce -> testRulesetReceiving) :<|>
     (coerce -> testRulesetSending) :<|>
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
     (coerce -> getSmsCodes) :<|>
     (coerce -> getSmsCount) :<|>
     (coerce -> getSmsMessage) :<|>
     (coerce -> getUnreadSmsCount) :<|>
     (coerce -> replyToSmsMessage) :<|>
     (coerce -> sendSms) :<|>
     (coerce -> setSmsFavourited) :<|>
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
     (coerce -> lookupMxRecord) :<|>
     (coerce -> lookupTlsReportingDomain) :<|>
     (coerce -> createTrackingPixel) :<|>
     (coerce -> getAllTrackingPixels) :<|>
     (coerce -> getTrackingPixel) :<|>
     (coerce -> createOrUpdateInboxRetentionPolicyForAccount) :<|>
     (coerce -> deleteInboxRetentionPolicyForAccount) :<|>
     (coerce -> getEntityAutomations) :<|>
     (coerce -> getEntityEvents) :<|>
     (coerce -> getEntityFavorites) :<|>
     (coerce -> getInboxRetentionPolicyForAccount) :<|>
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
     (coerce -> createWebhookForAITransformer) :<|>
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
     (coerce -> getTestWebhookPayloadNewAITransformResult) :<|>
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
      (coerce createTransformer :<|>
       coerce createTransformerMappings :<|>
       coerce deleteAllTransformerMappings :<|>
       coerce deleteTransformer :<|>
       coerce deleteTransformerMapping :<|>
       coerce deleteTransformers :<|>
       coerce exportTransformerResults :<|>
       coerce generateStructuredContentFromAttachment :<|>
       coerce generateStructuredContentFromEmail :<|>
       coerce generateStructuredContentFromSms :<|>
       coerce getExportTransformerResultsJob :<|>
       coerce getTransformer :<|>
       coerce getTransformerMapping :<|>
       coerce getTransformerMappings :<|>
       coerce getTransformerResult :<|>
       coerce getTransformerResults :<|>
       coerce getTransformerResultsTable :<|>
       coerce getTransformers :<|>
       coerce invokeTransformer :<|>
       coerce testTransformerMappingMatch :<|>
       coerce validateStructuredOutputSchema :<|>
       coerce createAlias :<|>
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
       coerce getAuditLogByEventId :<|>
       coerce getAuditLogs :<|>
       coerce searchAuditLogs :<|>
       coerce deleteAllAttachments :<|>
       coerce deleteAttachment :<|>
       coerce downloadAttachmentAsBase64Encoded :<|>
       coerce downloadAttachmentAsBytes :<|>
       coerce extractAttachmentText :<|>
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
       coerce getReputationItems :<|>
       coerce getTenantReputationFindings :<|>
       coerce getTenantReputationStatusSummary :<|>
       coerce bulkCreateInboxes :<|>
       coerce bulkDeleteInboxes :<|>
       coerce bulkSendEmails :<|>
       coerce createCampaignProbe :<|>
       coerce deleteCampaignProbe :<|>
       coerce getCampaignProbe :<|>
       coerce getCampaignProbeInsights :<|>
       coerce getCampaignProbeRuns :<|>
       coerce getCampaignProbeSeries :<|>
       coerce getCampaignProbes :<|>
       coerce runCampaignProbeNow :<|>
       coerce runDueCampaignProbes :<|>
       coerce updateCampaignProbe :<|>
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
       coerce cancelDeliverabilitySimulationJob :<|>
       coerce createDeliverabilitySimulationJob :<|>
       coerce createDeliverabilityTest :<|>
       coerce deleteDeliverabilityTest :<|>
       coerce duplicateDeliverabilityTest :<|>
       coerce exportDeliverabilityTestReport :<|>
       coerce exportDeliverabilityTestResults :<|>
       coerce getDeliverabilityAnalyticsSeries :<|>
       coerce getDeliverabilityFailureHotspots :<|>
       coerce getDeliverabilitySimulationJob :<|>
       coerce getDeliverabilitySimulationJobEvents :<|>
       coerce getDeliverabilityTest :<|>
       coerce getDeliverabilityTestResults :<|>
       coerce getDeliverabilityTests :<|>
       coerce getLatestDeliverabilitySimulationJob :<|>
       coerce pauseDeliverabilitySimulationJob :<|>
       coerce pauseDeliverabilityTest :<|>
       coerce pollDeliverabilityTestStatus :<|>
       coerce resumeDeliverabilitySimulationJob :<|>
       coerce startDeliverabilityTest :<|>
       coerce stopDeliverabilityTest :<|>
       coerce updateDeliverabilityTest :<|>
       coerce cancelDevicePreviewRun :<|>
       coerce createDevicePreviewFeedback :<|>
       coerce createDevicePreviewRun :<|>
       coerce deleteDevicePreviewRun :<|>
       coerce ensureDevicePreviewRun :<|>
       coerce getDevicePreviewFeedback :<|>
       coerce getDevicePreviewFeedbackItems :<|>
       coerce getDevicePreviewRun :<|>
       coerce getDevicePreviewRunProviderProgress :<|>
       coerce getDevicePreviewRunResults :<|>
       coerce getDevicePreviewRunScreenshot :<|>
       coerce getDevicePreviewRuns :<|>
       coerce getDevicePreviewRunsForAccount :<|>
       coerce getDevicePreviewRunsOffsetPaginated :<|>
       coerce updateDevicePreviewFeedback :<|>
       coerce addDomainWildcardCatchAll :<|>
       coerce createDomain :<|>
       coerce deleteDomain :<|>
       coerce getAvailableDomainRegions :<|>
       coerce getAvailableDomains :<|>
       coerce getDomain :<|>
       coerce getDomainIssues :<|>
       coerce getDomainWildcardCatchAllInbox :<|>
       coerce getDomains :<|>
       coerce getMailSlurpDomains :<|>
       coerce updateDomain :<|>
       coerce createDomainMonitor :<|>
       coerce createDomainMonitorAlertSink :<|>
       coerce deleteDomainMonitor :<|>
       coerce deleteDomainMonitorAlertSink :<|>
       coerce getDomainMonitor :<|>
       coerce getDomainMonitorAlertSinks :<|>
       coerce getDomainMonitorInsights :<|>
       coerce getDomainMonitorRuns :<|>
       coerce getDomainMonitorSeries :<|>
       coerce getDomainMonitors :<|>
       coerce runDomainMonitorNow :<|>
       coerce runDueDomainMonitors :<|>
       coerce updateDomainMonitor :<|>
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
       coerce getEmailCodes :<|>
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
       coerce getEmailSignature :<|>
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
       coerce doesInboxHaveAutomations :<|>
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
       coerce getOrCreateInboxPlusAddress :<|>
       coerce getOrCreatePlusAddressByFullAddress :<|>
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
       coerce createTotpDeviceForBase32SecretKey :<|>
       coerce createTotpDeviceForCustom :<|>
       coerce createTotpDeviceForOtpAuthUrl :<|>
       coerce getTotpDevice :<|>
       coerce getTotpDeviceBy :<|>
       coerce getTotpDeviceCode :<|>
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
       coerce getAllMissedSmsMessages :<|>
       coerce getMissedSmsCount :<|>
       coerce getMissedSmsMessage :<|>
       coerce createEmergencyAddress :<|>
       coerce createPhoneNumber :<|>
       coerce deleteAllPhoneNumber :<|>
       coerce deleteEmergencyAddress :<|>
       coerce deletePhoneMessageThreadItems :<|>
       coerce deletePhoneNumber :<|>
       coerce getAllPhoneMessageThreads :<|>
       coerce getAllPhoneNumberReleases :<|>
       coerce getConsentStatus :<|>
       coerce getEmergencyAddress :<|>
       coerce getEmergencyAddresses :<|>
       coerce getPhoneMessageThreadItems :<|>
       coerce getPhoneMessageThreads :<|>
       coerce getPhoneNumber :<|>
       coerce getPhoneNumberByName :<|>
       coerce getPhoneNumberByPhoneNumber :<|>
       coerce getPhoneNumberRelease :<|>
       coerce getPhoneNumbers :<|>
       coerce getPhonePlans :<|>
       coerce getPhonePlansAvailability :<|>
       coerce getPhoneSummary :<|>
       coerce getSentSmsByPhoneNumber :<|>
       coerce getSmsByPhoneNumber :<|>
       coerce reassignPhoneNumberRelease :<|>
       coerce sendSmsFromPhoneNumber :<|>
       coerce setConsentStatus :<|>
       coerce setPhoneFavourited :<|>
       coerce testPhoneNumberSendSms :<|>
       coerce updatePhoneNumber :<|>
       coerce validatePhoneNumber :<|>
       coerce createNewRuleset :<|>
       coerce deleteRuleset :<|>
       coerce deleteRulesets :<|>
       coerce getRuleset :<|>
       coerce getRulesets :<|>
       coerce testInboxRulesetsForInbox :<|>
       coerce testNewRuleset :<|>
       coerce testRuleset :<|>
       coerce testRulesetReceiving :<|>
       coerce testRulesetSending :<|>
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
       coerce getSmsCodes :<|>
       coerce getSmsCount :<|>
       coerce getSmsMessage :<|>
       coerce getUnreadSmsCount :<|>
       coerce replyToSmsMessage :<|>
       coerce sendSms :<|>
       coerce setSmsFavourited :<|>
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
       coerce lookupMxRecord :<|>
       coerce lookupTlsReportingDomain :<|>
       coerce createTrackingPixel :<|>
       coerce getAllTrackingPixels :<|>
       coerce getTrackingPixel :<|>
       coerce createOrUpdateInboxRetentionPolicyForAccount :<|>
       coerce deleteInboxRetentionPolicyForAccount :<|>
       coerce getEntityAutomations :<|>
       coerce getEntityEvents :<|>
       coerce getEntityFavorites :<|>
       coerce getInboxRetentionPolicyForAccount :<|>
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
       coerce createWebhookForAITransformer :<|>
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
       coerce getTestWebhookPayloadNewAITransformResult :<|>
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
