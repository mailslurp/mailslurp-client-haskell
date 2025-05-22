{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module MailSlurp.Types (
  AbstractWebhookPayload (..),
  AccountBounceBlockDto (..),
  AliasDto (..),
  AliasProjection (..),
  AliasThreadProjection (..),
  AttachmentEntityDto (..),
  AttachmentMetaData (..),
  AttachmentProjection (..),
  BasicAuthOptions (..),
  BounceProjection (..),
  BounceRecipientProjection (..),
  BouncedEmailDto (..),
  BouncedRecipientDto (..),
  BulkSendEmailOptions (..),
  CanSendEmailResults (..),
  CheckEmailBodyFeatureSupportResults (..),
  CheckEmailBodyResults (..),
  CheckEmailClientSupportOptions (..),
  CheckEmailClientSupportResults (..),
  CheckEmailFeaturesClientSupportOptions (..),
  CheckEmailFeaturesClientSupportResults (..),
  Complaint (..),
  ConditionOption (..),
  ConnectorDto (..),
  ConnectorEventDto (..),
  ConnectorEventProjection (..),
  ConnectorImapConnectionDto (..),
  ConnectorImapConnectionTestResult (..),
  ConnectorProjection (..),
  ConnectorProviderSettingsDto (..),
  ConnectorSmtpConnectionDto (..),
  ConnectorSmtpConnectionTestResult (..),
  ConnectorSyncRequestResult (..),
  ConnectorSyncResult (..),
  ConnectorSyncSettingsDto (..),
  ConsentStatusDto (..),
  ContactDto (..),
  ContactProjection (..),
  ContentMatchOptions (..),
  CountDto (..),
  CreateAliasOptions (..),
  CreateConnectorImapConnectionOptions (..),
  CreateConnectorOptions (..),
  CreateConnectorSmtpConnectionOptions (..),
  CreateConnectorSyncSettingsOptions (..),
  CreateConnectorWithOptions (..),
  CreateContactOptions (..),
  CreateDomainOptions (..),
  CreateEmergencyAddressOptions (..),
  CreateGroupOptions (..),
  CreateInboxDto (..),
  CreateInboxForwarderOptions (..),
  CreateInboxReplierOptions (..),
  CreateInboxRulesetOptions (..),
  CreateOAuthConnectionResult (..),
  CreateOAuthExchangeResult (..),
  CreatePhoneNumberOptions (..),
  CreatePortalOptions (..),
  CreatePortalUserOptions (..),
  CreateTemplateOptions (..),
  CreateTrackingPixelOptions (..),
  CreateWebhookOptions (..),
  DNSLookupOptions (..),
  DNSLookupResult (..),
  DNSLookupResults (..),
  DNSLookupsOptions (..),
  DeliveryStatusDto (..),
  DescribeDomainOptions (..),
  DescribeMailServerDomainResult (..),
  DomainDto (..),
  DomainGroup (..),
  DomainGroupsDto (..),
  DomainInformation (..),
  DomainIssuesDto (..),
  DomainNameRecord (..),
  DomainPreview (..),
  DownloadAttachmentDto (..),
  Email (..),
  EmailAnalysis (..),
  EmailAvailableResult (..),
  EmailContentMatchResult (..),
  EmailContentPartResult (..),
  EmailFeatureCategoryName (..),
  EmailFeatureFamilyName (..),
  EmailFeatureFamilyStatistics (..),
  EmailFeatureNames (..),
  EmailFeatureOverview (..),
  EmailFeaturePlatformName (..),
  EmailFeaturePlatformStatistics (..),
  EmailFeatureSupportFlags (..),
  EmailFeatureSupportResult (..),
  EmailFeatureSupportStatusPercentage (..),
  EmailFeatureVersionStatistics (..),
  EmailHtmlDto (..),
  EmailLinksResult (..),
  EmailPreview (..),
  EmailPreviewUrls (..),
  EmailProjection (..),
  EmailRecipients (..),
  EmailRecipientsProjection (..),
  EmailScreenshotResult (..),
  EmailTextLinesResult (..),
  EmailThreadDto (..),
  EmailThreadItem (..),
  EmailThreadItemsDto (..),
  EmailThreadProjection (..),
  EmailValidationRequestDto (..),
  EmailVerificationResult (..),
  EmergencyAddress (..),
  EmergencyAddressDto (..),
  EmptyResponseDto (..),
  EntityAutomationItemProjection (..),
  EntityEventItemProjection (..),
  EntityFavouriteItemProjection (..),
  ExpirationDefaults (..),
  ExpiredInboxDto (..),
  ExpiredInboxRecordProjection (..),
  ExportLink (..),
  ExportOptions (..),
  FakeEmailDto (..),
  FakeEmailPreview (..),
  FakeEmailResult (..),
  FilterBouncedRecipientsOptions (..),
  FilterBouncedRecipientsResult (..),
  FlushExpiredInboxesResult (..),
  ForwardEmailOptions (..),
  GenerateBimiRecordOptions (..),
  GenerateBimiRecordResults (..),
  GenerateDmarcRecordOptions (..),
  GenerateDmarcRecordResults (..),
  GenerateMtaStsRecordOptions (..),
  GenerateMtaStsRecordResults (..),
  GenerateStructuredContentEmailOptions (..),
  GenerateTlsReportingRecordOptions (..),
  GenerateTlsReportingRecordResults (..),
  GetEmailScreenshotOptions (..),
  GravatarUrl (..),
  GroupContactsDto (..),
  GroupDto (..),
  GroupProjection (..),
  GuestPortalDto (..),
  GuestPortalUserCreateDto (..),
  GuestPortalUserDto (..),
  GuestPortalUserProjection (..),
  HTMLValidationResult (..),
  IPAddressResult (..),
  ImageIssue (..),
  ImapAccessDetails (..),
  ImapEmailProjection (..),
  ImapFlagOperationOptions (..),
  ImapMailboxStatus (..),
  ImapServerFetchItem (..),
  ImapServerFetchResult (..),
  ImapServerGetResult (..),
  ImapServerListOptions (..),
  ImapServerListResult (..),
  ImapServerMailboxResult (..),
  ImapServerSearchOptions (..),
  ImapServerSearchResult (..),
  ImapServerStatusOptions (..),
  ImapServerStatusResult (..),
  ImapSmtpAccessDetails (..),
  ImapSmtpAccessServers (..),
  ImapUpdateFlagsOptions (..),
  InboxByEmailAddressResult (..),
  InboxByNameResult (..),
  InboxDto (..),
  InboxExistsDto (..),
  InboxForwarderDto (..),
  InboxForwarderEventDto (..),
  InboxForwarderEventProjection (..),
  InboxForwarderTestOptions (..),
  InboxForwarderTestResult (..),
  InboxIdItem (..),
  InboxIdsResult (..),
  InboxPreview (..),
  InboxReplierDto (..),
  InboxReplierEventProjection (..),
  InboxRulesetDto (..),
  InboxRulesetTestOptions (..),
  InboxRulesetTestResult (..),
  InlineObject (..),
  JSONSchemaDto (..),
  LinkIssue (..),
  ListUnsubscribeRecipientProjection (..),
  LookupBimiDomainOptions (..),
  LookupBimiDomainResults (..),
  LookupDmarcDomainOptions (..),
  LookupDmarcDomainResults (..),
  LookupMtaStsDomainOptions (..),
  LookupMtaStsDomainResults (..),
  LookupTlsReportingDomainOptions (..),
  LookupTlsReportingDomainResults (..),
  MatchOption (..),
  MatchOptions (..),
  MissedEmailDto (..),
  MissedEmailProjection (..),
  NameServerRecord (..),
  NewFakeEmailAddressResult (..),
  OptInConsentOptions (..),
  OptInConsentSendResult (..),
  OptInIdentityProjection (..),
  OptInSendingConsentDto (..),
  OptionalConnectorDto (..),
  OptionalConnectorImapConnectionDto (..),
  OptionalConnectorSmtpConnectionDto (..),
  OptionalConnectorSyncSettingsDto (..),
  OrganizationInboxProjection (..),
  PageAlias (..),
  PageAliasThreadProjection (..),
  PageAttachmentEntity (..),
  PageBouncedEmail (..),
  PageBouncedRecipients (..),
  PageComplaint (..),
  PageConnector (..),
  PageConnectorEvents (..),
  PageContactProjection (..),
  PageDeliveryStatus (..),
  PageEmailPreview (..),
  PageEmailProjection (..),
  PageEmailThreadProjection (..),
  PageEmailValidationRequest (..),
  PageEntityAutomationItems (..),
  PageEntityEventItems (..),
  PageEntityFavouriteItems (..),
  PageExpiredInboxRecordProjection (..),
  PageGroupProjection (..),
  PageGuestPortalUsers (..),
  PageInboxForwarderDto (..),
  PageInboxForwarderEvents (..),
  PageInboxProjection (..),
  PageInboxReplierDto (..),
  PageInboxReplierEvents (..),
  PageInboxRulesetDto (..),
  PageInboxTags (..),
  PageListUnsubscribeRecipients (..),
  PageMissedEmailProjection (..),
  PageOptInIdentityProjection (..),
  PageOrganizationInboxProjection (..),
  PagePhoneMessageThreadItemProjection (..),
  PagePhoneMessageThreadProjection (..),
  PagePhoneNumberProjection (..),
  PagePlusAddressProjection (..),
  PageReputationItems (..),
  PageScheduledJobs (..),
  PageSentEmailProjection (..),
  PageSentEmailWithQueueProjection (..),
  PageSentSmsProjection (..),
  PageSmsProjection (..),
  PageTemplateProjection (..),
  PageTrackingPixelProjection (..),
  PageUnknownMissedEmailProjection (..),
  PageWebhookEndpointProjection (..),
  PageWebhookProjection (..),
  PageWebhookResult (..),
  PageableObject (..),
  PhoneMessageThreadItemProjection (..),
  PhoneMessageThreadProjection (..),
  PhoneNumberDto (..),
  PhoneNumberProjection (..),
  PhoneNumberValidationDto (..),
  PhonePlanAvailability (..),
  PhonePlanAvailabilityItem (..),
  PhonePlanDto (..),
  PlusAddressDto (..),
  PlusAddressProjection (..),
  ProviderSettings (..),
  RawEmailJson (..),
  Recipient (..),
  RecipientProjection (..),
  ReplyForSms (..),
  ReplyToAliasEmailOptions (..),
  ReplyToEmailOptions (..),
  ReputationItemProjection (..),
  ScheduledJob (..),
  ScheduledJobDto (..),
  SearchEmailsOptions (..),
  SearchInboxesOptions (..),
  SendEmailBodyPart (..),
  SendEmailOptions (..),
  SendOptInConsentEmailOptions (..),
  SendSMTPEnvelopeOptions (..),
  SendWithQueueResult (..),
  Sender (..),
  SenderProjection (..),
  SentEmailDto (..),
  SentEmailProjection (..),
  SentSmsDto (..),
  SentSmsProjection (..),
  ServerEndpoints (..),
  SetInboxFavouritedOptions (..),
  SetPhoneFavouritedOptions (..),
  SimpleSendEmailOptions (..),
  SmsDto (..),
  SmsMatchOption (..),
  SmsPreview (..),
  SmsProjection (..),
  SmsReplyOptions (..),
  SmsSendOptions (..),
  SmtpAccessDetails (..),
  SortObject (..),
  SpellingIssue (..),
  StructuredContentResult (..),
  StructuredOutputSchema (..),
  TemplateDto (..),
  TemplatePreview (..),
  TemplateProjection (..),
  TemplateVariable (..),
  TestInboxRulesetReceivingOptions (..),
  TestInboxRulesetReceivingResult (..),
  TestInboxRulesetSendingOptions (..),
  TestInboxRulesetSendingResult (..),
  TestNewInboxForwarderOptions (..),
  TestNewInboxRulesetOptions (..),
  TestPhoneNumberOptions (..),
  TrackingPixelDto (..),
  TrackingPixelProjection (..),
  UnknownMissedEmailProjection (..),
  UnreadCount (..),
  UnseenErrorCountDto (..),
  UpdateAliasOptions (..),
  UpdateDomainOptions (..),
  UpdateGroupContacts (..),
  UpdateImapAccessOptions (..),
  UpdateInboxOptions (..),
  UpdateInboxReplierOptions (..),
  UpdatePhoneNumberOptions (..),
  UpdateSmtpAccessOptions (..),
  UploadAttachmentOptions (..),
  UserInfoDto (..),
  ValidateEmailAddressListOptions (..),
  ValidateEmailAddressListResult (..),
  ValidatePhoneNumberOptions (..),
  ValidationDto (..),
  ValidationMessage (..),
  VerifyEmailAddressOptions (..),
  VerifyWebhookSignatureOptions (..),
  VerifyWebhookSignatureResults (..),
  WaitForConditions (..),
  WaitForSingleSmsOptions (..),
  WaitForSmsConditions (..),
  WebhookBouncePayload (..),
  WebhookBounceRecipientPayload (..),
  WebhookDeliveryStatusPayload (..),
  WebhookDto (..),
  WebhookEmailOpenedPayload (..),
  WebhookEmailReadPayload (..),
  WebhookEndpointProjection (..),
  WebhookHeaderNameValue (..),
  WebhookHeaders (..),
  WebhookNewAttachmentPayload (..),
  WebhookNewContactPayload (..),
  WebhookNewEmailPayload (..),
  WebhookNewSmsPayload (..),
  WebhookProjection (..),
  WebhookRedriveAllResult (..),
  WebhookRedriveResult (..),
  WebhookResultDto (..),
  WebhookTestRequest (..),
  WebhookTestResponse (..),
  WebhookTestResult (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | Abstract webhook payload. Use the correct payload type for your webhook event type in order to access all the specific properties for that event. See the &#x60;NEW_EMAIL&#x60;,&#x60;NEW_CONTACT&#x60;, &#x60;NEW_ATTACHMENT&#x60; and &#x60;EMAIL_OPENED&#x60; payloads for the properties available for those events.
data AbstractWebhookPayload = AbstractWebhookPayload
  { abstractWebhookPayloadEventName :: Text -- ^ 
  , abstractWebhookPayloadMessageId :: Text -- ^ 
  , abstractWebhookPayloadWebhookId :: UUID -- ^ 
  , abstractWebhookPayloadWebhookName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AbstractWebhookPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "abstractWebhookPayload")
instance ToJSON AbstractWebhookPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "abstractWebhookPayload")


-- | 
data AccountBounceBlockDto = AccountBounceBlockDto
  { accountBounceBlockDtoIsFrozen :: Bool -- ^ 
  , accountBounceBlockDtoIsSendingBlocked :: Bool -- ^ 
  , accountBounceBlockDtoBounceCount :: Integer -- ^ 
  , accountBounceBlockDtoBounceCountToday :: Integer -- ^ 
  , accountBounceBlockDtoMaximumDailyBounces :: Integer -- ^ 
  , accountBounceBlockDtoMaximumAccountBounces :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AccountBounceBlockDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "accountBounceBlockDto")
instance ToJSON AccountBounceBlockDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "accountBounceBlockDto")


-- | Email alias representation
data AliasDto = AliasDto
  { aliasDtoId :: UUID -- ^ 
  , aliasDtoEmailAddress :: Text -- ^ The alias's email address for receiving email
  , aliasDtoMaskedEmailAddress :: Maybe Text -- ^ The underlying email address that is hidden and will received forwarded email
  , aliasDtoUserId :: UUID -- ^ 
  , aliasDtoInboxId :: UUID -- ^ Inbox that is associated with the alias
  , aliasDtoName :: Maybe Text -- ^ 
  , aliasDtoUseThreads :: Maybe Bool -- ^ If alias will generate response threads or not when email are received by it
  , aliasDtoIsVerified :: Bool -- ^ Has the alias been verified. You must verify an alias if the masked email address has not yet been verified by your account
  , aliasDtoDomainId :: Maybe UUID -- ^ Domain ID associated with the alias
  , aliasDtoCreatedAt :: Maybe UTCTime -- ^ 
  , aliasDtoUpdatedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasDto")
instance ToJSON AliasDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasDto")


-- | Representation of a alias
data AliasProjection = AliasProjection
  { aliasProjectionUserId :: UUID -- ^ 
  , aliasProjectionEmailAddress :: Text -- ^ 
  , aliasProjectionInboxId :: UUID -- ^ 
  , aliasProjectionUpdatedAt :: UTCTime -- ^ 
  , aliasProjectionCreatedAt :: UTCTime -- ^ 
  , aliasProjectionUseThreads :: Maybe Bool -- ^ 
  , aliasProjectionName :: Maybe Text -- ^ 
  , aliasProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasProjection")
instance ToJSON AliasProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasProjection")


-- | A thread is a message thread created for a message received by an alias
data AliasThreadProjection = AliasThreadProjection
  { aliasThreadProjectionUserId :: UUID -- ^ User ID
  , aliasThreadProjectionInboxId :: UUID -- ^ Inbox ID
  , aliasThreadProjectionUpdatedAt :: UTCTime -- ^ Updated at DateTime
  , aliasThreadProjectionCreatedAt :: UTCTime -- ^ Created at DateTime
  , aliasThreadProjectionTo :: [Text] -- ^ To recipients
  , aliasThreadProjectionCc :: Maybe [Text] -- ^ CC recipients
  , aliasThreadProjectionBcc :: Maybe [Text] -- ^ BCC recipients
  , aliasThreadProjectionAliasId :: UUID -- ^ Alias ID
  , aliasThreadProjectionSubject :: Maybe Text -- ^ Thread subject
  , aliasThreadProjectionName :: Maybe Text -- ^ Name of thread
  , aliasThreadProjectionId :: UUID -- ^ ID of email thread
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasThreadProjection")
instance ToJSON AliasThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasThreadProjection")


-- | DTO representation of an attachment.
data AttachmentEntityDto = AttachmentEntityDto
  { attachmentEntityDtoId :: UUID -- ^ The unique identifier for this attachment.
  , attachmentEntityDtoAttachmentId :: Text -- ^ The identifier of the attachment file
  , attachmentEntityDtoUserId :: UUID -- ^ The user identifier associated with this attachment.
  , attachmentEntityDtoContentType :: Maybe Text -- ^ The content type of the attachment.
  , attachmentEntityDtoContentLength :: Maybe Integer -- ^ The content length of the attachment in bytes.
  , attachmentEntityDtoContentId :: Maybe Text -- ^ The content identifier, which is a unique ID for the content part of the email.
  , attachmentEntityDtoName :: Maybe Text -- ^ The name of the attachment file.
  , attachmentEntityDtoInboxId :: Maybe UUID -- ^ The inbox identifier associated with this attachment.
  , attachmentEntityDtoCreatedAt :: UTCTime -- ^ The timestamp when this attachment was created.
  , attachmentEntityDtoUpdatedAt :: UTCTime -- ^ The timestamp when this attachment was last updated.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentEntityDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentEntityDto")
instance ToJSON AttachmentEntityDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentEntityDto")


-- | Meta data associated with an attachment. Attachments are stored as byte blobs so the meta data is stored separately.
data AttachmentMetaData = AttachmentMetaData
  { attachmentMetaDataName :: Text -- ^ Name of attachment if given
  , attachmentMetaDataContentType :: Text -- ^ Content type of attachment such as `image/png`
  , attachmentMetaDataContentLength :: Integer -- ^ Size of attachment in bytes
  , attachmentMetaDataId :: Text -- ^ ID of attachment. Can be used to with attachment controller endpoints to download attachment or with sending methods to attach to an email.
  , attachmentMetaDataContentId :: Maybe Text -- ^ CID of attachment
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentMetaData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentMetaData")
instance ToJSON AttachmentMetaData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentMetaData")


-- | Email attachment data
data AttachmentProjection = AttachmentProjection
  { attachmentProjectionUserId :: UUID -- ^ 
  , attachmentProjectionInboxId :: Maybe UUID -- ^ Inbox ID
  , attachmentProjectionUpdatedAt :: UTCTime -- ^ 
  , attachmentProjectionCreatedAt :: UTCTime -- ^ 
  , attachmentProjectionContentId :: Maybe Text -- ^ Content ID of attachment.
  , attachmentProjectionAttachmentId :: Text -- ^ Attachment ID
  , attachmentProjectionName :: Maybe Text -- ^ 
  , attachmentProjectionId :: UUID -- ^ ID
  , attachmentProjectionContentLength :: Maybe Integer -- ^ Content length of attachment in bytes
  , attachmentProjectionContentType :: Maybe Text -- ^ Content type of attachment.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentProjection")
instance ToJSON AttachmentProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentProjection")


-- | Basic Authentication options for webhooks. Will be used is present when calling webhook endpoints.
data BasicAuthOptions = BasicAuthOptions
  { basicAuthOptionsUsername :: Text -- ^ 
  , basicAuthOptionsPassword :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BasicAuthOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "basicAuthOptions")
instance ToJSON BasicAuthOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "basicAuthOptions")


-- | Bounced email event
data BounceProjection = BounceProjection
  { bounceProjectionSender :: Text -- ^ 
  , bounceProjectionCreatedAt :: UTCTime -- ^ 
  , bounceProjectionBounceType :: Maybe Text -- ^ 
  , bounceProjectionBounceMta :: Maybe Text -- ^ 
  , bounceProjectionSubject :: Maybe Text -- ^ 
  , bounceProjectionId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BounceProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bounceProjection")
instance ToJSON BounceProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bounceProjection")


-- | Bounced recipient
data BounceRecipientProjection = BounceRecipientProjection
  { bounceRecipientProjectionSentEmailId :: Maybe UUID -- ^ 
  , bounceRecipientProjectionCreatedAt :: UTCTime -- ^ 
  , bounceRecipientProjectionRecipient :: Text -- ^ 
  , bounceRecipientProjectionBounceType :: Maybe Text -- ^ 
  , bounceRecipientProjectionAction :: Maybe Text -- ^ 
  , bounceRecipientProjectionId :: Maybe UUID -- ^ 
  , bounceRecipientProjectionStatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BounceRecipientProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bounceRecipientProjection")
instance ToJSON BounceRecipientProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bounceRecipientProjection")


-- | Bounced email
data BouncedEmailDto = BouncedEmailDto
  { bouncedEmailDtoId :: UUID -- ^ 
  , bouncedEmailDtoUserId :: UUID -- ^ 
  , bouncedEmailDtoNotificationType :: Text -- ^ 
  , bouncedEmailDtoSentToRecipients :: Maybe [Text] -- ^ 
  , bouncedEmailDtoSender :: Text -- ^ 
  , bouncedEmailDtoBounceMta :: Maybe Text -- ^ 
  , bouncedEmailDtoBounceType :: Maybe Text -- ^ 
  , bouncedEmailDtoBounceRecipients :: Maybe [Text] -- ^ 
  , bouncedEmailDtoBounceSubType :: Maybe Text -- ^ 
  , bouncedEmailDtoSentEmailId :: Maybe UUID -- ^ 
  , bouncedEmailDtoSubject :: Maybe Text -- ^ 
  , bouncedEmailDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BouncedEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bouncedEmailDto")
instance ToJSON BouncedEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bouncedEmailDto")


-- | Bounced recipient
data BouncedRecipientDto = BouncedRecipientDto
  { bouncedRecipientDtoId :: UUID -- ^ 
  , bouncedRecipientDtoUserId :: Maybe UUID -- ^ 
  , bouncedRecipientDtoSentEmailId :: Maybe UUID -- ^ 
  , bouncedRecipientDtoRecipient :: Text -- ^ 
  , bouncedRecipientDtoDiagnosticCode :: Maybe Text -- ^ 
  , bouncedRecipientDtoAction :: Maybe Text -- ^ 
  , bouncedRecipientDtoBounceType :: Maybe Text -- ^ 
  , bouncedRecipientDtoStatus :: Maybe Text -- ^ 
  , bouncedRecipientDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BouncedRecipientDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bouncedRecipientDto")
instance ToJSON BouncedRecipientDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bouncedRecipientDto")


-- | Options for bulk sending an email from multiple addresses. See regular &#x60;sendEmail&#x60; methods for more information.
data BulkSendEmailOptions = BulkSendEmailOptions
  { bulkSendEmailOptionsInboxIds :: [UUID] -- ^ Inboxes to send the email from
  , bulkSendEmailOptionsSendEmailOptions :: SendEmailOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BulkSendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bulkSendEmailOptions")
instance ToJSON BulkSendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bulkSendEmailOptions")


-- | 
data CanSendEmailResults = CanSendEmailResults
  { canSendEmailResultsIsSendingPermitted :: Bool -- ^ 
  , canSendEmailResultsMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CanSendEmailResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "canSendEmailResults")
instance ToJSON CanSendEmailResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "canSendEmailResults")


-- | 
data CheckEmailBodyFeatureSupportResults = CheckEmailBodyFeatureSupportResults
  { checkEmailBodyFeatureSupportResultsResult :: EmailFeatureSupportResult -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailBodyFeatureSupportResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailBodyFeatureSupportResults")
instance ToJSON CheckEmailBodyFeatureSupportResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailBodyFeatureSupportResults")


-- | 
data CheckEmailBodyResults = CheckEmailBodyResults
  { checkEmailBodyResultsHasIssues :: Bool -- ^ 
  , checkEmailBodyResultsLinkIssues :: [LinkIssue] -- ^ 
  , checkEmailBodyResultsImageIssues :: [ImageIssue] -- ^ 
  , checkEmailBodyResultsSpellingIssues :: [SpellingIssue] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailBodyResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailBodyResults")
instance ToJSON CheckEmailBodyResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailBodyResults")


-- | Options for the email to be validated
data CheckEmailClientSupportOptions = CheckEmailClientSupportOptions
  { checkEmailClientSupportOptionsEmailBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailClientSupportOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailClientSupportOptions")
instance ToJSON CheckEmailClientSupportOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailClientSupportOptions")


-- | 
data CheckEmailClientSupportResults = CheckEmailClientSupportResults
  { checkEmailClientSupportResultsResult :: EmailFeatureSupportResult -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailClientSupportResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailClientSupportResults")
instance ToJSON CheckEmailClientSupportResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailClientSupportResults")


-- | 
data CheckEmailFeaturesClientSupportOptions = CheckEmailFeaturesClientSupportOptions
  { checkEmailFeaturesClientSupportOptionsEmailBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailFeaturesClientSupportOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailFeaturesClientSupportOptions")
instance ToJSON CheckEmailFeaturesClientSupportOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailFeaturesClientSupportOptions")


-- | 
data CheckEmailFeaturesClientSupportResults = CheckEmailFeaturesClientSupportResults
  { checkEmailFeaturesClientSupportResultsResult :: EmailFeatureSupportResult -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CheckEmailFeaturesClientSupportResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "checkEmailFeaturesClientSupportResults")
instance ToJSON CheckEmailFeaturesClientSupportResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "checkEmailFeaturesClientSupportResults")


-- | 
data Complaint = Complaint
  { complaintId :: UUID -- ^ 
  , complaintUserId :: Maybe UUID -- ^ 
  , complaintEventType :: Maybe Text -- ^ 
  , complaintMailSource :: Maybe Text -- ^ 
  , complaintMailMessageId :: Maybe Text -- ^ 
  , complaintComplaintRecipient :: Text -- ^ 
  , complaintCreatedAt :: UTCTime -- ^ 
  , complaintUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Complaint where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "complaint")
instance ToJSON Complaint where
  toJSON = genericToJSON (removeFieldLabelPrefix False "complaint")


-- | Options for matching emails in an inbox based on a condition such as &#x60;HAS_ATTACHMENTS&#x3D;TRUE&#x60;
data ConditionOption = ConditionOption
  { conditionOptionCondition :: Text -- ^ Condition of an email object that can be used to filter results
  , conditionOptionValue :: Text -- ^ Expected condition value
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConditionOption where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "conditionOption")
instance ToJSON ConditionOption where
  toJSON = genericToJSON (removeFieldLabelPrefix False "conditionOption")


-- | 
data ConnectorDto = ConnectorDto
  { connectorDtoId :: UUID -- ^ 
  , connectorDtoName :: Maybe Text -- ^ 
  , connectorDtoEnabled :: Bool -- ^ 
  , connectorDtoEmailAddress :: Maybe Text -- ^ 
  , connectorDtoUserId :: UUID -- ^ 
  , connectorDtoInboxId :: UUID -- ^ 
  , connectorDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorDto")
instance ToJSON ConnectorDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorDto")


-- | 
data ConnectorEventDto = ConnectorEventDto
  { connectorEventDtoId :: UUID -- ^ 
  , connectorEventDtoConnectorId :: UUID -- ^ 
  , connectorEventDtoStatus :: Text -- ^ 
  , connectorEventDtoEventType :: Text -- ^ 
  , connectorEventDtoSize :: Integer -- ^ 
  , connectorEventDtoMessage :: Maybe Text -- ^ 
  , connectorEventDtoLogs :: Maybe Text -- ^ 
  , connectorEventDtoSeen :: Maybe Bool -- ^ 
  , connectorEventDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorEventDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorEventDto")
instance ToJSON ConnectorEventDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorEventDto")


-- | ConnectorEventProjection
data ConnectorEventProjection = ConnectorEventProjection
  { connectorEventProjectionEventType :: Text -- ^ 
  , connectorEventProjectionCreatedAt :: UTCTime -- ^ 
  , connectorEventProjectionConnectorId :: UUID -- ^ 
  , connectorEventProjectionSeen :: Maybe Bool -- ^ 
  , connectorEventProjectionMessage :: Maybe Text -- ^ 
  , connectorEventProjectionId :: Maybe UUID -- ^ 
  , connectorEventProjectionSize :: Integer -- ^ 
  , connectorEventProjectionStatus :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorEventProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorEventProjection")
instance ToJSON ConnectorEventProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorEventProjection")


-- | 
data ConnectorImapConnectionDto = ConnectorImapConnectionDto
  { connectorImapConnectionDtoConnectorId :: UUID -- ^ 
  , connectorImapConnectionDtoImapHost :: Text -- ^ 
  , connectorImapConnectionDtoImapPort :: Maybe Int -- ^ 
  , connectorImapConnectionDtoImapUsername :: Maybe Text -- ^ 
  , connectorImapConnectionDtoImapPassword :: Maybe Text -- ^ 
  , connectorImapConnectionDtoImapSsl :: Maybe Bool -- ^ 
  , connectorImapConnectionDtoSelectFolder :: Maybe Text -- ^ 
  , connectorImapConnectionDtoSearchTerms :: Maybe Text -- ^ 
  , connectorImapConnectionDtoStartTls :: Maybe Bool -- ^ 
  , connectorImapConnectionDtoProxyHost :: Maybe Text -- ^ 
  , connectorImapConnectionDtoProxyPort :: Maybe Int -- ^ 
  , connectorImapConnectionDtoProxyEnabled :: Maybe Bool -- ^ 
  , connectorImapConnectionDtoLocalHostName :: Maybe Text -- ^ 
  , connectorImapConnectionDtoMechanisms :: Maybe [Text] -- ^ 
  , connectorImapConnectionDtoSslProtocols :: Maybe [Text] -- ^ 
  , connectorImapConnectionDtoSslTrust :: Maybe Text -- ^ 
  , connectorImapConnectionDtoEnabled :: Maybe Bool -- ^ 
  , connectorImapConnectionDtoCreatedAt :: UTCTime -- ^ 
  , connectorImapConnectionDtoId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorImapConnectionDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorImapConnectionDto")
instance ToJSON ConnectorImapConnectionDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorImapConnectionDto")


-- | 
data ConnectorImapConnectionTestResult = ConnectorImapConnectionTestResult
  { connectorImapConnectionTestResultError :: Maybe Text -- ^ 
  , connectorImapConnectionTestResultSuccess :: Bool -- ^ 
  , connectorImapConnectionTestResultMessage :: Maybe Text -- ^ 
  , connectorImapConnectionTestResultLogs :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorImapConnectionTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorImapConnectionTestResult")
instance ToJSON ConnectorImapConnectionTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorImapConnectionTestResult")


-- | Connector
data ConnectorProjection = ConnectorProjection
  { connectorProjectionEnabled :: Maybe Bool -- ^ 
  , connectorProjectionUserId :: UUID -- ^ 
  , connectorProjectionEmailAddress :: Maybe Text -- ^ 
  , connectorProjectionInboxId :: UUID -- ^ 
  , connectorProjectionCreatedAt :: UTCTime -- ^ 
  , connectorProjectionName :: Maybe Text -- ^ 
  , connectorProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorProjection")
instance ToJSON ConnectorProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorProjection")


-- | 
data ConnectorProviderSettingsDto = ConnectorProviderSettingsDto
  { connectorProviderSettingsDtoGoogleSettings :: ProviderSettings -- ^ 
  , connectorProviderSettingsDtoMicrosoftSettings :: ProviderSettings -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorProviderSettingsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorProviderSettingsDto")
instance ToJSON ConnectorProviderSettingsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorProviderSettingsDto")


-- | 
data ConnectorSmtpConnectionDto = ConnectorSmtpConnectionDto
  { connectorSmtpConnectionDtoConnectorId :: UUID -- ^ 
  , connectorSmtpConnectionDtoSmtpHost :: Text -- ^ 
  , connectorSmtpConnectionDtoSmtpPort :: Maybe Int -- ^ 
  , connectorSmtpConnectionDtoSmtpUsername :: Maybe Text -- ^ 
  , connectorSmtpConnectionDtoSmtpPassword :: Maybe Text -- ^ 
  , connectorSmtpConnectionDtoSmtpSsl :: Maybe Bool -- ^ 
  , connectorSmtpConnectionDtoStartTls :: Maybe Bool -- ^ 
  , connectorSmtpConnectionDtoSmtpMechanisms :: Maybe [Text] -- ^ 
  , connectorSmtpConnectionDtoLocalHostName :: Maybe Text -- ^ 
  , connectorSmtpConnectionDtoProxyHost :: Maybe Text -- ^ 
  , connectorSmtpConnectionDtoProxyPort :: Maybe Int -- ^ 
  , connectorSmtpConnectionDtoProxyEnabled :: Maybe Bool -- ^ 
  , connectorSmtpConnectionDtoEnabled :: Maybe Bool -- ^ 
  , connectorSmtpConnectionDtoSslTrust :: Maybe Text -- ^ 
  , connectorSmtpConnectionDtoSslProtocols :: Maybe [Text] -- ^ 
  , connectorSmtpConnectionDtoCreatedAt :: UTCTime -- ^ 
  , connectorSmtpConnectionDtoId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorSmtpConnectionDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorSmtpConnectionDto")
instance ToJSON ConnectorSmtpConnectionDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorSmtpConnectionDto")


-- | 
data ConnectorSmtpConnectionTestResult = ConnectorSmtpConnectionTestResult
  { connectorSmtpConnectionTestResultError :: Maybe Text -- ^ 
  , connectorSmtpConnectionTestResultSuccess :: Bool -- ^ 
  , connectorSmtpConnectionTestResultMessage :: Maybe Text -- ^ 
  , connectorSmtpConnectionTestResultLogs :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorSmtpConnectionTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorSmtpConnectionTestResult")
instance ToJSON ConnectorSmtpConnectionTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorSmtpConnectionTestResult")


-- | 
data ConnectorSyncRequestResult = ConnectorSyncRequestResult
  { connectorSyncRequestResultSyncResult :: Maybe ConnectorSyncResult -- ^ 
  , connectorSyncRequestResultException :: Maybe Text -- ^ 
  , connectorSyncRequestResultEventId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorSyncRequestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorSyncRequestResult")
instance ToJSON ConnectorSyncRequestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorSyncRequestResult")


-- | 
data ConnectorSyncResult = ConnectorSyncResult
  { connectorSyncResultEmailSyncCount :: Int -- ^ 
  , connectorSyncResultLogs :: Maybe [Text] -- ^ 
  , connectorSyncResultEmailIds :: Maybe [UUID] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorSyncResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorSyncResult")
instance ToJSON ConnectorSyncResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorSyncResult")


-- | 
data ConnectorSyncSettingsDto = ConnectorSyncSettingsDto
  { connectorSyncSettingsDtoId :: UUID -- ^ 
  , connectorSyncSettingsDtoUserId :: UUID -- ^ 
  , connectorSyncSettingsDtoConnectorId :: UUID -- ^ 
  , connectorSyncSettingsDtoSyncEnabled :: Bool -- ^ 
  , connectorSyncSettingsDtoSyncScheduleType :: Maybe Text -- ^ 
  , connectorSyncSettingsDtoSyncInterval :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConnectorSyncSettingsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectorSyncSettingsDto")
instance ToJSON ConnectorSyncSettingsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectorSyncSettingsDto")


-- | 
data ConsentStatusDto = ConsentStatusDto
  { consentStatusDtoConsented :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConsentStatusDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "consentStatusDto")
instance ToJSON ConsentStatusDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "consentStatusDto")


-- | Contact object. For saving a user in contact book.
data ContactDto = ContactDto
  { contactDtoId :: UUID -- ^ 
  , contactDtoGroupId :: Maybe UUID -- ^ 
  , contactDtoFirstName :: Maybe Text -- ^ 
  , contactDtoLastName :: Maybe Text -- ^ 
  , contactDtoCompany :: Maybe Text -- ^ 
  , contactDtoEmailAddresses :: [Text] -- ^ 
  , contactDtoPrimaryEmailAddress :: Maybe Text -- ^ 
  , contactDtoTags :: [Text] -- ^ 
  , contactDtoMetaData :: Maybe Value -- ^ 
  , contactDtoOptOut :: Maybe Bool -- ^ 
  , contactDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactDto")
instance ToJSON ContactDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactDto")


-- | Email contact for address book
data ContactProjection = ContactProjection
  { contactProjectionEmailAddress :: Maybe Text -- ^ 
  , contactProjectionCreatedAt :: UTCTime -- ^ 
  , contactProjectionEmailAddresses :: Maybe [Text] -- ^ 
  , contactProjectionFirstName :: Maybe Text -- ^ 
  , contactProjectionLastName :: Maybe Text -- ^ 
  , contactProjectionCompany :: Maybe Text -- ^ 
  , contactProjectionOptOut :: Maybe Bool -- ^ 
  , contactProjectionId :: UUID -- ^ 
  , contactProjectionGroupId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactProjection")
instance ToJSON ContactProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactProjection")


-- | Options for matching content using regex patterns based on Java Pattern syntax
data ContentMatchOptions = ContentMatchOptions
  { contentMatchOptionsPattern :: Text -- ^ Java style regex pattern. Do not include the typical `/` at start or end of regex in some languages. Given an example `your code is: 12345` the pattern to extract match looks like `code is: (\\d{6})`. This will return an array of matches with the first matching the entire pattern and the subsequent matching the groups: `['code is: 123456', '123456']` See https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html for more information of available patterns.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContentMatchOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contentMatchOptions")
instance ToJSON ContentMatchOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contentMatchOptions")


-- | Number of elements
data CountDto = CountDto
  { countDtoTotalElements :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CountDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "countDto")
instance ToJSON CountDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "countDto")


-- | Create email alias options. Email aliases can be used to mask real email addresses behind an ID. You can also attach an inbox to an alias so that any email received by the inbox email address if forwarded to the alias email address.
data CreateAliasOptions = CreateAliasOptions
  { createAliasOptionsEmailAddress :: Text -- ^ Email address to be hidden behind alias. Emails sent to the alias email address will be forwarded to this address. If you want to enable replies set useThreads true and the reply-to for the email will allow outbound communication via a thread. Some email addresses may require verification if they are not added as a contact first.
  , createAliasOptionsInboxId :: Maybe UUID -- ^ Optional inbox ID to attach to alias. Null by default means an a new inbox will be created for the alias. Use a custom inbox to control what email address the alias uses. To use custom email addresses create a domain and an inbox, the use the inbox ID with this call. Emails received by this inbox will be forwarded to the alias email address
  , createAliasOptionsName :: Maybe Text -- ^ Optional name for alias
  , createAliasOptionsUseThreads :: Bool -- ^ Enable threads options. If true emails will be sent with a unique reply-to thread address. This means you can reply to the forwarded email and it will be sent to the recipients via your alias address. That way a thread conversation is preserved.
  , createAliasOptionsDomainId :: Maybe UUID -- ^ Custom domain ID to use when generating alias email addresses
  , createAliasOptionsVerifyEmailAddress :: Maybe Bool -- ^ Whether to verify the masked email address exists before sending an email to it
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateAliasOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createAliasOptions")
instance ToJSON CreateAliasOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createAliasOptions")


-- | Options for IMAP connection to external email inbox. Allows syncing emails via IMAP.
data CreateConnectorImapConnectionOptions = CreateConnectorImapConnectionOptions
  { createConnectorImapConnectionOptionsImapSsl :: Maybe Bool -- ^ 
  , createConnectorImapConnectionOptionsImapUsername :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsImapPassword :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsSelectFolder :: Maybe Text -- ^ Optional folder to select during IMAP connection
  , createConnectorImapConnectionOptionsSearchTerms :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsImapPort :: Maybe Int -- ^ IMAP server port
  , createConnectorImapConnectionOptionsImapHost :: Text -- ^ IMAP server host
  , createConnectorImapConnectionOptionsEnabled :: Maybe Bool -- ^ IMAP server enabled
  , createConnectorImapConnectionOptionsStartTls :: Maybe Bool -- ^ 
  , createConnectorImapConnectionOptionsProxyEnabled :: Maybe Bool -- ^ 
  , createConnectorImapConnectionOptionsProxyPort :: Maybe Int -- ^ 
  , createConnectorImapConnectionOptionsProxyHost :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsLocalHostName :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsMechanisms :: Maybe [Text] -- ^ List of IMAP mechanisms
  , createConnectorImapConnectionOptionsSslTrust :: Maybe Text -- ^ 
  , createConnectorImapConnectionOptionsSslProtocols :: Maybe [Text] -- ^ List of SSL protocols
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateConnectorImapConnectionOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createConnectorImapConnectionOptions")
instance ToJSON CreateConnectorImapConnectionOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createConnectorImapConnectionOptions")


-- | Options for creating an inbox connection with an external mail provider
data CreateConnectorOptions = CreateConnectorOptions
  { createConnectorOptionsName :: Maybe Text -- ^ Name of connector
  , createConnectorOptionsEmailAddress :: Maybe Text -- ^ Email address of external inbox
  , createConnectorOptionsEnabled :: Maybe Bool -- ^ Is connector enabled
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateConnectorOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createConnectorOptions")
instance ToJSON CreateConnectorOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createConnectorOptions")


-- | 
data CreateConnectorSmtpConnectionOptions = CreateConnectorSmtpConnectionOptions
  { createConnectorSmtpConnectionOptionsEnabled :: Maybe Bool -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpHost :: Text -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpPort :: Maybe Int -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpSsl :: Maybe Bool -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpUsername :: Maybe Text -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpPassword :: Maybe Text -- ^ 
  , createConnectorSmtpConnectionOptionsSmtpMechanisms :: Maybe [Text] -- ^ 
  , createConnectorSmtpConnectionOptionsStartTls :: Maybe Bool -- ^ 
  , createConnectorSmtpConnectionOptionsLocalHostName :: Maybe Text -- ^ 
  , createConnectorSmtpConnectionOptionsProxyHost :: Maybe Text -- ^ 
  , createConnectorSmtpConnectionOptionsProxyPort :: Maybe Int -- ^ 
  , createConnectorSmtpConnectionOptionsProxyEnabled :: Maybe Bool -- ^ 
  , createConnectorSmtpConnectionOptionsSslTrust :: Maybe Text -- ^ 
  , createConnectorSmtpConnectionOptionsSslProtocols :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateConnectorSmtpConnectionOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createConnectorSmtpConnectionOptions")
instance ToJSON CreateConnectorSmtpConnectionOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createConnectorSmtpConnectionOptions")


-- | Options for creating automatic syncing between an inbox connection and an external mail provider
data CreateConnectorSyncSettingsOptions = CreateConnectorSyncSettingsOptions
  { createConnectorSyncSettingsOptionsSyncEnabled :: Maybe Bool -- ^ Enable automatic background sync
  , createConnectorSyncSettingsOptionsSyncScheduleType :: Maybe Text -- ^ Sync schedule type
  , createConnectorSyncSettingsOptionsSyncInterval :: Maybe Int -- ^ Sync interval in minutes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateConnectorSyncSettingsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createConnectorSyncSettingsOptions")
instance ToJSON CreateConnectorSyncSettingsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createConnectorSyncSettingsOptions")


-- | Options for creating an inbox connection with an external mail provider including extra settings
data CreateConnectorWithOptions = CreateConnectorWithOptions
  { createConnectorWithOptionsCreateConnectorOptions :: CreateConnectorOptions -- ^ 
  , createConnectorWithOptionsCreateConnectorImapConnectionOptions :: Maybe CreateConnectorImapConnectionOptions -- ^ 
  , createConnectorWithOptionsCreateConnectorSmtpConnectionOptions :: Maybe CreateConnectorSmtpConnectionOptions -- ^ 
  , createConnectorWithOptionsCreateConnectorSyncSettingsOptions :: Maybe CreateConnectorSyncSettingsOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateConnectorWithOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createConnectorWithOptions")
instance ToJSON CreateConnectorWithOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createConnectorWithOptions")


-- | Options for creating an email contact in address book
data CreateContactOptions = CreateContactOptions
  { createContactOptionsFirstName :: Maybe Text -- ^ 
  , createContactOptionsLastName :: Maybe Text -- ^ 
  , createContactOptionsCompany :: Maybe Text -- ^ 
  , createContactOptionsEmailAddresses :: Maybe [Text] -- ^ Set of email addresses belonging to the contact
  , createContactOptionsTags :: Maybe [Text] -- ^ Tags that can be used to search and group contacts
  , createContactOptionsMetaData :: Maybe Value -- ^ 
  , createContactOptionsOptOut :: Maybe Bool -- ^ Has the user explicitly or implicitly opted out of being contacted? If so MailSlurp will ignore them in all actions.
  , createContactOptionsGroupId :: Maybe UUID -- ^ Group IDs that contact belongs to
  , createContactOptionsVerifyEmailAddresses :: Maybe Bool -- ^ Whether to validate contact email address exists
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createContactOptions")
instance ToJSON CreateContactOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createContactOptions")


-- | Options for creating a domain to use with MailSlurp. You must have ownership access to this domain in order to verify it. Domains will not function correctly until the domain has been verified. See https://www.mailslurp.com/guides/custom-domains for help. Domains can be either &#x60;HTTP&#x60; or &#x60;SMTP&#x60; type. The type of domain determines which inboxes can be used with it. &#x60;SMTP&#x60; inboxes use a mail server running &#x60;mxslurp.click&#x60; while &#x60;HTTP&#x60; inboxes are handled by AWS SES.
data CreateDomainOptions = CreateDomainOptions
  { createDomainOptionsDomain :: Text -- ^ The top level domain you wish to use with MailSlurp. Do not specify subdomain just the top level. So `test.com` covers all subdomains such as `mail.test.com`. Don't include a protocol such as `http://`. Once added you must complete the verification steps by adding the returned records to your domain.
  , createDomainOptionsDescription :: Maybe Text -- ^ Optional description of the domain.
  , createDomainOptionsCreatedCatchAllInbox :: Maybe Bool -- ^ Whether to create a catch all inbox for the domain. Any email sent to an address using your domain that cannot be matched to an existing inbox you created with the domain will be routed to the created catch all inbox. You can access emails using the regular methods on this inbox ID.
  , createDomainOptionsDomainType :: Maybe Text -- ^ Type of domain. Dictates type of inbox that can be created with domain. HTTP means inboxes are processed using SES while SMTP inboxes use a custom SMTP mail server. SMTP does not support sending so use HTTP for sending emails.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDomainOptions")
instance ToJSON CreateDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDomainOptions")


-- | 
data CreateEmergencyAddressOptions = CreateEmergencyAddressOptions
  { createEmergencyAddressOptionsCustomerName :: Text -- ^ 
  , createEmergencyAddressOptionsAddress1 :: Text -- ^ 
  , createEmergencyAddressOptionsCity :: Text -- ^ 
  , createEmergencyAddressOptionsRegion :: Text -- ^ 
  , createEmergencyAddressOptionsPostalCode :: Text -- ^ 
  , createEmergencyAddressOptionsIsoCountryCode :: Text -- ^ 
  , createEmergencyAddressOptionsDisplayName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateEmergencyAddressOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createEmergencyAddressOptions")
instance ToJSON CreateEmergencyAddressOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createEmergencyAddressOptions")


-- | Create contact group options
data CreateGroupOptions = CreateGroupOptions
  { createGroupOptionsName :: Text -- ^ 
  , createGroupOptionsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupOptions")
instance ToJSON CreateGroupOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupOptions")


-- | Options for creating an inbox. An inbox has a real email address that can send and receive emails. Inboxes can be permanent or expire at a given time. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. &#x60;SMTP&#x60; inboxes are processed by a mail server running at &#x60;mailslurp.mx&#x60; while &#x60;HTTP&#x60; inboxes are processed by AWS SES backed mailservers. An inbox email address is randomly assigned by default ending in either &#x60;mailslurp.com&#x60; or (if &#x60;useDomainPool&#x60; is enabled) ending in a similar domain such as &#x60;mailslurp.xyz&#x60; (selected at random). To specify an address use a custom domain: either pass the &#x60;emailAddress&#x60; options with &#x60;&lt;your-recipient&gt;@&lt;your-domain&gt;&#x60;. To create a randomized address for your domain set the &#x60;domainName&#x60; to the domain you have verified or pass the &#x60;domainId&#x60;. Virtual inboxes prevent outbound sending and instead trap mail.
data CreateInboxDto = CreateInboxDto
  { createInboxDtoEmailAddress :: Maybe Text -- ^ A custom email address to use with the inbox. Defaults to null. When null MailSlurp will assign a random email address to the inbox such as `123@mailslurp.com`. If you use the `useDomainPool` option when the email address is null it will generate an email address with a more varied domain ending such as `123@mailslurp.info` or `123@mailslurp.biz`. When a custom email address is provided the address is split into a domain and the domain is queried against your user. If you have created the domain in the MailSlurp dashboard and verified it you can use any email address that ends with the domain. Note domain types must match the inbox type - so `SMTP` inboxes will only work with `SMTP` type domains. Avoid `SMTP` inboxes if you need to send emails as they can only receive. Send an email to this address and the inbox will receive and store it for you. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , createInboxDtoDomainName :: Maybe Text -- ^ FQDN domain name for the domain you have verified. Will be appended with a randomly assigned recipient name. Use the `emailAddress` option instead to specify the full custom inbox.
  , createInboxDtoDomainId :: Maybe UUID -- ^ ID of custom domain to use for email address.
  , createInboxDtoName :: Maybe Text -- ^ Optional name of the inbox. Displayed in the dashboard for easier search and used as the sender name when sending emails.
  , createInboxDtoDescription :: Maybe Text -- ^ Optional description of the inbox for labelling purposes. Is shown in the dashboard and can be used with
  , createInboxDtoUseDomainPool :: Maybe Bool -- ^ Use the MailSlurp domain name pool with this inbox when creating the email address. Defaults to null. If enabled the inbox will be an email address with a domain randomly chosen from a list of the MailSlurp domains. This is useful when the default `@mailslurp.com` email addresses used with inboxes are blocked or considered spam by a provider or receiving service. When domain pool is enabled an email address will be generated ending in `@mailslurp.{world,info,xyz,...}` . This means a TLD is randomly selecting from a list of `.biz`, `.info`, `.xyz` etc to add variance to the generated email addresses. When null or false MailSlurp uses the default behavior of `@mailslurp.com` or custom email address provided by the emailAddress field. Note this feature is only available for `HTTP` inbox types.
  , createInboxDtoTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , createInboxDtoExpiresAt :: Maybe UTCTime -- ^ Optional inbox expiration date. If null then this inbox is permanent and the emails in it won't be deleted. If an expiration date is provided or is required by your plan the inbox will be closed when the expiration time is reached. Expired inboxes still contain their emails but can no longer send or receive emails. An ExpiredInboxRecord is created when an inbox and the email address and inbox ID are recorded. The expiresAt property is a timestamp string in ISO DateTime Format yyyy-MM-dd'T'HH:mm:ss.SSSXXX.
  , createInboxDtoFavourite :: Maybe Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  , createInboxDtoExpiresIn :: Maybe Integer -- ^ Number of milliseconds that inbox should exist for
  , createInboxDtoAllowTeamAccess :: Maybe Bool -- ^ DEPRECATED (team access is always true). Grant team access to this inbox and the emails that belong to it for team members of your organization.
  , createInboxDtoInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , createInboxDtoVirtualInbox :: Maybe Bool -- ^ Virtual inbox prevents any outbound emails from being sent. It creates sent email records but will never send real emails to recipients. Great for testing and faking email sending.
  , createInboxDtoUseShortAddress :: Maybe Bool -- ^ Use a shorter email address under 31 characters
  , createInboxDtoPrefix :: Maybe Text -- ^ Prefix to add before the email address for easier labelling or identification.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxDto")
instance ToJSON CreateInboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxDto")


-- | Options for creating an inbox forwarder
data CreateInboxForwarderOptions = CreateInboxForwarderOptions
  { createInboxForwarderOptionsField :: Text -- ^ Field to match against to trigger inbox forwarding for inbound email
  , createInboxForwarderOptionsMatch :: Text -- ^ String or wildcard style match for field specified when evaluating forwarding rules
  , createInboxForwarderOptionsForwardToRecipients :: [Text] -- ^ Email addresses to forward an email to if it matches the field and match criteria of the forwarder
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxForwarderOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxForwarderOptions")
instance ToJSON CreateInboxForwarderOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxForwarderOptions")


-- | Options for creating an inbox replier. Repliers can be attached to inboxes and send automated responses when an inbound email matches given criteria.
data CreateInboxReplierOptions = CreateInboxReplierOptions
  { createInboxReplierOptionsInboxId :: Maybe UUID -- ^ Inbox ID to attach replier to
  , createInboxReplierOptionsName :: Maybe Text -- ^ Name for replier
  , createInboxReplierOptionsField :: Text -- ^ Field to match against to trigger inbox replier for inbound email
  , createInboxReplierOptionsMatch :: Text -- ^ String or wildcard style match for field specified when evaluating reply rules. Use `*` to match anything.
  , createInboxReplierOptionsReplyTo :: Maybe Text -- ^ Reply-to email address when sending replying
  , createInboxReplierOptionsSubject :: Maybe Text -- ^ Subject override when replying to email
  , createInboxReplierOptionsFrom :: Maybe Text -- ^ Send email from address
  , createInboxReplierOptionsCharset :: Maybe Text -- ^ Email reply charset
  , createInboxReplierOptionsIgnoreReplyTo :: Maybe Bool -- ^ Ignore sender replyTo when responding. Send directly to the sender if enabled.
  , createInboxReplierOptionsIsHTML :: Maybe Bool -- ^ Send HTML email
  , createInboxReplierOptionsBody :: Maybe Text -- ^ Email body for reply
  , createInboxReplierOptionsTemplateId :: Maybe UUID -- ^ ID of template to use when sending a reply
  , createInboxReplierOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variable values
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxReplierOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxReplierOptions")
instance ToJSON CreateInboxReplierOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxReplierOptions")


-- | Options for creating inbox rulesets. Inbox rulesets can be used to block, allow, filter, or forward emails when sending or receiving using the inbox.
data CreateInboxRulesetOptions = CreateInboxRulesetOptions
  { createInboxRulesetOptionsScope :: Text -- ^ What type of emails actions to apply ruleset to. Either `SENDING_EMAILS` or `RECEIVING_EMAILS` will apply action and target to any sending or receiving of emails respectively.
  , createInboxRulesetOptionsAction :: Text -- ^ Action to be taken when the ruleset matches an email for the given scope. For example: `BLOCK` action with target `*` and scope `SENDING_EMAILS` blocks sending to all recipients. Note `ALLOW` takes precedent over `BLOCK`. `FILTER_REMOVE` is like block but will remove offending email addresses during a send or receive event instead of blocking the action.
  , createInboxRulesetOptionsTarget :: Text -- ^ Target to match emails with. Can be a wild-card type pattern or a valid email address. For instance `*@gmail.com` matches all gmail addresses while `test@gmail.com` matches one address exactly. The target is applied to every recipient field email address when `SENDING_EMAILS` is the scope and is applied to sender of email when `RECEIVING_EMAILS`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxRulesetOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxRulesetOptions")
instance ToJSON CreateInboxRulesetOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxRulesetOptions")


-- | 
data CreateOAuthConnectionResult = CreateOAuthConnectionResult
  { createOAuthConnectionResultUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateOAuthConnectionResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createOAuthConnectionResult")
instance ToJSON CreateOAuthConnectionResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createOAuthConnectionResult")


-- | 
data CreateOAuthExchangeResult = CreateOAuthExchangeResult
  { createOAuthExchangeResultSuccess :: Bool -- ^ 
  , createOAuthExchangeResultInboxId :: Maybe UUID -- ^ 
  , createOAuthExchangeResultError :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateOAuthExchangeResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createOAuthExchangeResult")
instance ToJSON CreateOAuthExchangeResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createOAuthExchangeResult")


-- | 
data CreatePhoneNumberOptions = CreatePhoneNumberOptions
  { createPhoneNumberOptionsPhoneCountry :: Text -- ^ 
  , createPhoneNumberOptionsName :: Maybe Text -- ^ 
  , createPhoneNumberOptionsDescription :: Maybe Text -- ^ 
  , createPhoneNumberOptionsSchedule :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreatePhoneNumberOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createPhoneNumberOptions")
instance ToJSON CreatePhoneNumberOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createPhoneNumberOptions")


-- | 
data CreatePortalOptions = CreatePortalOptions
  { createPortalOptionsName :: Maybe Text -- ^ 
  , createPortalOptionsDomainId :: Maybe UUID -- ^ 
  , createPortalOptionsDescription :: Maybe Text -- ^ 
  , createPortalOptionsLinkHelp :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreatePortalOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createPortalOptions")
instance ToJSON CreatePortalOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createPortalOptions")


-- | 
data CreatePortalUserOptions = CreatePortalUserOptions
  { createPortalUserOptionsPassword :: Maybe Text -- ^ 
  , createPortalUserOptionsName :: Maybe Text -- ^ 
  , createPortalUserOptionsUsername :: Maybe Text -- ^ 
  , createPortalUserOptionsSkipInboxCreation :: Maybe Bool -- ^ 
  , createPortalUserOptionsInboxId :: Maybe UUID -- ^ 
  , createPortalUserOptionsCreateInboxOptions :: Maybe CreateInboxDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreatePortalUserOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createPortalUserOptions")
instance ToJSON CreatePortalUserOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createPortalUserOptions")


-- | Create template options
data CreateTemplateOptions = CreateTemplateOptions
  { createTemplateOptionsName :: Text -- ^ Name of template
  , createTemplateOptionsContent :: Text -- ^ Template content. Can include moustache style variables such as {{var_name}}
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTemplateOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTemplateOptions")
instance ToJSON CreateTemplateOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTemplateOptions")


-- | Options for creating a tracking pixel for email open tracking
data CreateTrackingPixelOptions = CreateTrackingPixelOptions
  { createTrackingPixelOptionsName :: Maybe Text -- ^ 
  , createTrackingPixelOptionsRecipient :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTrackingPixelOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTrackingPixelOptions")
instance ToJSON CreateTrackingPixelOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTrackingPixelOptions")


-- | Options for creating a webhook. Webhooks can be attached to inboxes and MailSlurp will POST a webhook payload to the URL specified whenever the webhook&#39;s event is triggered. Webhooks are great for processing many inbound emails and responding to other events at scale. Customize the payload sent to your endpoint by setting the &#x60;requestBodyTemplate&#x60; property to a string with moustache style variables. Property names from the standard payload model for the given event are available as variables.
data CreateWebhookOptions = CreateWebhookOptions
  { createWebhookOptionsUrl :: Text -- ^ Public URL on your server that MailSlurp can post WebhookNotification payload to when an email is received or an event is trigger. The payload of the submitted JSON is dependent on the webhook event type. See docs.mailslurp.com/webhooks for event payload documentation.
  , createWebhookOptionsBasicAuth :: Maybe BasicAuthOptions -- ^ 
  , createWebhookOptionsName :: Maybe Text -- ^ Optional name for the webhook
  , createWebhookOptionsEventName :: Maybe Text -- ^ Optional webhook event name. Default is `EMAIL_RECEIVED` and is triggered when an email is received by the inbox associated with the webhook. Payload differ according to the webhook event name.
  , createWebhookOptionsIncludeHeaders :: Maybe WebhookHeaders -- ^ 
  , createWebhookOptionsRequestBodyTemplate :: Maybe Text -- ^ Template for the JSON body of the webhook request that will be sent to your server. Use Moustache style `{{variableName}}` templating to use parts of the standard webhook payload for the given event.
  , createWebhookOptionsUseStaticIpRange :: Maybe Bool -- ^ Use static IP range when calling webhook endpoint
  , createWebhookOptionsIgnoreInsecureSslCertificates :: Maybe Bool -- ^ Ignore insecure SSL certificates when sending request. Useful for self-signed certs.
  , createWebhookOptionsTags :: Maybe [Text] -- ^ Optional list of tags
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateWebhookOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createWebhookOptions")
instance ToJSON CreateWebhookOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createWebhookOptions")


-- | Options for DNS query.
data DNSLookupOptions = DNSLookupOptions
  { dNSLookupOptionsHostname :: Text -- ^ List of record types you wish to query such as MX, DNS, TXT, NS, A etc.
  , dNSLookupOptionsRecordTypes :: [Text] -- ^ List of record types you wish to query such as MX, DNS, TXT, NS, A etc.
  , dNSLookupOptionsOmitFinalDNSDot :: Bool -- ^ Optionally control whether to omit the final dot in full DNS name values.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DNSLookupOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dNSLookupOptions")
instance ToJSON DNSLookupOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dNSLookupOptions")


-- | DNS lookup result. Includes record type, time to live, raw response, and name value for the name server response.
data DNSLookupResult = DNSLookupResult
  { dNSLookupResultRecordType :: Text -- ^ Domain Name Server Record Types
  , dNSLookupResultTtl :: Integer -- ^ 
  , dNSLookupResultRecordEntries :: [Text] -- ^ 
  , dNSLookupResultName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DNSLookupResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dNSLookupResult")
instance ToJSON DNSLookupResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dNSLookupResult")


-- | Results of query on domain name servers
data DNSLookupResults = DNSLookupResults
  { dNSLookupResultsResults :: [DNSLookupResult] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DNSLookupResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dNSLookupResults")
instance ToJSON DNSLookupResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dNSLookupResults")


-- | Options for multiple DNS queries
data DNSLookupsOptions = DNSLookupsOptions
  { dNSLookupsOptionsLookups :: [DNSLookupOptions] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DNSLookupsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dNSLookupsOptions")
instance ToJSON DNSLookupsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dNSLookupsOptions")


-- | 
data DeliveryStatusDto = DeliveryStatusDto
  { deliveryStatusDtoId :: UUID -- ^ 
  , deliveryStatusDtoUserId :: UUID -- ^ 
  , deliveryStatusDtoSentId :: Maybe UUID -- ^ 
  , deliveryStatusDtoRemoteMtaIp :: Maybe Text -- ^ 
  , deliveryStatusDtoInboxId :: Maybe UUID -- ^ 
  , deliveryStatusDtoReportingMta :: Maybe Text -- ^ 
  , deliveryStatusDtoRecipients :: Maybe [Text] -- ^ 
  , deliveryStatusDtoSmtpResponse :: Maybe Text -- ^ 
  , deliveryStatusDtoSmtpStatusCode :: Maybe Int -- ^ 
  , deliveryStatusDtoProcessingTimeMillis :: Maybe Integer -- ^ 
  , deliveryStatusDtoReceived :: Maybe UTCTime -- ^ 
  , deliveryStatusDtoSubject :: Maybe Text -- ^ 
  , deliveryStatusDtoCreatedAt :: UTCTime -- ^ 
  , deliveryStatusDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryStatusDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deliveryStatusDto")
instance ToJSON DeliveryStatusDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deliveryStatusDto")


-- | Domain record description
data DescribeDomainOptions = DescribeDomainOptions
  { describeDomainOptionsDomain :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DescribeDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "describeDomainOptions")
instance ToJSON DescribeDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "describeDomainOptions")


-- | Name Server lookup result
data DescribeMailServerDomainResult = DescribeMailServerDomainResult
  { describeMailServerDomainResultMxRecords :: [NameServerRecord] -- ^ 
  , describeMailServerDomainResultDomain :: Text -- ^ 
  , describeMailServerDomainResultMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DescribeMailServerDomainResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "describeMailServerDomainResult")
instance ToJSON DescribeMailServerDomainResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "describeMailServerDomainResult")


-- | Domain plus verification records and status
data DomainDto = DomainDto
  { domainDtoId :: UUID -- ^ 
  , domainDtoUserId :: UUID -- ^ 
  , domainDtoDomain :: Text -- ^ Custom domain name
  , domainDtoVerificationToken :: Text -- ^ Verification tokens
  , domainDtoDkimTokens :: [Text] -- ^ Unique token DKIM tokens
  , domainDtoDuplicateRecordsMessage :: Maybe Text -- ^ If the domain is duplicate records.
  , domainDtoHasDuplicateRecords :: Bool -- ^ Whether the domain has duplicated required records. If true then see the domain in the dashboard app.
  , domainDtoMissingRecordsMessage :: Maybe Text -- ^ If the domain is missing records then show which pairs are missing.
  , domainDtoHasMissingRecords :: Bool -- ^ Whether the domain has missing required records. If true then see the domain in the dashboard app.
  , domainDtoIsVerified :: Bool -- ^ Whether domain has been verified or not. If the domain is not verified after 72 hours there is most likely an issue with the domains DNS records.
  , domainDtoDomainNameRecords :: [DomainNameRecord] -- ^ List of DNS domain name records (C, MX, TXT) etc that you must add to the DNS server associated with your domain provider.
  , domainDtoCatchAllInboxId :: Maybe UUID -- ^ The optional catch all inbox that will receive emails sent to the domain that cannot be matched.
  , domainDtoCreatedAt :: UTCTime -- ^ 
  , domainDtoUpdatedAt :: UTCTime -- ^ 
  , domainDtoDomainType :: Text -- ^ Type of domain. Dictates type of inbox that can be created with domain. HTTP means inboxes are processed using SES while SMTP inboxes use a custom SMTP mail server. SMTP does not support sending so use HTTP for sending emails.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainDto")
instance ToJSON DomainDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainDto")


-- | 
data DomainGroup = DomainGroup
  { domainGroupLabel :: Text -- ^ 
  , domainGroupDomains :: [DomainInformation] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainGroup where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainGroup")
instance ToJSON DomainGroup where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainGroup")


-- | 
data DomainGroupsDto = DomainGroupsDto
  { domainGroupsDtoDomainGroups :: [DomainGroup] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainGroupsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainGroupsDto")
instance ToJSON DomainGroupsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainGroupsDto")


-- | 
data DomainInformation = DomainInformation
  { domainInformationDomainName :: Text -- ^ 
  , domainInformationVerified :: Bool -- ^ 
  , domainInformationDomainType :: Text -- ^ Type of domain. Dictates type of inbox that can be created with domain. HTTP means inboxes are processed using SES while SMTP inboxes use a custom SMTP mail server. SMTP does not support sending so use HTTP for sending emails.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainInformation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainInformation")
instance ToJSON DomainInformation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainInformation")


-- | 
data DomainIssuesDto = DomainIssuesDto
  { domainIssuesDtoHasIssues :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainIssuesDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainIssuesDto")
instance ToJSON DomainIssuesDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainIssuesDto")


-- | DNS Record required for verification of a domain. Record vary depending on domain type.
data DomainNameRecord = DomainNameRecord
  { domainNameRecordLabel :: Text -- ^ Domain Name Server Record Label
  , domainNameRecordRequired :: Bool -- ^ 
  , domainNameRecordRecordType :: Text -- ^ Domain Name Server Record Types
  , domainNameRecordName :: Text -- ^ 
  , domainNameRecordRecordEntries :: [Text] -- ^ 
  , domainNameRecordTtl :: Integer -- ^ 
  , domainNameRecordAlternativeRecordEntries :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainNameRecord where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainNameRecord")
instance ToJSON DomainNameRecord where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainNameRecord")


-- | Preview object for domain entity
data DomainPreview = DomainPreview
  { domainPreviewId :: UUID -- ^ 
  , domainPreviewDomain :: Text -- ^ 
  , domainPreviewCatchAllInboxId :: Maybe UUID -- ^ 
  , domainPreviewCreatedAt :: UTCTime -- ^ 
  , domainPreviewDomainType :: Text -- ^ Type of domain. Dictates type of inbox that can be created with domain. HTTP means inboxes are processed using SES while SMTP inboxes use a custom SMTP mail server. SMTP does not support sending so use HTTP for sending emails.
  , domainPreviewIsVerified :: Bool -- ^ 
  , domainPreviewHasMissingRecords :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainPreview")
instance ToJSON DomainPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainPreview")


-- | Content of attachment
data DownloadAttachmentDto = DownloadAttachmentDto
  { downloadAttachmentDtoBase64FileContents :: Text -- ^ Base64 encoded string of attachment bytes. Decode the base64 encoded string to get the raw contents. If the file has a content type such as `text/html` you can read the contents directly by converting it to string using `utf-8` encoding.
  , downloadAttachmentDtoContentType :: Text -- ^ Content type of attachment. Examples are `image/png`, `application/msword`, `text/csv` etc.
  , downloadAttachmentDtoSizeBytes :: Integer -- ^ Size in bytes of attachment content
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DownloadAttachmentDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "downloadAttachmentDto")
instance ToJSON DownloadAttachmentDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "downloadAttachmentDto")


-- | Email entity (also known as EmailDto). When an SMTP email message is received by MailSlurp it is parsed. The body and attachments are written to disk and the fields such as to, from, subject etc are stored in a database. The &#x60;body&#x60; contains the email content. If you want the original SMTP message see the &#x60;getRawEmail&#x60; endpoints. The attachments can be fetched using the AttachmentController
data Email = Email
  { emailId :: UUID -- ^ ID of the email entity
  , emailUserId :: UUID -- ^ ID of user that email belongs to
  , emailInboxId :: UUID -- ^ ID of the inbox that received the email
  , emailDomainId :: Maybe UUID -- ^ ID of the domain that received the email
  , emailTo :: [Text] -- ^ List of `To` recipient email addresses that the email was addressed to. See recipients object for names.
  , emailFrom :: Maybe Text -- ^ Who the email was sent from. An email address - see fromName for the sender name.
  , emailSender :: Maybe Sender -- ^ 
  , emailRecipients :: Maybe EmailRecipients -- ^ 
  , emailReplyTo :: Maybe Text -- ^ The `replyTo` field on the received email message
  , emailCc :: Maybe [Text] -- ^ List of `CC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailBcc :: Maybe [Text] -- ^ List of `BCC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailHeaders :: Maybe (Map.Map String Text) -- ^ Collection of SMTP headers attached to email
  , emailHeadersMap :: Maybe (Map.Map String [Text]) -- ^ Multi-value map of SMTP headers attached to email
  , emailAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  , emailSubject :: Maybe Text -- ^ The subject line of the email message as specified by SMTP subject header
  , emailBody :: Maybe Text -- ^ The body of the email message as text parsed from the SMTP message body (does not include attachments). Fetch the raw content to access the SMTP message and use the attachments property to access attachments. The body is stored separately to the email entity so the body is not returned in paginated results only in full single email or wait requests.
  , emailBodyExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message for quick preview. Takes HTML content part if exists falls back to TEXT content part if not
  , emailTextExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message for quick preview. Takes TEXT content part if exists
  , emailBodyMD5Hash :: Maybe Text -- ^ A hash signature of the email message using MD5. Useful for comparing emails without fetching full body.
  , emailIsHTML :: Maybe Bool -- ^ Is the email body content type HTML?
  , emailCharset :: Maybe Text -- ^ Detected character set of the email body such as UTF-8
  , emailAnalysis :: Maybe EmailAnalysis -- ^ 
  , emailCreatedAt :: UTCTime -- ^ When was the email received by MailSlurp
  , emailUpdatedAt :: UTCTime -- ^ When was the email last updated
  , emailRead :: Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API with a hydrated body? If so the email is marked as read. Paginated results do not affect read status. Read status is different to email opened event as it depends on your own account accessing the email. Email opened is determined by tracking pixels sent to other uses if enable during sending. You can listened for both email read and email opened events using webhooks.
  , emailTeamAccess :: Bool -- ^ Can the email be accessed by organization team members
  , emailIsXAmpHtml :: Maybe Bool -- ^ Is the email body content type x-amp-html Amp4Email?
  , emailBodyPartContentTypes :: Maybe [Text] -- ^ A list of detected multipart mime message body part content types such as text/plain and text/html. Can be used with email bodyPart endpoints to fetch individual body parts.
  , emailExternalId :: Maybe Text -- ^ UID used by external IMAP server to identify email
  , emailMessageId :: Maybe Text -- ^ RFC 5322 Message-ID header value without angle brackets.
  , emailThreadId :: Maybe UUID -- ^ MailSlurp thread ID for email chain that enables lookup for In-Reply-To and References fields.
  , emailInReplyTo :: Maybe Text -- ^ Parsed value of In-Reply-To header. A Message-ID in a thread.
  , emailFavourite :: Maybe Bool -- ^ Is email favourited
  , emailSizeBytes :: Maybe Integer -- ^ Size of raw email message in bytes
  , emailHtml :: Maybe Bool -- ^ 
  , emailXampHtml :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "email")
instance ToJSON Email where
  toJSON = genericToJSON (removeFieldLabelPrefix False "email")


-- | Analysis result for email. Each verdict property is a string PASS|FAIL|GRAY or dynamic error message
data EmailAnalysis = EmailAnalysis
  { emailAnalysisSpamVerdict :: Maybe Text -- ^ Verdict of spam ranking analysis
  , emailAnalysisVirusVerdict :: Maybe Text -- ^ Verdict of virus scan analysis
  , emailAnalysisSpfVerdict :: Maybe Text -- ^ Verdict of Send Policy Framework record spoofing analysis
  , emailAnalysisDkimVerdict :: Maybe Text -- ^ Verdict of DomainKeys Identified Mail analysis
  , emailAnalysisDmarcVerdict :: Maybe Text -- ^ Verdict of Domain-based Message Authentication Reporting and Conformance analysis
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailAnalysis where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailAnalysis")
instance ToJSON EmailAnalysis where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailAnalysis")


-- | 
data EmailAvailableResult = EmailAvailableResult
  { emailAvailableResultAvailable :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailAvailableResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailAvailableResult")
instance ToJSON EmailAvailableResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailAvailableResult")


-- | Matches for the given pattern
data EmailContentMatchResult = EmailContentMatchResult
  { emailContentMatchResultPattern :: Text -- ^ 
  , emailContentMatchResultMatches :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailContentMatchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailContentMatchResult")
instance ToJSON EmailContentMatchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailContentMatchResult")


-- | 
data EmailContentPartResult = EmailContentPartResult
  { emailContentPartResultContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailContentPartResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailContentPartResult")
instance ToJSON EmailContentPartResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailContentPartResult")


-- | 
data EmailFeatureCategoryName = EmailFeatureCategoryName
  { emailFeatureCategoryNameSlug :: Text -- ^ 
  , emailFeatureCategoryNameName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureCategoryName where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureCategoryName")
instance ToJSON EmailFeatureCategoryName where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureCategoryName")


-- | 
data EmailFeatureFamilyName = EmailFeatureFamilyName
  { emailFeatureFamilyNameSlug :: Text -- ^ 
  , emailFeatureFamilyNameName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureFamilyName where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureFamilyName")
instance ToJSON EmailFeatureFamilyName where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureFamilyName")


-- | 
data EmailFeatureFamilyStatistics = EmailFeatureFamilyStatistics
  { emailFeatureFamilyStatisticsFeature :: Text -- ^ 
  , emailFeatureFamilyStatisticsFamily :: Text -- ^ 
  , emailFeatureFamilyStatisticsPlatforms :: [EmailFeaturePlatformStatistics] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureFamilyStatistics where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureFamilyStatistics")
instance ToJSON EmailFeatureFamilyStatistics where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureFamilyStatistics")


-- | 
data EmailFeatureNames = EmailFeatureNames
  { emailFeatureNamesFamily :: [EmailFeatureFamilyName] -- ^ 
  , emailFeatureNamesPlatform :: [EmailFeaturePlatformName] -- ^ 
  , emailFeatureNamesCategory :: [EmailFeatureCategoryName] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureNames where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureNames")
instance ToJSON EmailFeatureNames where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureNames")


-- | 
data EmailFeatureOverview = EmailFeatureOverview
  { emailFeatureOverviewFeature :: Text -- ^ 
  , emailFeatureOverviewTitle :: Maybe Text -- ^ 
  , emailFeatureOverviewDescription :: Maybe Text -- ^ 
  , emailFeatureOverviewCategory :: Maybe Text -- ^ 
  , emailFeatureOverviewNotes :: Maybe Text -- ^ 
  , emailFeatureOverviewNotesNumbers :: Maybe (Map.Map String Text) -- ^ 
  , emailFeatureOverviewFeatureStatistics :: Maybe [EmailFeatureFamilyStatistics] -- ^ 
  , emailFeatureOverviewStatuses :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureOverview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureOverview")
instance ToJSON EmailFeatureOverview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureOverview")


-- | 
data EmailFeaturePlatformName = EmailFeaturePlatformName
  { emailFeaturePlatformNameSlug :: Text -- ^ 
  , emailFeaturePlatformNameName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeaturePlatformName where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeaturePlatformName")
instance ToJSON EmailFeaturePlatformName where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeaturePlatformName")


-- | 
data EmailFeaturePlatformStatistics = EmailFeaturePlatformStatistics
  { emailFeaturePlatformStatisticsPlatform :: Text -- ^ 
  , emailFeaturePlatformStatisticsVersions :: [EmailFeatureVersionStatistics] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeaturePlatformStatistics where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeaturePlatformStatistics")
instance ToJSON EmailFeaturePlatformStatistics where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeaturePlatformStatistics")


-- | 
data EmailFeatureSupportFlags = EmailFeatureSupportFlags
  { emailFeatureSupportFlagsStatus :: Text -- ^ 
  , emailFeatureSupportFlagsNotes :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureSupportFlags where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureSupportFlags")
instance ToJSON EmailFeatureSupportFlags where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureSupportFlags")


-- | 
data EmailFeatureSupportResult = EmailFeatureSupportResult
  { emailFeatureSupportResultNames :: EmailFeatureNames -- ^ 
  , emailFeatureSupportResultDetectedFeatures :: [Text] -- ^ 
  , emailFeatureSupportResultFeatureOverviews :: [EmailFeatureOverview] -- ^ 
  , emailFeatureSupportResultFeaturePercentages :: [EmailFeatureSupportStatusPercentage] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureSupportResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureSupportResult")
instance ToJSON EmailFeatureSupportResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureSupportResult")


-- | 
data EmailFeatureSupportStatusPercentage = EmailFeatureSupportStatusPercentage
  { emailFeatureSupportStatusPercentageStatus :: Text -- ^ 
  , emailFeatureSupportStatusPercentagePercentage :: Float -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureSupportStatusPercentage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureSupportStatusPercentage")
instance ToJSON EmailFeatureSupportStatusPercentage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureSupportStatusPercentage")


-- | 
data EmailFeatureVersionStatistics = EmailFeatureVersionStatistics
  { emailFeatureVersionStatisticsVersion :: Text -- ^ 
  , emailFeatureVersionStatisticsSupportFlags :: EmailFeatureSupportFlags -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailFeatureVersionStatistics where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailFeatureVersionStatistics")
instance ToJSON EmailFeatureVersionStatistics where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailFeatureVersionStatistics")


-- | 
data EmailHtmlDto = EmailHtmlDto
  { emailHtmlDtoSubject :: Maybe Text -- ^ 
  , emailHtmlDtoBody :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailHtmlDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailHtmlDto")
instance ToJSON EmailHtmlDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailHtmlDto")


-- | Links found in HTML
data EmailLinksResult = EmailLinksResult
  { emailLinksResultLinks :: [Text] -- ^ 
  , emailLinksResultBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailLinksResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailLinksResult")
instance ToJSON EmailLinksResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailLinksResult")


-- | Preview of an email message. For full message (including body and attachments) call the &#x60;getEmail&#x60; or other email endpoints with the provided email ID.
data EmailPreview = EmailPreview
  { emailPreviewId :: UUID -- ^ ID of the email entity
  , emailPreviewInboxId :: Maybe UUID -- ^ ID of the inbox that received the email
  , emailPreviewDomainId :: Maybe UUID -- ^ ID of the domain that received the email
  , emailPreviewSubject :: Maybe Text -- ^ The subject line of the email message as specified by SMTP subject header
  , emailPreviewTo :: [Text] -- ^ List of `To` recipient email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewFrom :: Maybe Text -- ^ Who the email was sent from. An email address - see fromName for the sender name.
  , emailPreviewBcc :: Maybe [Text] -- ^ List of `BCC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewCc :: Maybe [Text] -- ^ List of `CC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewCreatedAt :: UTCTime -- ^ When was the email received by MailSlurp
  , emailPreviewRead :: Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API with a hydrated body? If so the email is marked as read. Paginated results do not affect read status. Read status is different to email opened event as it depends on your own account accessing the email. Email opened is determined by tracking pixels sent to other uses if enable during sending. You can listened for both email read and email opened events using webhooks.
  , emailPreviewAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  , emailPreviewThreadId :: Maybe UUID -- ^ MailSlurp thread ID for email chain that enables lookup for In-Reply-To and References fields.
  , emailPreviewMessageId :: Maybe Text -- ^ RFC 5322 Message-ID header value without angle brackets.
  , emailPreviewInReplyTo :: Maybe Text -- ^ Parsed value of In-Reply-To header. A Message-ID in a thread.
  , emailPreviewSender :: Maybe Sender -- ^ 
  , emailPreviewRecipients :: Maybe EmailRecipients -- ^ 
  , emailPreviewFavourite :: Maybe Bool -- ^ 
  , emailPreviewBodyPartContentTypes :: Maybe [Text] -- ^ 
  , emailPreviewPlusAddress :: Maybe UUID -- ^ 
  , emailPreviewSizeBytes :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailPreview")
instance ToJSON EmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailPreview")


-- | URLs for email body
data EmailPreviewUrls = EmailPreviewUrls
  { emailPreviewUrlsRawSmtpMessageUrl :: Text -- ^ 
  , emailPreviewUrlsPlainHtmlBodyUrl :: Text -- ^ 
  , emailPreviewUrlsOrigin :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailPreviewUrls where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailPreviewUrls")
instance ToJSON EmailPreviewUrls where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailPreviewUrls")


-- | A compact representation of a full email. Used in list endpoints to keep response sizes low. Body and attachments are not included. To get all fields of the email use the &#x60;getEmail&#x60; method with the email projection&#39;s ID. See &#x60;EmailDto&#x60; for documentation on projection properties.
data EmailProjection = EmailProjection
  { emailProjectionSender :: Maybe Sender -- ^ 
  , emailProjectionRecipients :: Maybe EmailRecipients -- ^ 
  , emailProjectionAttachments :: Maybe [Text] -- ^ 
  , emailProjectionInboxId :: UUID -- ^ 
  , emailProjectionCreatedAt :: UTCTime -- ^ 
  , emailProjectionTo :: [Text] -- ^ 
  , emailProjectionCc :: Maybe [Text] -- ^ 
  , emailProjectionBcc :: Maybe [Text] -- ^ 
  , emailProjectionMessageId :: Maybe Text -- ^ 
  , emailProjectionDomainId :: Maybe UUID -- ^ 
  , emailProjectionFavourite :: Maybe Bool -- ^ 
  , emailProjectionInReplyTo :: Maybe Text -- ^ 
  , emailProjectionPlusAddress :: Maybe UUID -- ^ 
  , emailProjectionSizeBytes :: Maybe Integer -- ^ 
  , emailProjectionRead :: Bool -- ^ 
  , emailProjectionBodyExcerpt :: Maybe Text -- ^ 
  , emailProjectionTextExcerpt :: Maybe Text -- ^ 
  , emailProjectionBodyPartContentTypes :: Maybe [Text] -- ^ 
  , emailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  , emailProjectionTeamAccess :: Bool -- ^ 
  , emailProjectionSubject :: Maybe Text -- ^ 
  , emailProjectionId :: UUID -- ^ 
  , emailProjectionThreadId :: Maybe UUID -- ^ 
  , emailProjectionFrom :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailProjection")
instance ToJSON EmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailProjection")


-- | The &#x60;To&#x60;,&#x60;CC&#x60;,&#x60;BCC&#x60; recipients stored in object form with email address and name accessible.
data EmailRecipients = EmailRecipients
  { emailRecipientsTo :: Maybe [Recipient] -- ^ 
  , emailRecipientsCc :: Maybe [Recipient] -- ^ 
  , emailRecipientsBcc :: Maybe [Recipient] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailRecipients where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailRecipients")
instance ToJSON EmailRecipients where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailRecipients")


-- | Recipients of original email in thread
data EmailRecipientsProjection = EmailRecipientsProjection
  { emailRecipientsProjectionTo :: Maybe [RecipientProjection] -- ^ 
  , emailRecipientsProjectionCc :: Maybe [RecipientProjection] -- ^ 
  , emailRecipientsProjectionBcc :: Maybe [RecipientProjection] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailRecipientsProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailRecipientsProjection")
instance ToJSON EmailRecipientsProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailRecipientsProjection")


-- | 
data EmailScreenshotResult = EmailScreenshotResult
  { emailScreenshotResultBase64EncodedImage :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailScreenshotResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailScreenshotResult")
instance ToJSON EmailScreenshotResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailScreenshotResult")


-- | Parsed text of an email
data EmailTextLinesResult = EmailTextLinesResult
  { emailTextLinesResultLines :: [Text] -- ^ 
  , emailTextLinesResultBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailTextLinesResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailTextLinesResult")
instance ToJSON EmailTextLinesResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailTextLinesResult")


-- | 
data EmailThreadDto = EmailThreadDto
  { emailThreadDtoId :: UUID -- ^ ID of email thread
  , emailThreadDtoUserId :: UUID -- ^ User ID
  , emailThreadDtoInboxId :: Maybe UUID -- ^ Inbox ID
  , emailThreadDtoFrom :: Maybe Text -- ^ From sender
  , emailThreadDtoTo :: [Text] -- ^ To recipients
  , emailThreadDtoCc :: Maybe [Text] -- ^ CC recipients
  , emailThreadDtoBcc :: Maybe [Text] -- ^ BCC recipients
  , emailThreadDtoSender :: Maybe Sender -- ^ 
  , emailThreadDtoRecipients :: Maybe EmailRecipients -- ^ 
  , emailThreadDtoSubject :: Maybe Text -- ^ Thread topic subject
  , emailThreadDtoCreatedAt :: UTCTime -- ^ Created at DateTime
  , emailThreadDtoUpdatedAt :: UTCTime -- ^ Updated at DateTime
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailThreadDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailThreadDto")
instance ToJSON EmailThreadDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailThreadDto")


-- | 
data EmailThreadItem = EmailThreadItem
  { emailThreadItemItemType :: Text -- ^ 
  , emailThreadItemEntityId :: UUID -- ^ 
  , emailThreadItemBodyExcerpt :: Maybe Text -- ^ 
  , emailThreadItemTextExcerpt :: Maybe Text -- ^ 
  , emailThreadItemSubject :: Maybe Text -- ^ 
  , emailThreadItemTo :: [Text] -- ^ 
  , emailThreadItemFrom :: Maybe Text -- ^ 
  , emailThreadItemBcc :: Maybe [Text] -- ^ 
  , emailThreadItemCc :: Maybe [Text] -- ^ 
  , emailThreadItemAttachments :: Maybe [Text] -- ^ 
  , emailThreadItemCreatedAt :: UTCTime -- ^ 
  , emailThreadItemRead :: Bool -- ^ 
  , emailThreadItemInReplyTo :: Maybe Text -- ^ 
  , emailThreadItemMessageId :: Maybe Text -- ^ 
  , emailThreadItemThreadId :: Maybe UUID -- ^ 
  , emailThreadItemSender :: Maybe Sender -- ^ 
  , emailThreadItemRecipients :: Maybe EmailRecipients -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailThreadItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailThreadItem")
instance ToJSON EmailThreadItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailThreadItem")


-- | 
data EmailThreadItemsDto = EmailThreadItemsDto
  { emailThreadItemsDtoItems :: [EmailThreadItem] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailThreadItemsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailThreadItemsDto")
instance ToJSON EmailThreadItemsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailThreadItemsDto")


-- | An email thread is a message thread created for a email based on Message-ID, In-Reply-To, and References headers
data EmailThreadProjection = EmailThreadProjection
  { emailThreadProjectionSender :: Maybe SenderProjection -- ^ 
  , emailThreadProjectionRecipients :: Maybe EmailRecipientsProjection -- ^ 
  , emailThreadProjectionUserId :: UUID -- ^ User ID
  , emailThreadProjectionInboxId :: Maybe UUID -- ^ Inbox ID
  , emailThreadProjectionUpdatedAt :: UTCTime -- ^ Updated at DateTime
  , emailThreadProjectionCreatedAt :: UTCTime -- ^ Created at DateTime
  , emailThreadProjectionTo :: [Text] -- ^ To recipients
  , emailThreadProjectionCc :: Maybe [Text] -- ^ CC recipients
  , emailThreadProjectionBcc :: Maybe [Text] -- ^ BCC recipients
  , emailThreadProjectionHasAttachments :: Bool -- ^ Has attachments
  , emailThreadProjectionMessageCount :: Int -- ^ Number of messages in the thread
  , emailThreadProjectionLastBodyExcerpt :: Maybe Text -- ^ Last body excerpt
  , emailThreadProjectionLastTextExcerpt :: Maybe Text -- ^ Last text excerpt
  , emailThreadProjectionLastCreatedAt :: Maybe UTCTime -- ^ Last email created time
  , emailThreadProjectionLastFrom :: Maybe Text -- ^ Last sender
  , emailThreadProjectionLastSender :: Maybe SenderProjection -- ^ 
  , emailThreadProjectionUnread :: Bool -- ^ Has unread
  , emailThreadProjectionSubject :: Maybe Text -- ^ Thread topic subject
  , emailThreadProjectionId :: UUID -- ^ ID of email thread
  , emailThreadProjectionFrom :: Maybe Text -- ^ From sender
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailThreadProjection")
instance ToJSON EmailThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailThreadProjection")


-- | Email validation request
data EmailValidationRequestDto = EmailValidationRequestDto
  { emailValidationRequestDtoId :: UUID -- ^ 
  , emailValidationRequestDtoUserId :: UUID -- ^ 
  , emailValidationRequestDtoEmailAddress :: Text -- ^ 
  , emailValidationRequestDtoIsValid :: Bool -- ^ 
  , emailValidationRequestDtoCreatedAt :: UTCTime -- ^ 
  , emailValidationRequestDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailValidationRequestDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailValidationRequestDto")
instance ToJSON EmailValidationRequestDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailValidationRequestDto")


-- | Email verification result. Valid means email address exists according to response from mail server running at the domain and port given.
data EmailVerificationResult = EmailVerificationResult
  { emailVerificationResultDomainName :: Text -- ^ 
  , emailVerificationResultPort :: Int -- ^ 
  , emailVerificationResultEmailAddress :: Text -- ^ 
  , emailVerificationResultIsValid :: Bool -- ^ 
  , emailVerificationResultError :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailVerificationResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailVerificationResult")
instance ToJSON EmailVerificationResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailVerificationResult")


-- | 
data EmergencyAddress = EmergencyAddress
  { emergencyAddressId :: UUID -- ^ 
  , emergencyAddressSid :: Text -- ^ 
  , emergencyAddressUserId :: UUID -- ^ 
  , emergencyAddressDisplayName :: Text -- ^ 
  , emergencyAddressCustomerName :: Text -- ^ 
  , emergencyAddressAddress1 :: Text -- ^ 
  , emergencyAddressCity :: Text -- ^ 
  , emergencyAddressRegion :: Text -- ^ 
  , emergencyAddressPostalCode :: Text -- ^ 
  , emergencyAddressPhoneCountry :: Text -- ^ 
  , emergencyAddressAccountSid :: Text -- ^ 
  , emergencyAddressCreatedAt :: UTCTime -- ^ 
  , emergencyAddressUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmergencyAddress where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emergencyAddress")
instance ToJSON EmergencyAddress where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emergencyAddress")


-- | 
data EmergencyAddressDto = EmergencyAddressDto
  { emergencyAddressDtoId :: UUID -- ^ 
  , emergencyAddressDtoAddress1 :: Text -- ^ 
  , emergencyAddressDtoPhoneCountry :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmergencyAddressDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emergencyAddressDto")
instance ToJSON EmergencyAddressDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emergencyAddressDto")


-- | 
data EmptyResponseDto = EmptyResponseDto
  { emptyResponseDtoMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmptyResponseDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emptyResponseDto")
instance ToJSON EmptyResponseDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emptyResponseDto")


-- | 
data EntityAutomationItemProjection = EntityAutomationItemProjection
  { entityAutomationItemProjectionInboxId :: Maybe UUID -- ^ 
  , entityAutomationItemProjectionPhoneId :: Maybe UUID -- ^ 
  , entityAutomationItemProjectionAction :: Maybe Text -- ^ 
  , entityAutomationItemProjectionAutomationType :: Text -- ^ 
  , entityAutomationItemProjectionName :: Maybe Text -- ^ 
  , entityAutomationItemProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityAutomationItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityAutomationItemProjection")
instance ToJSON EntityAutomationItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityAutomationItemProjection")


-- | 
data EntityEventItemProjection = EntityEventItemProjection
  { entityEventItemProjectionEventType :: Text -- ^ 
  , entityEventItemProjectionInboxId :: Maybe UUID -- ^ 
  , entityEventItemProjectionPhoneId :: Maybe UUID -- ^ 
  , entityEventItemProjectionId :: UUID -- ^ 
  , entityEventItemProjectionSeverity :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityEventItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityEventItemProjection")
instance ToJSON EntityEventItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityEventItemProjection")


-- | 
data EntityFavouriteItemProjection = EntityFavouriteItemProjection
  { entityFavouriteItemProjectionCreatedAt :: UTCTime -- ^ 
  , entityFavouriteItemProjectionEntityType :: Text -- ^ 
  , entityFavouriteItemProjectionName :: Text -- ^ 
  , entityFavouriteItemProjectionId :: UUID -- ^ 
  , entityFavouriteItemProjectionDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityFavouriteItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityFavouriteItemProjection")
instance ToJSON EntityFavouriteItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityFavouriteItemProjection")


-- | Expiration defaults for your account
data ExpirationDefaults = ExpirationDefaults
  { expirationDefaultsDefaultExpirationMillis :: Maybe Integer -- ^ 
  , expirationDefaultsMaxExpirationMillis :: Maybe Integer -- ^ 
  , expirationDefaultsDefaultExpiresAt :: Maybe UTCTime -- ^ 
  , expirationDefaultsCanPermanentInbox :: Bool -- ^ 
  , expirationDefaultsNextInboxAllowsPermanent :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExpirationDefaults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "expirationDefaults")
instance ToJSON ExpirationDefaults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "expirationDefaults")


-- | Expired inbox
data ExpiredInboxDto = ExpiredInboxDto
  { expiredInboxDtoId :: UUID -- ^ 
  , expiredInboxDtoInboxId :: UUID -- ^ 
  , expiredInboxDtoEmailAddress :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExpiredInboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "expiredInboxDto")
instance ToJSON ExpiredInboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "expiredInboxDto")


-- | Record of inbox expiration
data ExpiredInboxRecordProjection = ExpiredInboxRecordProjection
  { expiredInboxRecordProjectionUserId :: UUID -- ^ 
  , expiredInboxRecordProjectionEmailAddress :: Text -- ^ 
  , expiredInboxRecordProjectionCreatedAt :: UTCTime -- ^ 
  , expiredInboxRecordProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExpiredInboxRecordProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "expiredInboxRecordProjection")
instance ToJSON ExpiredInboxRecordProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "expiredInboxRecordProjection")


-- | Export download link
data ExportLink = ExportLink
  { exportLinkDownloadLink :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportLink where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportLink")
instance ToJSON ExportLink where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportLink")


-- | Options for exporting user data
data ExportOptions = ExportOptions
  { exportOptionsOutputFormat :: Text -- ^ 
  , exportOptionsExcludePreviouslyExported :: Maybe Bool -- ^ 
  , exportOptionsCreatedEarliestTime :: Maybe UTCTime -- ^ 
  , exportOptionsCreatedOldestTime :: Maybe UTCTime -- ^ 
  , exportOptionsFilter :: Maybe Text -- ^ 
  , exportOptionsListSeparatorToken :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportOptions")
instance ToJSON ExportOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportOptions")


-- | 
data FakeEmailDto = FakeEmailDto
  { fakeEmailDtoId :: UUID -- ^ 
  , fakeEmailDtoEmailAddress :: Text -- ^ 
  , fakeEmailDtoSender :: Maybe Sender -- ^ 
  , fakeEmailDtoRecipients :: Maybe EmailRecipients -- ^ 
  , fakeEmailDtoAttachmentNames :: [Text] -- ^ 
  , fakeEmailDtoSubject :: Maybe Text -- ^ 
  , fakeEmailDtoPreview :: Maybe Text -- ^ 
  , fakeEmailDtoBody :: Text -- ^ 
  , fakeEmailDtoSeen :: Bool -- ^ 
  , fakeEmailDtoCreatedAt :: UTCTime -- ^ 
  , fakeEmailDtoContentType :: Text -- ^ 
  , fakeEmailDtoBodyUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FakeEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fakeEmailDto")
instance ToJSON FakeEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fakeEmailDto")


-- | 
data FakeEmailPreview = FakeEmailPreview
  { fakeEmailPreviewId :: UUID -- ^ 
  , fakeEmailPreviewEmailAddress :: Text -- ^ 
  , fakeEmailPreviewSender :: Maybe Sender -- ^ 
  , fakeEmailPreviewRecipients :: Maybe EmailRecipients -- ^ 
  , fakeEmailPreviewHasAttachments :: Bool -- ^ 
  , fakeEmailPreviewSubject :: Maybe Text -- ^ 
  , fakeEmailPreviewPreview :: Maybe Text -- ^ 
  , fakeEmailPreviewCreatedAt :: UTCTime -- ^ 
  , fakeEmailPreviewSeen :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FakeEmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fakeEmailPreview")
instance ToJSON FakeEmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fakeEmailPreview")


-- | 
data FakeEmailResult = FakeEmailResult
  { fakeEmailResultEmail :: Maybe FakeEmailDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FakeEmailResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fakeEmailResult")
instance ToJSON FakeEmailResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fakeEmailResult")


-- | Options for filtering bounced email recipients
data FilterBouncedRecipientsOptions = FilterBouncedRecipientsOptions
  { filterBouncedRecipientsOptionsEmailRecipients :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilterBouncedRecipientsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filterBouncedRecipientsOptions")
instance ToJSON FilterBouncedRecipientsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filterBouncedRecipientsOptions")


-- | Remaining recipients that were filtered to remove bounced recipients
data FilterBouncedRecipientsResult = FilterBouncedRecipientsResult
  { filterBouncedRecipientsResultFilteredRecipients :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilterBouncedRecipientsResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filterBouncedRecipientsResult")
instance ToJSON FilterBouncedRecipientsResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filterBouncedRecipientsResult")


-- | Result from calling expire on any inboxes that have applicable expiration dates given current time.
data FlushExpiredInboxesResult = FlushExpiredInboxesResult
  { flushExpiredInboxesResultInboxIds :: [UUID] -- ^ Inbox IDs affected by expiration
  , flushExpiredInboxesResultExpireBefore :: UTCTime -- ^ DateTime to filter inboxes so that those expiring before this time are expired
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FlushExpiredInboxesResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "flushExpiredInboxesResult")
instance ToJSON FlushExpiredInboxesResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "flushExpiredInboxesResult")


-- | Options for forwarding an email
data ForwardEmailOptions = ForwardEmailOptions
  { forwardEmailOptionsTo :: [Text] -- ^ To recipients for forwarded email
  , forwardEmailOptionsSubject :: Maybe Text -- ^ Subject for forwarded email
  , forwardEmailOptionsCc :: Maybe [Text] -- ^ Optional cc recipients
  , forwardEmailOptionsBcc :: Maybe [Text] -- ^ Optional bcc recipients
  , forwardEmailOptionsFrom :: Maybe Text -- ^ Optional from override
  , forwardEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  , forwardEmailOptionsFilterBouncedRecipients :: Maybe Bool -- ^ Filter recipients to remove any bounced recipients from to, bcc, and cc before sending
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ForwardEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "forwardEmailOptions")
instance ToJSON ForwardEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "forwardEmailOptions")


-- | 
data GenerateBimiRecordOptions = GenerateBimiRecordOptions
  { generateBimiRecordOptionsDomain :: Text -- ^ 
  , generateBimiRecordOptionsVersion :: Text -- ^ 
  , generateBimiRecordOptionsLogoUrl :: Text -- ^ 
  , generateBimiRecordOptionsVmcUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateBimiRecordOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateBimiRecordOptions")
instance ToJSON GenerateBimiRecordOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateBimiRecordOptions")


-- | 
data GenerateBimiRecordResults = GenerateBimiRecordResults
  { generateBimiRecordResultsName :: Text -- ^ 
  , generateBimiRecordResultsType :: Text -- ^ Domain Name Server Record Types
  , generateBimiRecordResultsTtl :: Int -- ^ 
  , generateBimiRecordResultsValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateBimiRecordResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateBimiRecordResults")
instance ToJSON GenerateBimiRecordResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateBimiRecordResults")


-- | 
data GenerateDmarcRecordOptions = GenerateDmarcRecordOptions
  { generateDmarcRecordOptionsDomain :: Text -- ^ 
  , generateDmarcRecordOptionsVersion :: Text -- ^ 
  , generateDmarcRecordOptionsPolicy :: Text -- ^ 
  , generateDmarcRecordOptionsSubdomainPolicy :: Maybe Text -- ^ 
  , generateDmarcRecordOptionsReportEmailAddress :: Maybe [Text] -- ^ 
  , generateDmarcRecordOptionsForensicEmailAddress :: Maybe [Text] -- ^ 
  , generateDmarcRecordOptionsPercentage :: Maybe Int -- ^ 
  , generateDmarcRecordOptionsReportFormat :: Maybe Text -- ^ 
  , generateDmarcRecordOptionsSecondsBetweenReports :: Maybe Int -- ^ 
  , generateDmarcRecordOptionsAdkim :: Maybe Text -- ^ 
  , generateDmarcRecordOptionsAspf :: Maybe Text -- ^ 
  , generateDmarcRecordOptionsFo :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateDmarcRecordOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateDmarcRecordOptions")
instance ToJSON GenerateDmarcRecordOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateDmarcRecordOptions")


-- | 
data GenerateDmarcRecordResults = GenerateDmarcRecordResults
  { generateDmarcRecordResultsName :: Text -- ^ 
  , generateDmarcRecordResultsType :: Text -- ^ Domain Name Server Record Types
  , generateDmarcRecordResultsTtl :: Int -- ^ 
  , generateDmarcRecordResultsValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateDmarcRecordResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateDmarcRecordResults")
instance ToJSON GenerateDmarcRecordResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateDmarcRecordResults")


-- | 
data GenerateMtaStsRecordOptions = GenerateMtaStsRecordOptions
  { generateMtaStsRecordOptionsHost :: Text -- ^ 
  , generateMtaStsRecordOptionsVersion :: Text -- ^ 
  , generateMtaStsRecordOptionsMode :: Text -- ^ 
  , generateMtaStsRecordOptionsTtl :: Int -- ^ 
  , generateMtaStsRecordOptionsMaxAgeSeconds :: Int -- ^ 
  , generateMtaStsRecordOptionsMxRecords :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateMtaStsRecordOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateMtaStsRecordOptions")
instance ToJSON GenerateMtaStsRecordOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateMtaStsRecordOptions")


-- | 
data GenerateMtaStsRecordResults = GenerateMtaStsRecordResults
  { generateMtaStsRecordResultsName :: Text -- ^ 
  , generateMtaStsRecordResultsType :: Text -- ^ Domain Name Server Record Types
  , generateMtaStsRecordResultsTtl :: Int -- ^ 
  , generateMtaStsRecordResultsValue :: Text -- ^ 
  , generateMtaStsRecordResultsWellKnownValue :: Text -- ^ 
  , generateMtaStsRecordResultsWellKnownUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateMtaStsRecordResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateMtaStsRecordResults")
instance ToJSON GenerateMtaStsRecordResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateMtaStsRecordResults")


-- | Options for generating structured content output from an email
data GenerateStructuredContentEmailOptions = GenerateStructuredContentEmailOptions
  { generateStructuredContentEmailOptionsEmailId :: UUID -- ^ Email ID to read and pass to AI
  , generateStructuredContentEmailOptionsContentSelector :: Maybe Text -- ^ Content selector to select part of email to operate on
  , generateStructuredContentEmailOptionsInstructions :: Maybe Text -- ^ Optional instructions for the AI to follow. Try to be precise and clear. You can include examples and hints.
  , generateStructuredContentEmailOptionsOutputSchema :: StructuredOutputSchema -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateStructuredContentEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateStructuredContentEmailOptions")
instance ToJSON GenerateStructuredContentEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateStructuredContentEmailOptions")


-- | 
data GenerateTlsReportingRecordOptions = GenerateTlsReportingRecordOptions
  { generateTlsReportingRecordOptionsReportingAddresses :: [Text] -- ^ 
  , generateTlsReportingRecordOptionsReportingUrl :: Maybe Text -- ^ 
  , generateTlsReportingRecordOptionsHost :: Text -- ^ 
  , generateTlsReportingRecordOptionsVersion :: Text -- ^ 
  , generateTlsReportingRecordOptionsTtl :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateTlsReportingRecordOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateTlsReportingRecordOptions")
instance ToJSON GenerateTlsReportingRecordOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateTlsReportingRecordOptions")


-- | 
data GenerateTlsReportingRecordResults = GenerateTlsReportingRecordResults
  { generateTlsReportingRecordResultsName :: Text -- ^ 
  , generateTlsReportingRecordResultsType :: Text -- ^ Domain Name Server Record Types
  , generateTlsReportingRecordResultsTtl :: Int -- ^ 
  , generateTlsReportingRecordResultsValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GenerateTlsReportingRecordResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "generateTlsReportingRecordResults")
instance ToJSON GenerateTlsReportingRecordResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "generateTlsReportingRecordResults")


-- | Options taking a screenshot capture of a rendered email
data GetEmailScreenshotOptions = GetEmailScreenshotOptions
  { getEmailScreenshotOptionsHeight :: Maybe Int -- ^ Window height in pixels
  , getEmailScreenshotOptionsWidth :: Maybe Int -- ^ Window width in pixels
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetEmailScreenshotOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getEmailScreenshotOptions")
instance ToJSON GetEmailScreenshotOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getEmailScreenshotOptions")


-- | User image
data GravatarUrl = GravatarUrl
  { gravatarUrlUrl :: Text -- ^ 
  , gravatarUrlHash :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GravatarUrl where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "gravatarUrl")
instance ToJSON GravatarUrl where
  toJSON = genericToJSON (removeFieldLabelPrefix False "gravatarUrl")


-- | Describes contacts attached to a contact group
data GroupContactsDto = GroupContactsDto
  { groupContactsDtoGroup :: GroupDto -- ^ 
  , groupContactsDtoContacts :: [ContactDto] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupContactsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupContactsDto")
instance ToJSON GroupContactsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupContactsDto")


-- | Contact group data
data GroupDto = GroupDto
  { groupDtoId :: UUID -- ^ 
  , groupDtoName :: Text -- ^ 
  , groupDtoDescription :: Maybe Text -- ^ 
  , groupDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupDto")
instance ToJSON GroupDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupDto")


-- | Data for contact group
data GroupProjection = GroupProjection
  { groupProjectionCreatedAt :: UTCTime -- ^ 
  , groupProjectionName :: Text -- ^ 
  , groupProjectionId :: UUID -- ^ 
  , groupProjectionDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupProjection")
instance ToJSON GroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupProjection")


-- | 
data GuestPortalDto = GuestPortalDto
  { guestPortalDtoId :: UUID -- ^ 
  , guestPortalDtoDomainId :: Maybe UUID -- ^ 
  , guestPortalDtoUserId :: UUID -- ^ 
  , guestPortalDtoName :: Maybe Text -- ^ 
  , guestPortalDtoDescription :: Maybe Text -- ^ 
  , guestPortalDtoLinkHelp :: Maybe Text -- ^ 
  , guestPortalDtoCreatedAt :: UTCTime -- ^ 
  , guestPortalDtoUpdatedAt :: UTCTime -- ^ 
  , guestPortalDtoLoginUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GuestPortalDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "guestPortalDto")
instance ToJSON GuestPortalDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "guestPortalDto")


-- | 
data GuestPortalUserCreateDto = GuestPortalUserCreateDto
  { guestPortalUserCreateDtoGuest :: GuestPortalUserDto -- ^ 
  , guestPortalUserCreateDtoPassword :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GuestPortalUserCreateDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "guestPortalUserCreateDto")
instance ToJSON GuestPortalUserCreateDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "guestPortalUserCreateDto")


-- | 
data GuestPortalUserDto = GuestPortalUserDto
  { guestPortalUserDtoId :: UUID -- ^ 
  , guestPortalUserDtoUserId :: UUID -- ^ 
  , guestPortalUserDtoPortalId :: UUID -- ^ 
  , guestPortalUserDtoName :: Maybe Text -- ^ 
  , guestPortalUserDtoUsername :: Text -- ^ 
  , guestPortalUserDtoEmailAddress :: Maybe Text -- ^ 
  , guestPortalUserDtoInboxId :: Maybe UUID -- ^ 
  , guestPortalUserDtoLoginUrl :: Text -- ^ 
  , guestPortalUserDtoCreatedAt :: UTCTime -- ^ 
  , guestPortalUserDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GuestPortalUserDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "guestPortalUserDto")
instance ToJSON GuestPortalUserDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "guestPortalUserDto")


-- | Representation of a guest portal user
data GuestPortalUserProjection = GuestPortalUserProjection
  { guestPortalUserProjectionUsername :: Text -- ^ 
  , guestPortalUserProjectionUserId :: UUID -- ^ 
  , guestPortalUserProjectionEmailAddress :: Maybe Text -- ^ 
  , guestPortalUserProjectionInboxId :: Maybe Text -- ^ 
  , guestPortalUserProjectionUpdatedAt :: UTCTime -- ^ 
  , guestPortalUserProjectionCreatedAt :: UTCTime -- ^ 
  , guestPortalUserProjectionPortalId :: UUID -- ^ 
  , guestPortalUserProjectionName :: Maybe Text -- ^ 
  , guestPortalUserProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GuestPortalUserProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "guestPortalUserProjection")
instance ToJSON GuestPortalUserProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "guestPortalUserProjection")


-- | HTML Validation Results
data HTMLValidationResult = HTMLValidationResult
  { hTMLValidationResultIsValid :: Bool -- ^ Is HTML validation result valid
  , hTMLValidationResultInfos :: [ValidationMessage] -- ^ Optional infos resulting from HTML validation
  , hTMLValidationResultErrors :: [ValidationMessage] -- ^ Optional errors resulting from HTML validation
  , hTMLValidationResultWarnings :: [ValidationMessage] -- ^ Optional warnings resulting from HTML validation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON HTMLValidationResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "hTMLValidationResult")
instance ToJSON HTMLValidationResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "hTMLValidationResult")


-- | IP Address look up result for a given domain / hostname
data IPAddressResult = IPAddressResult
  { iPAddressResultAddress :: Text -- ^ 
  , iPAddressResultHostname :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IPAddressResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "iPAddressResult")
instance ToJSON IPAddressResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "iPAddressResult")


-- | 
data ImageIssue = ImageIssue
  { imageIssueUrl :: Text -- ^ 
  , imageIssueResponseStatus :: Maybe Int -- ^ 
  , imageIssueSeverity :: Text -- ^ 
  , imageIssueMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImageIssue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imageIssue")
instance ToJSON ImageIssue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imageIssue")


-- | Access details for inbox using IMAP
data ImapAccessDetails = ImapAccessDetails
  { imapAccessDetailsSecureImapServerHost :: Text -- ^ Secure TLS IMAP server host domain
  , imapAccessDetailsSecureImapServerPort :: Int -- ^ Secure TLS IMAP server host port
  , imapAccessDetailsSecureImapUsername :: Text -- ^ Secure TLS IMAP username for login
  , imapAccessDetailsSecureImapPassword :: Text -- ^ Secure TLS IMAP password for login
  , imapAccessDetailsImapServerHost :: Text -- ^ IMAP server host domain
  , imapAccessDetailsImapServerPort :: Int -- ^ IMAP server host port
  , imapAccessDetailsImapUsername :: Text -- ^ IMAP username for login
  , imapAccessDetailsImapPassword :: Text -- ^ IMAP password for login
  , imapAccessDetailsImapMailbox :: Text -- ^ IMAP mailbox to SELECT
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapAccessDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapAccessDetails")
instance ToJSON ImapAccessDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapAccessDetails")


-- | 
data ImapEmailProjection = ImapEmailProjection
  { imapEmailProjectionCreatedAt :: UTCTime -- ^ 
  , imapEmailProjectionRead :: Maybe Bool -- ^ 
  , imapEmailProjectionUid :: Integer -- ^ 
  , imapEmailProjectionSeqNum :: Integer -- ^ 
  , imapEmailProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapEmailProjection")
instance ToJSON ImapEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapEmailProjection")


-- | IMAP operation flags
data ImapFlagOperationOptions = ImapFlagOperationOptions
  { imapFlagOperationOptionsFlagOperation :: Text -- ^ 
  , imapFlagOperationOptionsFlags :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapFlagOperationOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapFlagOperationOptions")
instance ToJSON ImapFlagOperationOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapFlagOperationOptions")


-- | 
data ImapMailboxStatus = ImapMailboxStatus
  { imapMailboxStatusName :: Text -- ^ The mailbox name.
  , imapMailboxStatusReadOnly :: Bool -- ^ True if the mailbox is open in read-only mode.
  , imapMailboxStatusItems :: Value -- ^ Results map
  , imapMailboxStatusFlags :: [Text] -- ^ The mailbox flags.
  , imapMailboxStatusPermanentFlags :: [Text] -- ^ The mailbox permanent flags.
  , imapMailboxStatusUnseenSeqNum :: Integer -- ^ The sequence number of the first unseen message in the mailbox.
  , imapMailboxStatusMessages :: Int -- ^ The number of messages in this mailbox.
  , imapMailboxStatusRecent :: Int -- ^ The number of messages not seen since the last time the mailbox was opened.
  , imapMailboxStatusUnseen :: Int -- ^ The number of unread messages.
  , imapMailboxStatusUidNext :: Integer -- ^ The next UID.
  , imapMailboxStatusUidValidity :: Int -- ^ Together with a UID, it is a unique identifier for a message. Must be greater than or equal to 1.
  , imapMailboxStatusAppendLimit :: Maybe Int -- ^ Per-mailbox limit of message size. Set only if server supports the APPENDLIMIT extension
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapMailboxStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapMailboxStatus")
instance ToJSON ImapMailboxStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapMailboxStatus")


-- | IMAP fetch content in raw format
data ImapServerFetchItem = ImapServerFetchItem
  { imapServerFetchItemContent :: Text -- ^ Content of the email
  , imapServerFetchItemId :: UUID -- ^ ID of the email
  , imapServerFetchItemUid :: Integer -- ^ UID of the email
  , imapServerFetchItemSeqNum :: Integer -- ^ Sequence number of the email
  , imapServerFetchItemRead :: Bool -- ^ Read status of the email
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerFetchItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerFetchItem")
instance ToJSON ImapServerFetchItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerFetchItem")


-- | IMAP fetch email result
data ImapServerFetchResult = ImapServerFetchResult
  { imapServerFetchResultResult :: Maybe ImapServerFetchItem -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerFetchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerFetchResult")
instance ToJSON ImapServerFetchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerFetchResult")


-- | 
data ImapServerGetResult = ImapServerGetResult
  { imapServerGetResultResult :: Maybe ImapEmailProjection -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerGetResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerGetResult")
instance ToJSON ImapServerGetResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerGetResult")


-- | 
data ImapServerListOptions = ImapServerListOptions
  { imapServerListOptionsUidSet :: Maybe Text -- ^ 
  , imapServerListOptionsSeqSet :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerListOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerListOptions")
instance ToJSON ImapServerListOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerListOptions")


-- | 
data ImapServerListResult = ImapServerListResult
  { imapServerListResultResults :: [ImapEmailProjection] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerListResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerListResult")
instance ToJSON ImapServerListResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerListResult")


-- | 
data ImapServerMailboxResult = ImapServerMailboxResult
  { imapServerMailboxResultMessage :: Maybe Text -- ^ 
  , imapServerMailboxResultSuccess :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerMailboxResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerMailboxResult")
instance ToJSON ImapServerMailboxResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerMailboxResult")


-- | IMAP server search options
data ImapServerSearchOptions = ImapServerSearchOptions
  { imapServerSearchOptionsSeqNum :: Maybe Text -- ^ 
  , imapServerSearchOptionsUid :: Maybe Text -- ^ 
  , imapServerSearchOptionsSince :: Maybe UTCTime -- ^ 
  , imapServerSearchOptionsBefore :: Maybe UTCTime -- ^ 
  , imapServerSearchOptionsSentSince :: Maybe UTCTime -- ^ 
  , imapServerSearchOptionsSentBefore :: Maybe UTCTime -- ^ 
  , imapServerSearchOptionsHeader :: Maybe (Map.Map String [Text]) -- ^ 
  , imapServerSearchOptionsBody :: Maybe [Text] -- ^ 
  , imapServerSearchOptionsText :: Maybe [Text] -- ^ 
  , imapServerSearchOptionsWithFlags :: Maybe [Text] -- ^ 
  , imapServerSearchOptionsWithoutFlags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerSearchOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerSearchOptions")
instance ToJSON ImapServerSearchOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerSearchOptions")


-- | 
data ImapServerSearchResult = ImapServerSearchResult
  { imapServerSearchResultResults :: [ImapEmailProjection] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerSearchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerSearchResult")
instance ToJSON ImapServerSearchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerSearchResult")


-- | 
data ImapServerStatusOptions = ImapServerStatusOptions
  { imapServerStatusOptionsName :: Maybe Text -- ^ 
  , imapServerStatusOptionsStatusItems :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerStatusOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerStatusOptions")
instance ToJSON ImapServerStatusOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerStatusOptions")


-- | 
data ImapServerStatusResult = ImapServerStatusResult
  { imapServerStatusResultResult :: Maybe ImapMailboxStatus -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapServerStatusResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapServerStatusResult")
instance ToJSON ImapServerStatusResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapServerStatusResult")


-- | Access details for inbox using SMTP or IMAP
data ImapSmtpAccessDetails = ImapSmtpAccessDetails
  { imapSmtpAccessDetailsEmailAddress :: Text -- ^ Email address for SMTP/IMAP login
  , imapSmtpAccessDetailsSecureSmtpServerHost :: Text -- ^ Secure TLS SMTP server host domain
  , imapSmtpAccessDetailsSecureSmtpServerPort :: Int -- ^ Secure TLS SMTP server host port
  , imapSmtpAccessDetailsSecureSmtpUsername :: Text -- ^ Secure TLS SMTP username for login
  , imapSmtpAccessDetailsSecureSmtpPassword :: Text -- ^ Secure TLS SMTP password for login
  , imapSmtpAccessDetailsSmtpServerHost :: Text -- ^ SMTP server host domain
  , imapSmtpAccessDetailsSmtpServerPort :: Int -- ^ SMTP server host port
  , imapSmtpAccessDetailsSmtpUsername :: Text -- ^ SMTP username for login
  , imapSmtpAccessDetailsSmtpPassword :: Text -- ^ SMTP password for login
  , imapSmtpAccessDetailsSecureImapServerHost :: Text -- ^ Secure TLS IMAP server host domain
  , imapSmtpAccessDetailsSecureImapServerPort :: Int -- ^ Secure TLS IMAP server host port
  , imapSmtpAccessDetailsSecureImapUsername :: Text -- ^ Secure TLS IMAP username for login
  , imapSmtpAccessDetailsSecureImapPassword :: Text -- ^ Secure TLS IMAP password for login
  , imapSmtpAccessDetailsImapServerHost :: Text -- ^ IMAP server host domain
  , imapSmtpAccessDetailsImapServerPort :: Int -- ^ IMAP server host port
  , imapSmtpAccessDetailsImapUsername :: Text -- ^ IMAP username for login
  , imapSmtpAccessDetailsImapPassword :: Text -- ^ IMAP password for login
  , imapSmtpAccessDetailsImapMailbox :: Text -- ^ IMAP mailbox to SELECT
  , imapSmtpAccessDetailsMailFromDomain :: Maybe Text -- ^ Mail from domain or SMTP HELO value
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapSmtpAccessDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapSmtpAccessDetails")
instance ToJSON ImapSmtpAccessDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapSmtpAccessDetails")


-- | IMAP and SMTP server endpoints for MailSlurp
data ImapSmtpAccessServers = ImapSmtpAccessServers
  { imapSmtpAccessServersImapServer :: ServerEndpoints -- ^ 
  , imapSmtpAccessServersSecureImapServer :: ServerEndpoints -- ^ 
  , imapSmtpAccessServersSmtpServer :: ServerEndpoints -- ^ 
  , imapSmtpAccessServersSecureSmtpServer :: ServerEndpoints -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapSmtpAccessServers where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapSmtpAccessServers")
instance ToJSON ImapSmtpAccessServers where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapSmtpAccessServers")


-- | 
data ImapUpdateFlagsOptions = ImapUpdateFlagsOptions
  { imapUpdateFlagsOptionsOperation :: Text -- ^ 
  , imapUpdateFlagsOptionsFlags :: Maybe [Text] -- ^ 
  , imapUpdateFlagsOptionsUidSet :: Maybe Text -- ^ 
  , imapUpdateFlagsOptionsSeqSet :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapUpdateFlagsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapUpdateFlagsOptions")
instance ToJSON ImapUpdateFlagsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapUpdateFlagsOptions")


-- | Result of search for inbox by email address
data InboxByEmailAddressResult = InboxByEmailAddressResult
  { inboxByEmailAddressResultInboxId :: Maybe UUID -- ^ 
  , inboxByEmailAddressResultExists :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxByEmailAddressResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxByEmailAddressResult")
instance ToJSON InboxByEmailAddressResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxByEmailAddressResult")


-- | Result of search for inbox by name
data InboxByNameResult = InboxByNameResult
  { inboxByNameResultInboxId :: Maybe UUID -- ^ 
  , inboxByNameResultExists :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxByNameResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxByNameResult")
instance ToJSON InboxByNameResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxByNameResult")


-- | Representation of a MailSlurp inbox. An inbox has an ID and a real email address. Emails can be sent to or from this email address. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. The default, &#x60;HTTP&#x60; inboxes, use AWS SES to process emails and are best suited as test email accounts and do not support IMAP or POP3. &#x60;SMTP&#x60; inboxes use a custom mail server at &#x60;mxslurp.click&#x60; and support SMTP login, IMAP and POP3. Use the &#x60;EmailController&#x60; or the &#x60;InboxController&#x60; methods to send and receive emails and attachments. Inboxes may have a description, name, and tags for display purposes. You can also favourite an inbox for easier searching.
data InboxDto = InboxDto
  { inboxDtoId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , inboxDtoUserId :: UUID -- ^ ID of user that inbox belongs to
  , inboxDtoCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , inboxDtoName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , inboxDtoDomainId :: Maybe UUID -- ^ ID of custom domain used by the inbox if any
  , inboxDtoDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , inboxDtoEmailAddress :: Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , inboxDtoExpiresAt :: Maybe UTCTime -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , inboxDtoFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , inboxDtoTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , inboxDtoInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , inboxDtoReadOnly :: Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  , inboxDtoVirtualInbox :: Bool -- ^ Virtual inbox can receive email but will not send emails to real recipients. Will save sent email record but never send an actual email. Perfect for testing mail server actions.
  , inboxDtoFunctionsAs :: Maybe Text -- ^ Inbox function if used as a primitive for another system.
  , inboxDtoLocalPart :: Maybe Text -- ^ Local part of email addresses before the @ symbol
  , inboxDtoDomain :: Maybe Text -- ^ Domain name of the email address
  , inboxDtoAccountRegion :: Maybe Text -- ^ Region of the inbox
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxDto")
instance ToJSON InboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxDto")


-- | Result of email exists query
data InboxExistsDto = InboxExistsDto
  { inboxExistsDtoExists :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxExistsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxExistsDto")
instance ToJSON InboxExistsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxExistsDto")


-- | Inbox forwarder. Describes how an inbox will forward matching emails to designated recipients.
data InboxForwarderDto = InboxForwarderDto
  { inboxForwarderDtoId :: UUID -- ^ 
  , inboxForwarderDtoInboxId :: Maybe UUID -- ^ 
  , inboxForwarderDtoName :: Maybe Text -- ^ Name of inbox forwarder
  , inboxForwarderDtoField :: Text -- ^ Which field to match against
  , inboxForwarderDtoMatch :: Text -- ^ Wild-card type pattern to apply to field
  , inboxForwarderDtoForwardToRecipients :: [Text] -- ^ Who to send forwarded email to
  , inboxForwarderDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderDto")
instance ToJSON InboxForwarderDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderDto")


-- | Inbox forwarder event. Describes how an email was handled by an inbox forwarder.
data InboxForwarderEventDto = InboxForwarderEventDto
  { inboxForwarderEventDtoId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoInboxId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoEmailId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoSentId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoUserId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoForwarderId :: Maybe UUID -- ^ 
  , inboxForwarderEventDtoMessage :: Maybe Text -- ^ 
  , inboxForwarderEventDtoStatus :: Maybe Text -- ^ 
  , inboxForwarderEventDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderEventDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderEventDto")
instance ToJSON InboxForwarderEventDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderEventDto")


-- | Inbox forwarder event
data InboxForwarderEventProjection = InboxForwarderEventProjection
  { inboxForwarderEventProjectionUserId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionEmailId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionInboxId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionCreatedAt :: UTCTime -- ^ 
  , inboxForwarderEventProjectionSentId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionForwarderId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionMessage :: Maybe Text -- ^ 
  , inboxForwarderEventProjectionId :: Maybe UUID -- ^ 
  , inboxForwarderEventProjectionStatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderEventProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderEventProjection")
instance ToJSON InboxForwarderEventProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderEventProjection")


-- | Options for testing an inbox forwarder against a value
data InboxForwarderTestOptions = InboxForwarderTestOptions
  { inboxForwarderTestOptionsTestValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderTestOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderTestOptions")
instance ToJSON InboxForwarderTestOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderTestOptions")


-- | Results of inbox forwarder test
data InboxForwarderTestResult = InboxForwarderTestResult
  { inboxForwarderTestResultMatches :: (Map.Map String Bool) -- ^ 
  , inboxForwarderTestResultDoesMatch :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderTestResult")
instance ToJSON InboxForwarderTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderTestResult")


-- | Inbox ID and email address pair
data InboxIdItem = InboxIdItem
  { inboxIdItemId :: UUID -- ^ 
  , inboxIdItemEmailAddress :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxIdItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxIdItem")
instance ToJSON InboxIdItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxIdItem")


-- | List of inbox IDs and email addresses
data InboxIdsResult = InboxIdsResult
  { inboxIdsResultInboxIds :: [InboxIdItem] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxIdsResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxIdsResult")
instance ToJSON InboxIdsResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxIdsResult")


-- | Inbox data preview element.
data InboxPreview = InboxPreview
  { inboxPreviewId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , inboxPreviewDomainId :: Maybe UUID -- ^ ID of custom domain used by the inbox if any
  , inboxPreviewEmailAddress :: Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , inboxPreviewCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , inboxPreviewFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , inboxPreviewName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , inboxPreviewTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , inboxPreviewTeamAccess :: Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  , inboxPreviewInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , inboxPreviewVirtualInbox :: Bool -- ^ Virtual inbox can receive email but will not send emails to real recipients. Will save sent email record but never send an actual email. Perfect for testing mail server actions.
  , inboxPreviewExpiresAt :: Maybe UTCTime -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , inboxPreviewFunctionsAs :: Maybe Text -- ^ Inbox function if used as a primitive for another system.
  , inboxPreviewUserId :: UUID -- ^ ID of user that inbox belongs to
  , inboxPreviewDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , inboxPreviewAccountRegion :: Maybe Text -- ^ Region of the inbox
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxPreview")
instance ToJSON InboxPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxPreview")


-- | Inbox replier. Will automatically reply to inbound emails that match given field for an inbox.
data InboxReplierDto = InboxReplierDto
  { inboxReplierDtoId :: UUID -- ^ 
  , inboxReplierDtoInboxId :: Maybe UUID -- ^ 
  , inboxReplierDtoName :: Maybe Text -- ^ 
  , inboxReplierDtoField :: Text -- ^ 
  , inboxReplierDtoMatch :: Text -- ^ 
  , inboxReplierDtoReplyTo :: Maybe Text -- ^ 
  , inboxReplierDtoSubject :: Maybe Text -- ^ 
  , inboxReplierDtoFrom :: Maybe Text -- ^ 
  , inboxReplierDtoCharset :: Maybe Text -- ^ 
  , inboxReplierDtoIsHTML :: Bool -- ^ 
  , inboxReplierDtoTemplateId :: Maybe UUID -- ^ 
  , inboxReplierDtoTemplateVariables :: Maybe (Map.Map String Value) -- ^ 
  , inboxReplierDtoIgnoreReplyTo :: Bool -- ^ 
  , inboxReplierDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxReplierDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxReplierDto")
instance ToJSON InboxReplierDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxReplierDto")


-- | Inbox replier event
data InboxReplierEventProjection = InboxReplierEventProjection
  { inboxReplierEventProjectionRecipients :: Maybe [Text] -- ^ 
  , inboxReplierEventProjectionUserId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionEmailId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionInboxId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionCreatedAt :: UTCTime -- ^ 
  , inboxReplierEventProjectionSentId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionReplierId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionMessage :: Maybe Text -- ^ 
  , inboxReplierEventProjectionId :: Maybe UUID -- ^ 
  , inboxReplierEventProjectionStatus :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxReplierEventProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxReplierEventProjection")
instance ToJSON InboxReplierEventProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxReplierEventProjection")


-- | Rules for inbox
data InboxRulesetDto = InboxRulesetDto
  { inboxRulesetDtoId :: UUID -- ^ 
  , inboxRulesetDtoInboxId :: Maybe UUID -- ^ 
  , inboxRulesetDtoScope :: Text -- ^ 
  , inboxRulesetDtoAction :: Text -- ^ 
  , inboxRulesetDtoTarget :: Text -- ^ 
  , inboxRulesetDtoHandler :: Text -- ^ 
  , inboxRulesetDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetDto")
instance ToJSON InboxRulesetDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetDto")


-- | Test options for inbox ruleset
data InboxRulesetTestOptions = InboxRulesetTestOptions
  { inboxRulesetTestOptionsTestTarget :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetTestOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetTestOptions")
instance ToJSON InboxRulesetTestOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetTestOptions")


-- | Result of test of inbox ruleset
data InboxRulesetTestResult = InboxRulesetTestResult
  { inboxRulesetTestResultRulesetMatches :: (Map.Map String Bool) -- ^ Map of inbox ruleset ID to boolean of if target matches
  , inboxRulesetTestResultMatches :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetTestResult")
instance ToJSON InboxRulesetTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetTestResult")


-- | 
data InlineObject = InlineObject
  { inlineObjectFile :: FilePath -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineObject")
instance ToJSON InlineObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineObject")


-- | JSONSchema for payload
data JSONSchemaDto = JSONSchemaDto
  { jSONSchemaDtoValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JSONSchemaDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jSONSchemaDto")
instance ToJSON JSONSchemaDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jSONSchemaDto")


-- | 
data LinkIssue = LinkIssue
  { linkIssueUrl :: Text -- ^ 
  , linkIssueResponseStatus :: Maybe Int -- ^ 
  , linkIssueSeverity :: Text -- ^ 
  , linkIssueMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LinkIssue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "linkIssue")
instance ToJSON LinkIssue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "linkIssue")


-- | List unsubscribe recipient
data ListUnsubscribeRecipientProjection = ListUnsubscribeRecipientProjection
  { listUnsubscribeRecipientProjectionEmailAddress :: Text -- ^ 
  , listUnsubscribeRecipientProjectionCreatedAt :: UTCTime -- ^ 
  , listUnsubscribeRecipientProjectionDomainId :: Maybe UUID -- ^ 
  , listUnsubscribeRecipientProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ListUnsubscribeRecipientProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "listUnsubscribeRecipientProjection")
instance ToJSON ListUnsubscribeRecipientProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "listUnsubscribeRecipientProjection")


-- | 
data LookupBimiDomainOptions = LookupBimiDomainOptions
  { lookupBimiDomainOptionsHost :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupBimiDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupBimiDomainOptions")
instance ToJSON LookupBimiDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupBimiDomainOptions")


-- | 
data LookupBimiDomainResults = LookupBimiDomainResults
  { lookupBimiDomainResultsValid :: Bool -- ^ 
  , lookupBimiDomainResultsQuery :: DNSLookupOptions -- ^ 
  , lookupBimiDomainResultsRecords :: [DNSLookupResult] -- ^ 
  , lookupBimiDomainResultsErrors :: [Text] -- ^ 
  , lookupBimiDomainResultsWarnings :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupBimiDomainResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupBimiDomainResults")
instance ToJSON LookupBimiDomainResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupBimiDomainResults")


-- | 
data LookupDmarcDomainOptions = LookupDmarcDomainOptions
  { lookupDmarcDomainOptionsHost :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupDmarcDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupDmarcDomainOptions")
instance ToJSON LookupDmarcDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupDmarcDomainOptions")


-- | 
data LookupDmarcDomainResults = LookupDmarcDomainResults
  { lookupDmarcDomainResultsValid :: Bool -- ^ 
  , lookupDmarcDomainResultsQuery :: DNSLookupOptions -- ^ 
  , lookupDmarcDomainResultsRecords :: [DNSLookupResult] -- ^ 
  , lookupDmarcDomainResultsErrors :: [Text] -- ^ 
  , lookupDmarcDomainResultsWarnings :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupDmarcDomainResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupDmarcDomainResults")
instance ToJSON LookupDmarcDomainResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupDmarcDomainResults")


-- | 
data LookupMtaStsDomainOptions = LookupMtaStsDomainOptions
  { lookupMtaStsDomainOptionsHost :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupMtaStsDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupMtaStsDomainOptions")
instance ToJSON LookupMtaStsDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupMtaStsDomainOptions")


-- | 
data LookupMtaStsDomainResults = LookupMtaStsDomainResults
  { lookupMtaStsDomainResultsValid :: Bool -- ^ 
  , lookupMtaStsDomainResultsQuery :: DNSLookupOptions -- ^ 
  , lookupMtaStsDomainResultsRecords :: [DNSLookupResult] -- ^ 
  , lookupMtaStsDomainResultsWellKnownQuery :: Text -- ^ 
  , lookupMtaStsDomainResultsWellKnownPresent :: Bool -- ^ 
  , lookupMtaStsDomainResultsWellKnownValue :: Text -- ^ 
  , lookupMtaStsDomainResultsErrors :: [Text] -- ^ 
  , lookupMtaStsDomainResultsWarnings :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupMtaStsDomainResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupMtaStsDomainResults")
instance ToJSON LookupMtaStsDomainResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupMtaStsDomainResults")


-- | 
data LookupTlsReportingDomainOptions = LookupTlsReportingDomainOptions
  { lookupTlsReportingDomainOptionsHost :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupTlsReportingDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupTlsReportingDomainOptions")
instance ToJSON LookupTlsReportingDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupTlsReportingDomainOptions")


-- | 
data LookupTlsReportingDomainResults = LookupTlsReportingDomainResults
  { lookupTlsReportingDomainResultsValid :: Bool -- ^ 
  , lookupTlsReportingDomainResultsQuery :: DNSLookupOptions -- ^ 
  , lookupTlsReportingDomainResultsRecords :: [DNSLookupResult] -- ^ 
  , lookupTlsReportingDomainResultsErrors :: [Text] -- ^ 
  , lookupTlsReportingDomainResultsWarnings :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LookupTlsReportingDomainResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lookupTlsReportingDomainResults")
instance ToJSON LookupTlsReportingDomainResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lookupTlsReportingDomainResults")


-- | Options for matching emails in an inbox. Each match option object contains a &#x60;field&#x60;, &#x60;should&#x60; and &#x60;value&#x60; property. Together they form logical conditions such as &#x60;SUBJECT&#x60; should &#x60;CONTAIN&#x60; value.
data MatchOption = MatchOption
  { matchOptionField :: Text -- ^ Fields of an email object that can be used to filter results
  , matchOptionShould :: Text -- ^ How the value of the email field specified should be compared to the value given in the match options.
  , matchOptionValue :: Text -- ^ The value you wish to compare with the value of the field specified using the `should` value passed. For example `BODY` should `CONTAIN` a value passed.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MatchOption where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "matchOption")
instance ToJSON MatchOption where
  toJSON = genericToJSON (removeFieldLabelPrefix False "matchOption")


-- | Optional filter for matching emails based on fields. For instance filter results to only include emails whose &#x60;SUBJECT&#x60; value does &#x60;CONTAIN&#x60; given match value. An example payload would be &#x60;{ matches: [{ field: &#39;SUBJECT&#39;, should: &#39;CONTAIN&#39;, value: &#39;Welcome&#39; }] }&#x60;. You can also pass conditions such as &#x60;HAS_ATTACHMENT&#x60;. If you wish to extract regex matches inside the email content see the &#x60;getEmailContentMatch&#x60; method in the EmailController.
data MatchOptions = MatchOptions
  { matchOptionsMatches :: Maybe [MatchOption] -- ^ Zero or more match options such as `{ field: 'SUBJECT', should: 'CONTAIN', value: 'Welcome' }`. Options are additive so if one does not match the email is excluded from results
  , matchOptionsConditions :: Maybe [ConditionOption] -- ^ Zero or more conditions such as `{ condition: 'HAS_ATTACHMENTS', value: 'TRUE' }`. Note the values are the strings `TRUE|FALSE` not booleans.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MatchOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "matchOptions")
instance ToJSON MatchOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "matchOptions")


-- | Missed email
data MissedEmailDto = MissedEmailDto
  { missedEmailDtoId :: UUID -- ^ 
  , missedEmailDtoUserId :: Maybe UUID -- ^ 
  , missedEmailDtoSubject :: Maybe Text -- ^ 
  , missedEmailDtoBodyExcerpt :: Maybe Text -- ^ 
  , missedEmailDtoAttachmentCount :: Int -- ^ 
  , missedEmailDtoFrom :: Maybe Text -- ^ 
  , missedEmailDtoRawUrl :: Maybe Text -- ^ 
  , missedEmailDtoRawKey :: Maybe Text -- ^ 
  , missedEmailDtoRawBucket :: Maybe Text -- ^ 
  , missedEmailDtoCanRestore :: Maybe Bool -- ^ 
  , missedEmailDtoTo :: [Text] -- ^ 
  , missedEmailDtoCc :: [Text] -- ^ 
  , missedEmailDtoBcc :: [Text] -- ^ 
  , missedEmailDtoInboxIds :: [UUID] -- ^ 
  , missedEmailDtoCreatedAt :: UTCTime -- ^ 
  , missedEmailDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MissedEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "missedEmailDto")
instance ToJSON MissedEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "missedEmailDto")


-- | Missed email data
data MissedEmailProjection = MissedEmailProjection
  { missedEmailProjectionUserId :: Maybe UUID -- ^ 
  , missedEmailProjectionCreatedAt :: UTCTime -- ^ 
  , missedEmailProjectionSubject :: Maybe Text -- ^ 
  , missedEmailProjectionId :: UUID -- ^ 
  , missedEmailProjectionFrom :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "missedEmailProjection")
instance ToJSON MissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "missedEmailProjection")


-- | Name Server Record
data NameServerRecord = NameServerRecord
  { nameServerRecordRaw :: Text -- ^ 
  , nameServerRecordRecordType :: Text -- ^ 
  , nameServerRecordPriority :: Text -- ^ 
  , nameServerRecordValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NameServerRecord where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nameServerRecord")
instance ToJSON NameServerRecord where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nameServerRecord")


-- | 
data NewFakeEmailAddressResult = NewFakeEmailAddressResult
  { newFakeEmailAddressResultEmailAddress :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewFakeEmailAddressResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "newFakeEmailAddressResult")
instance ToJSON NewFakeEmailAddressResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "newFakeEmailAddressResult")


-- | 
data OptInConsentOptions = OptInConsentOptions
  { optInConsentOptionsEmailAddress :: Text -- ^ 
  , optInConsentOptionsCompanyName :: Maybe Text -- ^ 
  , optInConsentOptionsSendOptInOptions :: Maybe SendOptInConsentEmailOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptInConsentOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optInConsentOptions")
instance ToJSON OptInConsentOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optInConsentOptions")


-- | 
data OptInConsentSendResult = OptInConsentSendResult
  { optInConsentSendResultSent :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptInConsentSendResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optInConsentSendResult")
instance ToJSON OptInConsentSendResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optInConsentSendResult")


-- | 
data OptInIdentityProjection = OptInIdentityProjection
  { optInIdentityProjectionId :: UUID -- ^ 
  , optInIdentityProjectionEmailAddress :: Text -- ^ 
  , optInIdentityProjectionUpdatedAt :: UTCTime -- ^ 
  , optInIdentityProjectionCreatedAt :: UTCTime -- ^ 
  , optInIdentityProjectionVerified :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptInIdentityProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optInIdentityProjection")
instance ToJSON OptInIdentityProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optInIdentityProjection")


-- | 
data OptInSendingConsentDto = OptInSendingConsentDto
  { optInSendingConsentDtoVerificationCodeSent :: Bool -- ^ 
  , optInSendingConsentDtoUserHasConsented :: Bool -- ^ 
  , optInSendingConsentDtoCanSend :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptInSendingConsentDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optInSendingConsentDto")
instance ToJSON OptInSendingConsentDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optInSendingConsentDto")


-- | 
data OptionalConnectorDto = OptionalConnectorDto
  { optionalConnectorDtoPresent :: Bool -- ^ 
  , optionalConnectorDtoResult :: Maybe ConnectorDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptionalConnectorDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optionalConnectorDto")
instance ToJSON OptionalConnectorDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optionalConnectorDto")


-- | 
data OptionalConnectorImapConnectionDto = OptionalConnectorImapConnectionDto
  { optionalConnectorImapConnectionDtoPresent :: Bool -- ^ 
  , optionalConnectorImapConnectionDtoResult :: Maybe ConnectorImapConnectionDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptionalConnectorImapConnectionDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optionalConnectorImapConnectionDto")
instance ToJSON OptionalConnectorImapConnectionDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optionalConnectorImapConnectionDto")


-- | 
data OptionalConnectorSmtpConnectionDto = OptionalConnectorSmtpConnectionDto
  { optionalConnectorSmtpConnectionDtoPresent :: Bool -- ^ 
  , optionalConnectorSmtpConnectionDtoResult :: Maybe ConnectorSmtpConnectionDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptionalConnectorSmtpConnectionDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optionalConnectorSmtpConnectionDto")
instance ToJSON OptionalConnectorSmtpConnectionDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optionalConnectorSmtpConnectionDto")


-- | 
data OptionalConnectorSyncSettingsDto = OptionalConnectorSyncSettingsDto
  { optionalConnectorSyncSettingsDtoPresent :: Bool -- ^ 
  , optionalConnectorSyncSettingsDtoResult :: Maybe ConnectorSyncSettingsDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OptionalConnectorSyncSettingsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "optionalConnectorSyncSettingsDto")
instance ToJSON OptionalConnectorSyncSettingsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "optionalConnectorSyncSettingsDto")


-- | Organization team inbox
data OrganizationInboxProjection = OrganizationInboxProjection
  { organizationInboxProjectionId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , organizationInboxProjectionDomainId :: Maybe UUID -- ^ ID of custom domain used by the inbox if any
  , organizationInboxProjectionCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , organizationInboxProjectionName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , organizationInboxProjectionEmailAddress :: Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , organizationInboxProjectionFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , organizationInboxProjectionTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , organizationInboxProjectionTeamAccess :: Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  , organizationInboxProjectionInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , organizationInboxProjectionReadOnly :: Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  , organizationInboxProjectionVirtualInbox :: Bool -- ^ Virtual inbox can receive email but will not send emails to real recipients. Will save sent email record but never send an actual email. Perfect for testing mail server actions.
  , organizationInboxProjectionFunctionsAs :: Maybe Text -- ^ Inbox function if used as a primitive for another system.
  , organizationInboxProjectionUserId :: UUID -- ^ ID of user that inbox belongs to
  , organizationInboxProjectionDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , organizationInboxProjectionExpiresAt :: Maybe UTCTime -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , organizationInboxProjectionAccountRegion :: Maybe Text -- ^ Region of the inbox
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "organizationInboxProjection")
instance ToJSON OrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "organizationInboxProjection")


-- | Paginated email alias results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAlias = PageAlias
  { pageAliasContent :: Maybe [AliasProjection] -- ^ 
  , pageAliasPageable :: Maybe PageableObject -- ^ 
  , pageAliasTotalElements :: Integer -- ^ 
  , pageAliasTotalPages :: Int -- ^ 
  , pageAliasLast :: Maybe Bool -- ^ 
  , pageAliasNumberOfElements :: Maybe Int -- ^ 
  , pageAliasFirst :: Maybe Bool -- ^ 
  , pageAliasSize :: Maybe Int -- ^ 
  , pageAliasNumber :: Maybe Int -- ^ 
  , pageAliasSort :: Maybe SortObject -- ^ 
  , pageAliasEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAlias where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAlias")
instance ToJSON PageAlias where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAlias")


-- | Paginated alias thread projection results.
data PageAliasThreadProjection = PageAliasThreadProjection
  { pageAliasThreadProjectionContent :: Maybe [AliasThreadProjection] -- ^ 
  , pageAliasThreadProjectionPageable :: Maybe PageableObject -- ^ 
  , pageAliasThreadProjectionTotalElements :: Integer -- ^ 
  , pageAliasThreadProjectionTotalPages :: Int -- ^ 
  , pageAliasThreadProjectionLast :: Maybe Bool -- ^ 
  , pageAliasThreadProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageAliasThreadProjectionFirst :: Maybe Bool -- ^ 
  , pageAliasThreadProjectionSize :: Maybe Int -- ^ 
  , pageAliasThreadProjectionNumber :: Maybe Int -- ^ 
  , pageAliasThreadProjectionSort :: Maybe SortObject -- ^ 
  , pageAliasThreadProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAliasThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAliasThreadProjection")
instance ToJSON PageAliasThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAliasThreadProjection")


-- | Paginated attachment entity results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAttachmentEntity = PageAttachmentEntity
  { pageAttachmentEntityContent :: Maybe [AttachmentProjection] -- ^ 
  , pageAttachmentEntityPageable :: Maybe PageableObject -- ^ 
  , pageAttachmentEntityTotalElements :: Integer -- ^ 
  , pageAttachmentEntityTotalPages :: Int -- ^ 
  , pageAttachmentEntityLast :: Maybe Bool -- ^ 
  , pageAttachmentEntityNumberOfElements :: Maybe Int -- ^ 
  , pageAttachmentEntityFirst :: Maybe Bool -- ^ 
  , pageAttachmentEntitySize :: Maybe Int -- ^ 
  , pageAttachmentEntityNumber :: Maybe Int -- ^ 
  , pageAttachmentEntitySort :: Maybe SortObject -- ^ 
  , pageAttachmentEntityEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAttachmentEntity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAttachmentEntity")
instance ToJSON PageAttachmentEntity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAttachmentEntity")


-- | Paginated bounced email. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageBouncedEmail = PageBouncedEmail
  { pageBouncedEmailContent :: Maybe [BounceProjection] -- ^ 
  , pageBouncedEmailPageable :: Maybe PageableObject -- ^ 
  , pageBouncedEmailTotalElements :: Integer -- ^ 
  , pageBouncedEmailTotalPages :: Int -- ^ 
  , pageBouncedEmailLast :: Maybe Bool -- ^ 
  , pageBouncedEmailNumberOfElements :: Maybe Int -- ^ 
  , pageBouncedEmailFirst :: Maybe Bool -- ^ 
  , pageBouncedEmailSize :: Maybe Int -- ^ 
  , pageBouncedEmailNumber :: Maybe Int -- ^ 
  , pageBouncedEmailSort :: Maybe SortObject -- ^ 
  , pageBouncedEmailEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageBouncedEmail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageBouncedEmail")
instance ToJSON PageBouncedEmail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageBouncedEmail")


-- | Paginated bounced recipients. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageBouncedRecipients = PageBouncedRecipients
  { pageBouncedRecipientsContent :: Maybe [BounceRecipientProjection] -- ^ 
  , pageBouncedRecipientsPageable :: Maybe PageableObject -- ^ 
  , pageBouncedRecipientsTotalElements :: Integer -- ^ 
  , pageBouncedRecipientsTotalPages :: Int -- ^ 
  , pageBouncedRecipientsLast :: Maybe Bool -- ^ 
  , pageBouncedRecipientsNumberOfElements :: Maybe Int -- ^ 
  , pageBouncedRecipientsFirst :: Maybe Bool -- ^ 
  , pageBouncedRecipientsSize :: Maybe Int -- ^ 
  , pageBouncedRecipientsNumber :: Maybe Int -- ^ 
  , pageBouncedRecipientsSort :: Maybe SortObject -- ^ 
  , pageBouncedRecipientsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageBouncedRecipients where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageBouncedRecipients")
instance ToJSON PageBouncedRecipients where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageBouncedRecipients")


-- | Paginated complaint email. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageComplaint = PageComplaint
  { pageComplaintContent :: Maybe [Complaint] -- ^ 
  , pageComplaintPageable :: Maybe PageableObject -- ^ 
  , pageComplaintTotalElements :: Integer -- ^ 
  , pageComplaintTotalPages :: Int -- ^ 
  , pageComplaintLast :: Maybe Bool -- ^ 
  , pageComplaintNumberOfElements :: Maybe Int -- ^ 
  , pageComplaintFirst :: Maybe Bool -- ^ 
  , pageComplaintSize :: Maybe Int -- ^ 
  , pageComplaintNumber :: Maybe Int -- ^ 
  , pageComplaintSort :: Maybe SortObject -- ^ 
  , pageComplaintEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageComplaint where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageComplaint")
instance ToJSON PageComplaint where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageComplaint")


-- | Paginated inbox connectors. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageConnector = PageConnector
  { pageConnectorContent :: Maybe [ConnectorProjection] -- ^ 
  , pageConnectorPageable :: Maybe PageableObject -- ^ 
  , pageConnectorTotalElements :: Integer -- ^ 
  , pageConnectorTotalPages :: Int -- ^ 
  , pageConnectorLast :: Maybe Bool -- ^ 
  , pageConnectorNumberOfElements :: Maybe Int -- ^ 
  , pageConnectorFirst :: Maybe Bool -- ^ 
  , pageConnectorSize :: Maybe Int -- ^ 
  , pageConnectorNumber :: Maybe Int -- ^ 
  , pageConnectorSort :: Maybe SortObject -- ^ 
  , pageConnectorEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageConnector where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageConnector")
instance ToJSON PageConnector where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageConnector")


-- | Paginated inbox connector events. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageConnectorEvents = PageConnectorEvents
  { pageConnectorEventsContent :: Maybe [ConnectorEventProjection] -- ^ 
  , pageConnectorEventsPageable :: Maybe PageableObject -- ^ 
  , pageConnectorEventsTotalElements :: Integer -- ^ 
  , pageConnectorEventsTotalPages :: Int -- ^ 
  , pageConnectorEventsLast :: Maybe Bool -- ^ 
  , pageConnectorEventsNumberOfElements :: Maybe Int -- ^ 
  , pageConnectorEventsFirst :: Maybe Bool -- ^ 
  , pageConnectorEventsSize :: Maybe Int -- ^ 
  , pageConnectorEventsNumber :: Maybe Int -- ^ 
  , pageConnectorEventsSort :: Maybe SortObject -- ^ 
  , pageConnectorEventsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageConnectorEvents where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageConnectorEvents")
instance ToJSON PageConnectorEvents where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageConnectorEvents")


-- | Paginated contact results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageContactProjection = PageContactProjection
  { pageContactProjectionContent :: Maybe [ContactProjection] -- ^ 
  , pageContactProjectionPageable :: Maybe PageableObject -- ^ 
  , pageContactProjectionTotalElements :: Integer -- ^ 
  , pageContactProjectionTotalPages :: Int -- ^ 
  , pageContactProjectionLast :: Maybe Bool -- ^ 
  , pageContactProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageContactProjectionFirst :: Maybe Bool -- ^ 
  , pageContactProjectionSize :: Maybe Int -- ^ 
  , pageContactProjectionNumber :: Maybe Int -- ^ 
  , pageContactProjectionSort :: Maybe SortObject -- ^ 
  , pageContactProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageContactProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageContactProjection")
instance ToJSON PageContactProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageContactProjection")


-- | Paginated delivery status results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageDeliveryStatus = PageDeliveryStatus
  { pageDeliveryStatusContent :: Maybe [DeliveryStatusDto] -- ^ 
  , pageDeliveryStatusPageable :: Maybe PageableObject -- ^ 
  , pageDeliveryStatusTotalElements :: Integer -- ^ 
  , pageDeliveryStatusTotalPages :: Int -- ^ 
  , pageDeliveryStatusLast :: Maybe Bool -- ^ 
  , pageDeliveryStatusNumberOfElements :: Maybe Int -- ^ 
  , pageDeliveryStatusFirst :: Maybe Bool -- ^ 
  , pageDeliveryStatusSize :: Maybe Int -- ^ 
  , pageDeliveryStatusNumber :: Maybe Int -- ^ 
  , pageDeliveryStatusSort :: Maybe SortObject -- ^ 
  , pageDeliveryStatusEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageDeliveryStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageDeliveryStatus")
instance ToJSON PageDeliveryStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageDeliveryStatus")


-- | Paginated email preview results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageEmailPreview = PageEmailPreview
  { pageEmailPreviewContent :: Maybe [EmailPreview] -- ^ 
  , pageEmailPreviewPageable :: Maybe PageableObject -- ^ 
  , pageEmailPreviewTotalElements :: Integer -- ^ 
  , pageEmailPreviewTotalPages :: Int -- ^ 
  , pageEmailPreviewLast :: Maybe Bool -- ^ 
  , pageEmailPreviewNumberOfElements :: Maybe Int -- ^ 
  , pageEmailPreviewFirst :: Maybe Bool -- ^ 
  , pageEmailPreviewSize :: Maybe Int -- ^ 
  , pageEmailPreviewNumber :: Maybe Int -- ^ 
  , pageEmailPreviewSort :: Maybe SortObject -- ^ 
  , pageEmailPreviewEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailPreview")
instance ToJSON PageEmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailPreview")


-- | Paginated email projection results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full email entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageEmailProjection = PageEmailProjection
  { pageEmailProjectionContent :: Maybe [EmailProjection] -- ^ 
  , pageEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageEmailProjectionTotalElements :: Integer -- ^ 
  , pageEmailProjectionTotalPages :: Int -- ^ 
  , pageEmailProjectionLast :: Maybe Bool -- ^ 
  , pageEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageEmailProjectionSize :: Maybe Int -- ^ 
  , pageEmailProjectionNumber :: Maybe Int -- ^ 
  , pageEmailProjectionSort :: Maybe SortObject -- ^ 
  , pageEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailProjection")
instance ToJSON PageEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailProjection")


-- | Paginated email thread projection results.
data PageEmailThreadProjection = PageEmailThreadProjection
  { pageEmailThreadProjectionContent :: Maybe [EmailThreadProjection] -- ^ 
  , pageEmailThreadProjectionPageable :: Maybe PageableObject -- ^ 
  , pageEmailThreadProjectionTotalElements :: Integer -- ^ 
  , pageEmailThreadProjectionTotalPages :: Int -- ^ 
  , pageEmailThreadProjectionLast :: Maybe Bool -- ^ 
  , pageEmailThreadProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageEmailThreadProjectionFirst :: Maybe Bool -- ^ 
  , pageEmailThreadProjectionSize :: Maybe Int -- ^ 
  , pageEmailThreadProjectionNumber :: Maybe Int -- ^ 
  , pageEmailThreadProjectionSort :: Maybe SortObject -- ^ 
  , pageEmailThreadProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailThreadProjection")
instance ToJSON PageEmailThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailThreadProjection")


-- | Paginated email validation request records. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageEmailValidationRequest = PageEmailValidationRequest
  { pageEmailValidationRequestContent :: Maybe [EmailValidationRequestDto] -- ^ 
  , pageEmailValidationRequestPageable :: Maybe PageableObject -- ^ 
  , pageEmailValidationRequestTotalElements :: Integer -- ^ 
  , pageEmailValidationRequestTotalPages :: Int -- ^ 
  , pageEmailValidationRequestLast :: Maybe Bool -- ^ 
  , pageEmailValidationRequestNumberOfElements :: Maybe Int -- ^ 
  , pageEmailValidationRequestFirst :: Maybe Bool -- ^ 
  , pageEmailValidationRequestSize :: Maybe Int -- ^ 
  , pageEmailValidationRequestNumber :: Maybe Int -- ^ 
  , pageEmailValidationRequestSort :: Maybe SortObject -- ^ 
  , pageEmailValidationRequestEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailValidationRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailValidationRequest")
instance ToJSON PageEmailValidationRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailValidationRequest")


-- | Paginated automation items like auto-repliers, forwarders, and rulesets. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageEntityAutomationItems = PageEntityAutomationItems
  { pageEntityAutomationItemsContent :: Maybe [EntityAutomationItemProjection] -- ^ 
  , pageEntityAutomationItemsPageable :: Maybe PageableObject -- ^ 
  , pageEntityAutomationItemsTotalElements :: Integer -- ^ 
  , pageEntityAutomationItemsTotalPages :: Int -- ^ 
  , pageEntityAutomationItemsLast :: Maybe Bool -- ^ 
  , pageEntityAutomationItemsNumberOfElements :: Maybe Int -- ^ 
  , pageEntityAutomationItemsFirst :: Maybe Bool -- ^ 
  , pageEntityAutomationItemsSize :: Maybe Int -- ^ 
  , pageEntityAutomationItemsNumber :: Maybe Int -- ^ 
  , pageEntityAutomationItemsSort :: Maybe SortObject -- ^ 
  , pageEntityAutomationItemsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEntityAutomationItems where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEntityAutomationItems")
instance ToJSON PageEntityAutomationItems where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEntityAutomationItems")


-- | Paginated event items like webhook events and forwarding. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageEntityEventItems = PageEntityEventItems
  { pageEntityEventItemsContent :: Maybe [EntityEventItemProjection] -- ^ 
  , pageEntityEventItemsPageable :: Maybe PageableObject -- ^ 
  , pageEntityEventItemsTotalElements :: Integer -- ^ 
  , pageEntityEventItemsTotalPages :: Int -- ^ 
  , pageEntityEventItemsLast :: Maybe Bool -- ^ 
  , pageEntityEventItemsNumberOfElements :: Maybe Int -- ^ 
  , pageEntityEventItemsFirst :: Maybe Bool -- ^ 
  , pageEntityEventItemsSize :: Maybe Int -- ^ 
  , pageEntityEventItemsNumber :: Maybe Int -- ^ 
  , pageEntityEventItemsSort :: Maybe SortObject -- ^ 
  , pageEntityEventItemsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEntityEventItems where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEntityEventItems")
instance ToJSON PageEntityEventItems where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEntityEventItems")


-- | Paginated favourite items like inboxes, phones, sms, emails. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageEntityFavouriteItems = PageEntityFavouriteItems
  { pageEntityFavouriteItemsContent :: Maybe [EntityFavouriteItemProjection] -- ^ 
  , pageEntityFavouriteItemsPageable :: Maybe PageableObject -- ^ 
  , pageEntityFavouriteItemsTotalElements :: Integer -- ^ 
  , pageEntityFavouriteItemsTotalPages :: Int -- ^ 
  , pageEntityFavouriteItemsLast :: Maybe Bool -- ^ 
  , pageEntityFavouriteItemsNumberOfElements :: Maybe Int -- ^ 
  , pageEntityFavouriteItemsFirst :: Maybe Bool -- ^ 
  , pageEntityFavouriteItemsSize :: Maybe Int -- ^ 
  , pageEntityFavouriteItemsNumber :: Maybe Int -- ^ 
  , pageEntityFavouriteItemsSort :: Maybe SortObject -- ^ 
  , pageEntityFavouriteItemsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEntityFavouriteItems where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEntityFavouriteItems")
instance ToJSON PageEntityFavouriteItems where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEntityFavouriteItems")


-- | Paginated expired inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageExpiredInboxRecordProjection = PageExpiredInboxRecordProjection
  { pageExpiredInboxRecordProjectionContent :: Maybe [ExpiredInboxRecordProjection] -- ^ 
  , pageExpiredInboxRecordProjectionPageable :: Maybe PageableObject -- ^ 
  , pageExpiredInboxRecordProjectionTotalElements :: Integer -- ^ 
  , pageExpiredInboxRecordProjectionTotalPages :: Int -- ^ 
  , pageExpiredInboxRecordProjectionLast :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionFirst :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionSize :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionNumber :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionSort :: Maybe SortObject -- ^ 
  , pageExpiredInboxRecordProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageExpiredInboxRecordProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageExpiredInboxRecordProjection")
instance ToJSON PageExpiredInboxRecordProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageExpiredInboxRecordProjection")


-- | Paginated missed email results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageGroupProjection = PageGroupProjection
  { pageGroupProjectionContent :: Maybe [GroupProjection] -- ^ 
  , pageGroupProjectionPageable :: Maybe PageableObject -- ^ 
  , pageGroupProjectionTotalElements :: Integer -- ^ 
  , pageGroupProjectionTotalPages :: Int -- ^ 
  , pageGroupProjectionLast :: Maybe Bool -- ^ 
  , pageGroupProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageGroupProjectionFirst :: Maybe Bool -- ^ 
  , pageGroupProjectionSize :: Maybe Int -- ^ 
  , pageGroupProjectionNumber :: Maybe Int -- ^ 
  , pageGroupProjectionSort :: Maybe SortObject -- ^ 
  , pageGroupProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageGroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageGroupProjection")
instance ToJSON PageGroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageGroupProjection")


-- | Paginated guest portal users
data PageGuestPortalUsers = PageGuestPortalUsers
  { pageGuestPortalUsersContent :: Maybe [GuestPortalUserProjection] -- ^ 
  , pageGuestPortalUsersPageable :: Maybe PageableObject -- ^ 
  , pageGuestPortalUsersTotalElements :: Integer -- ^ 
  , pageGuestPortalUsersTotalPages :: Int -- ^ 
  , pageGuestPortalUsersLast :: Maybe Bool -- ^ 
  , pageGuestPortalUsersNumberOfElements :: Maybe Int -- ^ 
  , pageGuestPortalUsersFirst :: Maybe Bool -- ^ 
  , pageGuestPortalUsersSize :: Maybe Int -- ^ 
  , pageGuestPortalUsersNumber :: Maybe Int -- ^ 
  , pageGuestPortalUsersSort :: Maybe SortObject -- ^ 
  , pageGuestPortalUsersEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageGuestPortalUsers where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageGuestPortalUsers")
instance ToJSON PageGuestPortalUsers where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageGuestPortalUsers")


-- | Paginated inbox forwarder results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxForwarderDto = PageInboxForwarderDto
  { pageInboxForwarderDtoContent :: Maybe [InboxForwarderDto] -- ^ 
  , pageInboxForwarderDtoPageable :: Maybe PageableObject -- ^ 
  , pageInboxForwarderDtoTotalElements :: Integer -- ^ 
  , pageInboxForwarderDtoTotalPages :: Int -- ^ 
  , pageInboxForwarderDtoLast :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxForwarderDtoFirst :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoSize :: Maybe Int -- ^ 
  , pageInboxForwarderDtoNumber :: Maybe Int -- ^ 
  , pageInboxForwarderDtoSort :: Maybe SortObject -- ^ 
  , pageInboxForwarderDtoEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxForwarderDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxForwarderDto")
instance ToJSON PageInboxForwarderDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxForwarderDto")


-- | Paginated inbox forwarder events. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxForwarderEvents = PageInboxForwarderEvents
  { pageInboxForwarderEventsContent :: Maybe [InboxForwarderEventProjection] -- ^ 
  , pageInboxForwarderEventsPageable :: Maybe PageableObject -- ^ 
  , pageInboxForwarderEventsTotalElements :: Integer -- ^ 
  , pageInboxForwarderEventsTotalPages :: Int -- ^ 
  , pageInboxForwarderEventsLast :: Maybe Bool -- ^ 
  , pageInboxForwarderEventsNumberOfElements :: Maybe Int -- ^ 
  , pageInboxForwarderEventsFirst :: Maybe Bool -- ^ 
  , pageInboxForwarderEventsSize :: Maybe Int -- ^ 
  , pageInboxForwarderEventsNumber :: Maybe Int -- ^ 
  , pageInboxForwarderEventsSort :: Maybe SortObject -- ^ 
  , pageInboxForwarderEventsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxForwarderEvents where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxForwarderEvents")
instance ToJSON PageInboxForwarderEvents where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxForwarderEvents")


-- | Paginated inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxProjection = PageInboxProjection
  { pageInboxProjectionContent :: Maybe [InboxPreview] -- ^ 
  , pageInboxProjectionPageable :: Maybe PageableObject -- ^ 
  , pageInboxProjectionTotalElements :: Integer -- ^ 
  , pageInboxProjectionTotalPages :: Int -- ^ 
  , pageInboxProjectionLast :: Maybe Bool -- ^ 
  , pageInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageInboxProjectionSize :: Maybe Int -- ^ 
  , pageInboxProjectionNumber :: Maybe Int -- ^ 
  , pageInboxProjectionSort :: Maybe SortObject -- ^ 
  , pageInboxProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxProjection")
instance ToJSON PageInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxProjection")


-- | Paginated inbox replier results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxReplierDto = PageInboxReplierDto
  { pageInboxReplierDtoContent :: Maybe [InboxReplierDto] -- ^ 
  , pageInboxReplierDtoPageable :: Maybe PageableObject -- ^ 
  , pageInboxReplierDtoTotalElements :: Integer -- ^ 
  , pageInboxReplierDtoTotalPages :: Int -- ^ 
  , pageInboxReplierDtoLast :: Maybe Bool -- ^ 
  , pageInboxReplierDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxReplierDtoFirst :: Maybe Bool -- ^ 
  , pageInboxReplierDtoSize :: Maybe Int -- ^ 
  , pageInboxReplierDtoNumber :: Maybe Int -- ^ 
  , pageInboxReplierDtoSort :: Maybe SortObject -- ^ 
  , pageInboxReplierDtoEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxReplierDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxReplierDto")
instance ToJSON PageInboxReplierDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxReplierDto")


-- | Paginated inbox replier events. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxReplierEvents = PageInboxReplierEvents
  { pageInboxReplierEventsContent :: Maybe [InboxReplierEventProjection] -- ^ 
  , pageInboxReplierEventsPageable :: Maybe PageableObject -- ^ 
  , pageInboxReplierEventsTotalElements :: Integer -- ^ 
  , pageInboxReplierEventsTotalPages :: Int -- ^ 
  , pageInboxReplierEventsLast :: Maybe Bool -- ^ 
  , pageInboxReplierEventsNumberOfElements :: Maybe Int -- ^ 
  , pageInboxReplierEventsFirst :: Maybe Bool -- ^ 
  , pageInboxReplierEventsSize :: Maybe Int -- ^ 
  , pageInboxReplierEventsNumber :: Maybe Int -- ^ 
  , pageInboxReplierEventsSort :: Maybe SortObject -- ^ 
  , pageInboxReplierEventsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxReplierEvents where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxReplierEvents")
instance ToJSON PageInboxReplierEvents where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxReplierEvents")


-- | Paginated inbox ruleset results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxRulesetDto = PageInboxRulesetDto
  { pageInboxRulesetDtoContent :: Maybe [InboxRulesetDto] -- ^ 
  , pageInboxRulesetDtoPageable :: Maybe PageableObject -- ^ 
  , pageInboxRulesetDtoTotalElements :: Integer -- ^ 
  , pageInboxRulesetDtoTotalPages :: Int -- ^ 
  , pageInboxRulesetDtoLast :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxRulesetDtoFirst :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoSize :: Maybe Int -- ^ 
  , pageInboxRulesetDtoNumber :: Maybe Int -- ^ 
  , pageInboxRulesetDtoSort :: Maybe SortObject -- ^ 
  , pageInboxRulesetDtoEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxRulesetDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxRulesetDto")
instance ToJSON PageInboxRulesetDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxRulesetDto")


-- | Paginated inbox tags. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxTags = PageInboxTags
  { pageInboxTagsContent :: Maybe [Text] -- ^ 
  , pageInboxTagsPageable :: Maybe PageableObject -- ^ 
  , pageInboxTagsTotalElements :: Integer -- ^ 
  , pageInboxTagsTotalPages :: Int -- ^ 
  , pageInboxTagsLast :: Maybe Bool -- ^ 
  , pageInboxTagsNumberOfElements :: Maybe Int -- ^ 
  , pageInboxTagsFirst :: Maybe Bool -- ^ 
  , pageInboxTagsSize :: Maybe Int -- ^ 
  , pageInboxTagsNumber :: Maybe Int -- ^ 
  , pageInboxTagsSort :: Maybe SortObject -- ^ 
  , pageInboxTagsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxTags where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxTags")
instance ToJSON PageInboxTags where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxTags")


-- | Paginated list unsubscribe recipients. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageListUnsubscribeRecipients = PageListUnsubscribeRecipients
  { pageListUnsubscribeRecipientsContent :: Maybe [ListUnsubscribeRecipientProjection] -- ^ 
  , pageListUnsubscribeRecipientsPageable :: Maybe PageableObject -- ^ 
  , pageListUnsubscribeRecipientsTotalElements :: Integer -- ^ 
  , pageListUnsubscribeRecipientsTotalPages :: Int -- ^ 
  , pageListUnsubscribeRecipientsLast :: Maybe Bool -- ^ 
  , pageListUnsubscribeRecipientsNumberOfElements :: Maybe Int -- ^ 
  , pageListUnsubscribeRecipientsFirst :: Maybe Bool -- ^ 
  , pageListUnsubscribeRecipientsSize :: Maybe Int -- ^ 
  , pageListUnsubscribeRecipientsNumber :: Maybe Int -- ^ 
  , pageListUnsubscribeRecipientsSort :: Maybe SortObject -- ^ 
  , pageListUnsubscribeRecipientsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageListUnsubscribeRecipients where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageListUnsubscribeRecipients")
instance ToJSON PageListUnsubscribeRecipients where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageListUnsubscribeRecipients")


-- | Paginated MissedEmail results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageMissedEmailProjection = PageMissedEmailProjection
  { pageMissedEmailProjectionContent :: Maybe [MissedEmailProjection] -- ^ 
  , pageMissedEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageMissedEmailProjectionTotalElements :: Integer -- ^ 
  , pageMissedEmailProjectionTotalPages :: Int -- ^ 
  , pageMissedEmailProjectionLast :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageMissedEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionSize :: Maybe Int -- ^ 
  , pageMissedEmailProjectionNumber :: Maybe Int -- ^ 
  , pageMissedEmailProjectionSort :: Maybe SortObject -- ^ 
  , pageMissedEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageMissedEmailProjection")
instance ToJSON PageMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageMissedEmailProjection")


-- | Paginated opt in identity projections reflecting users who have verified double-opt in consent to receive emails from your account.
data PageOptInIdentityProjection = PageOptInIdentityProjection
  { pageOptInIdentityProjectionContent :: Maybe [OptInIdentityProjection] -- ^ 
  , pageOptInIdentityProjectionPageable :: Maybe PageableObject -- ^ 
  , pageOptInIdentityProjectionTotalElements :: Integer -- ^ 
  , pageOptInIdentityProjectionTotalPages :: Int -- ^ 
  , pageOptInIdentityProjectionLast :: Maybe Bool -- ^ 
  , pageOptInIdentityProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageOptInIdentityProjectionFirst :: Maybe Bool -- ^ 
  , pageOptInIdentityProjectionSize :: Maybe Int -- ^ 
  , pageOptInIdentityProjectionNumber :: Maybe Int -- ^ 
  , pageOptInIdentityProjectionSort :: Maybe SortObject -- ^ 
  , pageOptInIdentityProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageOptInIdentityProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageOptInIdentityProjection")
instance ToJSON PageOptInIdentityProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageOptInIdentityProjection")


-- | Paginated organization inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageOrganizationInboxProjection = PageOrganizationInboxProjection
  { pageOrganizationInboxProjectionContent :: Maybe [OrganizationInboxProjection] -- ^ 
  , pageOrganizationInboxProjectionPageable :: Maybe PageableObject -- ^ 
  , pageOrganizationInboxProjectionTotalElements :: Integer -- ^ 
  , pageOrganizationInboxProjectionTotalPages :: Int -- ^ 
  , pageOrganizationInboxProjectionLast :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionSize :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionNumber :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionSort :: Maybe SortObject -- ^ 
  , pageOrganizationInboxProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageOrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageOrganizationInboxProjection")
instance ToJSON PageOrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageOrganizationInboxProjection")


-- | Paginated phone message thread items. These are messages in a phone thread. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PagePhoneMessageThreadItemProjection = PagePhoneMessageThreadItemProjection
  { pagePhoneMessageThreadItemProjectionContent :: Maybe [PhoneMessageThreadItemProjection] -- ^ 
  , pagePhoneMessageThreadItemProjectionPageable :: Maybe PageableObject -- ^ 
  , pagePhoneMessageThreadItemProjectionTotalElements :: Integer -- ^ 
  , pagePhoneMessageThreadItemProjectionTotalPages :: Int -- ^ 
  , pagePhoneMessageThreadItemProjectionLast :: Maybe Bool -- ^ 
  , pagePhoneMessageThreadItemProjectionNumberOfElements :: Maybe Int -- ^ 
  , pagePhoneMessageThreadItemProjectionFirst :: Maybe Bool -- ^ 
  , pagePhoneMessageThreadItemProjectionSize :: Maybe Int -- ^ 
  , pagePhoneMessageThreadItemProjectionNumber :: Maybe Int -- ^ 
  , pagePhoneMessageThreadItemProjectionSort :: Maybe SortObject -- ^ 
  , pagePhoneMessageThreadItemProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PagePhoneMessageThreadItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagePhoneMessageThreadItemProjection")
instance ToJSON PagePhoneMessageThreadItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagePhoneMessageThreadItemProjection")


-- | Paginated phone message threads. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PagePhoneMessageThreadProjection = PagePhoneMessageThreadProjection
  { pagePhoneMessageThreadProjectionContent :: Maybe [PhoneMessageThreadProjection] -- ^ 
  , pagePhoneMessageThreadProjectionPageable :: Maybe PageableObject -- ^ 
  , pagePhoneMessageThreadProjectionTotalElements :: Integer -- ^ 
  , pagePhoneMessageThreadProjectionTotalPages :: Int -- ^ 
  , pagePhoneMessageThreadProjectionLast :: Maybe Bool -- ^ 
  , pagePhoneMessageThreadProjectionNumberOfElements :: Maybe Int -- ^ 
  , pagePhoneMessageThreadProjectionFirst :: Maybe Bool -- ^ 
  , pagePhoneMessageThreadProjectionSize :: Maybe Int -- ^ 
  , pagePhoneMessageThreadProjectionNumber :: Maybe Int -- ^ 
  , pagePhoneMessageThreadProjectionSort :: Maybe SortObject -- ^ 
  , pagePhoneMessageThreadProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PagePhoneMessageThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagePhoneMessageThreadProjection")
instance ToJSON PagePhoneMessageThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagePhoneMessageThreadProjection")


-- | Paginated phone numbers. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PagePhoneNumberProjection = PagePhoneNumberProjection
  { pagePhoneNumberProjectionContent :: Maybe [PhoneNumberProjection] -- ^ 
  , pagePhoneNumberProjectionPageable :: Maybe PageableObject -- ^ 
  , pagePhoneNumberProjectionTotalElements :: Integer -- ^ 
  , pagePhoneNumberProjectionTotalPages :: Int -- ^ 
  , pagePhoneNumberProjectionLast :: Maybe Bool -- ^ 
  , pagePhoneNumberProjectionNumberOfElements :: Maybe Int -- ^ 
  , pagePhoneNumberProjectionFirst :: Maybe Bool -- ^ 
  , pagePhoneNumberProjectionSize :: Maybe Int -- ^ 
  , pagePhoneNumberProjectionNumber :: Maybe Int -- ^ 
  , pagePhoneNumberProjectionSort :: Maybe SortObject -- ^ 
  , pagePhoneNumberProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PagePhoneNumberProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagePhoneNumberProjection")
instance ToJSON PagePhoneNumberProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagePhoneNumberProjection")


-- | Paginated inbox plus addresses. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PagePlusAddressProjection = PagePlusAddressProjection
  { pagePlusAddressProjectionContent :: Maybe [PlusAddressProjection] -- ^ 
  , pagePlusAddressProjectionPageable :: Maybe PageableObject -- ^ 
  , pagePlusAddressProjectionTotalElements :: Integer -- ^ 
  , pagePlusAddressProjectionTotalPages :: Int -- ^ 
  , pagePlusAddressProjectionLast :: Maybe Bool -- ^ 
  , pagePlusAddressProjectionNumberOfElements :: Maybe Int -- ^ 
  , pagePlusAddressProjectionFirst :: Maybe Bool -- ^ 
  , pagePlusAddressProjectionSize :: Maybe Int -- ^ 
  , pagePlusAddressProjectionNumber :: Maybe Int -- ^ 
  , pagePlusAddressProjectionSort :: Maybe SortObject -- ^ 
  , pagePlusAddressProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PagePlusAddressProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pagePlusAddressProjection")
instance ToJSON PagePlusAddressProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pagePlusAddressProjection")


-- | Paginated reputation items like complaints and bounces. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageReputationItems = PageReputationItems
  { pageReputationItemsContent :: Maybe [ReputationItemProjection] -- ^ 
  , pageReputationItemsPageable :: Maybe PageableObject -- ^ 
  , pageReputationItemsTotalElements :: Integer -- ^ 
  , pageReputationItemsTotalPages :: Int -- ^ 
  , pageReputationItemsLast :: Maybe Bool -- ^ 
  , pageReputationItemsNumberOfElements :: Maybe Int -- ^ 
  , pageReputationItemsFirst :: Maybe Bool -- ^ 
  , pageReputationItemsSize :: Maybe Int -- ^ 
  , pageReputationItemsNumber :: Maybe Int -- ^ 
  , pageReputationItemsSort :: Maybe SortObject -- ^ 
  , pageReputationItemsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageReputationItems where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageReputationItems")
instance ToJSON PageReputationItems where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageReputationItems")


-- | Paginated scheduled jobs results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageScheduledJobs = PageScheduledJobs
  { pageScheduledJobsContent :: Maybe [ScheduledJob] -- ^ 
  , pageScheduledJobsPageable :: Maybe PageableObject -- ^ 
  , pageScheduledJobsTotalElements :: Integer -- ^ 
  , pageScheduledJobsTotalPages :: Int -- ^ 
  , pageScheduledJobsLast :: Maybe Bool -- ^ 
  , pageScheduledJobsNumberOfElements :: Maybe Int -- ^ 
  , pageScheduledJobsFirst :: Maybe Bool -- ^ 
  , pageScheduledJobsSize :: Maybe Int -- ^ 
  , pageScheduledJobsNumber :: Maybe Int -- ^ 
  , pageScheduledJobsSort :: Maybe SortObject -- ^ 
  , pageScheduledJobsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageScheduledJobs where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageScheduledJobs")
instance ToJSON PageScheduledJobs where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageScheduledJobs")


-- | Paginated sent email results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full sent email entity use the projection ID with individual method calls.
data PageSentEmailProjection = PageSentEmailProjection
  { pageSentEmailProjectionContent :: Maybe [SentEmailProjection] -- ^ 
  , pageSentEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageSentEmailProjectionTotalElements :: Integer -- ^ 
  , pageSentEmailProjectionTotalPages :: Int -- ^ 
  , pageSentEmailProjectionLast :: Maybe Bool -- ^ 
  , pageSentEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageSentEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageSentEmailProjectionSize :: Maybe Int -- ^ 
  , pageSentEmailProjectionNumber :: Maybe Int -- ^ 
  , pageSentEmailProjectionSort :: Maybe SortObject -- ^ 
  , pageSentEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSentEmailProjection")
instance ToJSON PageSentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSentEmailProjection")


-- | Paginated sent email results for emails sent with queue. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full sent email entity use the projection ID with individual method calls.
data PageSentEmailWithQueueProjection = PageSentEmailWithQueueProjection
  { pageSentEmailWithQueueProjectionContent :: Maybe [SendWithQueueResult] -- ^ 
  , pageSentEmailWithQueueProjectionPageable :: Maybe PageableObject -- ^ 
  , pageSentEmailWithQueueProjectionTotalElements :: Integer -- ^ 
  , pageSentEmailWithQueueProjectionTotalPages :: Int -- ^ 
  , pageSentEmailWithQueueProjectionLast :: Maybe Bool -- ^ 
  , pageSentEmailWithQueueProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageSentEmailWithQueueProjectionFirst :: Maybe Bool -- ^ 
  , pageSentEmailWithQueueProjectionSize :: Maybe Int -- ^ 
  , pageSentEmailWithQueueProjectionNumber :: Maybe Int -- ^ 
  , pageSentEmailWithQueueProjectionSort :: Maybe SortObject -- ^ 
  , pageSentEmailWithQueueProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSentEmailWithQueueProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSentEmailWithQueueProjection")
instance ToJSON PageSentEmailWithQueueProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSentEmailWithQueueProjection")


-- | Paginated sent SMS messages. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageSentSmsProjection = PageSentSmsProjection
  { pageSentSmsProjectionContent :: Maybe [SentSmsProjection] -- ^ 
  , pageSentSmsProjectionPageable :: Maybe PageableObject -- ^ 
  , pageSentSmsProjectionTotalElements :: Integer -- ^ 
  , pageSentSmsProjectionTotalPages :: Int -- ^ 
  , pageSentSmsProjectionLast :: Maybe Bool -- ^ 
  , pageSentSmsProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageSentSmsProjectionFirst :: Maybe Bool -- ^ 
  , pageSentSmsProjectionSize :: Maybe Int -- ^ 
  , pageSentSmsProjectionNumber :: Maybe Int -- ^ 
  , pageSentSmsProjectionSort :: Maybe SortObject -- ^ 
  , pageSentSmsProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSentSmsProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSentSmsProjection")
instance ToJSON PageSentSmsProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSentSmsProjection")


-- | Paginated SMS messages. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageSmsProjection = PageSmsProjection
  { pageSmsProjectionContent :: Maybe [SmsProjection] -- ^ 
  , pageSmsProjectionPageable :: Maybe PageableObject -- ^ 
  , pageSmsProjectionTotalElements :: Integer -- ^ 
  , pageSmsProjectionTotalPages :: Int -- ^ 
  , pageSmsProjectionLast :: Maybe Bool -- ^ 
  , pageSmsProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageSmsProjectionFirst :: Maybe Bool -- ^ 
  , pageSmsProjectionSize :: Maybe Int -- ^ 
  , pageSmsProjectionNumber :: Maybe Int -- ^ 
  , pageSmsProjectionSort :: Maybe SortObject -- ^ 
  , pageSmsProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSmsProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSmsProjection")
instance ToJSON PageSmsProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSmsProjection")


-- | Paginated email template results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTemplateProjection = PageTemplateProjection
  { pageTemplateProjectionContent :: Maybe [TemplateProjection] -- ^ 
  , pageTemplateProjectionPageable :: Maybe PageableObject -- ^ 
  , pageTemplateProjectionTotalElements :: Integer -- ^ 
  , pageTemplateProjectionTotalPages :: Int -- ^ 
  , pageTemplateProjectionLast :: Maybe Bool -- ^ 
  , pageTemplateProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTemplateProjectionFirst :: Maybe Bool -- ^ 
  , pageTemplateProjectionSize :: Maybe Int -- ^ 
  , pageTemplateProjectionNumber :: Maybe Int -- ^ 
  , pageTemplateProjectionSort :: Maybe SortObject -- ^ 
  , pageTemplateProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageTemplateProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageTemplateProjection")
instance ToJSON PageTemplateProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageTemplateProjection")


-- | Paginated TrackingPixel results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTrackingPixelProjection = PageTrackingPixelProjection
  { pageTrackingPixelProjectionContent :: Maybe [TrackingPixelProjection] -- ^ 
  , pageTrackingPixelProjectionPageable :: Maybe PageableObject -- ^ 
  , pageTrackingPixelProjectionTotalElements :: Integer -- ^ 
  , pageTrackingPixelProjectionTotalPages :: Int -- ^ 
  , pageTrackingPixelProjectionLast :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionFirst :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionSize :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionNumber :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionSort :: Maybe SortObject -- ^ 
  , pageTrackingPixelProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageTrackingPixelProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageTrackingPixelProjection")
instance ToJSON PageTrackingPixelProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageTrackingPixelProjection")


-- | Paginated unknown MissedEmail results. Unknown missed emails are emails that were sent to MailSlurp /Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageUnknownMissedEmailProjection = PageUnknownMissedEmailProjection
  { pageUnknownMissedEmailProjectionContent :: Maybe [UnknownMissedEmailProjection] -- ^ 
  , pageUnknownMissedEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageUnknownMissedEmailProjectionTotalElements :: Integer -- ^ 
  , pageUnknownMissedEmailProjectionTotalPages :: Int -- ^ 
  , pageUnknownMissedEmailProjectionLast :: Maybe Bool -- ^ 
  , pageUnknownMissedEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageUnknownMissedEmailProjectionSize :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionNumber :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionSort :: Maybe SortObject -- ^ 
  , pageUnknownMissedEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageUnknownMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageUnknownMissedEmailProjection")
instance ToJSON PageUnknownMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageUnknownMissedEmailProjection")


-- | Paginated webhook endpoint with latest health status. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageWebhookEndpointProjection = PageWebhookEndpointProjection
  { pageWebhookEndpointProjectionContent :: Maybe [WebhookEndpointProjection] -- ^ 
  , pageWebhookEndpointProjectionPageable :: Maybe PageableObject -- ^ 
  , pageWebhookEndpointProjectionTotalElements :: Integer -- ^ 
  , pageWebhookEndpointProjectionTotalPages :: Int -- ^ 
  , pageWebhookEndpointProjectionLast :: Maybe Bool -- ^ 
  , pageWebhookEndpointProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookEndpointProjectionFirst :: Maybe Bool -- ^ 
  , pageWebhookEndpointProjectionSize :: Maybe Int -- ^ 
  , pageWebhookEndpointProjectionNumber :: Maybe Int -- ^ 
  , pageWebhookEndpointProjectionSort :: Maybe SortObject -- ^ 
  , pageWebhookEndpointProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookEndpointProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookEndpointProjection")
instance ToJSON PageWebhookEndpointProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookEndpointProjection")


-- | Paginated webhook entity. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageWebhookProjection = PageWebhookProjection
  { pageWebhookProjectionContent :: Maybe [WebhookProjection] -- ^ 
  , pageWebhookProjectionPageable :: Maybe PageableObject -- ^ 
  , pageWebhookProjectionTotalElements :: Integer -- ^ 
  , pageWebhookProjectionTotalPages :: Int -- ^ 
  , pageWebhookProjectionLast :: Maybe Bool -- ^ 
  , pageWebhookProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookProjectionFirst :: Maybe Bool -- ^ 
  , pageWebhookProjectionSize :: Maybe Int -- ^ 
  , pageWebhookProjectionNumber :: Maybe Int -- ^ 
  , pageWebhookProjectionSort :: Maybe SortObject -- ^ 
  , pageWebhookProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookProjection")
instance ToJSON PageWebhookProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookProjection")


-- | Paginated webhook results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageWebhookResult = PageWebhookResult
  { pageWebhookResultContent :: Maybe [WebhookResultDto] -- ^ 
  , pageWebhookResultPageable :: Maybe PageableObject -- ^ 
  , pageWebhookResultTotalElements :: Integer -- ^ 
  , pageWebhookResultTotalPages :: Int -- ^ 
  , pageWebhookResultLast :: Maybe Bool -- ^ 
  , pageWebhookResultNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookResultFirst :: Maybe Bool -- ^ 
  , pageWebhookResultSize :: Maybe Int -- ^ 
  , pageWebhookResultNumber :: Maybe Int -- ^ 
  , pageWebhookResultSort :: Maybe SortObject -- ^ 
  , pageWebhookResultEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookResult")
instance ToJSON PageWebhookResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookResult")


-- | 
data PageableObject = PageableObject
  { pageableObjectPageSize :: Maybe Int -- ^ 
  , pageableObjectPaged :: Maybe Bool -- ^ 
  , pageableObjectPageNumber :: Maybe Int -- ^ 
  , pageableObjectUnpaged :: Maybe Bool -- ^ 
  , pageableObjectOffset :: Maybe Integer -- ^ 
  , pageableObjectSort :: Maybe SortObject -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageableObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageableObject")
instance ToJSON PageableObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageableObject")


-- | 
data PhoneMessageThreadItemProjection = PhoneMessageThreadItemProjection
  { phoneMessageThreadItemProjectionBody :: Text -- ^ 
  , phoneMessageThreadItemProjectionPhoneNumberId :: UUID -- ^ 
  , phoneMessageThreadItemProjectionCreated :: UTCTime -- ^ 
  , phoneMessageThreadItemProjectionToPhoneNumber :: Text -- ^ 
  , phoneMessageThreadItemProjectionMessageDirection :: Text -- ^ 
  , phoneMessageThreadItemProjectionFromPhoneNumber :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhoneMessageThreadItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phoneMessageThreadItemProjection")
instance ToJSON PhoneMessageThreadItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phoneMessageThreadItemProjection")


-- | 
data PhoneMessageThreadProjection = PhoneMessageThreadProjection
  { phoneMessageThreadProjectionPhoneNumber :: Text -- ^ 
  , phoneMessageThreadProjectionPhoneNumberId :: UUID -- ^ 
  , phoneMessageThreadProjectionOtherPhoneNumber :: Text -- ^ 
  , phoneMessageThreadProjectionLastMessageDirection :: Text -- ^ 
  , phoneMessageThreadProjectionLastBody :: Text -- ^ 
  , phoneMessageThreadProjectionLastCreated :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhoneMessageThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phoneMessageThreadProjection")
instance ToJSON PhoneMessageThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phoneMessageThreadProjection")


-- | 
data PhoneNumberDto = PhoneNumberDto
  { phoneNumberDtoId :: UUID -- ^ 
  , phoneNumberDtoName :: Maybe Text -- ^ 
  , phoneNumberDtoDescription :: Maybe Text -- ^ 
  , phoneNumberDtoUserId :: UUID -- ^ 
  , phoneNumberDtoComplianceAddress :: Maybe UUID -- ^ 
  , phoneNumberDtoEmergencyAddress :: Maybe UUID -- ^ 
  , phoneNumberDtoPhoneNumber :: Text -- ^ 
  , phoneNumberDtoPhoneCountry :: Text -- ^ 
  , phoneNumberDtoPhonePlan :: UUID -- ^ 
  , phoneNumberDtoCreatedAt :: UTCTime -- ^ 
  , phoneNumberDtoUpdatedAt :: UTCTime -- ^ 
  , phoneNumberDtoFavourite :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhoneNumberDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phoneNumberDto")
instance ToJSON PhoneNumberDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phoneNumberDto")


-- | Phone number projection
data PhoneNumberProjection = PhoneNumberProjection
  { phoneNumberProjectionUserId :: UUID -- ^ 
  , phoneNumberProjectionCreatedAt :: UTCTime -- ^ 
  , phoneNumberProjectionPhoneNumber :: Text -- ^ 
  , phoneNumberProjectionPhoneCountry :: Text -- ^ 
  , phoneNumberProjectionName :: Maybe Text -- ^ 
  , phoneNumberProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhoneNumberProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phoneNumberProjection")
instance ToJSON PhoneNumberProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phoneNumberProjection")


-- | 
data PhoneNumberValidationDto = PhoneNumberValidationDto
  { phoneNumberValidationDtoCountryCode :: Maybe Text -- ^ 
  , phoneNumberValidationDtoCountryPrefix :: Maybe Text -- ^ 
  , phoneNumberValidationDtoPhoneNumber :: Text -- ^ 
  , phoneNumberValidationDtoIsValid :: Bool -- ^ 
  , phoneNumberValidationDtoValidationErrors :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhoneNumberValidationDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phoneNumberValidationDto")
instance ToJSON PhoneNumberValidationDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phoneNumberValidationDto")


-- | 
data PhonePlanAvailability = PhonePlanAvailability
  { phonePlanAvailabilityItems :: [PhonePlanAvailabilityItem] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhonePlanAvailability where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phonePlanAvailability")
instance ToJSON PhonePlanAvailability where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phonePlanAvailability")


-- | 
data PhonePlanAvailabilityItem = PhonePlanAvailabilityItem
  { phonePlanAvailabilityItemPhoneCountry :: Text -- ^ 
  , phonePlanAvailabilityItemAvailabilityStatus :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhonePlanAvailabilityItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phonePlanAvailabilityItem")
instance ToJSON PhonePlanAvailabilityItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phonePlanAvailabilityItem")


-- | 
data PhonePlanDto = PhonePlanDto
  { phonePlanDtoId :: UUID -- ^ 
  , phonePlanDtoUserId :: UUID -- ^ 
  , phonePlanDtoPhoneCountry :: Text -- ^ 
  , phonePlanDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PhonePlanDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "phonePlanDto")
instance ToJSON PhonePlanDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "phonePlanDto")


-- | 
data PlusAddressDto = PlusAddressDto
  { plusAddressDtoId :: UUID -- ^ 
  , plusAddressDtoPlusAddress :: Text -- ^ 
  , plusAddressDtoFullAddress :: Text -- ^ 
  , plusAddressDtoUserId :: UUID -- ^ 
  , plusAddressDtoInboxId :: UUID -- ^ 
  , plusAddressDtoCreatedAt :: UTCTime -- ^ 
  , plusAddressDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PlusAddressDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "plusAddressDto")
instance ToJSON PlusAddressDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "plusAddressDto")


-- | 
data PlusAddressProjection = PlusAddressProjection
  { plusAddressProjectionUserId :: UUID -- ^ 
  , plusAddressProjectionInboxId :: UUID -- ^ 
  , plusAddressProjectionUpdatedAt :: UTCTime -- ^ 
  , plusAddressProjectionCreatedAt :: UTCTime -- ^ 
  , plusAddressProjectionPlusAddress :: Text -- ^ 
  , plusAddressProjectionFullAddress :: Text -- ^ 
  , plusAddressProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PlusAddressProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "plusAddressProjection")
instance ToJSON PlusAddressProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "plusAddressProjection")


-- | 
data ProviderSettings = ProviderSettings
  { providerSettingsMailProvider :: Text -- ^ 
  , providerSettingsImapHost :: Text -- ^ 
  , providerSettingsImapPort :: Int -- ^ 
  , providerSettingsImapSsl :: Bool -- ^ 
  , providerSettingsImapStartTls :: Maybe Bool -- ^ 
  , providerSettingsSmtpHost :: Text -- ^ 
  , providerSettingsSmtpPort :: Int -- ^ 
  , providerSettingsSmtpSsl :: Bool -- ^ 
  , providerSettingsSmtpStartTls :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProviderSettings where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "providerSettings")
instance ToJSON ProviderSettings where
  toJSON = genericToJSON (removeFieldLabelPrefix False "providerSettings")


-- | Content in raw format
data RawEmailJson = RawEmailJson
  { rawEmailJsonContent :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RawEmailJson where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "rawEmailJson")
instance ToJSON RawEmailJson where
  toJSON = genericToJSON (removeFieldLabelPrefix False "rawEmailJson")


-- | Email recipient
data Recipient = Recipient
  { recipientRawValue :: Text -- ^ 
  , recipientEmailAddress :: Text -- ^ 
  , recipientName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Recipient where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "recipient")
instance ToJSON Recipient where
  toJSON = genericToJSON (removeFieldLabelPrefix False "recipient")


-- | 
data RecipientProjection = RecipientProjection
  { recipientProjectionEmailAddress :: Text -- ^ 
  , recipientProjectionRawValue :: Text -- ^ 
  , recipientProjectionName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RecipientProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "recipientProjection")
instance ToJSON RecipientProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "recipientProjection")


-- | 
data ReplyForSms = ReplyForSms
  { replyForSmsReply :: Maybe SentSmsDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyForSms where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyForSms")
instance ToJSON ReplyForSms where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyForSms")


-- | Options for replying to an alias email using the alias inbox
data ReplyToAliasEmailOptions = ReplyToAliasEmailOptions
  { replyToAliasEmailOptionsBody :: Text -- ^ Body of the reply email you want to send
  , replyToAliasEmailOptionsIsHTML :: Bool -- ^ Is the reply HTML
  , replyToAliasEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToAliasEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToAliasEmailOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variables if using a template
  , replyToAliasEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToAliasEmailOptionsSendStrategy :: Maybe Text -- ^ How an email should be sent based on its recipients
  , replyToAliasEmailOptionsCustomHeaders :: Maybe (Map.Map String Text) -- ^ Optional custom headers
  , replyToAliasEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  , replyToAliasEmailOptionsHtml :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyToAliasEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyToAliasEmailOptions")
instance ToJSON ReplyToAliasEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyToAliasEmailOptions")


-- | Options for replying to email with API
data ReplyToEmailOptions = ReplyToEmailOptions
  { replyToEmailOptionsBody :: Text -- ^ Body of the reply email you want to send
  , replyToEmailOptionsIsHTML :: Bool -- ^ Is the reply HTML
  , replyToEmailOptionsFrom :: Maybe Text -- ^ The from header that should be used. Optional
  , replyToEmailOptionsReplyTo :: Maybe Text -- ^ The replyTo header that should be used. Optional
  , replyToEmailOptionsCustomHeaders :: Maybe (Map.Map String Text) -- ^ Optional custom headers
  , replyToEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToEmailOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variables if using a template
  , replyToEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToEmailOptionsSendStrategy :: Maybe Text -- ^ How an email should be sent based on its recipients
  , replyToEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  , replyToEmailOptionsHtml :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyToEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyToEmailOptions")
instance ToJSON ReplyToEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyToEmailOptions")


-- | 
data ReputationItemProjection = ReputationItemProjection
  { reputationItemProjectionCreatedAt :: UTCTime -- ^ 
  , reputationItemProjectionRecipient :: Maybe Text -- ^ 
  , reputationItemProjectionReputationType :: Text -- ^ 
  , reputationItemProjectionId :: UUID -- ^ 
  , reputationItemProjectionSeverity :: Text -- ^ 
  , reputationItemProjectionSource :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReputationItemProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reputationItemProjection")
instance ToJSON ReputationItemProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reputationItemProjection")


-- | 
data ScheduledJob = ScheduledJob
  { scheduledJobId :: UUID -- ^ 
  , scheduledJobUserId :: UUID -- ^ 
  , scheduledJobInboxId :: UUID -- ^ 
  , scheduledJobJobId :: Text -- ^ 
  , scheduledJobGroupId :: Text -- ^ 
  , scheduledJobTriggerId :: Text -- ^ 
  , scheduledJobStatus :: Text -- ^ 
  , scheduledJobSendAtTimestamp :: UTCTime -- ^ 
  , scheduledJobCreatedAt :: UTCTime -- ^ 
  , scheduledJobUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ScheduledJob where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduledJob")
instance ToJSON ScheduledJob where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduledJob")


-- | 
data ScheduledJobDto = ScheduledJobDto
  { scheduledJobDtoId :: UUID -- ^ 
  , scheduledJobDtoUserId :: UUID -- ^ 
  , scheduledJobDtoInboxId :: UUID -- ^ 
  , scheduledJobDtoJobId :: Text -- ^ 
  , scheduledJobDtoGroupId :: Text -- ^ 
  , scheduledJobDtoTriggerId :: Text -- ^ 
  , scheduledJobDtoStatus :: Text -- ^ 
  , scheduledJobDtoSendAtTimestamp :: UTCTime -- ^ 
  , scheduledJobDtoCreatedAt :: UTCTime -- ^ 
  , scheduledJobDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ScheduledJobDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduledJobDto")
instance ToJSON ScheduledJobDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduledJobDto")


-- | 
data SearchEmailsOptions = SearchEmailsOptions
  { searchEmailsOptionsInboxIds :: Maybe [UUID] -- ^ Optional inbox ids to filter by. Can be repeated. By default will use all inboxes belonging to your account.
  , searchEmailsOptionsPageIndex :: Maybe Int -- ^ Optional page index in email list pagination
  , searchEmailsOptionsPageSize :: Maybe Int -- ^ Optional page size in email list pagination. Maximum size is 100. Use page index and sort to page through larger results
  , searchEmailsOptionsSortDirection :: Maybe Text -- ^ Optional createdAt sort direction ASC or DESC
  , searchEmailsOptionsUnreadOnly :: Maybe Bool -- ^ Optional filter for unread emails only. All emails are considered unread until they are viewed in the dashboard or requested directly
  , searchEmailsOptionsSearchFilter :: Maybe Text -- ^ Optional search filter. Searches email recipients, sender, subject, email address and ID. Does not search email body
  , searchEmailsOptionsSince :: Maybe UTCTime -- ^ Optional filter emails received after given date time
  , searchEmailsOptionsBefore :: Maybe UTCTime -- ^ Optional filter emails received before given date time
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SearchEmailsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "searchEmailsOptions")
instance ToJSON SearchEmailsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "searchEmailsOptions")


-- | 
data SearchInboxesOptions = SearchInboxesOptions
  { searchInboxesOptionsPageIndex :: Maybe Int -- ^ Optional page index in list pagination
  , searchInboxesOptionsPageSize :: Maybe Int -- ^ Optional page size in list pagination
  , searchInboxesOptionsSortDirection :: Maybe Text -- ^ Optional createdAt sort direction ASC or DESC
  , searchInboxesOptionsFavourite :: Maybe Bool -- ^ Optionally filter results for favourites only
  , searchInboxesOptionsSearch :: Maybe Text -- ^ Optionally filter by search words partial matching ID, tags, name, and email address
  , searchInboxesOptionsTag :: Maybe Text -- ^ Optionally filter by tags. Will return inboxes that include given tags
  , searchInboxesOptionsSince :: Maybe UTCTime -- ^ Optional filter by created after given date time
  , searchInboxesOptionsBefore :: Maybe UTCTime -- ^ Optional filter by created before given date time
  , searchInboxesOptionsInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , searchInboxesOptionsInboxFunction :: Maybe Text -- ^ Optional filter by inbox function
  , searchInboxesOptionsDomainId :: Maybe UUID -- ^ Optional domain ID filter
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SearchInboxesOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "searchInboxesOptions")
instance ToJSON SearchInboxesOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "searchInboxesOptions")


-- | Email body content parts for multipart mime message. Will override body.
data SendEmailBodyPart = SendEmailBodyPart
  { sendEmailBodyPartContentType :: Text -- ^ 
  , sendEmailBodyPartContentBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendEmailBodyPart where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendEmailBodyPart")
instance ToJSON SendEmailBodyPart where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendEmailBodyPart")


-- | Options for the email to be sent
data SendEmailOptions = SendEmailOptions
  { sendEmailOptionsToContacts :: Maybe [UUID] -- ^ Optional list of contact IDs to send email to. Manage your contacts via the API or dashboard. When contacts are used the email is sent to each contact separately so they will not see other recipients.
  , sendEmailOptionsToGroup :: Maybe UUID -- ^ Optional contact group ID to send email to. You can create contacts and contact groups in the API or dashboard and use them for email campaigns. When contact groups are used the email is sent to each contact separately so they will not see other recipients
  , sendEmailOptionsTo :: Maybe [Text] -- ^ List of destination email addresses. Each email address must be RFC 5322 format. Even single recipients must be in array form. Maximum recipients per email depends on your plan. If you need to send many emails try using contacts or contact groups or use a non standard sendStrategy to ensure that spam filters are not triggered (many recipients in one email can affect your spam rating). Be cautious when sending emails that your recipients exist. High bounce rates (meaning a high percentage of emails cannot be delivered because an address does not exist) can result in account freezing.
  , sendEmailOptionsFrom :: Maybe Text -- ^ Optional from address. Email address is RFC 5322 format and may include a display name and email in angle brackets (`my@address.com` or `My inbox <my@address.com>`). If no sender is set the source inbox address will be used for this field. If you set `useInboxName` to `true` the from field will include the inbox name as a display name: `inbox_name <inbox@address.com>`. For this to work use the name field when creating an inbox. Beware of potential spam penalties when setting the from field to an address not used by the inbox. Your emails may get blocked by services if you impersonate another address. To use a custom email addresses use a custom domain. You can create domains with the DomainController. The domain must be verified in the dashboard before it can be used.
  , sendEmailOptionsFromName :: Maybe Text -- ^ Optional from name if not passed with address. If you set `useInboxName` to `true` the from field will include the inbox name as a display name
  , sendEmailOptionsCc :: Maybe [Text] -- ^ Optional list of cc destination email addresses
  , sendEmailOptionsBcc :: Maybe [Text] -- ^ Optional list of bcc destination email addresses
  , sendEmailOptionsSubject :: Maybe Text -- ^ Optional email subject line
  , sendEmailOptionsReplyTo :: Maybe Text -- ^ Optional replyTo header
  , sendEmailOptionsCustomHeaders :: Maybe (Map.Map String Text) -- ^ Optional custom headers
  , sendEmailOptionsBody :: Maybe Text -- ^ Optional contents of email. If body contains HTML then set `isHTML` to true to ensure that email clients render it correctly. You can use moustache template syntax in the email body in conjunction with `toGroup` contact variables or `templateVariables` data. If you need more templating control consider creating a template and using the `template` property instead of the body.
  , sendEmailOptionsHtml :: Maybe Bool -- ^ Optional HTML flag to indicate that contents is HTML. Set's a `content-type: text/html` for email. (Deprecated: use `isHTML` instead.)
  , sendEmailOptionsIsHTML :: Maybe Bool -- ^ Optional HTML flag. If true the `content-type` of the email will be `text/html`. Set to true when sending HTML to ensure proper rending on email clients
  , sendEmailOptionsCharset :: Maybe Text -- ^ Optional charset
  , sendEmailOptionsAttachments :: Maybe [Text] -- ^ Optional list of attachment IDs to send with this email. You must first upload each attachment separately via method call or dashboard in order to obtain attachment IDs. This way you can reuse attachments with different emails once uploaded. There are several ways to upload that support `multi-part form`, `base64 file encoding`, and octet stream binary uploads. See the `UploadController` for available methods. 
  , sendEmailOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Optional map of template variables. Will replace moustache syntax variables in subject and body or template with the associated values if found.
  , sendEmailOptionsTemplate :: Maybe UUID -- ^ Optional template ID to use for body. Will override body if provided. When using a template make sure you pass the corresponding map of `templateVariables`. You can find which variables are needed by fetching the template itself or viewing it in the dashboard.
  , sendEmailOptionsSendStrategy :: Maybe Text -- ^ How an email should be sent based on its recipients
  , sendEmailOptionsUseInboxName :: Maybe Bool -- ^ Use name of inbox as sender email address name. Will construct RFC 5322 email address with `Inbox name <inbox@address.com>` if the inbox has a name.
  , sendEmailOptionsAddTrackingPixel :: Maybe Bool -- ^ Add tracking pixel to email
  , sendEmailOptionsFilterBouncedRecipients :: Maybe Bool -- ^ Filter recipients to remove any bounced recipients from to, bcc, and cc before sending
  , sendEmailOptionsValidateEmailAddresses :: Maybe Text -- ^ Validate recipient email addresses before sending
  , sendEmailOptionsIgnoreEmptyRecipients :: Maybe Bool -- ^ Ignore empty recipients after validation removes all recipients as invalid and fail silently
  , sendEmailOptionsIsXAmpHtml :: Maybe Bool -- ^ Is content AMP4EMAIL compatible. If set will send as x-amp-html part.
  , sendEmailOptionsBodyParts :: Maybe [SendEmailBodyPart] -- ^ Email body content parts for multipart mime message. Will override body.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendEmailOptions")
instance ToJSON SendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendEmailOptions")


-- | 
data SendOptInConsentEmailOptions = SendOptInConsentEmailOptions
  { sendOptInConsentEmailOptionsTemplateHtml :: Text -- ^ 
  , sendOptInConsentEmailOptionsSubject :: Text -- ^ 
  , sendOptInConsentEmailOptionsSenderInbox :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendOptInConsentEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendOptInConsentEmailOptions")
instance ToJSON SendOptInConsentEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendOptInConsentEmailOptions")


-- | Options for the email envelope
data SendSMTPEnvelopeOptions = SendSMTPEnvelopeOptions
  { sendSMTPEnvelopeOptionsRcptTo :: [Text] -- ^ 
  , sendSMTPEnvelopeOptionsMailFrom :: Text -- ^ 
  , sendSMTPEnvelopeOptionsData :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendSMTPEnvelopeOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendSMTPEnvelopeOptions")
instance ToJSON SendSMTPEnvelopeOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendSMTPEnvelopeOptions")


-- | 
data SendWithQueueResult = SendWithQueueResult
  { sendWithQueueResultId :: UUID -- ^ 
  , sendWithQueueResultUserId :: UUID -- ^ 
  , sendWithQueueResultSubject :: Maybe Text -- ^ 
  , sendWithQueueResultInboxId :: Maybe UUID -- ^ 
  , sendWithQueueResultHeaderId :: Text -- ^ 
  , sendWithQueueResultDelivered :: Bool -- ^ 
  , sendWithQueueResultExceptionName :: Maybe Text -- ^ 
  , sendWithQueueResultMessage :: Maybe Text -- ^ 
  , sendWithQueueResultCreatedAt :: UTCTime -- ^ 
  , sendWithQueueResultUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendWithQueueResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendWithQueueResult")
instance ToJSON SendWithQueueResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendWithQueueResult")


-- | Sender object containing from email address and from personal name if provided in address
data Sender = Sender
  { senderRawValue :: Text -- ^ 
  , senderEmailAddress :: Text -- ^ 
  , senderName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Sender where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sender")
instance ToJSON Sender where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sender")


-- | Last sender object
data SenderProjection = SenderProjection
  { senderProjectionEmailAddress :: Text -- ^ 
  , senderProjectionRawValue :: Text -- ^ 
  , senderProjectionName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SenderProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "senderProjection")
instance ToJSON SenderProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "senderProjection")


-- | Sent email details
data SentEmailDto = SentEmailDto
  { sentEmailDtoId :: UUID -- ^ ID of sent email
  , sentEmailDtoUserId :: UUID -- ^ User ID
  , sentEmailDtoInboxId :: UUID -- ^ Inbox ID email was sent from
  , sentEmailDtoDomainId :: Maybe UUID -- ^ Domain ID
  , sentEmailDtoTo :: Maybe [Text] -- ^ Recipients email was sent to
  , sentEmailDtoFrom :: Maybe Text -- ^ Sent from address
  , sentEmailDtoSender :: Maybe Sender -- ^ 
  , sentEmailDtoRecipients :: Maybe EmailRecipients -- ^ 
  , sentEmailDtoReplyTo :: Maybe Text -- ^ 
  , sentEmailDtoCc :: Maybe [Text] -- ^ 
  , sentEmailDtoBcc :: Maybe [Text] -- ^ 
  , sentEmailDtoAttachments :: Maybe [Text] -- ^ Array of IDs of attachments that were sent with this email
  , sentEmailDtoSubject :: Maybe Text -- ^ 
  , sentEmailDtoBodyMD5Hash :: Maybe Text -- ^ MD5 Hash
  , sentEmailDtoBody :: Maybe Text -- ^ Sent email body
  , sentEmailDtoToContacts :: Maybe [UUID] -- ^ 
  , sentEmailDtoToGroup :: Maybe UUID -- ^ 
  , sentEmailDtoCharset :: Maybe Text -- ^ 
  , sentEmailDtoIsHTML :: Maybe Bool -- ^ 
  , sentEmailDtoSentAt :: UTCTime -- ^ 
  , sentEmailDtoCreatedAt :: UTCTime -- ^ 
  , sentEmailDtoPixelIds :: Maybe [UUID] -- ^ 
  , sentEmailDtoMessageId :: Maybe Text -- ^ RFC 5322 Message-ID header value without angle brackets.
  , sentEmailDtoMessageIds :: Maybe [Text] -- ^ 
  , sentEmailDtoVirtualSend :: Maybe Bool -- ^ 
  , sentEmailDtoTemplateId :: Maybe UUID -- ^ 
  , sentEmailDtoTemplateVariables :: Maybe (Map.Map String Value) -- ^ 
  , sentEmailDtoHeaders :: Maybe (Map.Map String Text) -- ^ 
  , sentEmailDtoThreadId :: Maybe UUID -- ^ MailSlurp thread ID for email chain that enables lookup for In-Reply-To and References fields.
  , sentEmailDtoBodyExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message for quick preview. Takes HTML content part if exists falls back to TEXT content part if not
  , sentEmailDtoTextExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message for quick preview. Takes TEXT content part if exists
  , sentEmailDtoInReplyTo :: Maybe Text -- ^ Parsed value of In-Reply-To header. A Message-ID in a thread.
  , sentEmailDtoFavourite :: Maybe Bool -- ^ Is email favourited
  , sentEmailDtoSizeBytes :: Maybe Integer -- ^ Size of raw email message in bytes
  , sentEmailDtoHtml :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailDto")
instance ToJSON SentEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailDto")


-- | 
data SentEmailProjection = SentEmailProjection
  { sentEmailProjectionId :: UUID -- ^ 
  , sentEmailProjectionRecipients :: Maybe EmailRecipients -- ^ 
  , sentEmailProjectionSubject :: Maybe Text -- ^ 
  , sentEmailProjectionFrom :: Maybe Text -- ^ 
  , sentEmailProjectionSender :: Maybe Sender -- ^ 
  , sentEmailProjectionUserId :: UUID -- ^ 
  , sentEmailProjectionAttachments :: Maybe [Text] -- ^ 
  , sentEmailProjectionInboxId :: UUID -- ^ 
  , sentEmailProjectionCreatedAt :: UTCTime -- ^ 
  , sentEmailProjectionTo :: [Text] -- ^ 
  , sentEmailProjectionCc :: [Text] -- ^ 
  , sentEmailProjectionBcc :: [Text] -- ^ 
  , sentEmailProjectionMessageId :: Maybe Text -- ^ 
  , sentEmailProjectionInReplyTo :: Maybe Text -- ^ 
  , sentEmailProjectionBodyExcerpt :: Maybe Text -- ^ 
  , sentEmailProjectionTextExcerpt :: Maybe Text -- ^ 
  , sentEmailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  , sentEmailProjectionVirtualSend :: Bool -- ^ 
  , sentEmailProjectionThreadId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailProjection")
instance ToJSON SentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailProjection")


-- | 
data SentSmsDto = SentSmsDto
  { sentSmsDtoId :: UUID -- ^ 
  , sentSmsDtoUserId :: UUID -- ^ 
  , sentSmsDtoPhoneNumber :: UUID -- ^ 
  , sentSmsDtoFromNumber :: Text -- ^ 
  , sentSmsDtoToNumber :: Text -- ^ 
  , sentSmsDtoBody :: Text -- ^ 
  , sentSmsDtoSid :: Text -- ^ 
  , sentSmsDtoReplyToSid :: Maybe Text -- ^ 
  , sentSmsDtoReplyToId :: Maybe UUID -- ^ 
  , sentSmsDtoCreatedAt :: UTCTime -- ^ 
  , sentSmsDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentSmsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentSmsDto")
instance ToJSON SentSmsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentSmsDto")


-- | Sent SMS projection
data SentSmsProjection = SentSmsProjection
  { sentSmsProjectionBody :: Text -- ^ 
  , sentSmsProjectionUserId :: UUID -- ^ 
  , sentSmsProjectionCreatedAt :: UTCTime -- ^ 
  , sentSmsProjectionPhoneNumber :: UUID -- ^ 
  , sentSmsProjectionFromNumber :: Text -- ^ 
  , sentSmsProjectionToNumber :: Text -- ^ 
  , sentSmsProjectionReplyToId :: Maybe Text -- ^ 
  , sentSmsProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentSmsProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentSmsProjection")
instance ToJSON SentSmsProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentSmsProjection")


-- | 
data ServerEndpoints = ServerEndpoints
  { serverEndpointsHost :: Text -- ^ 
  , serverEndpointsPort :: Int -- ^ 
  , serverEndpointsTls :: Bool -- ^ 
  , serverEndpointsAltPorts :: [Int] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ServerEndpoints where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serverEndpoints")
instance ToJSON ServerEndpoints where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serverEndpoints")


-- | Options for setting inbox favourite state
data SetInboxFavouritedOptions = SetInboxFavouritedOptions
  { setInboxFavouritedOptionsState :: Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SetInboxFavouritedOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "setInboxFavouritedOptions")
instance ToJSON SetInboxFavouritedOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "setInboxFavouritedOptions")


-- | Options for setting phone favourite state
data SetPhoneFavouritedOptions = SetPhoneFavouritedOptions
  { setPhoneFavouritedOptionsState :: Bool -- ^ Phone favourite state
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SetPhoneFavouritedOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "setPhoneFavouritedOptions")
instance ToJSON SetPhoneFavouritedOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "setPhoneFavouritedOptions")


-- | Simplified send email options
data SimpleSendEmailOptions = SimpleSendEmailOptions
  { simpleSendEmailOptionsSenderId :: Maybe UUID -- ^ ID of inbox to send from. If null an inbox will be created for sending
  , simpleSendEmailOptionsTo :: Text -- ^ Email address to send to
  , simpleSendEmailOptionsBody :: Maybe Text -- ^ Body of the email message. Supports HTML
  , simpleSendEmailOptionsSubject :: Maybe Text -- ^ Subject line of the email
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SimpleSendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "simpleSendEmailOptions")
instance ToJSON SimpleSendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "simpleSendEmailOptions")


-- | 
data SmsDto = SmsDto
  { smsDtoId :: UUID -- ^ 
  , smsDtoUserId :: UUID -- ^ 
  , smsDtoPhoneNumber :: UUID -- ^ 
  , smsDtoFromNumber :: Text -- ^ 
  , smsDtoToNumber :: Maybe Text -- ^ 
  , smsDtoFavourite :: Bool -- ^ 
  , smsDtoBody :: Text -- ^ 
  , smsDtoRead :: Bool -- ^ 
  , smsDtoCreatedAt :: UTCTime -- ^ 
  , smsDtoUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsDto")
instance ToJSON SmsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsDto")


-- | Options for matching SMS messages in a phone number. Each match option object contains a &#x60;field&#x60;, &#x60;should&#x60; and &#x60;value&#x60; property. Together they form logical conditions such as &#x60;BODY&#x60; should &#x60;CONTAIN&#x60; value.
data SmsMatchOption = SmsMatchOption
  { smsMatchOptionField :: Text -- ^ Fields of an SMS object that can be used to filter results
  , smsMatchOptionShould :: Text -- ^ How the value of the email field specified should be compared to the value given in the match options.
  , smsMatchOptionValue :: Text -- ^ The value you wish to compare with the value of the field specified using the `should` value passed. For example `BODY` should `CONTAIN` a value passed.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsMatchOption where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsMatchOption")
instance ToJSON SmsMatchOption where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsMatchOption")


-- | 
data SmsPreview = SmsPreview
  { smsPreviewId :: UUID -- ^ 
  , smsPreviewUserId :: UUID -- ^ 
  , smsPreviewBody :: Text -- ^ 
  , smsPreviewPhoneNumber :: UUID -- ^ 
  , smsPreviewFromNumber :: Text -- ^ 
  , smsPreviewRead :: Bool -- ^ 
  , smsPreviewCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsPreview")
instance ToJSON SmsPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsPreview")


-- | SMS projection
data SmsProjection = SmsProjection
  { smsProjectionBody :: Text -- ^ 
  , smsProjectionUserId :: UUID -- ^ 
  , smsProjectionCreatedAt :: UTCTime -- ^ 
  , smsProjectionPhoneNumber :: UUID -- ^ 
  , smsProjectionFromNumber :: Text -- ^ 
  , smsProjectionRead :: Bool -- ^ 
  , smsProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsProjection")
instance ToJSON SmsProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsProjection")


-- | 
data SmsReplyOptions = SmsReplyOptions
  { smsReplyOptionsBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsReplyOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsReplyOptions")
instance ToJSON SmsReplyOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsReplyOptions")


-- | 
data SmsSendOptions = SmsSendOptions
  { smsSendOptionsTo :: Text -- ^ 
  , smsSendOptionsBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsSendOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smsSendOptions")
instance ToJSON SmsSendOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smsSendOptions")


-- | Access details for inbox using SMTP
data SmtpAccessDetails = SmtpAccessDetails
  { smtpAccessDetailsSecureSmtpServerHost :: Text -- ^ Secure TLS SMTP server host domain
  , smtpAccessDetailsSecureSmtpServerPort :: Int -- ^ Secure TLS SMTP server host port
  , smtpAccessDetailsSecureSmtpUsername :: Text -- ^ Secure TLS SMTP username for login
  , smtpAccessDetailsSecureSmtpPassword :: Text -- ^ Secure TLS SMTP password for login
  , smtpAccessDetailsSmtpServerHost :: Text -- ^ SMTP server host domain
  , smtpAccessDetailsSmtpServerPort :: Int -- ^ SMTP server host port
  , smtpAccessDetailsSmtpUsername :: Text -- ^ SMTP username for login
  , smtpAccessDetailsSmtpPassword :: Text -- ^ SMTP password for login
  , smtpAccessDetailsMailFromDomain :: Maybe Text -- ^ Mail from domain or SMTP HELO value
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmtpAccessDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "smtpAccessDetails")
instance ToJSON SmtpAccessDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "smtpAccessDetails")


-- | 
data SortObject = SortObject
  { sortObjectSorted :: Maybe Bool -- ^ 
  , sortObjectUnsorted :: Maybe Bool -- ^ 
  , sortObjectEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SortObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sortObject")
instance ToJSON SortObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sortObject")


-- | 
data SpellingIssue = SpellingIssue
  { spellingIssueGroup :: Text -- ^ 
  , spellingIssueSuggestion :: Text -- ^ 
  , spellingIssueSeverity :: Text -- ^ 
  , spellingIssueMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SpellingIssue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "spellingIssue")
instance ToJSON SpellingIssue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "spellingIssue")


-- | 
data StructuredContentResult = StructuredContentResult
  { structuredContentResultResult :: Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON StructuredContentResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "structuredContentResult")
instance ToJSON StructuredContentResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "structuredContentResult")


-- | Structured output schema to use for generation
data StructuredOutputSchema = StructuredOutputSchema
  { structuredOutputSchemaAnyOf :: Maybe [StructuredOutputSchema] -- ^ 
  , structuredOutputSchemaDefault :: Maybe Value -- ^ 
  , structuredOutputSchemaDescription :: Maybe Text -- ^ 
  , structuredOutputSchemaEnum :: Maybe [Text] -- ^ 
  , structuredOutputSchemaExample :: Maybe Value -- ^ 
  , structuredOutputSchemaFormat :: Maybe Text -- ^ 
  , structuredOutputSchemaItems :: Maybe StructuredOutputSchema -- ^ 
  , structuredOutputSchemaMaxItems :: Maybe Integer -- ^ 
  , structuredOutputSchemaMinItems :: Maybe Integer -- ^ 
  , structuredOutputSchemaMaxLength :: Maybe Integer -- ^ 
  , structuredOutputSchemaMinLength :: Maybe Integer -- ^ 
  , structuredOutputSchemaPattern :: Maybe Text -- ^ 
  , structuredOutputSchemaProperties :: Maybe (Map.Map String StructuredOutputSchema) -- ^ 
  , structuredOutputSchemaPropertyOrdering :: Maybe [Text] -- ^ 
  , structuredOutputSchemaRequired :: Maybe [Text] -- ^ 
  , structuredOutputSchemaMaxProperties :: Maybe Integer -- ^ 
  , structuredOutputSchemaMinProperties :: Maybe Integer -- ^ 
  , structuredOutputSchemaMaximum :: Maybe Double -- ^ 
  , structuredOutputSchemaMinimum :: Maybe Double -- ^ 
  , structuredOutputSchemaNullable :: Maybe Bool -- ^ 
  , structuredOutputSchemaTitle :: Maybe Text -- ^ 
  , structuredOutputSchemaType :: Maybe Text -- ^ Primitive JSON schema types with a fallback CUSTOM for unknown values.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON StructuredOutputSchema where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "structuredOutputSchema")
instance ToJSON StructuredOutputSchema where
  toJSON = genericToJSON (removeFieldLabelPrefix False "structuredOutputSchema")


-- | Email template
data TemplateDto = TemplateDto
  { templateDtoId :: UUID -- ^ ID of template
  , templateDtoName :: Text -- ^ Template name
  , templateDtoVariables :: [TemplateVariable] -- ^ Variables available in template that can be replaced with values
  , templateDtoContent :: Text -- ^ Content of the template
  , templateDtoCreatedAt :: UTCTime -- ^ Created at time
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateDto")
instance ToJSON TemplateDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateDto")


-- | 
data TemplatePreview = TemplatePreview
  { templatePreviewPreview :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplatePreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templatePreview")
instance ToJSON TemplatePreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templatePreview")


-- | Email template data
data TemplateProjection = TemplateProjection
  { templateProjectionUpdatedAt :: UTCTime -- ^ 
  , templateProjectionCreatedAt :: UTCTime -- ^ 
  , templateProjectionVariables :: [Text] -- ^ 
  , templateProjectionName :: Text -- ^ 
  , templateProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateProjection")
instance ToJSON TemplateProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateProjection")


-- | Variable for use with email template
data TemplateVariable = TemplateVariable
  { templateVariableName :: Text -- ^ Name of variable. This can be used in a template as {{name}}
  , templateVariableVariableType :: Text -- ^ The type of variable
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateVariable where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateVariable")
instance ToJSON TemplateVariable where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateVariable")


-- | Test options for inbox ruleset receiving test
data TestInboxRulesetReceivingOptions = TestInboxRulesetReceivingOptions
  { testInboxRulesetReceivingOptionsInboxId :: UUID -- ^ 
  , testInboxRulesetReceivingOptionsFromSender :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestInboxRulesetReceivingOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testInboxRulesetReceivingOptions")
instance ToJSON TestInboxRulesetReceivingOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testInboxRulesetReceivingOptions")


-- | 
data TestInboxRulesetReceivingResult = TestInboxRulesetReceivingResult
  { testInboxRulesetReceivingResultCanReceive :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestInboxRulesetReceivingResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testInboxRulesetReceivingResult")
instance ToJSON TestInboxRulesetReceivingResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testInboxRulesetReceivingResult")


-- | Test options for inbox ruleset sending test
data TestInboxRulesetSendingOptions = TestInboxRulesetSendingOptions
  { testInboxRulesetSendingOptionsInboxId :: UUID -- ^ 
  , testInboxRulesetSendingOptionsRecipient :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestInboxRulesetSendingOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testInboxRulesetSendingOptions")
instance ToJSON TestInboxRulesetSendingOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testInboxRulesetSendingOptions")


-- | 
data TestInboxRulesetSendingResult = TestInboxRulesetSendingResult
  { testInboxRulesetSendingResultCanSend :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestInboxRulesetSendingResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testInboxRulesetSendingResult")
instance ToJSON TestInboxRulesetSendingResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testInboxRulesetSendingResult")


-- | Options for testing new inbox forwarder rules
data TestNewInboxForwarderOptions = TestNewInboxForwarderOptions
  { testNewInboxForwarderOptionsInboxForwarderTestOptions :: InboxForwarderTestOptions -- ^ 
  , testNewInboxForwarderOptionsCreateInboxForwarderOptions :: CreateInboxForwarderOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestNewInboxForwarderOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testNewInboxForwarderOptions")
instance ToJSON TestNewInboxForwarderOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testNewInboxForwarderOptions")


-- | Test inbox ruleset options
data TestNewInboxRulesetOptions = TestNewInboxRulesetOptions
  { testNewInboxRulesetOptionsInboxRulesetTestOptions :: InboxRulesetTestOptions -- ^ 
  , testNewInboxRulesetOptionsCreateInboxRulesetOptions :: CreateInboxRulesetOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestNewInboxRulesetOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testNewInboxRulesetOptions")
instance ToJSON TestNewInboxRulesetOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testNewInboxRulesetOptions")


-- | 
data TestPhoneNumberOptions = TestPhoneNumberOptions
  { testPhoneNumberOptionsMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestPhoneNumberOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testPhoneNumberOptions")
instance ToJSON TestPhoneNumberOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testPhoneNumberOptions")


-- | Tracking pixel
data TrackingPixelDto = TrackingPixelDto
  { trackingPixelDtoId :: UUID -- ^ 
  , trackingPixelDtoSeen :: Bool -- ^ 
  , trackingPixelDtoRecipient :: Maybe Text -- ^ 
  , trackingPixelDtoHtml :: Text -- ^ 
  , trackingPixelDtoUrl :: Text -- ^ 
  , trackingPixelDtoInboxId :: Maybe UUID -- ^ 
  , trackingPixelDtoSentEmailId :: Maybe UUID -- ^ 
  , trackingPixelDtoSeenAt :: Maybe UTCTime -- ^ 
  , trackingPixelDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackingPixelDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trackingPixelDto")
instance ToJSON TrackingPixelDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trackingPixelDto")


-- | Tracking pixel data
data TrackingPixelProjection = TrackingPixelProjection
  { trackingPixelProjectionUserId :: UUID -- ^ 
  , trackingPixelProjectionInboxId :: Maybe UUID -- ^ 
  , trackingPixelProjectionSentEmailId :: Maybe UUID -- ^ 
  , trackingPixelProjectionCreatedAt :: UTCTime -- ^ 
  , trackingPixelProjectionRecipient :: Maybe Text -- ^ 
  , trackingPixelProjectionSeen :: Bool -- ^ 
  , trackingPixelProjectionSeenAt :: Maybe UTCTime -- ^ 
  , trackingPixelProjectionName :: Maybe Text -- ^ 
  , trackingPixelProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackingPixelProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trackingPixelProjection")
instance ToJSON TrackingPixelProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trackingPixelProjection")


-- | Unknown missed email projection
data UnknownMissedEmailProjection = UnknownMissedEmailProjection
  { unknownMissedEmailProjectionCreatedAt :: UTCTime -- ^ 
  , unknownMissedEmailProjectionTo :: Maybe [Text] -- ^ 
  , unknownMissedEmailProjectionSubject :: Maybe Text -- ^ 
  , unknownMissedEmailProjectionId :: UUID -- ^ 
  , unknownMissedEmailProjectionFrom :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnknownMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unknownMissedEmailProjection")
instance ToJSON UnknownMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unknownMissedEmailProjection")


-- | Number of unread entities
data UnreadCount = UnreadCount
  { unreadCountCount :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnreadCount where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unreadCount")
instance ToJSON UnreadCount where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unreadCount")


-- | Number of unseen errors
data UnseenErrorCountDto = UnseenErrorCountDto
  { unseenErrorCountDtoCount :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnseenErrorCountDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unseenErrorCountDto")
instance ToJSON UnseenErrorCountDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unseenErrorCountDto")


-- | Update an email alias
data UpdateAliasOptions = UpdateAliasOptions
  { updateAliasOptionsName :: Maybe Text -- ^ Optional name for alias
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateAliasOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateAliasOptions")
instance ToJSON UpdateAliasOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateAliasOptions")


-- | Options for creating a domain to use with MailSlurp. You must have ownership access to this domain in order to verify it. Domains will not functionally currently until the domain has been verified. See https://www.mailslurp.com/guides/custom-domains for help.
data UpdateDomainOptions = UpdateDomainOptions
  { updateDomainOptionsCatchAllInboxId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDomainOptions")
instance ToJSON UpdateDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDomainOptions")


-- | Update group contacts options. Pass a list of contact ids to replace existing group contacts.
data UpdateGroupContacts = UpdateGroupContacts
  { updateGroupContactsContactIds :: [UUID] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupContacts where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupContacts")
instance ToJSON UpdateGroupContacts where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupContacts")


-- | Edit access details for inbox using IMAP
data UpdateImapAccessOptions = UpdateImapAccessOptions
  { updateImapAccessOptionsImapUsername :: Maybe Text -- ^ IMAP username for login
  , updateImapAccessOptionsImapPassword :: Maybe Text -- ^ IMAP password for login
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateImapAccessOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateImapAccessOptions")
instance ToJSON UpdateImapAccessOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateImapAccessOptions")


-- | Options for updating inbox properties
data UpdateInboxOptions = UpdateInboxOptions
  { updateInboxOptionsName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , updateInboxOptionsDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , updateInboxOptionsTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , updateInboxOptionsExpiresAt :: Maybe UTCTime -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , updateInboxOptionsFavourite :: Maybe Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateInboxOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateInboxOptions")
instance ToJSON UpdateInboxOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateInboxOptions")


-- | Options for updating an inbox replier
data UpdateInboxReplierOptions = UpdateInboxReplierOptions
  { updateInboxReplierOptionsInboxId :: UUID -- ^ Inbox ID to attach replier to
  , updateInboxReplierOptionsName :: Maybe Text -- ^ Name for replier
  , updateInboxReplierOptionsField :: Text -- ^ Field to match against to trigger inbox replier for inbound email
  , updateInboxReplierOptionsMatch :: Text -- ^ String or wildcard style match for field specified when evaluating reply rules
  , updateInboxReplierOptionsReplyTo :: Maybe Text -- ^ Reply-to email address when sending replying
  , updateInboxReplierOptionsSubject :: Maybe Text -- ^ Subject override when replying to email
  , updateInboxReplierOptionsFrom :: Maybe Text -- ^ Send email from address
  , updateInboxReplierOptionsCharset :: Maybe Text -- ^ Email reply charset
  , updateInboxReplierOptionsIsHTML :: Maybe Bool -- ^ Send HTML email
  , updateInboxReplierOptionsIgnoreReplyTo :: Maybe Bool -- ^ Ignore sender replyTo when responding. Send directly to the sender if enabled.
  , updateInboxReplierOptionsBody :: Maybe Text -- ^ Email body for reply
  , updateInboxReplierOptionsTemplateId :: Maybe UUID -- ^ ID of template to use when sending a reply
  , updateInboxReplierOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variable values
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateInboxReplierOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateInboxReplierOptions")
instance ToJSON UpdateInboxReplierOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateInboxReplierOptions")


-- | 
data UpdatePhoneNumberOptions = UpdatePhoneNumberOptions
  { updatePhoneNumberOptionsName :: Maybe Text -- ^ 
  , updatePhoneNumberOptionsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdatePhoneNumberOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updatePhoneNumberOptions")
instance ToJSON UpdatePhoneNumberOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updatePhoneNumberOptions")


-- | Edit access details for inbox using SMTP
data UpdateSmtpAccessOptions = UpdateSmtpAccessOptions
  { updateSmtpAccessOptionsSmtpUsername :: Maybe Text -- ^ SMTP username for login
  , updateSmtpAccessOptionsSmtpPassword :: Maybe Text -- ^ SMTP password for login
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSmtpAccessOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSmtpAccessOptions")
instance ToJSON UpdateSmtpAccessOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSmtpAccessOptions")


-- | Options for uploading files for attachments. When sending emails with the API that require attachments first upload each attachment. Then use the returned attachment ID in your &#x60;SendEmailOptions&#x60; when sending an email. This way you can use attachments multiple times once they have been uploaded.
data UploadAttachmentOptions = UploadAttachmentOptions
  { uploadAttachmentOptionsContentId :: Maybe Text -- ^ Optional contentId for file.
  , uploadAttachmentOptionsContentType :: Maybe Text -- ^ Optional contentType for file. For instance `application/pdf`
  , uploadAttachmentOptionsFilename :: Maybe Text -- ^ Optional filename to save upload with. Will be the name that is shown in email clients
  , uploadAttachmentOptionsBase64Contents :: Text -- ^ Base64 encoded string of file contents. Typically this means reading the bytes or string content of a file and then converting that to a base64 encoded string. For examples of how to do this see https://www.mailslurp.com/guides/base64-file-uploads/
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UploadAttachmentOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uploadAttachmentOptions")
instance ToJSON UploadAttachmentOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uploadAttachmentOptions")


-- | 
data UserInfoDto = UserInfoDto
  { userInfoDtoId :: UUID -- ^ 
  , userInfoDtoEmailAddress :: Text -- ^ 
  , userInfoDtoAccountState :: Text -- ^ 
  , userInfoDtoSubscriptionType :: Maybe Text -- ^ 
  , userInfoDtoAccountType :: Text -- ^ 
  , userInfoDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserInfoDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userInfoDto")
instance ToJSON UserInfoDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userInfoDto")


-- | Options for validating a list of email addresses
data ValidateEmailAddressListOptions = ValidateEmailAddressListOptions
  { validateEmailAddressListOptionsEmailAddressList :: [Text] -- ^ 
  , validateEmailAddressListOptionsIgnoreOldResults :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidateEmailAddressListOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validateEmailAddressListOptions")
instance ToJSON ValidateEmailAddressListOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validateEmailAddressListOptions")


-- | Result of validating a list of email addresses
data ValidateEmailAddressListResult = ValidateEmailAddressListResult
  { validateEmailAddressListResultValidEmailAddresses :: [Text] -- ^ 
  , validateEmailAddressListResultInvalidEmailAddresses :: [Text] -- ^ 
  , validateEmailAddressListResultResultMapEmailAddressIsValid :: (Map.Map String Bool) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidateEmailAddressListResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validateEmailAddressListResult")
instance ToJSON ValidateEmailAddressListResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validateEmailAddressListResult")


-- | 
data ValidatePhoneNumberOptions = ValidatePhoneNumberOptions
  { validatePhoneNumberOptionsPhoneNumber :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidatePhoneNumberOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validatePhoneNumberOptions")
instance ToJSON ValidatePhoneNumberOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validatePhoneNumberOptions")


-- | Response object for email validation operation
data ValidationDto = ValidationDto
  { validationDtoEmailId :: UUID -- ^ ID of the email validated
  , validationDtoHtml :: HTMLValidationResult -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidationDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validationDto")
instance ToJSON ValidationDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validationDto")


-- | Optional warnings resulting from HTML validation
data ValidationMessage = ValidationMessage
  { validationMessageLineNumber :: Int -- ^ 
  , validationMessageMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidationMessage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validationMessage")
instance ToJSON ValidationMessage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validationMessage")


-- | Options for verifying that an email address exists at a remote mail server.
data VerifyEmailAddressOptions = VerifyEmailAddressOptions
  { verifyEmailAddressOptionsMailServerDomain :: Maybe Text -- ^ 
  , verifyEmailAddressOptionsEmailAddress :: Text -- ^ 
  , verifyEmailAddressOptionsSenderEmailAddress :: Maybe Text -- ^ 
  , verifyEmailAddressOptionsPort :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON VerifyEmailAddressOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "verifyEmailAddressOptions")
instance ToJSON VerifyEmailAddressOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "verifyEmailAddressOptions")


-- | 
data VerifyWebhookSignatureOptions = VerifyWebhookSignatureOptions
  { verifyWebhookSignatureOptionsMessageId :: Text -- ^ 
  , verifyWebhookSignatureOptionsSignature :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON VerifyWebhookSignatureOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "verifyWebhookSignatureOptions")
instance ToJSON VerifyWebhookSignatureOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "verifyWebhookSignatureOptions")


-- | 
data VerifyWebhookSignatureResults = VerifyWebhookSignatureResults
  { verifyWebhookSignatureResultsIsValid :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON VerifyWebhookSignatureResults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "verifyWebhookSignatureResults")
instance ToJSON VerifyWebhookSignatureResults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "verifyWebhookSignatureResults")


-- | Conditions to apply to emails that you are waiting for
data WaitForConditions = WaitForConditions
  { waitForConditionsInboxId :: UUID -- ^ ID of inbox to search within and apply conditions to. Essentially filtering the emails found to give a count.
  , waitForConditionsCount :: Maybe Int -- ^ Number of results that should match conditions. Either exactly or at least this amount based on the `countType`. If count condition is not met and the timeout has not been reached the `waitFor` method will retry the operation.
  , waitForConditionsDelayTimeout :: Maybe Integer -- ^ Max time in milliseconds to wait between retries if a `timeout` is specified.
  , waitForConditionsTimeout :: Integer -- ^ Max time in milliseconds to retry the `waitFor` operation until conditions are met.
  , waitForConditionsUnreadOnly :: Maybe Bool -- ^ Apply conditions only to **unread** emails. All emails begin with `read=false`. An email is marked `read=true` when an `EmailDto` representation of it has been returned to the user at least once. For example you have called `getEmail` or `waitForLatestEmail` etc., or you have viewed the email in the dashboard.
  , waitForConditionsCountType :: Maybe Text -- ^ How result size should be compared with the expected size. Exactly or at-least matching result?
  , waitForConditionsMatches :: Maybe [MatchOption] -- ^ Conditions that should be matched for an email to qualify for results. Each condition will be applied in order to each email within an inbox to filter a result list of matching emails you are waiting for.
  , waitForConditionsSortDirection :: Maybe Text -- ^ Direction to sort matching emails by created time
  , waitForConditionsSince :: Maybe UTCTime -- ^ ISO Date Time earliest time of email to consider. Filter for matching emails that were received after this date
  , waitForConditionsBefore :: Maybe UTCTime -- ^ ISO Date Time latest time of email to consider. Filter for matching emails that were received before this date
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WaitForConditions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "waitForConditions")
instance ToJSON WaitForConditions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "waitForConditions")


-- | 
data WaitForSingleSmsOptions = WaitForSingleSmsOptions
  { waitForSingleSmsOptionsPhoneNumberId :: UUID -- ^ 
  , waitForSingleSmsOptionsTimeout :: Integer -- ^ 
  , waitForSingleSmsOptionsUnreadOnly :: Maybe Bool -- ^ 
  , waitForSingleSmsOptionsBefore :: Maybe UTCTime -- ^ 
  , waitForSingleSmsOptionsSince :: Maybe UTCTime -- ^ 
  , waitForSingleSmsOptionsSortDirection :: Maybe Text -- ^ 
  , waitForSingleSmsOptionsDelay :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WaitForSingleSmsOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "waitForSingleSmsOptions")
instance ToJSON WaitForSingleSmsOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "waitForSingleSmsOptions")


-- | Conditions to apply to emails that you are waiting for
data WaitForSmsConditions = WaitForSmsConditions
  { waitForSmsConditionsPhoneNumberId :: UUID -- ^ ID of phone number to search within and apply conditions to. Essentially filtering the SMS found to give a count.
  , waitForSmsConditionsLimit :: Maybe Int -- ^ Limit results
  , waitForSmsConditionsCount :: Integer -- ^ Number of results that should match conditions. Either exactly or at least this amount based on the `countType`. If count condition is not met and the timeout has not been reached the `waitFor` method will retry the operation.
  , waitForSmsConditionsDelayTimeout :: Maybe Integer -- ^ Max time in milliseconds to wait between retries if a `timeout` is specified.
  , waitForSmsConditionsTimeout :: Integer -- ^ Max time in milliseconds to retry the `waitFor` operation until conditions are met.
  , waitForSmsConditionsUnreadOnly :: Maybe Bool -- ^ Apply conditions only to **unread** SMS. All SMS messages begin with `read=false`. An SMS is marked `read=true` when an `SMS` has been returned to the user at least once. For example you have called `getSms`, or you have viewed the SMS in the dashboard.
  , waitForSmsConditionsCountType :: Maybe Text -- ^ How result size should be compared with the expected size. Exactly or at-least matching result?
  , waitForSmsConditionsMatches :: Maybe [SmsMatchOption] -- ^ Conditions that should be matched for an SMS to qualify for results. Each condition will be applied in order to each SMS within a phone number to filter a result list of matching SMSs you are waiting for.
  , waitForSmsConditionsSortDirection :: Maybe Text -- ^ Direction to sort matching SMSs by created time
  , waitForSmsConditionsSince :: Maybe UTCTime -- ^ ISO Date Time earliest time of SMS to consider. Filter for matching SMSs that were received after this date
  , waitForSmsConditionsBefore :: Maybe UTCTime -- ^ ISO Date Time latest time of SMS to consider. Filter for matching SMSs that were received before this date
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WaitForSmsConditions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "waitForSmsConditions")
instance ToJSON WaitForSmsConditions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "waitForSmsConditions")


-- | BOUNCE webhook payload. Sent to your webhook url endpoint via HTTP POST when an email bounced or was rejected by a recipient. Save the recipients to a ban list on your server and avoid emailing them again. It is recommended you also listen to the BOUNCE_RECIPIENT payload.
data WebhookBouncePayload = WebhookBouncePayload
  { webhookBouncePayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookBouncePayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookBouncePayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookBouncePayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookBouncePayloadBounceId :: UUID -- ^ ID of the bounce email record. Use the ID with the bounce controller to view more information
  , webhookBouncePayloadSentToRecipients :: Maybe [Text] -- ^ Email sent to recipients
  , webhookBouncePayloadSender :: Text -- ^ Sender causing bounce
  , webhookBouncePayloadBounceRecipients :: Maybe [Text] -- ^ Email addresses that resulted in a bounce or email being rejected. Please save these recipients and avoid emailing them in the future to maintain your reputation.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookBouncePayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookBouncePayload")
instance ToJSON WebhookBouncePayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookBouncePayload")


-- | BOUNCE_RECIPIENT webhook payload. Sent to your webhook url endpoint via HTTP POST when an email caused a bounce to occur for a recipient. Save the recipient to a ban list of your server and avoid email them again.
data WebhookBounceRecipientPayload = WebhookBounceRecipientPayload
  { webhookBounceRecipientPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookBounceRecipientPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookBounceRecipientPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookBounceRecipientPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookBounceRecipientPayloadRecipient :: Text -- ^ Email address that caused a bounce. Make note of the address and try not to message it again to preserve your reputation.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookBounceRecipientPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookBounceRecipientPayload")
instance ToJSON WebhookBounceRecipientPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookBounceRecipientPayload")


-- | DELIVERY_STATUS webhook payload. Sent to your webhook url endpoint via HTTP POST when an email delivery status is created. This could be a successful delivery or a delivery failure.
data WebhookDeliveryStatusPayload = WebhookDeliveryStatusPayload
  { webhookDeliveryStatusPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookDeliveryStatusPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookDeliveryStatusPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookDeliveryStatusPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookDeliveryStatusPayloadId :: UUID -- ^ ID of delivery status
  , webhookDeliveryStatusPayloadUserId :: UUID -- ^ User ID of event
  , webhookDeliveryStatusPayloadSentId :: Maybe UUID -- ^ ID of sent email
  , webhookDeliveryStatusPayloadRemoteMtaIp :: Maybe Text -- ^ IP address of the remote Mail Transfer Agent
  , webhookDeliveryStatusPayloadInboxId :: Maybe UUID -- ^ Id of the inbox
  , webhookDeliveryStatusPayloadReportingMta :: Maybe Text -- ^ Mail Transfer Agent reporting delivery status
  , webhookDeliveryStatusPayloadRecipients :: Maybe [Text] -- ^ Recipients for delivery
  , webhookDeliveryStatusPayloadSmtpResponse :: Maybe Text -- ^ SMTP server response message
  , webhookDeliveryStatusPayloadSmtpStatusCode :: Maybe Int -- ^ SMTP server status
  , webhookDeliveryStatusPayloadProcessingTimeMillis :: Maybe Integer -- ^ Time in milliseconds for delivery processing
  , webhookDeliveryStatusPayloadReceived :: Maybe UTCTime -- ^ Time event was received
  , webhookDeliveryStatusPayloadSubject :: Maybe Text -- ^ Email subject
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookDeliveryStatusPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookDeliveryStatusPayload")
instance ToJSON WebhookDeliveryStatusPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookDeliveryStatusPayload")


-- | Representation of a webhook for an inbox. The URL specified will be using by MailSlurp whenever an email is received by the attached inbox. A webhook entity should have a URL that points to your server. Your server should accept HTTP/S POST requests and return a success 200. MailSlurp will retry your webhooks if they fail. See https://api.mailslurp.com/schemas/webhook-payload for the payload schema.
data WebhookDto = WebhookDto
  { webhookDtoId :: UUID -- ^ ID of the Webhook
  , webhookDtoUserId :: UUID -- ^ User ID of the Webhook
  , webhookDtoBasicAuth :: Bool -- ^ Does webhook expect basic authentication? If true it means you created this webhook with a username and password. MailSlurp will use these in the URL to authenticate itself.
  , webhookDtoName :: Maybe Text -- ^ Name of the webhook
  , webhookDtoPhoneId :: Maybe UUID -- ^ The phoneNumberId that the Webhook will be triggered by. If null then webhook triggered at account level or inbox level if inboxId set
  , webhookDtoInboxId :: Maybe UUID -- ^ The inbox that the Webhook will be triggered by. If null then webhook triggered at account level or phone level if phoneId set
  , webhookDtoRequestBodyTemplate :: Maybe Text -- ^ Request body template for HTTP request that will be sent for the webhook. Use Moustache style template variables to insert values from the original event payload.
  , webhookDtoUrl :: Text -- ^ URL of your server that the webhook will be sent to. The schema of the JSON that is sent is described by the payloadJsonSchema.
  , webhookDtoMethod :: Text -- ^ HTTP method that your server endpoint must listen for
  , webhookDtoPayloadJsonSchema :: Text -- ^ Deprecated. Fetch JSON Schema for webhook using the getJsonSchemaForWebhookPayload method
  , webhookDtoCreatedAt :: UTCTime -- ^ When the webhook was created
  , webhookDtoUpdatedAt :: UTCTime -- ^ 
  , webhookDtoEventName :: Maybe Text -- ^ Webhook trigger event name
  , webhookDtoRequestHeaders :: Maybe WebhookHeaders -- ^ 
  , webhookDtoIgnoreInsecureSslCertificates :: Maybe Bool -- ^ Should notifier ignore insecure SSL certificates
  , webhookDtoUseStaticIpRange :: Maybe Bool -- ^ Should notifier use static IP range when sending webhook payload
  , webhookDtoHealthStatus :: Maybe Text -- ^ Webhook health
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookDto")
instance ToJSON WebhookDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookDto")


-- | EMAIL_OPENED webhook payload. Sent to your webhook url endpoint via HTTP POST when an email containing a tracking pixel is opened and the pixel image is loaded by a reader.
data WebhookEmailOpenedPayload = WebhookEmailOpenedPayload
  { webhookEmailOpenedPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookEmailOpenedPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookEmailOpenedPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookEmailOpenedPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookEmailOpenedPayloadInboxId :: UUID -- ^ Id of the inbox
  , webhookEmailOpenedPayloadPixelId :: UUID -- ^ ID of the tracking pixel
  , webhookEmailOpenedPayloadSentEmailId :: UUID -- ^ ID of sent email
  , webhookEmailOpenedPayloadRecipient :: Text -- ^ Email address for the recipient of the tracking pixel
  , webhookEmailOpenedPayloadCreatedAt :: UTCTime -- ^ Date time of event creation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEmailOpenedPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookEmailOpenedPayload")
instance ToJSON WebhookEmailOpenedPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookEmailOpenedPayload")


-- | EMAIL_READ webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is read. This happens when an email is requested in full from the API or a user views the email in the dashboard.
data WebhookEmailReadPayload = WebhookEmailReadPayload
  { webhookEmailReadPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookEmailReadPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookEmailReadPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookEmailReadPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookEmailReadPayloadEmailId :: UUID -- ^ ID of the email that was received. Use this ID for fetching the email with the `EmailController`.
  , webhookEmailReadPayloadInboxId :: UUID -- ^ Id of the inbox
  , webhookEmailReadPayloadEmailIsRead :: Bool -- ^ Is the email read
  , webhookEmailReadPayloadCreatedAt :: UTCTime -- ^ Date time of event creation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEmailReadPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookEmailReadPayload")
instance ToJSON WebhookEmailReadPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookEmailReadPayload")


-- | 
data WebhookEndpointProjection = WebhookEndpointProjection
  { webhookEndpointProjectionUrl :: Text -- ^ 
  , webhookEndpointProjectionHealth :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEndpointProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookEndpointProjection")
instance ToJSON WebhookEndpointProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookEndpointProjection")


-- | Name value pair for webhook header
data WebhookHeaderNameValue = WebhookHeaderNameValue
  { webhookHeaderNameValueName :: Text -- ^ Name of header
  , webhookHeaderNameValueValue :: Text -- ^ Value of header
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookHeaderNameValue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookHeaderNameValue")
instance ToJSON WebhookHeaderNameValue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookHeaderNameValue")


-- | Webhook HTTP headers to include with each request from MailSlurp to your server
data WebhookHeaders = WebhookHeaders
  { webhookHeadersHeaders :: [WebhookHeaderNameValue] -- ^ List of header name value pairs to include with webhook requests
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookHeaders where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookHeaders")
instance ToJSON WebhookHeaders where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookHeaders")


-- | NEW_ATTACHMENT webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to that contains an attachment. You can use the attachmentId to download the attachment.
data WebhookNewAttachmentPayload = WebhookNewAttachmentPayload
  { webhookNewAttachmentPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewAttachmentPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookNewAttachmentPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookNewAttachmentPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewAttachmentPayloadAttachmentId :: Text -- ^ ID of attachment. Use the `AttachmentController` to
  , webhookNewAttachmentPayloadName :: Text -- ^ Filename of the attachment if present
  , webhookNewAttachmentPayloadContentType :: Text -- ^ Content type of attachment such as 'image/png' or 'application/pdf
  , webhookNewAttachmentPayloadContentLength :: Integer -- ^ Size of attachment in bytes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewAttachmentPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewAttachmentPayload")
instance ToJSON WebhookNewAttachmentPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewAttachmentPayload")


-- | NEW_CONTACT webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to that contains a recipient that has not been saved as a contact.
data WebhookNewContactPayload = WebhookNewContactPayload
  { webhookNewContactPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewContactPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookNewContactPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookNewContactPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewContactPayloadContactId :: UUID -- ^ Contact ID
  , webhookNewContactPayloadGroupId :: Maybe UUID -- ^ Contact group ID
  , webhookNewContactPayloadFirstName :: Maybe Text -- ^ Contact first name
  , webhookNewContactPayloadLastName :: Maybe Text -- ^ Contact last name
  , webhookNewContactPayloadCompany :: Maybe Text -- ^ Contact company name
  , webhookNewContactPayloadPrimaryEmailAddress :: Maybe Text -- ^ Primary email address for contact
  , webhookNewContactPayloadEmailAddresses :: [Text] -- ^ Email addresses for contact
  , webhookNewContactPayloadTags :: [Text] -- ^ Tags for contact
  , webhookNewContactPayloadMetaData :: Maybe Value -- ^ 
  , webhookNewContactPayloadOptOut :: Bool -- ^ Has contact opted out of emails
  , webhookNewContactPayloadCreatedAt :: UTCTime -- ^ Date time of event creation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewContactPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewContactPayload")
instance ToJSON WebhookNewContactPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewContactPayload")


-- | NEW_EMAIL webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to. Use the email ID to fetch the full email body or attachments.
data WebhookNewEmailPayload = WebhookNewEmailPayload
  { webhookNewEmailPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewEmailPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookNewEmailPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewEmailPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookNewEmailPayloadInboxId :: UUID -- ^ Id of the inbox
  , webhookNewEmailPayloadDomainId :: Maybe UUID -- ^ Id of the domain that received an email
  , webhookNewEmailPayloadEmailId :: UUID -- ^ ID of the email that was received. Use this ID for fetching the email with the `EmailController`.
  , webhookNewEmailPayloadCreatedAt :: UTCTime -- ^ Date time of event creation
  , webhookNewEmailPayloadTo :: [Text] -- ^ List of `To` recipient email addresses that the email was addressed to. See recipients object for names.
  , webhookNewEmailPayloadFrom :: Text -- ^ Who the email was sent from. An email address - see fromName for the sender name.
  , webhookNewEmailPayloadCc :: [Text] -- ^ List of `CC` recipients email addresses that the email was addressed to. See recipients object for names.
  , webhookNewEmailPayloadBcc :: [Text] -- ^ List of `BCC` recipients email addresses that the email was addressed to. See recipients object for names.
  , webhookNewEmailPayloadSubject :: Maybe Text -- ^ The subject line of the email message as specified by SMTP subject header
  , webhookNewEmailPayloadAttachmentMetaDatas :: [AttachmentMetaData] -- ^ List of attachment meta data objects if attachments present
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewEmailPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewEmailPayload")
instance ToJSON WebhookNewEmailPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewEmailPayload")


-- | NEW_SMS webhook payload. Sent to your webhook url endpoint via HTTP POST when an sms is received by the phone number that your webhook is attached to. Use the SMS ID to fetch the full SMS details.
data WebhookNewSmsPayload = WebhookNewSmsPayload
  { webhookNewSmsPayloadMessageId :: Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewSmsPayloadWebhookId :: UUID -- ^ ID of webhook entity being triggered
  , webhookNewSmsPayloadEventName :: Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewSmsPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  , webhookNewSmsPayloadSmsId :: UUID -- ^ ID of SMS message
  , webhookNewSmsPayloadUserId :: UUID -- ^ User ID of event
  , webhookNewSmsPayloadPhoneNumber :: UUID -- ^ ID of phone number receiving SMS
  , webhookNewSmsPayloadToNumber :: Text -- ^ Recipient phone number
  , webhookNewSmsPayloadFromNumber :: Text -- ^ Sender phone number
  , webhookNewSmsPayloadBody :: Text -- ^ SMS message body
  , webhookNewSmsPayloadRead :: Bool -- ^ SMS has been read
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewSmsPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewSmsPayload")
instance ToJSON WebhookNewSmsPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewSmsPayload")


-- | Representation of a webhook
data WebhookProjection = WebhookProjection
  { webhookProjectionUrl :: Text -- ^ 
  , webhookProjectionPassword :: Maybe Text -- ^ 
  , webhookProjectionUsername :: Maybe Text -- ^ 
  , webhookProjectionUserId :: UUID -- ^ 
  , webhookProjectionInboxId :: Maybe UUID -- ^ 
  , webhookProjectionEventName :: Maybe Text -- ^ 
  , webhookProjectionUpdatedAt :: UTCTime -- ^ 
  , webhookProjectionCreatedAt :: UTCTime -- ^ 
  , webhookProjectionHealthStatus :: Maybe Text -- ^ 
  , webhookProjectionPhoneNumberId :: Maybe UUID -- ^ 
  , webhookProjectionName :: Maybe Text -- ^ 
  , webhookProjectionId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookProjection")
instance ToJSON WebhookProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookProjection")


-- | Result of retrying all failed webhook
data WebhookRedriveAllResult = WebhookRedriveAllResult
  { webhookRedriveAllResultSuccess :: Bool -- ^ 
  , webhookRedriveAllResultMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookRedriveAllResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookRedriveAllResult")
instance ToJSON WebhookRedriveAllResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookRedriveAllResult")


-- | Result of retrying webhook
data WebhookRedriveResult = WebhookRedriveResult
  { webhookRedriveResultWebhookResultId :: UUID -- ^ 
  , webhookRedriveResultSuccess :: Bool -- ^ 
  , webhookRedriveResultMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookRedriveResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookRedriveResult")
instance ToJSON WebhookRedriveResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookRedriveResult")


-- | Result of a webhook notification
data WebhookResultDto = WebhookResultDto
  { webhookResultDtoId :: Maybe UUID -- ^ 
  , webhookResultDtoUserId :: UUID -- ^ 
  , webhookResultDtoWebhookId :: UUID -- ^ 
  , webhookResultDtoWebhookUrl :: Text -- ^ 
  , webhookResultDtoMessageId :: Text -- ^ 
  , webhookResultDtoRedriveId :: Maybe UUID -- ^ 
  , webhookResultDtoHttpMethod :: Text -- ^ 
  , webhookResultDtoWebhookEvent :: Text -- ^ 
  , webhookResultDtoResponseStatus :: Maybe Int -- ^ 
  , webhookResultDtoResponseTimeMillis :: Integer -- ^ 
  , webhookResultDtoResponseBodyExtract :: Maybe Text -- ^ 
  , webhookResultDtoResultType :: Maybe Text -- ^ 
  , webhookResultDtoCreatedAt :: UTCTime -- ^ 
  , webhookResultDtoUpdatedAt :: UTCTime -- ^ 
  , webhookResultDtoSeen :: Bool -- ^ 
  , webhookResultDtoInboxId :: Maybe UUID -- ^ 
  , webhookResultDtoEmailId :: Maybe UUID -- ^ 
  , webhookResultDtoAttachmentId :: Maybe UUID -- ^ 
  , webhookResultDtoPhoneId :: Maybe UUID -- ^ 
  , webhookResultDtoSmsId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookResultDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookResultDto")
instance ToJSON WebhookResultDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookResultDto")


-- | Result of webhook test request
data WebhookTestRequest = WebhookTestRequest
  { webhookTestRequestUrl :: Text -- ^ 
  , webhookTestRequestMethod :: Text -- ^ 
  , webhookTestRequestHeaders :: (Map.Map String Text) -- ^ 
  , webhookTestRequestPayload :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTestRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTestRequest")
instance ToJSON WebhookTestRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTestRequest")


-- | Response from webhook test request
data WebhookTestResponse = WebhookTestResponse
  { webhookTestResponseStatusCode :: Maybe Int -- ^ 
  , webhookTestResponseMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTestResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTestResponse")
instance ToJSON WebhookTestResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTestResponse")


-- | Results of testing a webhook
data WebhookTestResult = WebhookTestResult
  { webhookTestResultMessage :: Maybe Text -- ^ 
  , webhookTestResultResponse :: WebhookTestResponse -- ^ 
  , webhookTestResultRequest :: WebhookTestRequest -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTestResult")
instance ToJSON WebhookTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTestResult")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("~=", "'Tilde_Equal")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
