{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module MailSlurp.Types (
  AbstractWebhookPayload (..),
  Alias (..),
  AliasDto (..),
  AliasProjection (..),
  AttachmentMetaData (..),
  AttachmentProjection (..),
  BasicAuthOptions (..),
  BulkSendEmailOptions (..),
  ConditionOption (..),
  ContactDto (..),
  ContactProjection (..),
  ContentMatchOptions (..),
  CreateAliasOptions (..),
  CreateContactOptions (..),
  CreateDomainOptions (..),
  CreateGroupOptions (..),
  CreateInboxDto (..),
  CreateInboxForwarderOptions (..),
  CreateInboxRulesetOptions (..),
  CreateTemplateOptions (..),
  CreateTrackingPixelOptions (..),
  CreateWebhookOptions (..),
  DNSLookupOptions (..),
  DNSLookupResult (..),
  DNSLookupResults (..),
  DescribeDomainOptions (..),
  DescribeMailServerDomainResult (..),
  DomainDto (..),
  DomainNameRecord (..),
  DomainPreview (..),
  DownloadAttachmentDto (..),
  Email (..),
  EmailAnalysis (..),
  EmailContentMatchResult (..),
  EmailPreview (..),
  EmailProjection (..),
  EmailTextLinesResult (..),
  EmailVerificationResult (..),
  ExpirationDefaults (..),
  ExpiredInboxDto (..),
  ExpiredInboxRecordProjection (..),
  ExportLink (..),
  ExportOptions (..),
  ForwardEmailOptions (..),
  GroupContactsDto (..),
  GroupDto (..),
  GroupProjection (..),
  HTMLValidationResult (..),
  IPAddressResult (..),
  Inbox (..),
  InboxForwarderDto (..),
  InboxForwarderTestOptions (..),
  InboxForwarderTestResult (..),
  InboxProjection (..),
  InboxRulesetDto (..),
  InboxRulesetTestOptions (..),
  InboxRulesetTestResult (..),
  MatchOption (..),
  MatchOptions (..),
  MissedEmail (..),
  MissedEmailProjection (..),
  NameServerRecord (..),
  OrganizationInboxProjection (..),
  PageAlias (..),
  PageAttachmentEntity (..),
  PageContactProjection (..),
  PageEmailPreview (..),
  PageEmailProjection (..),
  PageExpiredInboxRecordProjection (..),
  PageGroupProjection (..),
  PageInboxForwarderDto (..),
  PageInboxProjection (..),
  PageInboxRulesetDto (..),
  PageMissedEmailProjection (..),
  PageOrganizationInboxProjection (..),
  PageSentEmailProjection (..),
  PageTemplateProjection (..),
  PageThreadProjection (..),
  PageTrackingPixelProjection (..),
  PageWebhookProjection (..),
  PageWebhookResult (..),
  Pageable (..),
  RawEmailJson (..),
  ReplyToAliasEmailOptions (..),
  ReplyToEmailOptions (..),
  SendEmailOptions (..),
  SentEmailDto (..),
  SentEmailProjection (..),
  SetInboxFavouritedOptions (..),
  SimpleSendEmailOptions (..),
  Sort (..),
  TemplateDto (..),
  TemplateProjection (..),
  TemplateVariable (..),
  TestNewInboxForwarderOptions (..),
  TestNewInboxRulesetOptions (..),
  ThreadProjection (..),
  TrackingPixelDto (..),
  TrackingPixelProjection (..),
  UnreadCount (..),
  UpdateAliasOptions (..),
  UpdateDomainOptions (..),
  UpdateGroupContacts (..),
  UpdateInboxOptions (..),
  UploadAttachmentOptions (..),
  ValidationDto (..),
  ValidationMessage (..),
  VerifyEmailAddressOptions (..),
  WaitForConditions (..),
  WebhookDto (..),
  WebhookEmailOpenedPayload (..),
  WebhookNewAttachmentPayload (..),
  WebhookNewContactPayload (..),
  WebhookNewEmailPayload (..),
  WebhookProjection (..),
  WebhookResultEntity (..),
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
data Alias = Alias
  { aliasCreatedAt :: UTCTime -- ^ 
  , aliasEmailAddress :: Text -- ^ 
  , aliasId :: Maybe UUID -- ^ 
  , aliasInboxId :: UUID -- ^ 
  , aliasName :: Maybe Text -- ^ 
  , aliasUpdatedAt :: UTCTime -- ^ 
  , aliasUseThreads :: Maybe Bool -- ^ 
  , aliasUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Alias where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "alias")
instance ToJSON Alias where
  toJSON = genericToJSON (removeFieldLabelPrefix False "alias")


-- | Email alias representation
data AliasDto = AliasDto
  { aliasDtoCreatedAt :: Maybe UTCTime -- ^ 
  , aliasDtoEmailAddress :: Maybe Text -- ^ The alias's email address for receiving email
  , aliasDtoId :: UUID -- ^ 
  , aliasDtoInboxId :: Maybe UUID -- ^ Inbox that is associated with the alias
  , aliasDtoIsVerified :: Maybe Bool -- ^ Has the alias been verified. You must verify an alias if the masked email address has not yet been verified by your account
  , aliasDtoMaskedEmailAddress :: Maybe Text -- ^ The underlying email address that is hidden and will received forwarded email
  , aliasDtoName :: Maybe Text -- ^ 
  , aliasDtoUpdatedAt :: Maybe UTCTime -- ^ 
  , aliasDtoUseThreads :: Maybe Bool -- ^ If alias will generate response threads or not when email are received by it
  , aliasDtoUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasDto")
instance ToJSON AliasDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasDto")


-- | Representation of a alias
data AliasProjection = AliasProjection
  { aliasProjectionCreatedAt :: UTCTime -- ^ 
  , aliasProjectionEmailAddress :: Text -- ^ 
  , aliasProjectionId :: UUID -- ^ 
  , aliasProjectionInboxId :: UUID -- ^ 
  , aliasProjectionName :: Maybe Text -- ^ 
  , aliasProjectionUpdatedAt :: UTCTime -- ^ 
  , aliasProjectionUseThreads :: Maybe Bool -- ^ 
  , aliasProjectionUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasProjection")
instance ToJSON AliasProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasProjection")


-- | Meta data associated with an attachment. Attachments are stored as byte blobs so the meta data is stored separately.
data AttachmentMetaData = AttachmentMetaData
  { attachmentMetaDataContentLength :: Maybe Integer -- ^ Size of attachment in bytes
  , attachmentMetaDataContentType :: Maybe Text -- ^ Content type of attachment such as `image/png`
  , attachmentMetaDataId :: Maybe Text -- ^ ID of attachment
  , attachmentMetaDataName :: Maybe Text -- ^ Name of attachment if given
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentMetaData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentMetaData")
instance ToJSON AttachmentMetaData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentMetaData")


-- | 
data AttachmentProjection = AttachmentProjection
  { attachmentProjectionAttachmentId :: Maybe Text -- ^ Attachment ID
  , attachmentProjectionContentLength :: Maybe Integer -- ^ Content length of attachment in bytes
  , attachmentProjectionContentType :: Maybe Text -- ^ Content type of attachment.
  , attachmentProjectionCreatedAt :: UTCTime -- ^ 
  , attachmentProjectionName :: Maybe Text -- ^ 
  , attachmentProjectionUpdatedAt :: UTCTime -- ^ 
  , attachmentProjectionUserId :: UUID -- ^ 
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


-- | Options for bulk sending an email from multiple addresses. See regular &#x60;sendEmail&#x60; methods for more information.
data BulkSendEmailOptions = BulkSendEmailOptions
  { bulkSendEmailOptionsInboxIds :: Maybe [UUID] -- ^ Inboxes to send the email from
  , bulkSendEmailOptionsSendEmailOptions :: Maybe SendEmailOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BulkSendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bulkSendEmailOptions")
instance ToJSON BulkSendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bulkSendEmailOptions")


-- | Options for matching emails in an inbox based on a condition such as &#x60;HAS_ATTACHMENTS&#x3D;TRUE&#x60;
data ConditionOption = ConditionOption
  { conditionOptionCondition :: Maybe Text -- ^ The condition to evaluate against the email
  , conditionOptionValue :: Maybe Text -- ^ What the condition should evaluate to. A string 'TRUE|FALSE' not a boolean.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConditionOption where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "conditionOption")
instance ToJSON ConditionOption where
  toJSON = genericToJSON (removeFieldLabelPrefix False "conditionOption")


-- | 
data ContactDto = ContactDto
  { contactDtoCompany :: Maybe Text -- ^ 
  , contactDtoCreatedAt :: UTCTime -- ^ 
  , contactDtoEmailAddresses :: [Text] -- ^ 
  , contactDtoFirstName :: Maybe Text -- ^ 
  , contactDtoGroupId :: Maybe UUID -- ^ 
  , contactDtoId :: UUID -- ^ 
  , contactDtoLastName :: Maybe Text -- ^ 
  , contactDtoMetaData :: Maybe Value -- ^ 
  , contactDtoOptOut :: Maybe Bool -- ^ 
  , contactDtoPrimaryEmailAddress :: Maybe Text -- ^ 
  , contactDtoTags :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactDto")
instance ToJSON ContactDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactDto")


-- | 
data ContactProjection = ContactProjection
  { contactProjectionCompany :: Maybe Text -- ^ 
  , contactProjectionCreatedAt :: UTCTime -- ^ 
  , contactProjectionEmailAddresses :: Maybe [Text] -- ^ 
  , contactProjectionFirstName :: Maybe Text -- ^ 
  , contactProjectionGroupId :: Maybe UUID -- ^ 
  , contactProjectionId :: UUID -- ^ 
  , contactProjectionLastName :: Maybe Text -- ^ 
  , contactProjectionOptOut :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactProjection")
instance ToJSON ContactProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactProjection")


-- | Options for matching content using regex patterns based on Java Pattern syntax
data ContentMatchOptions = ContentMatchOptions
  { contentMatchOptionsPattern :: Maybe Text -- ^ Java style regex pattern. Do not include the typical `/` at start or end of regex in some languages. Given an example `your code is: 12345` the pattern to extract match looks like `code is: (\\d{6})`. This will return an array of matches with the first matching the entire pattern and the subsequent matching the groups: `['code is: 123456', '123456']` See https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html for more information of available patterns.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContentMatchOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contentMatchOptions")
instance ToJSON ContentMatchOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contentMatchOptions")


-- | Create email alias options. Email aliases can be used to mask real email addresses behind an ID. You can also attach an inbox to an alias so that any email received by the inbox email address if forwarded to the alias email address.
data CreateAliasOptions = CreateAliasOptions
  { createAliasOptionsEmailAddress :: Maybe Text -- ^ Email address to be hidden behind alias. Emails sent to the alias email address will be forwarded to this address. If you want to enable replies set useThreads true and the reply-to for the email will allow outbound communication via a thread.
  , createAliasOptionsInboxId :: Maybe UUID -- ^ Optional inbox ID to attach to alias. Null by default means an a new inbox will be created for the alias. Use a custom inbox to control what email address the alias uses. To use custom email addresses create a domain and an inbox, the use the inbox ID with this call. Emails received by this inbox will be forwarded to the alias email address
  , createAliasOptionsName :: Maybe Text -- ^ Optional name for alias
  , createAliasOptionsUseThreads :: Maybe Bool -- ^ Enable threads options. If true emails will be sent with a unique reply-to thread address. This means you can reply to the forwarded email and it will be sent to the recipients via your alias address. That way a thread conversation is preserved.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateAliasOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createAliasOptions")
instance ToJSON CreateAliasOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createAliasOptions")


-- | 
data CreateContactOptions = CreateContactOptions
  { createContactOptionsEmailAddresses :: Maybe [Text] -- ^ Set of email addresses belonging to the contact
  , createContactOptionsFirstName :: Maybe Text -- ^ 
  , createContactOptionsGroupId :: Maybe UUID -- ^ Group IDs that contact belongs to
  , createContactOptionsMetaData :: Maybe Value -- ^ 
  , createContactOptionsOptOut :: Maybe Bool -- ^ Has the user explicitly or implicitly opted out of being contacted? If so MailSlurp will ignore them in all actions.
  , createContactOptionsTags :: Maybe [Text] -- ^ Tags that can be used to search and group contacts
  , createContactOptionsLastName :: Maybe Text -- ^ 
  , createContactOptionsCompany :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createContactOptions")
instance ToJSON CreateContactOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createContactOptions")


-- | Options for creating a domain to use with MailSlurp. You must have ownership access to this domain in order to verify it. Domains will not function correctly until the domain has been verified. See https://www.mailslurp.com/guides/custom-domains for help. Domains can be either &#x60;HTTP&#x60; or &#x60;SMTP&#x60; type. The type of domain determines which inboxes can be used with it. &#x60;SMTP&#x60; inboxes use a mail server running &#x60;mx.mailslurp.com&#x60; while &#x60;HTTP&#x60; inboxes are handled by AWS SES.
data CreateDomainOptions = CreateDomainOptions
  { createDomainOptionsCreatedCatchAllInbox :: Maybe Bool -- ^ Whether to create a catch all inbox for the domain. Any email sent to an address using your domain that cannot be matched to an existing inbox you created with the domain will be routed to the created catch all inbox. You can access emails using the regular methods on this inbox ID.
  , createDomainOptionsDescription :: Maybe Text -- ^ Optional description of the domain.
  , createDomainOptionsDomain :: Maybe Text -- ^ The top level domain you wish to use with MailSlurp. Do not specify subdomain just the top level. So `test.com` covers all subdomains such as `mail.test.com`. Don't include a protocol such as `http://`. Once added you must complete the verification steps by adding the returned records to your domain.
  , createDomainOptionsDomainType :: Maybe Text -- ^ Domain type to create. HTTP or SMTP domain. HTTP domain uses MailSlurps SES MX records. SMTP uses a custom SMTP server MX record. SMTP domains can only be used with SMTP inboxes. SMTP inboxes are more reliable for public inbound emails while HTTP inboxes are more suitable for testing.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDomainOptions")
instance ToJSON CreateDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDomainOptions")


-- | 
data CreateGroupOptions = CreateGroupOptions
  { createGroupOptionsName :: Text -- ^ 
  , createGroupOptionsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupOptions")
instance ToJSON CreateGroupOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupOptions")


-- | Options for creating an inbox. An inbox has a real email address that can send and receive emails. Inboxes can be permanent or expire at a given time. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. Use &#x60;SMTP&#x60; for public facing mailboxes and &#x60;HTTP&#x60; for test email accounts. &#x60;SMTP&#x60; inboxes are processed by a mail server running at &#x60;mx.mailslurp.com&#x60; while &#x60;HTTP&#x60; inboxes are processed by AWS SES. Inboxes can use a custom email address (by verifying your own domain) or a randomly assigned email ending in either &#x60;mailslurp.com&#x60; or (if &#x60;useDomainPool&#x60; is enabled) ending in a similar domain such as &#x60;mailslurp.xyz&#x60; (selected at random). 
data CreateInboxDto = CreateInboxDto
  { createInboxDtoAllowTeamAccess :: Maybe Bool -- ^ Grant team access to this inbox and the emails that belong to it for team members of your organization.
  , createInboxDtoDescription :: Maybe Text -- ^ Optional description of the inbox for labelling purposes. Is shown in the dashboard and can be used with
  , createInboxDtoEmailAddress :: Maybe Text -- ^ A custom email address to use with the inbox. Defaults to null. When null MailSlurp will assign a random email address to the inbox such as `123@mailslurp.com`. If you use the `useDomainPool` option when the email address is null it will generate an email address with a more varied domain ending such as `123@mailslurp.info` or `123@mailslurp.biz`. When a custom email address is provided the address is split into a domain and the domain is queried against your user. If you have created the domain in the MailSlurp dashboard and verified it you can use any email address that ends with the domain. Note domain types must match the inbox type - so `SMTP` inboxes will only work with `SMTP` type domains. Send an email to this address and the inbox will receive and store it for you. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , createInboxDtoExpiresAt :: Maybe UTCTime -- ^ Optional inbox expiration date. If null then this inbox is permanent and the emails in it won't be deleted. If an expiration date is provided or is required by your plan the inbox will be closed when the expiration time is reached. Expired inboxes still contain their emails but can no longer send or receive emails. An ExpiredInboxRecord is created when an inbox and the email address and inbox ID are recorded. The expiresAt property is a timestamp string in ISO DateTime Format yyyy-MM-dd'T'HH:mm:ss.SSSXXX.
  , createInboxDtoExpiresIn :: Maybe Integer -- ^ Number of milliseconds that inbox should exist for
  , createInboxDtoFavourite :: Maybe Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  , createInboxDtoInboxType :: Maybe Text -- ^ HTTP (default) or SMTP inbox type. HTTP inboxes are best for testing while SMTP inboxes are more reliable for public inbound email consumption. When using custom domains the domain type must match the inbox type. HTTP inboxes are processed by AWS SES while SMTP inboxes use a custom mail server running at `mx.mailslurp.com`.
  , createInboxDtoName :: Maybe Text -- ^ Optional name of the inbox. Displayed in the dashboard for easier search and used as the sender name when sending emails.
  , createInboxDtoTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , createInboxDtoUseDomainPool :: Maybe Bool -- ^ Use the MailSlurp domain name pool with this inbox when creating the email address. Defaults to null. If enabled the inbox will be an email address with a domain randomly chosen from a list of the MailSlurp domains. This is useful when the default `@mailslurp.com` email addresses used with inboxes are blocked or considered spam by a provider or receiving service. When domain pool is enabled an email address will be generated ending in `@mailslurp.{world,info,xyz,...}` . This means a TLD is randomly selecting from a list of `.biz`, `.info`, `.xyz` etc to add variance to the generated email addresses. When null or false MailSlurp uses the default behavior of `@mailslurp.com` or custom email address provided by the emailAddress field. Note this feature is only available for `HTTP` inbox types.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxDto")
instance ToJSON CreateInboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxDto")


-- | 
data CreateInboxForwarderOptions = CreateInboxForwarderOptions
  { createInboxForwarderOptionsField :: Text -- ^ 
  , createInboxForwarderOptionsMatch :: Text -- ^ 
  , createInboxForwarderOptionsForwardToRecipients :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxForwarderOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxForwarderOptions")
instance ToJSON CreateInboxForwarderOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxForwarderOptions")


-- | Options for creating inbox rulesets. Inbox rulesets can be used to block, allow, filter, or forward emails when sending or receiving using the inbox.
data CreateInboxRulesetOptions = CreateInboxRulesetOptions
  { createInboxRulesetOptionsAction :: Maybe Text -- ^ Action to be taken when the ruleset matches an email for the given scope. For example: `BLOCK` action with target `*` and scope `SENDING_EMAILS` blocks sending to all recipients. Note `ALLOW` takes precedent over `BLOCK`. `FILTER_REMOVE` is like block but will remove offending email addresses during a send or receive event instead of blocking the action.
  , createInboxRulesetOptionsScope :: Maybe Text -- ^ What type of emails actions to apply ruleset to. Either `SENDING_EMAILS` or `RECEIVING_EMAILS` will apply action and target to any sending or receiving of emails respectively.
  , createInboxRulesetOptionsTarget :: Maybe Text -- ^ Target to match emails with. Can be a wild-card type pattern or a valid email address. For instance `*@gmail.com` matches all gmail addresses while `test@gmail.com` matches one address exactly. The target is applied to every recipient field email address when `SENDING_EMAILS` is the scope and is applied to sender of email when `RECEIVING_EMAILS`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateInboxRulesetOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createInboxRulesetOptions")
instance ToJSON CreateInboxRulesetOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createInboxRulesetOptions")


-- | 
data CreateTemplateOptions = CreateTemplateOptions
  { createTemplateOptionsName :: Text -- ^ 
  , createTemplateOptionsContent :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTemplateOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTemplateOptions")
instance ToJSON CreateTemplateOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTemplateOptions")


-- | 
data CreateTrackingPixelOptions = CreateTrackingPixelOptions
  { createTrackingPixelOptionsName :: Maybe Text -- ^ 
  , createTrackingPixelOptionsRecipient :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTrackingPixelOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTrackingPixelOptions")
instance ToJSON CreateTrackingPixelOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTrackingPixelOptions")


-- | Options for creating a webhook. Webhooks can be attached to inboxes and MailSlurp will POST a webhook payload to the URL specified whenever the inbox receives an email. Webhooks are great for processing many inbound emails.
data CreateWebhookOptions = CreateWebhookOptions
  { createWebhookOptionsBasicAuth :: Maybe BasicAuthOptions -- ^ 
  , createWebhookOptionsEventName :: Maybe Text -- ^ Optional webhook event name. Default is `EMAIL_RECEIVED` and is triggered when an email is received by the inbox associated with the webhook. Payload differ according to the webhook event name. The other events are `NEW_EMAIL`, `NEW_CONTACT`, and `NEW_ATTACHMENT` and `EMAIL_OPENED`. `EMAIL_OPENED` requires the use of tracking pixels when sending. See the email tracking guide for more information.
  , createWebhookOptionsName :: Maybe Text -- ^ Optional name for the webhook
  , createWebhookOptionsUrl :: Maybe Text -- ^ Public URL on your server that MailSlurp can post WebhookNotification payload to when an email is received or an event is trigger. The payload of the submitted JSON is dependent on the webhook event type. The default `EMAIL_RECEIVED` payload is described by `https://api.mailslurp.com/schemas/webhook-payload`. The other events, `NEW_EMAIL`, `NEW_CONTACT`, and `NEW_ATTACHMENT` are described by `https://api.mailslurp.com/schemas/webhook-new-email-payload`, `https://api.mailslurp.com/schemas/webhook-new-contact-payload`,`https://api.mailslurp.com/schemas/webhook-new-attachment-payload` respectively.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateWebhookOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createWebhookOptions")
instance ToJSON CreateWebhookOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createWebhookOptions")


-- | Options for DNS query. 
data DNSLookupOptions = DNSLookupOptions
  { dNSLookupOptionsHostname :: Maybe Text -- ^ List of record types you wish to query such as MX, DNS, TXT, NS, A etc.
  , dNSLookupOptionsOmitFinalDNSDot :: Maybe Bool -- ^ Optionally control whether to omit the final dot in full DNS name values.
  , dNSLookupOptionsRecordTypes :: Maybe [Text] -- ^ List of record types you wish to query such as MX, DNS, TXT, NS, A etc.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DNSLookupOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dNSLookupOptions")
instance ToJSON DNSLookupOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dNSLookupOptions")


-- | DNS lookup result. Includes record type, time to live, raw response, and name value for the name server response.
data DNSLookupResult = DNSLookupResult
  { dNSLookupResultName :: Text -- ^ 
  , dNSLookupResultRecordEntries :: [Text] -- ^ 
  , dNSLookupResultRecordType :: Text -- ^ 
  , dNSLookupResultTtl :: Integer -- ^ 
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


-- | 
data DescribeDomainOptions = DescribeDomainOptions
  { describeDomainOptionsDomain :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DescribeDomainOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "describeDomainOptions")
instance ToJSON DescribeDomainOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "describeDomainOptions")


-- | Name Server lookup result
data DescribeMailServerDomainResult = DescribeMailServerDomainResult
  { describeMailServerDomainResultDomain :: Text -- ^ 
  , describeMailServerDomainResultMessage :: Maybe Text -- ^ 
  , describeMailServerDomainResultMxRecords :: [NameServerRecord] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DescribeMailServerDomainResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "describeMailServerDomainResult")
instance ToJSON DescribeMailServerDomainResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "describeMailServerDomainResult")


-- | Domain plus verification records and status
data DomainDto = DomainDto
  { domainDtoCatchAllInboxId :: Maybe UUID -- ^ The optional catch all inbox that will receive emails sent to the domain that cannot be matched.
  , domainDtoCreatedAt :: UTCTime -- ^ 
  , domainDtoDkimTokens :: Maybe [Text] -- ^ Unique token DKIM tokens
  , domainDtoDomain :: Maybe Text -- ^ Custom domain name
  , domainDtoDomainNameRecords :: Maybe [DomainNameRecord] -- ^ List of DNS domain name records (C, MX, TXT) etc that you must add to the DNS server associated with your domain provider.
  , domainDtoDomainType :: Maybe Text -- ^ The type of domain. SMTP or HTTP domains differ in what inboxes can be used with them.
  , domainDtoId :: UUID -- ^ 
  , domainDtoIsVerified :: Maybe Bool -- ^ Whether domain has been verified or not. If the domain is not verified after 72 hours there is most likely an issue with the domains DNS records.
  , domainDtoUpdatedAt :: UTCTime -- ^ 
  , domainDtoUserId :: UUID -- ^ 
  , domainDtoVerificationToken :: Maybe Text -- ^ Verification tokens
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainDto")
instance ToJSON DomainDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainDto")


-- | DNS Record required for verification of a domain. Record vary depending on domain type.
data DomainNameRecord = DomainNameRecord
  { domainNameRecordName :: Text -- ^ 
  , domainNameRecordRecordEntries :: [Text] -- ^ 
  , domainNameRecordRecordType :: Text -- ^ 
  , domainNameRecordTtl :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainNameRecord where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainNameRecord")
instance ToJSON DomainNameRecord where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainNameRecord")


-- | Preview object for domain entity
data DomainPreview = DomainPreview
  { domainPreviewCatchAllInboxId :: Maybe UUID -- ^ 
  , domainPreviewCreatedAt :: UTCTime -- ^ 
  , domainPreviewDomain :: Text -- ^ 
  , domainPreviewDomainType :: Text -- ^ 
  , domainPreviewId :: UUID -- ^ 
  , domainPreviewIsVerified :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DomainPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainPreview")
instance ToJSON DomainPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainPreview")


-- | Content of attachment
data DownloadAttachmentDto = DownloadAttachmentDto
  { downloadAttachmentDtoBase64FileContents :: Maybe Text -- ^ Base64 encoded string of attachment bytes. Decode the base64 encoded string to get the raw contents. If the file has a content type such as `text/html` you can read the contents directly by converting it to string using `utf-8` encoding.
  , downloadAttachmentDtoContentType :: Maybe Text -- ^ Content type of attachment. Examples are `image/png`, `application/msword`, `text/csv` etc.
  , downloadAttachmentDtoSizeBytes :: Maybe Integer -- ^ Size in bytes of attachment content
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DownloadAttachmentDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "downloadAttachmentDto")
instance ToJSON DownloadAttachmentDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "downloadAttachmentDto")


-- | Email entity (also known as EmailDto). When an SMTP email message is received by MailSlurp it is parsed. The body and attachments are written to disk and the fields such as to, from, subject etc are stored in a database. The &#x60;body&#x60; contains the email content. If you want the original SMTP message see the &#x60;getRawEmail&#x60; endpoints. The attachments can be fetched using the AttachmentController
data Email = Email
  { emailAnalysis :: Maybe EmailAnalysis -- ^ 
  , emailAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  , emailBcc :: Maybe [Text] -- ^ List of `BCC` recipients email was addressed to
  , emailBody :: Maybe Text -- ^ The body of the email message
  , emailBodyExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message
  , emailBodyMD5Hash :: Maybe Text -- ^ A hash signature of the email message
  , emailCc :: Maybe [Text] -- ^ List of `CC` recipients email was addressed to
  , emailCharset :: Maybe Text -- ^ Detected character set of the email body such as UTF-8
  , emailCreatedAt :: Maybe UTCTime -- ^ When was the email received by MailSlurp
  , emailFrom :: Maybe Text -- ^ Who the email was sent from
  , emailHeaders :: Maybe (Map.Map String Text) -- ^ Collection of SMTP headers attached to email
  , emailId :: Maybe UUID -- ^ ID of the email entity
  , emailInboxId :: Maybe UUID -- ^ ID of the inbox that received the email
  , emailIsHTML :: Maybe Bool -- ^ Is the email body HTML
  , emailRead :: Maybe Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API? If so the email is marked as read.
  , emailReplyTo :: Maybe Text -- ^ The `replyTo` field on the received email message
  , emailSubject :: Maybe Text -- ^ The subject line of the email message
  , emailTeamAccess :: Maybe Bool -- ^ Can the email be accessed by organization team members
  , emailTo :: Maybe [Text] -- ^ List of `To` recipients that email was addressed to
  , emailUpdatedAt :: Maybe UTCTime -- ^ When was the email last updated
  , emailUserId :: Maybe UUID -- ^ ID of user that email belongs to
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "email")
instance ToJSON Email where
  toJSON = genericToJSON (removeFieldLabelPrefix False "email")


-- | Analysis result for email. Each verdict property is a string PASS|FAIL|GRAY or dynamic error message
data EmailAnalysis = EmailAnalysis
  { emailAnalysisDkimVerdict :: Maybe Text -- ^ Verdict of DomainKeys Identified Mail analysis
  , emailAnalysisDmarcVerdict :: Maybe Text -- ^ Verdict of Domain-based Message Authentication Reporting and Conformance analysis
  , emailAnalysisSpamVerdict :: Maybe Text -- ^ Verdict of spam ranking analysis
  , emailAnalysisSpfVerdict :: Maybe Text -- ^ Verdict of Send Policy Framework record spoofing analysis
  , emailAnalysisVirusVerdict :: Maybe Text -- ^ Verdict of virus scan analysis
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailAnalysis where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailAnalysis")
instance ToJSON EmailAnalysis where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailAnalysis")


-- | Matches for the given pattern
data EmailContentMatchResult = EmailContentMatchResult
  { emailContentMatchResultMatches :: [Text] -- ^ 
  , emailContentMatchResultPattern :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailContentMatchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailContentMatchResult")
instance ToJSON EmailContentMatchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailContentMatchResult")


-- | Preview of an email message. For full message (including body and attachments) call the &#x60;getEmail&#x60; or other email endpoints with the provided email ID.
data EmailPreview = EmailPreview
  { emailPreviewAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  , emailPreviewBcc :: Maybe [Text] -- ^ List of `BCC` recipients email was addressed to
  , emailPreviewCc :: Maybe [Text] -- ^ List of `CC` recipients email was addressed to
  , emailPreviewCreatedAt :: Maybe UTCTime -- ^ When was the email received by MailSlurp
  , emailPreviewFrom :: Maybe Text -- ^ Who the email was sent from
  , emailPreviewId :: Maybe UUID -- ^ ID of the email entity
  , emailPreviewRead :: Maybe Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API? If so the email is marked as read.
  , emailPreviewSubject :: Maybe Text -- ^ The subject line of the email message
  , emailPreviewTo :: Maybe [Text] -- ^ List of `To` recipients that email was addressed to
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailPreview")
instance ToJSON EmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailPreview")


-- | A compact representation of a full email. Used in list endpoints to keep response sizes low. Body and attachments are not included. To get all fields of the email use the &#x60;getEmail&#x60; method with the email projection&#39;s ID. See &#x60;EmailDto&#x60; for documentation on projection properties.
data EmailProjection = EmailProjection
  { emailProjectionAttachments :: Maybe [Text] -- ^ 
  , emailProjectionBcc :: Maybe [Text] -- ^ 
  , emailProjectionBodyExcerpt :: Maybe Text -- ^ 
  , emailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  , emailProjectionCc :: Maybe [Text] -- ^ 
  , emailProjectionCreatedAt :: UTCTime -- ^ 
  , emailProjectionFrom :: Maybe Text -- ^ 
  , emailProjectionId :: UUID -- ^ 
  , emailProjectionInboxId :: UUID -- ^ 
  , emailProjectionRead :: Maybe Bool -- ^ 
  , emailProjectionSubject :: Maybe Text -- ^ 
  , emailProjectionTeamAccess :: Maybe Bool -- ^ 
  , emailProjectionTo :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailProjection")
instance ToJSON EmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailProjection")


-- | Parsed text of an email
data EmailTextLinesResult = EmailTextLinesResult
  { emailTextLinesResultBody :: Text -- ^ 
  , emailTextLinesResultLines :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailTextLinesResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailTextLinesResult")
instance ToJSON EmailTextLinesResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailTextLinesResult")


-- | Email verification result. Valid means email address exists according to response from mail server running at the domian and port given.
data EmailVerificationResult = EmailVerificationResult
  { emailVerificationResultDomainName :: Text -- ^ 
  , emailVerificationResultEmailAddress :: Text -- ^ 
  , emailVerificationResultError :: Maybe Text -- ^ 
  , emailVerificationResultIsValid :: Bool -- ^ 
  , emailVerificationResultPort :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailVerificationResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailVerificationResult")
instance ToJSON EmailVerificationResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailVerificationResult")


-- | Expiration defaults for your account
data ExpirationDefaults = ExpirationDefaults
  { expirationDefaultsCanPermanentInbox :: Bool -- ^ 
  , expirationDefaultsDefaultExpirationMillis :: Maybe Integer -- ^ 
  , expirationDefaultsDefaultExpiresAt :: Maybe UTCTime -- ^ 
  , expirationDefaultsMaxExpirationMillis :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExpirationDefaults where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "expirationDefaults")
instance ToJSON ExpirationDefaults where
  toJSON = genericToJSON (removeFieldLabelPrefix False "expirationDefaults")


-- | Expired inbox
data ExpiredInboxDto = ExpiredInboxDto
  { expiredInboxDtoEmailAddress :: Text -- ^ 
  , expiredInboxDtoId :: UUID -- ^ 
  , expiredInboxDtoInboxId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExpiredInboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "expiredInboxDto")
instance ToJSON ExpiredInboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "expiredInboxDto")


-- | 
data ExpiredInboxRecordProjection = ExpiredInboxRecordProjection
  { expiredInboxRecordProjectionCreatedAt :: UTCTime -- ^ 
  , expiredInboxRecordProjectionEmailAddress :: Text -- ^ 
  , expiredInboxRecordProjectionId :: UUID -- ^ 
  , expiredInboxRecordProjectionUserId :: UUID -- ^ 
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
  , exportOptionsListSeparatorToken :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportOptions")
instance ToJSON ExportOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportOptions")


-- | Options for forwarding an email
data ForwardEmailOptions = ForwardEmailOptions
  { forwardEmailOptionsBcc :: Maybe [Text] -- ^ Optional bcc recipients
  , forwardEmailOptionsCc :: Maybe [Text] -- ^ Optional cc recipients
  , forwardEmailOptionsFrom :: Maybe Text -- ^ Optional from override
  , forwardEmailOptionsSubject :: Maybe Text -- ^ Subject for forwarded email
  , forwardEmailOptionsTo :: Maybe [Text] -- ^ To recipients for forwarded email
  , forwardEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ForwardEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "forwardEmailOptions")
instance ToJSON ForwardEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "forwardEmailOptions")


-- | 
data GroupContactsDto = GroupContactsDto
  { groupContactsDtoContacts :: [ContactDto] -- ^ 
  , groupContactsDtoGroup :: GroupDto -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupContactsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupContactsDto")
instance ToJSON GroupContactsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupContactsDto")


-- | 
data GroupDto = GroupDto
  { groupDtoCreatedAt :: UTCTime -- ^ 
  , groupDtoDescription :: Maybe Text -- ^ 
  , groupDtoId :: UUID -- ^ 
  , groupDtoName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupDto")
instance ToJSON GroupDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupDto")


-- | 
data GroupProjection = GroupProjection
  { groupProjectionCreatedAt :: UTCTime -- ^ 
  , groupProjectionDescription :: Maybe Text -- ^ 
  , groupProjectionId :: UUID -- ^ 
  , groupProjectionName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupProjection")
instance ToJSON GroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupProjection")


-- | HTML Validation Results
data HTMLValidationResult = HTMLValidationResult
  { hTMLValidationResultErrors :: Maybe [ValidationMessage] -- ^ Optional errors resulting from HTML validation
  , hTMLValidationResultIsValid :: Maybe Bool -- ^ Is HTML validation result valid
  , hTMLValidationResultWarnings :: Maybe [ValidationMessage] -- ^ Optional warnings resulting from HTML validation
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


-- | Representation of a MailSlurp inbox. An inbox has an ID and a real email address. Emails can be sent to or from this email address. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. The default, &#x60;HTTP&#x60; inboxes, use AWS SES to process emails and are best suited as test email accounts. &#x60;SMTP&#x60; inboxes use a custom mail server at &#x60;mx.mailslurp.com&#x60; and are best used for public facing email addresses. Use the &#x60;EmailController&#x60; or the &#x60;InboxController&#x60; methods to send and receive emails and attachments. Inboxes may have a description, name, and tags for display purposes. You can also favourite an inbox for easier searching. Inboxes can be private or allow team access. Team access enabled inboxes can be seen by other members of an organization. 
data Inbox = Inbox
  { inboxCreatedAt :: Maybe UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , inboxDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , inboxEmailAddress :: Maybe Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , inboxExpiresAt :: Maybe Text -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , inboxFavourite :: Maybe Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , inboxId :: Maybe UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , inboxInboxType :: Maybe Text -- ^ Type of inbox - either HTTP (default) or SMTP. HTTP inboxes are great for testing. SMTP inboxes are processed by a custom SMTP mail server and are better for public facing inboxes that receive emails from Gmail and other large providers. If using a custom domain the domain type must match the inbox type. Use an SMTP domain for SMTP inboxes that includes an MX record pointing to `10 mx.mailslurp.com` for inbound messages.
  , inboxName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , inboxReadOnly :: Maybe Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  , inboxTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , inboxTeamAccess :: Maybe Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  , inboxUserId :: Maybe UUID -- ^ ID of user that inbox belongs to
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Inbox where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inbox")
instance ToJSON Inbox where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inbox")


-- | 
data InboxForwarderDto = InboxForwarderDto
  { inboxForwarderDtoCreatedAt :: UTCTime -- ^ 
  , inboxForwarderDtoField :: Text -- ^ 
  , inboxForwarderDtoForwardToRecipients :: [Text] -- ^ 
  , inboxForwarderDtoId :: UUID -- ^ 
  , inboxForwarderDtoInboxId :: UUID -- ^ 
  , inboxForwarderDtoMatch :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderDto")
instance ToJSON InboxForwarderDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderDto")


-- | 
data InboxForwarderTestOptions = InboxForwarderTestOptions
  { inboxForwarderTestOptionsTestValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderTestOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderTestOptions")
instance ToJSON InboxForwarderTestOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderTestOptions")


-- | 
data InboxForwarderTestResult = InboxForwarderTestResult
  { inboxForwarderTestResultDoesMatch :: Bool -- ^ 
  , inboxForwarderTestResultMatches :: (Map.Map String Bool) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderTestResult")
instance ToJSON InboxForwarderTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderTestResult")


-- | 
data InboxProjection = InboxProjection
  { inboxProjectionCreatedAt :: UTCTime -- ^ 
  , inboxProjectionEmailAddress :: Maybe Text -- ^ 
  , inboxProjectionFavourite :: Bool -- ^ 
  , inboxProjectionId :: UUID -- ^ 
  , inboxProjectionInboxType :: Maybe Text -- ^ 
  , inboxProjectionName :: Maybe Text -- ^ 
  , inboxProjectionTags :: Maybe [Text] -- ^ 
  , inboxProjectionTeamAccess :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxProjection")
instance ToJSON InboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxProjection")


-- | 
data InboxRulesetDto = InboxRulesetDto
  { inboxRulesetDtoAction :: Text -- ^ 
  , inboxRulesetDtoCreatedAt :: UTCTime -- ^ 
  , inboxRulesetDtoHandler :: Text -- ^ 
  , inboxRulesetDtoId :: UUID -- ^ 
  , inboxRulesetDtoInboxId :: UUID -- ^ 
  , inboxRulesetDtoScope :: Text -- ^ 
  , inboxRulesetDtoTarget :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetDto")
instance ToJSON InboxRulesetDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetDto")


-- | 
data InboxRulesetTestOptions = InboxRulesetTestOptions
  { inboxRulesetTestOptionsTestTarget :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetTestOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetTestOptions")
instance ToJSON InboxRulesetTestOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetTestOptions")


-- | 
data InboxRulesetTestResult = InboxRulesetTestResult
  { inboxRulesetTestResultMatches :: Bool -- ^ 
  , inboxRulesetTestResultRulesetMatches :: Maybe (Map.Map String Bool) -- ^ Map of inbox ruleset ID to boolean of if target matches
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetTestResult")
instance ToJSON InboxRulesetTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetTestResult")


-- | Options for matching emails in an inbox. Each match option object contains a &#x60;field&#x60;, &#x60;should&#x60; and &#x60;value&#x60; property. Together they form logical conditions such as &#x60;SUBJECT&#x60; should &#x60;CONTAIN&#x60; value.
data MatchOption = MatchOption
  { matchOptionField :: Maybe Text -- ^ The email property to match on. One of SUBJECT, TO, BCC, CC or FROM
  , matchOptionShould :: Maybe Text -- ^ What criteria to apply. CONTAIN or EQUAL. Note CONTAIN is recommended due to some SMTP servers adding new lines to fields and body content.
  , matchOptionValue :: Maybe Text -- ^ The value you wish to compare with the value of the field specified using the `should` value passed. For example `BODY` should `CONTAIN` a value passed.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MatchOption where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "matchOption")
instance ToJSON MatchOption where
  toJSON = genericToJSON (removeFieldLabelPrefix False "matchOption")


-- | Optional filter for matching emails based on fields. For instance filter results to only include emails whose &#x60;SUBJECT&#x60; value does &#x60;CONTAIN&#x60; given match value. An example payload would be &#x60;{ matches: [{ field: &#39;SUBJECT&#39;, should: &#39;CONTAIN&#39;, value: &#39;Welcome&#39; }] }&#x60;. You can also pass conditions such as &#x60;HAS_ATTACHMENT&#x60;. If you wish to extract regex matches inside the email content see the &#x60;getEmailContentMatch&#x60; method in the EmailController.
data MatchOptions = MatchOptions
  { matchOptionsConditions :: Maybe [ConditionOption] -- ^ Zero or more conditions such as `{ condition: 'HAS_ATTACHMENTS', value: 'TRUE' }`. Note the values are the strings `TRUE|FALSE` not booleans.
  , matchOptionsMatches :: Maybe [MatchOption] -- ^ Zero or more match options such as `{ field: 'SUBJECT', should: 'CONTAIN', value: 'Welcome' }`. Options are additive so if one does not match the email is excluded from results
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MatchOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "matchOptions")
instance ToJSON MatchOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "matchOptions")


-- | 
data MissedEmail = MissedEmail
  { missedEmailAttachmentCount :: Int -- ^ 
  , missedEmailBcc :: [Text] -- ^ 
  , missedEmailBodyExcerpt :: Maybe Text -- ^ 
  , missedEmailCc :: [Text] -- ^ 
  , missedEmailCreatedAt :: UTCTime -- ^ 
  , missedEmailFrom :: Maybe Text -- ^ 
  , missedEmailId :: Maybe UUID -- ^ 
  , missedEmailInboxIds :: [UUID] -- ^ 
  , missedEmailSubject :: Maybe Text -- ^ 
  , missedEmailTo :: [Text] -- ^ 
  , missedEmailUpdatedAt :: UTCTime -- ^ 
  , missedEmailUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MissedEmail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "missedEmail")
instance ToJSON MissedEmail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "missedEmail")


-- | 
data MissedEmailProjection = MissedEmailProjection
  { missedEmailProjectionCreatedAt :: UTCTime -- ^ 
  , missedEmailProjectionFrom :: Maybe Text -- ^ 
  , missedEmailProjectionId :: UUID -- ^ 
  , missedEmailProjectionSubject :: Maybe Text -- ^ 
  , missedEmailProjectionUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "missedEmailProjection")
instance ToJSON MissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "missedEmailProjection")


-- | Name Server Record
data NameServerRecord = NameServerRecord
  { nameServerRecordPriority :: Text -- ^ 
  , nameServerRecordRaw :: Text -- ^ 
  , nameServerRecordRecordType :: Text -- ^ 
  , nameServerRecordValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NameServerRecord where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nameServerRecord")
instance ToJSON NameServerRecord where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nameServerRecord")


-- | 
data OrganizationInboxProjection = OrganizationInboxProjection
  { organizationInboxProjectionCreatedAt :: Maybe UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , organizationInboxProjectionEmailAddress :: Maybe Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , organizationInboxProjectionFavourite :: Maybe Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , organizationInboxProjectionId :: Maybe UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , organizationInboxProjectionInboxType :: Maybe Text -- ^ Type of inbox - either HTTP (default) or SMTP. HTTP inboxes are great for testing. SMTP inboxes are processed by a custom SMTP mail server and are better for public facing inboxes that receive emails from Gmail and other large providers. If using a custom domain the domain type must match the inbox type. Use an SMTP domain for SMTP inboxes that includes an MX record pointing to `10 mx.mailslurp.com` for inbound messages.
  , organizationInboxProjectionName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , organizationInboxProjectionReadOnly :: Maybe Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  , organizationInboxProjectionTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , organizationInboxProjectionTeamAccess :: Maybe Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "organizationInboxProjection")
instance ToJSON OrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "organizationInboxProjection")


-- | Paginated email alias results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAlias = PageAlias
  { pageAliasContent :: Maybe [AliasProjection] -- ^ 
  , pageAliasEmpty :: Maybe Bool -- ^ 
  , pageAliasFirst :: Maybe Bool -- ^ 
  , pageAliasLast :: Maybe Bool -- ^ 
  , pageAliasNumber :: Maybe Int -- ^ 
  , pageAliasNumberOfElements :: Maybe Int -- ^ 
  , pageAliasPageable :: Maybe Pageable -- ^ 
  , pageAliasSize :: Maybe Int -- ^ 
  , pageAliasSort :: Maybe Sort -- ^ 
  , pageAliasTotalElements :: Maybe Integer -- ^ 
  , pageAliasTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAlias where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAlias")
instance ToJSON PageAlias where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAlias")


-- | Paginated attachment entity results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAttachmentEntity = PageAttachmentEntity
  { pageAttachmentEntityContent :: Maybe [AttachmentProjection] -- ^ 
  , pageAttachmentEntityEmpty :: Maybe Bool -- ^ 
  , pageAttachmentEntityFirst :: Maybe Bool -- ^ 
  , pageAttachmentEntityLast :: Maybe Bool -- ^ 
  , pageAttachmentEntityNumber :: Maybe Int -- ^ 
  , pageAttachmentEntityNumberOfElements :: Maybe Int -- ^ 
  , pageAttachmentEntityPageable :: Maybe Pageable -- ^ 
  , pageAttachmentEntitySize :: Maybe Int -- ^ 
  , pageAttachmentEntitySort :: Maybe Sort -- ^ 
  , pageAttachmentEntityTotalElements :: Maybe Integer -- ^ 
  , pageAttachmentEntityTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAttachmentEntity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAttachmentEntity")
instance ToJSON PageAttachmentEntity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAttachmentEntity")


-- | Paginated contact results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageContactProjection = PageContactProjection
  { pageContactProjectionContent :: Maybe [ContactProjection] -- ^ 
  , pageContactProjectionEmpty :: Maybe Bool -- ^ 
  , pageContactProjectionFirst :: Maybe Bool -- ^ 
  , pageContactProjectionLast :: Maybe Bool -- ^ 
  , pageContactProjectionNumber :: Maybe Int -- ^ 
  , pageContactProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageContactProjectionPageable :: Maybe Pageable -- ^ 
  , pageContactProjectionSize :: Maybe Int -- ^ 
  , pageContactProjectionSort :: Maybe Sort -- ^ 
  , pageContactProjectionTotalElements :: Maybe Integer -- ^ 
  , pageContactProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageContactProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageContactProjection")
instance ToJSON PageContactProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageContactProjection")


-- | Paginated email preview results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageEmailPreview = PageEmailPreview
  { pageEmailPreviewContent :: Maybe [EmailPreview] -- ^ 
  , pageEmailPreviewEmpty :: Maybe Bool -- ^ 
  , pageEmailPreviewFirst :: Maybe Bool -- ^ 
  , pageEmailPreviewLast :: Maybe Bool -- ^ 
  , pageEmailPreviewNumber :: Maybe Int -- ^ 
  , pageEmailPreviewNumberOfElements :: Maybe Int -- ^ 
  , pageEmailPreviewPageable :: Maybe Pageable -- ^ 
  , pageEmailPreviewSize :: Maybe Int -- ^ 
  , pageEmailPreviewSort :: Maybe Sort -- ^ 
  , pageEmailPreviewTotalElements :: Maybe Integer -- ^ 
  , pageEmailPreviewTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailPreview")
instance ToJSON PageEmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailPreview")


-- | Paginated email projection results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full email entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageEmailProjection = PageEmailProjection
  { pageEmailProjectionContent :: Maybe [EmailProjection] -- ^ 
  , pageEmailProjectionEmpty :: Maybe Bool -- ^ 
  , pageEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageEmailProjectionLast :: Maybe Bool -- ^ 
  , pageEmailProjectionNumber :: Maybe Int -- ^ 
  , pageEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageEmailProjectionPageable :: Maybe Pageable -- ^ 
  , pageEmailProjectionSize :: Maybe Int -- ^ 
  , pageEmailProjectionSort :: Maybe Sort -- ^ 
  , pageEmailProjectionTotalElements :: Maybe Integer -- ^ 
  , pageEmailProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailProjection")
instance ToJSON PageEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailProjection")


-- | Paginated expired inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageExpiredInboxRecordProjection = PageExpiredInboxRecordProjection
  { pageExpiredInboxRecordProjectionContent :: Maybe [ExpiredInboxRecordProjection] -- ^ 
  , pageExpiredInboxRecordProjectionEmpty :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionFirst :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionLast :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionNumber :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionPageable :: Maybe Pageable -- ^ 
  , pageExpiredInboxRecordProjectionSize :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionSort :: Maybe Sort -- ^ 
  , pageExpiredInboxRecordProjectionTotalElements :: Maybe Integer -- ^ 
  , pageExpiredInboxRecordProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageExpiredInboxRecordProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageExpiredInboxRecordProjection")
instance ToJSON PageExpiredInboxRecordProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageExpiredInboxRecordProjection")


-- | Paginated missed email results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageGroupProjection = PageGroupProjection
  { pageGroupProjectionContent :: Maybe [GroupProjection] -- ^ 
  , pageGroupProjectionEmpty :: Maybe Bool -- ^ 
  , pageGroupProjectionFirst :: Maybe Bool -- ^ 
  , pageGroupProjectionLast :: Maybe Bool -- ^ 
  , pageGroupProjectionNumber :: Maybe Int -- ^ 
  , pageGroupProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageGroupProjectionPageable :: Maybe Pageable -- ^ 
  , pageGroupProjectionSize :: Maybe Int -- ^ 
  , pageGroupProjectionSort :: Maybe Sort -- ^ 
  , pageGroupProjectionTotalElements :: Maybe Integer -- ^ 
  , pageGroupProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageGroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageGroupProjection")
instance ToJSON PageGroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageGroupProjection")


-- | Paginated inbox forwarder results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxForwarderDto = PageInboxForwarderDto
  { pageInboxForwarderDtoContent :: Maybe [InboxForwarderDto] -- ^ 
  , pageInboxForwarderDtoEmpty :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoFirst :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoLast :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoNumber :: Maybe Int -- ^ 
  , pageInboxForwarderDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxForwarderDtoPageable :: Maybe Pageable -- ^ 
  , pageInboxForwarderDtoSize :: Maybe Int -- ^ 
  , pageInboxForwarderDtoSort :: Maybe Sort -- ^ 
  , pageInboxForwarderDtoTotalElements :: Maybe Integer -- ^ 
  , pageInboxForwarderDtoTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxForwarderDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxForwarderDto")
instance ToJSON PageInboxForwarderDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxForwarderDto")


-- | Paginated inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxProjection = PageInboxProjection
  { pageInboxProjectionContent :: Maybe [InboxProjection] -- ^ 
  , pageInboxProjectionEmpty :: Maybe Bool -- ^ 
  , pageInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageInboxProjectionLast :: Maybe Bool -- ^ 
  , pageInboxProjectionNumber :: Maybe Int -- ^ 
  , pageInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageInboxProjectionPageable :: Maybe Pageable -- ^ 
  , pageInboxProjectionSize :: Maybe Int -- ^ 
  , pageInboxProjectionSort :: Maybe Sort -- ^ 
  , pageInboxProjectionTotalElements :: Maybe Integer -- ^ 
  , pageInboxProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxProjection")
instance ToJSON PageInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxProjection")


-- | Paginated inbox ruleset results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxRulesetDto = PageInboxRulesetDto
  { pageInboxRulesetDtoContent :: Maybe [InboxRulesetDto] -- ^ 
  , pageInboxRulesetDtoEmpty :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoFirst :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoLast :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoNumber :: Maybe Int -- ^ 
  , pageInboxRulesetDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxRulesetDtoPageable :: Maybe Pageable -- ^ 
  , pageInboxRulesetDtoSize :: Maybe Int -- ^ 
  , pageInboxRulesetDtoSort :: Maybe Sort -- ^ 
  , pageInboxRulesetDtoTotalElements :: Maybe Integer -- ^ 
  , pageInboxRulesetDtoTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxRulesetDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxRulesetDto")
instance ToJSON PageInboxRulesetDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxRulesetDto")


-- | Paginated MissedEmail results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageMissedEmailProjection = PageMissedEmailProjection
  { pageMissedEmailProjectionContent :: Maybe [MissedEmailProjection] -- ^ 
  , pageMissedEmailProjectionEmpty :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionLast :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionNumber :: Maybe Int -- ^ 
  , pageMissedEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageMissedEmailProjectionPageable :: Maybe Pageable -- ^ 
  , pageMissedEmailProjectionSize :: Maybe Int -- ^ 
  , pageMissedEmailProjectionSort :: Maybe Sort -- ^ 
  , pageMissedEmailProjectionTotalElements :: Maybe Integer -- ^ 
  , pageMissedEmailProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageMissedEmailProjection")
instance ToJSON PageMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageMissedEmailProjection")


-- | Paginated organization inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageOrganizationInboxProjection = PageOrganizationInboxProjection
  { pageOrganizationInboxProjectionContent :: Maybe [OrganizationInboxProjection] -- ^ 
  , pageOrganizationInboxProjectionEmpty :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionLast :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionNumber :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionPageable :: Maybe Pageable -- ^ 
  , pageOrganizationInboxProjectionSize :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionSort :: Maybe Sort -- ^ 
  , pageOrganizationInboxProjectionTotalElements :: Maybe Integer -- ^ 
  , pageOrganizationInboxProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageOrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageOrganizationInboxProjection")
instance ToJSON PageOrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageOrganizationInboxProjection")


-- | Paginated sent email results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full sent email entity use the projection ID with individual method calls.
data PageSentEmailProjection = PageSentEmailProjection
  { pageSentEmailProjectionContent :: Maybe [SentEmailProjection] -- ^ Collection of items
  , pageSentEmailProjectionEmpty :: Maybe Bool -- ^ 
  , pageSentEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageSentEmailProjectionLast :: Maybe Bool -- ^ 
  , pageSentEmailProjectionNumber :: Maybe Int -- ^ Page number starting at 0
  , pageSentEmailProjectionNumberOfElements :: Maybe Int -- ^ Number of items returned
  , pageSentEmailProjectionPageable :: Maybe Pageable -- ^ 
  , pageSentEmailProjectionSize :: Maybe Int -- ^ Size of page requested
  , pageSentEmailProjectionSort :: Maybe Sort -- ^ 
  , pageSentEmailProjectionTotalElements :: Maybe Integer -- ^ Total number of items available for querying
  , pageSentEmailProjectionTotalPages :: Maybe Int -- ^ Total number of pages available
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSentEmailProjection")
instance ToJSON PageSentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSentEmailProjection")


-- | Paginated email template results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTemplateProjection = PageTemplateProjection
  { pageTemplateProjectionContent :: Maybe [TemplateProjection] -- ^ 
  , pageTemplateProjectionEmpty :: Maybe Bool -- ^ 
  , pageTemplateProjectionFirst :: Maybe Bool -- ^ 
  , pageTemplateProjectionLast :: Maybe Bool -- ^ 
  , pageTemplateProjectionNumber :: Maybe Int -- ^ 
  , pageTemplateProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTemplateProjectionPageable :: Maybe Pageable -- ^ 
  , pageTemplateProjectionSize :: Maybe Int -- ^ 
  , pageTemplateProjectionSort :: Maybe Sort -- ^ 
  , pageTemplateProjectionTotalElements :: Maybe Integer -- ^ 
  , pageTemplateProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageTemplateProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageTemplateProjection")
instance ToJSON PageTemplateProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageTemplateProjection")


-- | Paginated email projection results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full email entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageThreadProjection = PageThreadProjection
  { pageThreadProjectionContent :: Maybe [ThreadProjection] -- ^ 
  , pageThreadProjectionEmpty :: Maybe Bool -- ^ 
  , pageThreadProjectionFirst :: Maybe Bool -- ^ 
  , pageThreadProjectionLast :: Maybe Bool -- ^ 
  , pageThreadProjectionNumber :: Maybe Int -- ^ 
  , pageThreadProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageThreadProjectionPageable :: Maybe Pageable -- ^ 
  , pageThreadProjectionSize :: Maybe Int -- ^ 
  , pageThreadProjectionSort :: Maybe Sort -- ^ 
  , pageThreadProjectionTotalElements :: Maybe Integer -- ^ 
  , pageThreadProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageThreadProjection")
instance ToJSON PageThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageThreadProjection")


-- | Paginated TrackingPixel results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTrackingPixelProjection = PageTrackingPixelProjection
  { pageTrackingPixelProjectionContent :: Maybe [TrackingPixelProjection] -- ^ 
  , pageTrackingPixelProjectionEmpty :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionFirst :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionLast :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionNumber :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionPageable :: Maybe Pageable -- ^ 
  , pageTrackingPixelProjectionSize :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionSort :: Maybe Sort -- ^ 
  , pageTrackingPixelProjectionTotalElements :: Maybe Integer -- ^ 
  , pageTrackingPixelProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageTrackingPixelProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageTrackingPixelProjection")
instance ToJSON PageTrackingPixelProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageTrackingPixelProjection")


-- | 
data PageWebhookProjection = PageWebhookProjection
  { pageWebhookProjectionContent :: Maybe [WebhookProjection] -- ^ 
  , pageWebhookProjectionEmpty :: Maybe Bool -- ^ 
  , pageWebhookProjectionFirst :: Maybe Bool -- ^ 
  , pageWebhookProjectionLast :: Maybe Bool -- ^ 
  , pageWebhookProjectionNumber :: Maybe Int -- ^ 
  , pageWebhookProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookProjectionPageable :: Maybe Pageable -- ^ 
  , pageWebhookProjectionSize :: Maybe Int -- ^ 
  , pageWebhookProjectionSort :: Maybe Sort -- ^ 
  , pageWebhookProjectionTotalElements :: Maybe Integer -- ^ 
  , pageWebhookProjectionTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookProjection")
instance ToJSON PageWebhookProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookProjection")


-- | Paginated webhook results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageWebhookResult = PageWebhookResult
  { pageWebhookResultContent :: Maybe [WebhookResultEntity] -- ^ 
  , pageWebhookResultEmpty :: Maybe Bool -- ^ 
  , pageWebhookResultFirst :: Maybe Bool -- ^ 
  , pageWebhookResultLast :: Maybe Bool -- ^ 
  , pageWebhookResultNumber :: Maybe Int -- ^ 
  , pageWebhookResultNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookResultPageable :: Maybe Pageable -- ^ 
  , pageWebhookResultSize :: Maybe Int -- ^ 
  , pageWebhookResultSort :: Maybe Sort -- ^ 
  , pageWebhookResultTotalElements :: Maybe Integer -- ^ 
  , pageWebhookResultTotalPages :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookResult")
instance ToJSON PageWebhookResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookResult")


-- | 
data Pageable = Pageable
  { pageableOffset :: Maybe Integer -- ^ 
  , pageablePageNumber :: Maybe Int -- ^ 
  , pageablePageSize :: Maybe Int -- ^ 
  , pageablePaged :: Maybe Bool -- ^ 
  , pageableSort :: Maybe Sort -- ^ 
  , pageableUnpaged :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Pageable where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageable")
instance ToJSON Pageable where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageable")


-- | Content in raw format
data RawEmailJson = RawEmailJson
  { rawEmailJsonContent :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RawEmailJson where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "rawEmailJson")
instance ToJSON RawEmailJson where
  toJSON = genericToJSON (removeFieldLabelPrefix False "rawEmailJson")


-- | Options for replying to an alias email using the alias inbox
data ReplyToAliasEmailOptions = ReplyToAliasEmailOptions
  { replyToAliasEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToAliasEmailOptionsBody :: Maybe Text -- ^ Body of the reply email you want to send
  , replyToAliasEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToAliasEmailOptionsIsHTML :: Maybe Bool -- ^ Is the reply HTML
  , replyToAliasEmailOptionsSendStrategy :: Maybe Text -- ^ When to send the email. Typically immediately
  , replyToAliasEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToAliasEmailOptionsTemplateVariables :: Maybe Value -- ^ Template variables if using a template
  , replyToAliasEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyToAliasEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyToAliasEmailOptions")
instance ToJSON ReplyToAliasEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyToAliasEmailOptions")


-- | Options for replying to email with API
data ReplyToEmailOptions = ReplyToEmailOptions
  { replyToEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToEmailOptionsBody :: Maybe Text -- ^ Body of the reply email you want to send
  , replyToEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToEmailOptionsFrom :: Maybe Text -- ^ The from header that should be used. Optional
  , replyToEmailOptionsIsHTML :: Maybe Bool -- ^ Is the reply HTML
  , replyToEmailOptionsReplyTo :: Maybe Text -- ^ The replyTo header that should be used. Optional
  , replyToEmailOptionsSendStrategy :: Maybe Text -- ^ When to send the email. Typically immediately
  , replyToEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToEmailOptionsTemplateVariables :: Maybe Value -- ^ Template variables if using a template
  , replyToEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyToEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyToEmailOptions")
instance ToJSON ReplyToEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyToEmailOptions")


-- | Options for sending an email message from an inbox. You must provide one of: &#x60;to&#x60;, &#x60;toGroup&#x60;, or &#x60;toContacts&#x60; to send an email. All other parameters are optional. 
data SendEmailOptions = SendEmailOptions
  { sendEmailOptionsAddTrackingPixel :: Maybe Bool -- ^ Add tracking pixel to email
  , sendEmailOptionsAttachments :: Maybe [Text] -- ^ Optional list of attachment IDs to send with this email. You must first upload each attachment separately via method call or dashboard in order to obtain attachment IDs. This way you can reuse attachments with different emails once uploaded. There are several ways to upload that support `multi-part form`, `base64 file encoding`, and octet stream binary uploads. See the `UploadController` for available methods. 
  , sendEmailOptionsBcc :: Maybe [Text] -- ^ Optional list of bcc destination email addresses
  , sendEmailOptionsBody :: Maybe Text -- ^ Optional contents of email. If body contains HTML then set `isHTML` to true to ensure that email clients render it correctly. You can use moustache template syntax in the email body in conjunction with `toGroup` contact variables or `templateVariables` data. If you need more templating control consider creating a template and using the `template` property instead of the body.
  , sendEmailOptionsCc :: Maybe [Text] -- ^ Optional list of cc destination email addresses
  , sendEmailOptionsCharset :: Maybe Text -- ^ Optional charset
  , sendEmailOptionsFrom :: Maybe Text -- ^ Optional from address. Email address is RFC 5322 format and may include a display name and email in angle brackets (`my@address.com` or `My inbox <my@address.com>`). If no sender is set the source inbox address will be used for this field. If you set `useInboxName` to `true` the from field will include the inbox name as a display name: `inbox_name <inbox@address.com>`. For this to work use the name field when creating an inbox. Beware of potential spam penalties when setting the from field to an address not used by the inbox. Your emails may get blocked by services if you impersonate another address. To use a custom email addresses use a custom domain. You can create domains with the DomainController. The domain must be verified in the dashboard before it can be used.
  , sendEmailOptionsHtml :: Maybe Bool -- ^ Optional HTML flag to indicate that contents is HTML. Set's a `content-type: text/html` for email. (Deprecated: use `isHTML` instead.)
  , sendEmailOptionsIsHTML :: Maybe Bool -- ^ Optional HTML flag. If true the `content-type` of the email will be `text/html`. Set to true when sending HTML to ensure proper rending on email clients
  , sendEmailOptionsReplyTo :: Maybe Text -- ^ Optional replyTo header
  , sendEmailOptionsSendStrategy :: Maybe Text -- ^ Optional strategy to use when sending the email
  , sendEmailOptionsSubject :: Maybe Text -- ^ Optional email subject line
  , sendEmailOptionsTemplate :: Maybe UUID -- ^ Optional template ID to use for body. Will override body if provided. When using a template make sure you pass the corresponding map of `templateVariables`. You can find which variables are needed by fetching the template itself or viewing it in the dashboard.
  , sendEmailOptionsTemplateVariables :: Maybe Value -- ^ Optional map of template variables. Will replace moustache syntax variables in subject and body or template with the associated values if found.
  , sendEmailOptionsTo :: Maybe [Text] -- ^ List of destination email addresses. Each email address must be RFC 5322 format. Even single recipients must be in array form. Maximum recipients per email depends on your plan. If you need to send many emails try using contacts or contact groups or use a non standard sendStrategy to ensure that spam filters are not triggered (many recipients in one email can affect your spam rating). Be cautious when sending emails that your recipients exist. High bounce rates (meaning a high percentage of emails cannot be delivered because an address does not exist) can result in account freezing.
  , sendEmailOptionsToContacts :: Maybe [UUID] -- ^ Optional list of contact IDs to send email to. Manage your contacts via the API or dashboard. When contacts are used the email is sent to each contact separately so they will not see other recipients.
  , sendEmailOptionsToGroup :: Maybe UUID -- ^ Optional contact group ID to send email to. You can create contacts and contact groups in the API or dashboard and use them for email campaigns. When contact groups are used the email is sent to each contact separately so they will not see other recipients
  , sendEmailOptionsUseInboxName :: Maybe Bool -- ^ Use name of inbox as sender email address name. Will construct RFC 5322 email address with `Inbox name <inbox@address.com>` if the inbox has a name.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendEmailOptions")
instance ToJSON SendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendEmailOptions")


-- | Sent email details
data SentEmailDto = SentEmailDto
  { sentEmailDtoAttachments :: Maybe [Text] -- ^ Array of IDs of attachments that were sent with this email
  , sentEmailDtoBcc :: Maybe [Text] -- ^ 
  , sentEmailDtoBody :: Maybe Text -- ^ 
  , sentEmailDtoBodyMD5Hash :: Maybe Text -- ^ MD5 Hash
  , sentEmailDtoCc :: Maybe [Text] -- ^ 
  , sentEmailDtoCharset :: Maybe Text -- ^ 
  , sentEmailDtoFrom :: Maybe Text -- ^ 
  , sentEmailDtoId :: Maybe UUID -- ^ ID of sent email
  , sentEmailDtoInboxId :: Maybe UUID -- ^ Inbox ID email was sent from
  , sentEmailDtoIsHTML :: Maybe Bool -- ^ 
  , sentEmailDtoPixelIds :: Maybe [UUID] -- ^ 
  , sentEmailDtoReplyTo :: Maybe Text -- ^ 
  , sentEmailDtoSentAt :: UTCTime -- ^ 
  , sentEmailDtoSubject :: Maybe Text -- ^ 
  , sentEmailDtoTo :: Maybe [Text] -- ^ Recipients email was sent to
  , sentEmailDtoUserId :: Maybe UUID -- ^ User ID
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailDto")
instance ToJSON SentEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailDto")


-- | 
data SentEmailProjection = SentEmailProjection
  { sentEmailProjectionAttachments :: [Text] -- ^ 
  , sentEmailProjectionBcc :: [Text] -- ^ 
  , sentEmailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  , sentEmailProjectionCc :: [Text] -- ^ 
  , sentEmailProjectionCreatedAt :: UTCTime -- ^ 
  , sentEmailProjectionFrom :: Maybe Text -- ^ 
  , sentEmailProjectionId :: UUID -- ^ 
  , sentEmailProjectionInboxId :: UUID -- ^ 
  , sentEmailProjectionSubject :: Maybe Text -- ^ 
  , sentEmailProjectionTo :: [Text] -- ^ 
  , sentEmailProjectionUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailProjection")
instance ToJSON SentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailProjection")


-- | Options for setting inbox favourite state
data SetInboxFavouritedOptions = SetInboxFavouritedOptions
  { setInboxFavouritedOptionsState :: Maybe Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SetInboxFavouritedOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "setInboxFavouritedOptions")
instance ToJSON SetInboxFavouritedOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "setInboxFavouritedOptions")


-- | 
data SimpleSendEmailOptions = SimpleSendEmailOptions
  { simpleSendEmailOptionsBody :: Maybe Text -- ^ Body of the email message. Supports HTML
  , simpleSendEmailOptionsSenderId :: Maybe UUID -- ^ ID of inbox to send from. If null an inbox will be created for sending
  , simpleSendEmailOptionsSubject :: Maybe Text -- ^ Subject line of the email
  , simpleSendEmailOptionsTo :: Maybe Text -- ^ Email address to send to
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SimpleSendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "simpleSendEmailOptions")
instance ToJSON SimpleSendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "simpleSendEmailOptions")


-- | 
data Sort = Sort
  { sortEmpty :: Maybe Bool -- ^ 
  , sortSorted :: Maybe Bool -- ^ 
  , sortUnsorted :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Sort where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sort")
instance ToJSON Sort where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sort")


-- | 
data TemplateDto = TemplateDto
  { templateDtoContent :: Text -- ^ 
  , templateDtoCreatedAt :: UTCTime -- ^ 
  , templateDtoId :: UUID -- ^ 
  , templateDtoName :: Text -- ^ 
  , templateDtoVariables :: [TemplateVariable] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateDto")
instance ToJSON TemplateDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateDto")


-- | 
data TemplateProjection = TemplateProjection
  { templateProjectionCreatedAt :: UTCTime -- ^ 
  , templateProjectionId :: UUID -- ^ 
  , templateProjectionName :: Text -- ^ 
  , templateProjectionUpdatedAt :: UTCTime -- ^ 
  , templateProjectionVariables :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateProjection")
instance ToJSON TemplateProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateProjection")


-- | 
data TemplateVariable = TemplateVariable
  { templateVariableName :: Text -- ^ 
  , templateVariableVariableType :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateVariable where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateVariable")
instance ToJSON TemplateVariable where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateVariable")


-- | 
data TestNewInboxForwarderOptions = TestNewInboxForwarderOptions
  { testNewInboxForwarderOptionsInboxForwarderTestOptions :: InboxForwarderTestOptions -- ^ 
  , testNewInboxForwarderOptionsCreateInboxForwarderOptions :: CreateInboxForwarderOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestNewInboxForwarderOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testNewInboxForwarderOptions")
instance ToJSON TestNewInboxForwarderOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testNewInboxForwarderOptions")


-- | 
data TestNewInboxRulesetOptions = TestNewInboxRulesetOptions
  { testNewInboxRulesetOptionsInboxRulesetTestOptions :: InboxRulesetTestOptions -- ^ 
  , testNewInboxRulesetOptionsCreateInboxRulesetOptions :: CreateInboxRulesetOptions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TestNewInboxRulesetOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "testNewInboxRulesetOptions")
instance ToJSON TestNewInboxRulesetOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "testNewInboxRulesetOptions")


-- | A thread is a message thread created for a message received by an alias
data ThreadProjection = ThreadProjection
  { threadProjectionAliasId :: UUID -- ^ 
  , threadProjectionBcc :: Maybe [Text] -- ^ 
  , threadProjectionCc :: Maybe [Text] -- ^ 
  , threadProjectionCreatedAt :: UTCTime -- ^ 
  , threadProjectionId :: UUID -- ^ 
  , threadProjectionInboxId :: UUID -- ^ 
  , threadProjectionName :: Maybe Text -- ^ 
  , threadProjectionSubject :: Maybe Text -- ^ 
  , threadProjectionTo :: [Text] -- ^ 
  , threadProjectionUpdatedAt :: UTCTime -- ^ 
  , threadProjectionUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "threadProjection")
instance ToJSON ThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "threadProjection")


-- | 
data TrackingPixelDto = TrackingPixelDto
  { trackingPixelDtoCreatedAt :: UTCTime -- ^ 
  , trackingPixelDtoHtml :: Text -- ^ 
  , trackingPixelDtoId :: UUID -- ^ 
  , trackingPixelDtoInboxId :: Maybe UUID -- ^ 
  , trackingPixelDtoRecipient :: Maybe Text -- ^ 
  , trackingPixelDtoSeen :: Bool -- ^ 
  , trackingPixelDtoSeenAt :: Maybe UTCTime -- ^ 
  , trackingPixelDtoSentEmailId :: Maybe UUID -- ^ 
  , trackingPixelDtoUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackingPixelDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trackingPixelDto")
instance ToJSON TrackingPixelDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trackingPixelDto")


-- | 
data TrackingPixelProjection = TrackingPixelProjection
  { trackingPixelProjectionCreatedAt :: UTCTime -- ^ 
  , trackingPixelProjectionId :: UUID -- ^ 
  , trackingPixelProjectionInboxId :: Maybe UUID -- ^ 
  , trackingPixelProjectionName :: Maybe Text -- ^ 
  , trackingPixelProjectionRecipient :: Maybe Text -- ^ 
  , trackingPixelProjectionSeen :: Bool -- ^ 
  , trackingPixelProjectionSeenAt :: Maybe UTCTime -- ^ 
  , trackingPixelProjectionUserId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackingPixelProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trackingPixelProjection")
instance ToJSON TrackingPixelProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trackingPixelProjection")


-- | 
data UnreadCount = UnreadCount
  { unreadCountCount :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnreadCount where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unreadCount")
instance ToJSON UnreadCount where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unreadCount")


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


-- | 
data UpdateGroupContacts = UpdateGroupContacts
  { updateGroupContactsContactIds :: [UUID] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupContacts where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupContacts")
instance ToJSON UpdateGroupContacts where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupContacts")


-- | Options for updating inbox properties
data UpdateInboxOptions = UpdateInboxOptions
  { updateInboxOptionsDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , updateInboxOptionsExpiresAt :: Maybe UTCTime -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , updateInboxOptionsFavourite :: Maybe Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , updateInboxOptionsName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , updateInboxOptionsTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateInboxOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateInboxOptions")
instance ToJSON UpdateInboxOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateInboxOptions")


-- | Options for uploading files for attachments. When sending emails with the API that require attachments first upload each attachment. Then use the returned attachment ID in your &#x60;SendEmailOptions&#x60; when sending an email. This way you can use attachments multiple times once they have been uploaded.
data UploadAttachmentOptions = UploadAttachmentOptions
  { uploadAttachmentOptionsBase64Contents :: Text -- ^ Base64 encoded string of file contents. Typically this means reading the bytes or string content of a file and then converting that to a base64 encoded string.
  , uploadAttachmentOptionsContentType :: Maybe Text -- ^ Optional contentType for file. For instance `application/pdf`
  , uploadAttachmentOptionsFilename :: Maybe Text -- ^ Optional filename to save upload with. Will be the name that is shown in email clients
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UploadAttachmentOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uploadAttachmentOptions")
instance ToJSON UploadAttachmentOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uploadAttachmentOptions")


-- | Response object for email validation operation
data ValidationDto = ValidationDto
  { validationDtoEmailId :: Maybe UUID -- ^ ID of the email validated
  , validationDtoHtml :: Maybe HTMLValidationResult -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidationDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validationDto")
instance ToJSON ValidationDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validationDto")


-- | 
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


-- | Conditions that a &#x60;waitForXEmails&#x60; endpoint operates on. The methods wait until given conditions are met or a timeout is reached. If the conditions are met without needing to wait the results will be returned immediately. Can include &#x60;unreadOnly&#x60; to ignore already read emails that were returned in an API call or viewing in the dashboard. Can also include matches for emails containing &#x60;from&#x60;, &#x60;subject&#x60;, &#x60;hasAttachments&#x60; etc.
data WaitForConditions = WaitForConditions
  { waitForConditionsCount :: Maybe Int -- ^ Number of results that should match conditions. Either exactly or at least this amount based on the `countType`. If count condition is not met and the timeout has not been reached the `waitFor` method will retry the operation.
  , waitForConditionsCountType :: Maybe Text -- ^ How should the found count be compared to the expected count.
  , waitForConditionsInboxId :: Maybe UUID -- ^ ID of inbox to search within and apply conditions to. Essentially filtering the emails found to give a count.
  , waitForConditionsMatches :: Maybe [MatchOption] -- ^ Conditions that should be matched for an email to qualify for results. Each condition will be applied in order to each email within an inbox to filter a result list of matching emails you are waiting for.
  , waitForConditionsSortDirection :: Maybe Text -- ^ Direction to sort matching emails by created time
  , waitForConditionsTimeout :: Maybe Integer -- ^ Max time in milliseconds to retry the `waitFor` operation until conditions are met.
  , waitForConditionsUnreadOnly :: Maybe Bool -- ^ Apply conditions only to **unread** emails. All emails begin with `read=false`. An email is marked `read=true` when an `EmailDto` representation of it has been returned to the user at least once. For example you have called `getEmail` or `waitForLatestEmail` etc., or you have viewed the email in the dashboard. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WaitForConditions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "waitForConditions")
instance ToJSON WaitForConditions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "waitForConditions")


-- | Representation of a webhook for an inbox. The URL specified will be using by MailSlurp whenever an email is received by the attached inbox. A webhook entity should have a URL that points to your server. Your server should accept HTTP/S POST requests and return a success 200. MailSlurp will retry your webhooks if they fail. See https://api.mailslurp.com/schemas/webhook-payload for the payload schema.
data WebhookDto = WebhookDto
  { webhookDtoBasicAuth :: Maybe Bool -- ^ Does webhook expect basic authentication? If true it means you created this webhook with a username and password. MailSlurp will use these in the URL to authenticate itself.
  , webhookDtoCreatedAt :: Maybe UTCTime -- ^ When the webhook was created
  , webhookDtoEventName :: Maybe Text -- ^ 
  , webhookDtoId :: Maybe UUID -- ^ ID of the Webhook
  , webhookDtoInboxId :: Maybe UUID -- ^ The inbox that the Webhook will be triggered by
  , webhookDtoMethod :: Maybe Text -- ^ HTTP method that your server endpoint must listen for
  , webhookDtoName :: Maybe Text -- ^ Name of the webhook
  , webhookDtoPayloadJsonSchema :: Maybe Text -- ^ JSON Schema for the payload that will be sent to your URL via the HTTP method described.
  , webhookDtoUpdatedAt :: UTCTime -- ^ 
  , webhookDtoUrl :: Maybe Text -- ^ URL of your server that the webhook will be sent to. The schema of the JSON that is sent is described by the payloadJsonSchema.
  , webhookDtoUserId :: Maybe UUID -- ^ User ID of the Webhook
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookDto")
instance ToJSON WebhookDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookDto")


-- | EMAIL_OPENED webhook payload. Sent to your webhook url endpoint via HTTP POST when an email containing a tracking pixel is opened and the pixel image is loaded by a reader.
data WebhookEmailOpenedPayload = WebhookEmailOpenedPayload
  { webhookEmailOpenedPayloadCreatedAt :: Maybe UTCTime -- ^ Date time of event creation
  , webhookEmailOpenedPayloadEventName :: Maybe Text -- ^ Name of the event type webhook is being triggered for.
  , webhookEmailOpenedPayloadInboxId :: Maybe UUID -- ^ Id of the inbox that received an email
  , webhookEmailOpenedPayloadMessageId :: Maybe Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookEmailOpenedPayloadPixelId :: Maybe UUID -- ^ ID of the tracking pixel
  , webhookEmailOpenedPayloadRecipient :: Maybe Text -- ^ Email address for the recipient of the tracking pixel
  , webhookEmailOpenedPayloadSentEmailId :: Maybe UUID -- ^ ID of sent email
  , webhookEmailOpenedPayloadWebhookId :: Maybe UUID -- ^ ID of webhook entity being triggered
  , webhookEmailOpenedPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEmailOpenedPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookEmailOpenedPayload")
instance ToJSON WebhookEmailOpenedPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookEmailOpenedPayload")


-- | NEW_ATTACHMENT webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to that contains an attachment. You can use the attachmentId to download the attachment.
data WebhookNewAttachmentPayload = WebhookNewAttachmentPayload
  { webhookNewAttachmentPayloadAttachmentId :: Maybe Text -- ^ ID of attachment. Use the `AttachmentController` to
  , webhookNewAttachmentPayloadContentLength :: Maybe Integer -- ^ Size of attachment in bytes
  , webhookNewAttachmentPayloadContentType :: Maybe Text -- ^ Content type of attachment such as 'image/png' or 'application/pdf
  , webhookNewAttachmentPayloadEventName :: Maybe Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewAttachmentPayloadMessageId :: Maybe Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewAttachmentPayloadName :: Maybe Text -- ^ Filename of the attachment if present
  , webhookNewAttachmentPayloadWebhookId :: Maybe UUID -- ^ ID of webhook entity being triggered
  , webhookNewAttachmentPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewAttachmentPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewAttachmentPayload")
instance ToJSON WebhookNewAttachmentPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewAttachmentPayload")


-- | NEW_CONTACT webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to that contains a recipient that has not been saved as a contact.
data WebhookNewContactPayload = WebhookNewContactPayload
  { webhookNewContactPayloadCompany :: Maybe Text -- ^ 
  , webhookNewContactPayloadContactId :: UUID -- ^ 
  , webhookNewContactPayloadCreatedAt :: UTCTime -- ^ 
  , webhookNewContactPayloadEmailAddresses :: [Text] -- ^ 
  , webhookNewContactPayloadEventName :: Maybe Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewContactPayloadFirstName :: Maybe Text -- ^ 
  , webhookNewContactPayloadGroupId :: Maybe UUID -- ^ 
  , webhookNewContactPayloadLastName :: Maybe Text -- ^ 
  , webhookNewContactPayloadMessageId :: Maybe Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewContactPayloadMetaData :: Maybe Value -- ^ 
  , webhookNewContactPayloadOptOut :: Maybe Bool -- ^ 
  , webhookNewContactPayloadPrimaryEmailAddress :: Maybe Text -- ^ 
  , webhookNewContactPayloadTags :: [Text] -- ^ 
  , webhookNewContactPayloadWebhookId :: Maybe UUID -- ^ ID of webhook entity being triggered
  , webhookNewContactPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewContactPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewContactPayload")
instance ToJSON WebhookNewContactPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewContactPayload")


-- | NEW_EMAIL webhook payload. Sent to your webhook url endpoint via HTTP POST when an email is received by the inbox that your webhook is attached to. Use the email ID to fetch the full email body or attachments.
data WebhookNewEmailPayload = WebhookNewEmailPayload
  { webhookNewEmailPayloadAttachmentMetaDatas :: Maybe [AttachmentMetaData] -- ^ List of attachment meta data objects if attachments present
  , webhookNewEmailPayloadBcc :: Maybe [Text] -- ^ List of `BCC` recipients email was addressed to
  , webhookNewEmailPayloadCc :: Maybe [Text] -- ^ List of `CC` recipients email was addressed to
  , webhookNewEmailPayloadCreatedAt :: Maybe UTCTime -- ^ Date time of event creation
  , webhookNewEmailPayloadEmailId :: Maybe UUID -- ^ ID of the email that was received. Use this ID for fetching the email with the `EmailController`.
  , webhookNewEmailPayloadEventName :: Maybe Text -- ^ Name of the event type webhook is being triggered for.
  , webhookNewEmailPayloadFrom :: Maybe Text -- ^ Who the email was sent from
  , webhookNewEmailPayloadInboxId :: Maybe UUID -- ^ Id of the inbox that received an email
  , webhookNewEmailPayloadMessageId :: Maybe Text -- ^ Idempotent message ID. Store this ID locally or in a database to prevent message duplication.
  , webhookNewEmailPayloadSubject :: Maybe Text -- ^ The subject line of the email message
  , webhookNewEmailPayloadTo :: Maybe [Text] -- ^ List of `To` recipients that email was addressed to
  , webhookNewEmailPayloadWebhookId :: Maybe UUID -- ^ ID of webhook entity being triggered
  , webhookNewEmailPayloadWebhookName :: Maybe Text -- ^ Name of the webhook being triggered
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookNewEmailPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookNewEmailPayload")
instance ToJSON WebhookNewEmailPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookNewEmailPayload")


-- | Representation of a webhook
data WebhookProjection = WebhookProjection
  { webhookProjectionCreatedAt :: UTCTime -- ^ 
  , webhookProjectionId :: UUID -- ^ 
  , webhookProjectionInboxId :: UUID -- ^ 
  , webhookProjectionName :: Maybe Text -- ^ 
  , webhookProjectionUpdatedAt :: UTCTime -- ^ 
  , webhookProjectionUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookProjection")
instance ToJSON WebhookProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookProjection")


-- | 
data WebhookResultEntity = WebhookResultEntity
  { webhookResultEntityCreatedAt :: UTCTime -- ^ 
  , webhookResultEntityHttpMethod :: Text -- ^ 
  , webhookResultEntityId :: Maybe UUID -- ^ 
  , webhookResultEntityInboxId :: UUID -- ^ 
  , webhookResultEntityMessageId :: Text -- ^ 
  , webhookResultEntityResponseBodyExtract :: Maybe Text -- ^ 
  , webhookResultEntityResponseStatus :: Maybe Int -- ^ 
  , webhookResultEntityResponseTimeMillis :: Integer -- ^ 
  , webhookResultEntityUpdatedAt :: UTCTime -- ^ 
  , webhookResultEntityUserId :: UUID -- ^ 
  , webhookResultEntityWebhookEvent :: Text -- ^ 
  , webhookResultEntityWebhookId :: UUID -- ^ 
  , webhookResultEntityWebhookUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookResultEntity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookResultEntity")
instance ToJSON WebhookResultEntity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookResultEntity")


-- | 
data WebhookTestRequest = WebhookTestRequest
  { webhookTestRequestHeaders :: (Map.Map String Text) -- ^ 
  , webhookTestRequestMethod :: Text -- ^ 
  , webhookTestRequestPayload :: Maybe Text -- ^ 
  , webhookTestRequestUrl :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTestRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTestRequest")
instance ToJSON WebhookTestRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTestRequest")


-- | 
data WebhookTestResponse = WebhookTestResponse
  { webhookTestResponseMessage :: Maybe Text -- ^ 
  , webhookTestResponseStatusCode :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTestResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTestResponse")
instance ToJSON WebhookTestResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTestResponse")


-- | Results of testing a webhook
data WebhookTestResult = WebhookTestResult
  { webhookTestResultMessage :: Maybe Text -- ^ 
  , webhookTestResultRequest :: WebhookTestRequest -- ^ 
  , webhookTestResultResponse :: WebhookTestResponse -- ^ 
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
