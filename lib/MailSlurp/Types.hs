{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module MailSlurp.Types (
  AbstractWebhookPayload (..),
  AliasDto (..),
  AliasProjection (..),
  AttachmentEntity (..),
  AttachmentMetaData (..),
  AttachmentProjection (..),
  BasicAuthOptions (..),
  BounceProjection (..),
  BounceRecipient (..),
  BouncedEmailDto (..),
  BouncedRecipientDto (..),
  BulkSendEmailOptions (..),
  ConditionOption (..),
  ContactDto (..),
  ContactProjection (..),
  ContentMatchOptions (..),
  CountDto (..),
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
  EmailLinksResult (..),
  EmailPreview (..),
  EmailProjection (..),
  EmailRecipients (..),
  EmailTextLinesResult (..),
  EmailVerificationResult (..),
  ExpirationDefaults (..),
  ExpiredInboxDto (..),
  ExpiredInboxRecordProjection (..),
  ExportLink (..),
  ExportOptions (..),
  FlushExpiredInboxesResult (..),
  ForwardEmailOptions (..),
  GravatarUrl (..),
  GroupContactsDto (..),
  GroupDto (..),
  GroupProjection (..),
  HTMLValidationResult (..),
  IPAddressResult (..),
  ImapSmtpAccessDetails (..),
  InboxByEmailAddressResult (..),
  InboxDto (..),
  InboxExistsDto (..),
  InboxForwarderDto (..),
  InboxForwarderTestOptions (..),
  InboxForwarderTestResult (..),
  InboxIdItem (..),
  InboxIdsResult (..),
  InboxPreview (..),
  InboxRulesetDto (..),
  InboxRulesetTestOptions (..),
  InboxRulesetTestResult (..),
  InlineObject (..),
  JSONSchemaDto (..),
  MatchOption (..),
  MatchOptions (..),
  MissedEmail (..),
  MissedEmailProjection (..),
  NameServerRecord (..),
  OrganizationInboxProjection (..),
  PageAlias (..),
  PageAttachmentEntity (..),
  PageBouncedEmail (..),
  PageBouncedRecipients (..),
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
  PageUnknownMissedEmailProjection (..),
  PageWebhookProjection (..),
  PageWebhookResult (..),
  PageableObject (..),
  RawEmailJson (..),
  Recipient (..),
  ReplyToAliasEmailOptions (..),
  ReplyToEmailOptions (..),
  SendEmailOptions (..),
  Sender (..),
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
  UnknownMissedEmailProjection (..),
  UnreadCount (..),
  UnseenErrorCountDto (..),
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
  WebhookEmailReadPayload (..),
  WebhookNewAttachmentPayload (..),
  WebhookNewContactPayload (..),
  WebhookNewEmailPayload (..),
  WebhookProjection (..),
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
  , abstractWebhookPayloadWebhookId :: UUID -- ^ 
  , abstractWebhookPayloadMessageId :: Text -- ^ 
  , abstractWebhookPayloadWebhookName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AbstractWebhookPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "abstractWebhookPayload")
instance ToJSON AbstractWebhookPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "abstractWebhookPayload")


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
  , aliasDtoCreatedAt :: Maybe UTCTime -- ^ 
  , aliasDtoUpdatedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasDto")
instance ToJSON AliasDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasDto")


-- | Representation of a alias
data AliasProjection = AliasProjection
  { aliasProjectionName :: Maybe Text -- ^ 
  , aliasProjectionId :: UUID -- ^ 
  , aliasProjectionUserId :: UUID -- ^ 
  , aliasProjectionEmailAddress :: Text -- ^ 
  , aliasProjectionInboxId :: UUID -- ^ 
  , aliasProjectionCreatedAt :: UTCTime -- ^ 
  , aliasProjectionUseThreads :: Maybe Bool -- ^ 
  , aliasProjectionUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AliasProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aliasProjection")
instance ToJSON AliasProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aliasProjection")


-- | 
data AttachmentEntity = AttachmentEntity
  { attachmentEntityId :: Maybe UUID -- ^ 
  , attachmentEntityAttachmentId :: Text -- ^ 
  , attachmentEntityUserId :: UUID -- ^ 
  , attachmentEntityContentType :: Maybe Text -- ^ 
  , attachmentEntityContentLength :: Maybe Integer -- ^ 
  , attachmentEntityName :: Maybe Text -- ^ 
  , attachmentEntityCreatedAt :: UTCTime -- ^ 
  , attachmentEntityUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentEntity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentEntity")
instance ToJSON AttachmentEntity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentEntity")


-- | Meta data associated with an attachment. Attachments are stored as byte blobs so the meta data is stored separately.
data AttachmentMetaData = AttachmentMetaData
  { attachmentMetaDataName :: Text -- ^ Name of attachment if given
  , attachmentMetaDataContentType :: Text -- ^ Content type of attachment such as `image/png`
  , attachmentMetaDataContentLength :: Integer -- ^ Size of attachment in bytes
  , attachmentMetaDataId :: Text -- ^ ID of attachment
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttachmentMetaData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "attachmentMetaData")
instance ToJSON AttachmentMetaData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "attachmentMetaData")


-- | 
data AttachmentProjection = AttachmentProjection
  { attachmentProjectionName :: Maybe Text -- ^ 
  , attachmentProjectionContentLength :: Maybe Integer -- ^ Content length of attachment in bytes
  , attachmentProjectionUserId :: UUID -- ^ 
  , attachmentProjectionCreatedAt :: UTCTime -- ^ 
  , attachmentProjectionAttachmentId :: Text -- ^ Attachment ID
  , attachmentProjectionUpdatedAt :: UTCTime -- ^ 
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


-- | 
data BounceProjection = BounceProjection
  { bounceProjectionId :: Maybe UUID -- ^ 
  , bounceProjectionSender :: Text -- ^ 
  , bounceProjectionCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BounceProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bounceProjection")
instance ToJSON BounceProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bounceProjection")


-- | 
data BounceRecipient = BounceRecipient
  { bounceRecipientId :: Maybe UUID -- ^ 
  , bounceRecipientUserId :: UUID -- ^ 
  , bounceRecipientRecipient :: Text -- ^ 
  , bounceRecipientCreatedAt :: UTCTime -- ^ 
  , bounceRecipientUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BounceRecipient where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bounceRecipient")
instance ToJSON BounceRecipient where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bounceRecipient")


-- | Bounced email
data BouncedEmailDto = BouncedEmailDto
  { bouncedEmailDtoId :: Maybe UUID -- ^ 
  , bouncedEmailDtoUserId :: UUID -- ^ 
  , bouncedEmailDtoNotificationType :: Text -- ^ 
  , bouncedEmailDtoSentToRecipients :: Maybe [Text] -- ^ 
  , bouncedEmailDtoSender :: Text -- ^ 
  , bouncedEmailDtoBounceMta :: Maybe Text -- ^ 
  , bouncedEmailDtoBounceType :: Maybe Text -- ^ 
  , bouncedEmailDtoBounceRecipients :: Maybe [Text] -- ^ 
  , bouncedEmailDtoBounceSubType :: Maybe Text -- ^ 
  , bouncedEmailDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BouncedEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bouncedEmailDto")
instance ToJSON BouncedEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bouncedEmailDto")


-- | Bounced recipient
data BouncedRecipientDto = BouncedRecipientDto
  { bouncedRecipientDtoId :: Maybe UUID -- ^ 
  , bouncedRecipientDtoUserId :: UUID -- ^ 
  , bouncedRecipientDtoRecipient :: Text -- ^ 
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


-- | 
data ContactProjection = ContactProjection
  { contactProjectionId :: UUID -- ^ 
  , contactProjectionGroupId :: Maybe UUID -- ^ 
  , contactProjectionCreatedAt :: UTCTime -- ^ 
  , contactProjectionFirstName :: Maybe Text -- ^ 
  , contactProjectionLastName :: Maybe Text -- ^ 
  , contactProjectionCompany :: Maybe Text -- ^ 
  , contactProjectionEmailAddresses :: Maybe [Text] -- ^ 
  , contactProjectionOptOut :: Maybe Bool -- ^ 
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


-- | 
data CountDto = CountDto
  { countDtoTotalElements :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CountDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "countDto")
instance ToJSON CountDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "countDto")


-- | Create email alias options. Email aliases can be used to mask real email addresses behind an ID. You can also attach an inbox to an alias so that any email received by the inbox email address if forwarded to the alias email address.
data CreateAliasOptions = CreateAliasOptions
  { createAliasOptionsEmailAddress :: Text -- ^ Email address to be hidden behind alias. Emails sent to the alias email address will be forwarded to this address. If you want to enable replies set useThreads true and the reply-to for the email will allow outbound communication via a thread.
  , createAliasOptionsInboxId :: Maybe UUID -- ^ Optional inbox ID to attach to alias. Null by default means an a new inbox will be created for the alias. Use a custom inbox to control what email address the alias uses. To use custom email addresses create a domain and an inbox, the use the inbox ID with this call. Emails received by this inbox will be forwarded to the alias email address
  , createAliasOptionsName :: Maybe Text -- ^ Optional name for alias
  , createAliasOptionsUseThreads :: Bool -- ^ Enable threads options. If true emails will be sent with a unique reply-to thread address. This means you can reply to the forwarded email and it will be sent to the recipients via your alias address. That way a thread conversation is preserved.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateAliasOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createAliasOptions")
instance ToJSON CreateAliasOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createAliasOptions")


-- | 
data CreateContactOptions = CreateContactOptions
  { createContactOptionsFirstName :: Maybe Text -- ^ 
  , createContactOptionsLastName :: Maybe Text -- ^ 
  , createContactOptionsCompany :: Maybe Text -- ^ 
  , createContactOptionsEmailAddresses :: Maybe [Text] -- ^ Set of email addresses belonging to the contact
  , createContactOptionsTags :: Maybe [Text] -- ^ Tags that can be used to search and group contacts
  , createContactOptionsMetaData :: Maybe Value -- ^ 
  , createContactOptionsOptOut :: Maybe Bool -- ^ Has the user explicitly or implicitly opted out of being contacted? If so MailSlurp will ignore them in all actions.
  , createContactOptionsGroupId :: Maybe UUID -- ^ Group IDs that contact belongs to
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createContactOptions")
instance ToJSON CreateContactOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createContactOptions")


-- | Options for creating a domain to use with MailSlurp. You must have ownership access to this domain in order to verify it. Domains will not function correctly until the domain has been verified. See https://www.mailslurp.com/guides/custom-domains for help. Domains can be either &#x60;HTTP&#x60; or &#x60;SMTP&#x60; type. The type of domain determines which inboxes can be used with it. &#x60;SMTP&#x60; inboxes use a mail server running &#x60;mx.mailslurp.com&#x60; while &#x60;HTTP&#x60; inboxes are handled by AWS SES.
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
data CreateGroupOptions = CreateGroupOptions
  { createGroupOptionsName :: Text -- ^ 
  , createGroupOptionsDescription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupOptions")
instance ToJSON CreateGroupOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupOptions")


-- | Options for creating an inbox. An inbox has a real email address that can send and receive emails. Inboxes can be permanent or expire at a given time. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. &#x60;SMTP&#x60; inboxes are receive only and processed by a mail server running at &#x60;mx.mailslurp.com&#x60; while &#x60;HTTP&#x60; inboxes can send and receive and are processed by AWS SES. Inboxes can use a custom email address (by verifying your own domain) or a randomly assigned email ending in either &#x60;mailslurp.com&#x60; or (if &#x60;useDomainPool&#x60; is enabled) ending in a similar domain such as &#x60;mailslurp.xyz&#x60; (selected at random). 
data CreateInboxDto = CreateInboxDto
  { createInboxDtoEmailAddress :: Maybe Text -- ^ A custom email address to use with the inbox. Defaults to null. When null MailSlurp will assign a random email address to the inbox such as `123@mailslurp.com`. If you use the `useDomainPool` option when the email address is null it will generate an email address with a more varied domain ending such as `123@mailslurp.info` or `123@mailslurp.biz`. When a custom email address is provided the address is split into a domain and the domain is queried against your user. If you have created the domain in the MailSlurp dashboard and verified it you can use any email address that ends with the domain. Note domain types must match the inbox type - so `SMTP` inboxes will only work with `SMTP` type domains. Avoid `SMTP` inboxes if you need to send emails as they can only receive. Send an email to this address and the inbox will receive and store it for you. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , createInboxDtoName :: Maybe Text -- ^ Optional name of the inbox. Displayed in the dashboard for easier search and used as the sender name when sending emails.
  , createInboxDtoDescription :: Maybe Text -- ^ Optional description of the inbox for labelling purposes. Is shown in the dashboard and can be used with
  , createInboxDtoUseDomainPool :: Maybe Bool -- ^ Use the MailSlurp domain name pool with this inbox when creating the email address. Defaults to null. If enabled the inbox will be an email address with a domain randomly chosen from a list of the MailSlurp domains. This is useful when the default `@mailslurp.com` email addresses used with inboxes are blocked or considered spam by a provider or receiving service. When domain pool is enabled an email address will be generated ending in `@mailslurp.{world,info,xyz,...}` . This means a TLD is randomly selecting from a list of `.biz`, `.info`, `.xyz` etc to add variance to the generated email addresses. When null or false MailSlurp uses the default behavior of `@mailslurp.com` or custom email address provided by the emailAddress field. Note this feature is only available for `HTTP` inbox types.
  , createInboxDtoTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , createInboxDtoExpiresAt :: Maybe UTCTime -- ^ Optional inbox expiration date. If null then this inbox is permanent and the emails in it won't be deleted. If an expiration date is provided or is required by your plan the inbox will be closed when the expiration time is reached. Expired inboxes still contain their emails but can no longer send or receive emails. An ExpiredInboxRecord is created when an inbox and the email address and inbox ID are recorded. The expiresAt property is a timestamp string in ISO DateTime Format yyyy-MM-dd'T'HH:mm:ss.SSSXXX.
  , createInboxDtoFavourite :: Maybe Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  , createInboxDtoExpiresIn :: Maybe Integer -- ^ Number of milliseconds that inbox should exist for
  , createInboxDtoAllowTeamAccess :: Maybe Bool -- ^ DEPRECATED (team access is always true). Grant team access to this inbox and the emails that belong to it for team members of your organization.
  , createInboxDtoInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
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
  { createWebhookOptionsUrl :: Text -- ^ Public URL on your server that MailSlurp can post WebhookNotification payload to when an email is received or an event is trigger. The payload of the submitted JSON is dependent on the webhook event type. The default `EMAIL_RECEIVED` payload is described by `https://api.mailslurp.com/schemas/webhook-payload`. The other events, `NEW_EMAIL`, `NEW_CONTACT`, and `NEW_ATTACHMENT` are described by `https://api.mailslurp.com/schemas/webhook-new-email-payload`, `https://api.mailslurp.com/schemas/webhook-new-contact-payload`,`https://api.mailslurp.com/schemas/webhook-new-attachment-payload` respectively.
  , createWebhookOptionsBasicAuth :: Maybe BasicAuthOptions -- ^ 
  , createWebhookOptionsName :: Maybe Text -- ^ Optional name for the webhook
  , createWebhookOptionsEventName :: Maybe Text -- ^ Optional webhook event name. Default is `EMAIL_RECEIVED` and is triggered when an email is received by the inbox associated with the webhook. Payload differ according to the webhook event name. The other events are `NEW_EMAIL`, `NEW_CONTACT`, and `NEW_ATTACHMENT` and `EMAIL_OPENED`. `EMAIL_OPENED` requires the use of tracking pixels when sending. See the email tracking guide for more information.
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


-- | DNS Record required for verification of a domain. Record vary depending on domain type.
data DomainNameRecord = DomainNameRecord
  { domainNameRecordRecordType :: Text -- ^ Domain Name Server Record Types
  , domainNameRecordName :: Text -- ^ 
  , domainNameRecordRecordEntries :: [Text] -- ^ 
  , domainNameRecordTtl :: Integer -- ^ 
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
  , emailTo :: [Text] -- ^ List of `To` recipient email addresses that the email was addressed to. See recipients object for names.
  , emailFrom :: Maybe Text -- ^ Who the email was sent from. An email address - see fromName for the sender name.
  , emailSender :: Maybe Sender -- ^ 
  , emailRecipients :: Maybe EmailRecipients -- ^ 
  , emailReplyTo :: Maybe Text -- ^ The `replyTo` field on the received email message
  , emailCc :: Maybe [Text] -- ^ List of `CC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailBcc :: Maybe [Text] -- ^ List of `BCC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailHeaders :: Maybe (Map.Map String Text) -- ^ Collection of SMTP headers attached to email
  , emailAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  , emailSubject :: Maybe Text -- ^ The subject line of the email message as specified by SMTP subject header
  , emailBody :: Maybe Text -- ^ The body of the email message as text parsed from the SMTP message body (does not include attachments). Fetch the raw content to access the SMTP message and use the attachments property to access attachments. The body is stored separately to the email entity so the body is not returned in paginated results only in full single email or wait requests.
  , emailBodyExcerpt :: Maybe Text -- ^ An excerpt of the body of the email message for quick preview .
  , emailBodyMD5Hash :: Maybe Text -- ^ A hash signature of the email message using MD5. Useful for comparing emails without fetching full body.
  , emailIsHTML :: Maybe Bool -- ^ Is the email body content type HTML?
  , emailCharset :: Maybe Text -- ^ Detected character set of the email body such as UTF-8
  , emailAnalysis :: Maybe EmailAnalysis -- ^ 
  , emailCreatedAt :: UTCTime -- ^ When was the email received by MailSlurp
  , emailUpdatedAt :: UTCTime -- ^ When was the email last updated
  , emailRead :: Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API with a hydrated body? If so the email is marked as read. Paginated results do not affect read status. Read status is different to email opened event as it depends on your own account accessing the email. Email opened is determined by tracking pixels sent to other uses if enable during sending. You can listened for both email read and email opened events using webhooks.
  , emailTeamAccess :: Bool -- ^ Can the email be accessed by organization team members
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


-- | Matches for the given pattern
data EmailContentMatchResult = EmailContentMatchResult
  { emailContentMatchResultPattern :: Text -- ^ 
  , emailContentMatchResultMatches :: [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailContentMatchResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailContentMatchResult")
instance ToJSON EmailContentMatchResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailContentMatchResult")


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
  , emailPreviewSubject :: Maybe Text -- ^ The subject line of the email message as specified by SMTP subject header
  , emailPreviewTo :: [Text] -- ^ List of `To` recipient email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewFrom :: Maybe Text -- ^ Who the email was sent from. An email address - see fromName for the sender name.
  , emailPreviewBcc :: Maybe [Text] -- ^ List of `BCC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewCc :: Maybe [Text] -- ^ List of `CC` recipients email addresses that the email was addressed to. See recipients object for names.
  , emailPreviewCreatedAt :: UTCTime -- ^ When was the email received by MailSlurp
  , emailPreviewRead :: Maybe Bool -- ^ Read flag. Has the email ever been viewed in the dashboard or fetched via the API with a hydrated body? If so the email is marked as read. Paginated results do not affect read status. Read status is different to email opened event as it depends on your own account accessing the email. Email opened is determined by tracking pixels sent to other uses if enable during sending. You can listened for both email read and email opened events using webhooks.
  , emailPreviewAttachments :: Maybe [Text] -- ^ List of IDs of attachments found in the email. Use these IDs with the Inbox and Email Controllers to download attachments and attachment meta data such as filesize, name, extension.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailPreview")
instance ToJSON EmailPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailPreview")


-- | A compact representation of a full email. Used in list endpoints to keep response sizes low. Body and attachments are not included. To get all fields of the email use the &#x60;getEmail&#x60; method with the email projection&#39;s ID. See &#x60;EmailDto&#x60; for documentation on projection properties.
data EmailProjection = EmailProjection
  { emailProjectionId :: UUID -- ^ 
  , emailProjectionFrom :: Maybe Text -- ^ 
  , emailProjectionSubject :: Maybe Text -- ^ 
  , emailProjectionInboxId :: UUID -- ^ 
  , emailProjectionTo :: [Text] -- ^ 
  , emailProjectionAttachments :: Maybe [Text] -- ^ 
  , emailProjectionCreatedAt :: UTCTime -- ^ 
  , emailProjectionBcc :: Maybe [Text] -- ^ 
  , emailProjectionCc :: Maybe [Text] -- ^ 
  , emailProjectionTeamAccess :: Maybe Bool -- ^ 
  , emailProjectionRead :: Maybe Bool -- ^ 
  , emailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  , emailProjectionBodyExcerpt :: Maybe Text -- ^ 
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


-- | Parsed text of an email
data EmailTextLinesResult = EmailTextLinesResult
  { emailTextLinesResultLines :: [Text] -- ^ 
  , emailTextLinesResultBody :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailTextLinesResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailTextLinesResult")
instance ToJSON EmailTextLinesResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailTextLinesResult")


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


-- | 
data ExpiredInboxRecordProjection = ExpiredInboxRecordProjection
  { expiredInboxRecordProjectionId :: UUID -- ^ 
  , expiredInboxRecordProjectionUserId :: UUID -- ^ 
  , expiredInboxRecordProjectionEmailAddress :: Text -- ^ 
  , expiredInboxRecordProjectionCreatedAt :: UTCTime -- ^ 
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
data FlushExpiredInboxesResult = FlushExpiredInboxesResult
  { flushExpiredInboxesResultInboxIds :: [UUID] -- ^ 
  , flushExpiredInboxesResultExpireBefore :: UTCTime -- ^ 
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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ForwardEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "forwardEmailOptions")
instance ToJSON ForwardEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "forwardEmailOptions")


-- | 
data GravatarUrl = GravatarUrl
  { gravatarUrlUrl :: Text -- ^ 
  , gravatarUrlHash :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GravatarUrl where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "gravatarUrl")
instance ToJSON GravatarUrl where
  toJSON = genericToJSON (removeFieldLabelPrefix False "gravatarUrl")


-- | 
data GroupContactsDto = GroupContactsDto
  { groupContactsDtoGroup :: GroupDto -- ^ 
  , groupContactsDtoContacts :: [ContactDto] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupContactsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupContactsDto")
instance ToJSON GroupContactsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupContactsDto")


-- | 
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


-- | 
data GroupProjection = GroupProjection
  { groupProjectionName :: Text -- ^ 
  , groupProjectionId :: UUID -- ^ 
  , groupProjectionDescription :: Maybe Text -- ^ 
  , groupProjectionCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupProjection")
instance ToJSON GroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupProjection")


-- | HTML Validation Results
data HTMLValidationResult = HTMLValidationResult
  { hTMLValidationResultIsValid :: Bool -- ^ Is HTML validation result valid
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
data ImapSmtpAccessDetails = ImapSmtpAccessDetails
  { imapSmtpAccessDetailsSmtpServerHost :: Text -- ^ 
  , imapSmtpAccessDetailsSmtpServerPort :: Int -- ^ 
  , imapSmtpAccessDetailsSmtpUsername :: Text -- ^ 
  , imapSmtpAccessDetailsSmtpPassword :: Text -- ^ 
  , imapSmtpAccessDetailsImapServerHost :: Text -- ^ 
  , imapSmtpAccessDetailsImapServerPort :: Int -- ^ 
  , imapSmtpAccessDetailsImapUsername :: Text -- ^ 
  , imapSmtpAccessDetailsImapPassword :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImapSmtpAccessDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "imapSmtpAccessDetails")
instance ToJSON ImapSmtpAccessDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "imapSmtpAccessDetails")


-- | 
data InboxByEmailAddressResult = InboxByEmailAddressResult
  { inboxByEmailAddressResultInboxId :: Maybe UUID -- ^ 
  , inboxByEmailAddressResultExists :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxByEmailAddressResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxByEmailAddressResult")
instance ToJSON InboxByEmailAddressResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxByEmailAddressResult")


-- | Representation of a MailSlurp inbox. An inbox has an ID and a real email address. Emails can be sent to or from this email address. Inboxes are either &#x60;SMTP&#x60; or &#x60;HTTP&#x60; mailboxes. The default, &#x60;HTTP&#x60; inboxes, use AWS SES to process emails and are best suited as test email accounts. &#x60;SMTP&#x60; inboxes use a custom mail server at &#x60;mx.mailslurp.com&#x60; and can only receive emails - they  are best used for public facing email addresses. Use the &#x60;EmailController&#x60; or the &#x60;InboxController&#x60; methods to send and receive emails and attachments. Inboxes may have a description, name, and tags for display purposes. You can also favourite an inbox for easier searching. Inboxes can be private or allow team access. Team access enabled inboxes can be seen by other members of an organization. 
data InboxDto = InboxDto
  { inboxDtoId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , inboxDtoUserId :: Maybe UUID -- ^ ID of user that inbox belongs to
  , inboxDtoCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , inboxDtoName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , inboxDtoDescription :: Maybe Text -- ^ Description of an inbox for labelling and searching purposes
  , inboxDtoEmailAddress :: Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , inboxDtoExpiresAt :: Maybe Text -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  , inboxDtoFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , inboxDtoTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , inboxDtoInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , inboxDtoReadOnly :: Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxDto")
instance ToJSON InboxDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxDto")


-- | 
data InboxExistsDto = InboxExistsDto
  { inboxExistsDtoExists :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxExistsDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxExistsDto")
instance ToJSON InboxExistsDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxExistsDto")


-- | 
data InboxForwarderDto = InboxForwarderDto
  { inboxForwarderDtoId :: UUID -- ^ 
  , inboxForwarderDtoInboxId :: UUID -- ^ 
  , inboxForwarderDtoField :: Text -- ^ 
  , inboxForwarderDtoMatch :: Text -- ^ 
  , inboxForwarderDtoForwardToRecipients :: [Text] -- ^ 
  , inboxForwarderDtoCreatedAt :: UTCTime -- ^ 
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
  { inboxForwarderTestResultMatches :: (Map.Map String Bool) -- ^ 
  , inboxForwarderTestResultDoesMatch :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxForwarderTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxForwarderTestResult")
instance ToJSON InboxForwarderTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxForwarderTestResult")


-- | 
data InboxIdItem = InboxIdItem
  { inboxIdItemId :: UUID -- ^ 
  , inboxIdItemEmailAddress :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxIdItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxIdItem")
instance ToJSON InboxIdItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxIdItem")


-- | 
data InboxIdsResult = InboxIdsResult
  { inboxIdsResultInboxIds :: [InboxIdItem] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxIdsResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxIdsResult")
instance ToJSON InboxIdsResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxIdsResult")


-- | 
data InboxPreview = InboxPreview
  { inboxPreviewId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , inboxPreviewEmailAddress :: Maybe Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , inboxPreviewCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , inboxPreviewFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , inboxPreviewName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , inboxPreviewTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , inboxPreviewTeamAccess :: Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  , inboxPreviewInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , inboxPreviewExpiresAt :: Maybe Text -- ^ Inbox expiration time. When, if ever, the inbox should expire and be deleted. If null then this inbox is permanent and the emails in it won't be deleted. This is the default behavior unless expiration date is set. If an expiration date is set and the time is reached MailSlurp will expire the inbox and move it to an expired inbox entity. You can still access the emails belonging to it but it can no longer send or receive email.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxPreview where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxPreview")
instance ToJSON InboxPreview where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxPreview")


-- | 
data InboxRulesetDto = InboxRulesetDto
  { inboxRulesetDtoId :: UUID -- ^ 
  , inboxRulesetDtoInboxId :: UUID -- ^ 
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
  { inboxRulesetTestResultRulesetMatches :: (Map.Map String Bool) -- ^ Map of inbox ruleset ID to boolean of if target matches
  , inboxRulesetTestResultMatches :: Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InboxRulesetTestResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inboxRulesetTestResult")
instance ToJSON InboxRulesetTestResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inboxRulesetTestResult")


-- | 
data InlineObject = InlineObject
  { inlineObjectContentTypeHeader :: Maybe Text -- ^ Optional content type header of attachment
  , inlineObjectFile :: FilePath -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InlineObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineObject")
instance ToJSON InlineObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineObject")


-- | 
data JSONSchemaDto = JSONSchemaDto
  { jSONSchemaDtoValue :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JSONSchemaDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jSONSchemaDto")
instance ToJSON JSONSchemaDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jSONSchemaDto")


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


-- | 
data MissedEmail = MissedEmail
  { missedEmailId :: Maybe UUID -- ^ 
  , missedEmailUserId :: Maybe UUID -- ^ 
  , missedEmailSubject :: Maybe Text -- ^ 
  , missedEmailBodyExcerpt :: Maybe Text -- ^ 
  , missedEmailAttachmentCount :: Int -- ^ 
  , missedEmailFrom :: Maybe Text -- ^ 
  , missedEmailTo :: [Text] -- ^ 
  , missedEmailCc :: [Text] -- ^ 
  , missedEmailBcc :: [Text] -- ^ 
  , missedEmailInboxIds :: [UUID] -- ^ 
  , missedEmailCreatedAt :: UTCTime -- ^ 
  , missedEmailUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MissedEmail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "missedEmail")
instance ToJSON MissedEmail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "missedEmail")


-- | 
data MissedEmailProjection = MissedEmailProjection
  { missedEmailProjectionId :: UUID -- ^ 
  , missedEmailProjectionFrom :: Maybe Text -- ^ 
  , missedEmailProjectionSubject :: Maybe Text -- ^ 
  , missedEmailProjectionUserId :: Maybe UUID -- ^ 
  , missedEmailProjectionCreatedAt :: UTCTime -- ^ 
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
data OrganizationInboxProjection = OrganizationInboxProjection
  { organizationInboxProjectionId :: UUID -- ^ ID of the inbox. The ID is a UUID-V4 format string. Use the inboxId for calls to Inbox and Email Controller endpoints. See the emailAddress property for the email address or the inbox. To get emails in an inbox use the WaitFor and Inbox Controller methods `waitForLatestEmail` and `getEmails` methods respectively. Inboxes can be used with aliases to forward emails automatically.
  , organizationInboxProjectionCreatedAt :: UTCTime -- ^ When the inbox was created. Time stamps are in ISO DateTime Format `yyyy-MM-dd'T'HH:mm:ss.SSSXXX` e.g. `2000-10-31T01:30:00.000-05:00`.
  , organizationInboxProjectionName :: Maybe Text -- ^ Name of the inbox and used as the sender name when sending emails .Displayed in the dashboard for easier search
  , organizationInboxProjectionEmailAddress :: Maybe Text -- ^ The inbox's email address. Inbox projections and previews may not include the email address. To view the email address fetch the inbox entity directly. Send an email to this address and the inbox will receive and store it for you. Note the email address in MailSlurp match characters exactly and are case sensitive so `+123` additions are considered different addresses. To retrieve the email use the Inbox and Email Controller endpoints with the inbox ID.
  , organizationInboxProjectionFavourite :: Bool -- ^ Is the inbox a favorite inbox. Make an inbox a favorite is typically done in the dashboard for quick access or filtering
  , organizationInboxProjectionTags :: Maybe [Text] -- ^ Tags that inbox has been tagged with. Tags can be added to inboxes to group different inboxes within an account. You can also search for inboxes by tag in the dashboard UI.
  , organizationInboxProjectionTeamAccess :: Bool -- ^ Does inbox permit team access for organization team members. If so team users can use inbox and emails associated with it. See the team access guide at https://www.mailslurp.com/guides/team-email-account-sharing/
  , organizationInboxProjectionInboxType :: Maybe Text -- ^ Type of inbox. HTTP inboxes are faster and better for most cases. SMTP inboxes are more suited for public facing inbound messages (but cannot send).
  , organizationInboxProjectionReadOnly :: Bool -- ^ Is the inbox readOnly for the caller. Read only means can not be deleted or modified. This flag is present when using team accounts and shared inboxes.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "organizationInboxProjection")
instance ToJSON OrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "organizationInboxProjection")


-- | Paginated email alias results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAlias = PageAlias
  { pageAliasContent :: Maybe [AliasProjection] -- ^ 
  , pageAliasPageable :: Maybe PageableObject -- ^ 
  , pageAliasTotal :: Maybe Integer -- ^ 
  , pageAliasTotalElements :: Maybe Integer -- ^ 
  , pageAliasTotalPages :: Maybe Int -- ^ 
  , pageAliasLast :: Maybe Bool -- ^ 
  , pageAliasSize :: Maybe Int -- ^ 
  , pageAliasNumber :: Maybe Int -- ^ 
  , pageAliasSort :: Maybe Sort -- ^ 
  , pageAliasNumberOfElements :: Maybe Int -- ^ 
  , pageAliasFirst :: Maybe Bool -- ^ 
  , pageAliasEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageAlias where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageAlias")
instance ToJSON PageAlias where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageAlias")


-- | Paginated attachment entity results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageAttachmentEntity = PageAttachmentEntity
  { pageAttachmentEntityContent :: Maybe [AttachmentProjection] -- ^ 
  , pageAttachmentEntityPageable :: Maybe PageableObject -- ^ 
  , pageAttachmentEntityTotal :: Maybe Integer -- ^ 
  , pageAttachmentEntityTotalElements :: Maybe Integer -- ^ 
  , pageAttachmentEntityTotalPages :: Maybe Int -- ^ 
  , pageAttachmentEntityLast :: Maybe Bool -- ^ 
  , pageAttachmentEntitySize :: Maybe Int -- ^ 
  , pageAttachmentEntityNumber :: Maybe Int -- ^ 
  , pageAttachmentEntitySort :: Maybe Sort -- ^ 
  , pageAttachmentEntityNumberOfElements :: Maybe Int -- ^ 
  , pageAttachmentEntityFirst :: Maybe Bool -- ^ 
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
  , pageBouncedEmailTotal :: Maybe Integer -- ^ 
  , pageBouncedEmailTotalElements :: Maybe Integer -- ^ 
  , pageBouncedEmailTotalPages :: Maybe Int -- ^ 
  , pageBouncedEmailLast :: Maybe Bool -- ^ 
  , pageBouncedEmailSize :: Maybe Int -- ^ 
  , pageBouncedEmailNumber :: Maybe Int -- ^ 
  , pageBouncedEmailSort :: Maybe Sort -- ^ 
  , pageBouncedEmailNumberOfElements :: Maybe Int -- ^ 
  , pageBouncedEmailFirst :: Maybe Bool -- ^ 
  , pageBouncedEmailEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageBouncedEmail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageBouncedEmail")
instance ToJSON PageBouncedEmail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageBouncedEmail")


-- | Paginated bounced recipients. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageBouncedRecipients = PageBouncedRecipients
  { pageBouncedRecipientsContent :: Maybe [BounceRecipient] -- ^ 
  , pageBouncedRecipientsPageable :: Maybe PageableObject -- ^ 
  , pageBouncedRecipientsTotal :: Maybe Integer -- ^ 
  , pageBouncedRecipientsTotalElements :: Maybe Integer -- ^ 
  , pageBouncedRecipientsTotalPages :: Maybe Int -- ^ 
  , pageBouncedRecipientsLast :: Maybe Bool -- ^ 
  , pageBouncedRecipientsSize :: Maybe Int -- ^ 
  , pageBouncedRecipientsNumber :: Maybe Int -- ^ 
  , pageBouncedRecipientsSort :: Maybe Sort -- ^ 
  , pageBouncedRecipientsNumberOfElements :: Maybe Int -- ^ 
  , pageBouncedRecipientsFirst :: Maybe Bool -- ^ 
  , pageBouncedRecipientsEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageBouncedRecipients where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageBouncedRecipients")
instance ToJSON PageBouncedRecipients where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageBouncedRecipients")


-- | Paginated contact results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageContactProjection = PageContactProjection
  { pageContactProjectionContent :: Maybe [ContactProjection] -- ^ 
  , pageContactProjectionPageable :: Maybe PageableObject -- ^ 
  , pageContactProjectionTotal :: Maybe Integer -- ^ 
  , pageContactProjectionTotalElements :: Maybe Integer -- ^ 
  , pageContactProjectionTotalPages :: Maybe Int -- ^ 
  , pageContactProjectionLast :: Maybe Bool -- ^ 
  , pageContactProjectionSize :: Maybe Int -- ^ 
  , pageContactProjectionNumber :: Maybe Int -- ^ 
  , pageContactProjectionSort :: Maybe Sort -- ^ 
  , pageContactProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageContactProjectionFirst :: Maybe Bool -- ^ 
  , pageContactProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageContactProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageContactProjection")
instance ToJSON PageContactProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageContactProjection")


-- | Paginated email preview results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageEmailPreview = PageEmailPreview
  { pageEmailPreviewContent :: Maybe [EmailPreview] -- ^ 
  , pageEmailPreviewPageable :: Maybe PageableObject -- ^ 
  , pageEmailPreviewTotal :: Maybe Integer -- ^ 
  , pageEmailPreviewTotalElements :: Maybe Integer -- ^ 
  , pageEmailPreviewTotalPages :: Maybe Int -- ^ 
  , pageEmailPreviewLast :: Maybe Bool -- ^ 
  , pageEmailPreviewSize :: Maybe Int -- ^ 
  , pageEmailPreviewNumber :: Maybe Int -- ^ 
  , pageEmailPreviewSort :: Maybe Sort -- ^ 
  , pageEmailPreviewNumberOfElements :: Maybe Int -- ^ 
  , pageEmailPreviewFirst :: Maybe Bool -- ^ 
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
  , pageEmailProjectionTotal :: Maybe Integer -- ^ 
  , pageEmailProjectionTotalElements :: Maybe Integer -- ^ 
  , pageEmailProjectionTotalPages :: Maybe Int -- ^ 
  , pageEmailProjectionLast :: Maybe Bool -- ^ 
  , pageEmailProjectionSize :: Maybe Int -- ^ 
  , pageEmailProjectionNumber :: Maybe Int -- ^ 
  , pageEmailProjectionSort :: Maybe Sort -- ^ 
  , pageEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageEmailProjection")
instance ToJSON PageEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageEmailProjection")


-- | Paginated expired inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageExpiredInboxRecordProjection = PageExpiredInboxRecordProjection
  { pageExpiredInboxRecordProjectionContent :: Maybe [ExpiredInboxRecordProjection] -- ^ 
  , pageExpiredInboxRecordProjectionPageable :: Maybe PageableObject -- ^ 
  , pageExpiredInboxRecordProjectionTotal :: Maybe Integer -- ^ 
  , pageExpiredInboxRecordProjectionTotalElements :: Maybe Integer -- ^ 
  , pageExpiredInboxRecordProjectionTotalPages :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionLast :: Maybe Bool -- ^ 
  , pageExpiredInboxRecordProjectionSize :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionNumber :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionSort :: Maybe Sort -- ^ 
  , pageExpiredInboxRecordProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageExpiredInboxRecordProjectionFirst :: Maybe Bool -- ^ 
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
  , pageGroupProjectionTotal :: Maybe Integer -- ^ 
  , pageGroupProjectionTotalElements :: Maybe Integer -- ^ 
  , pageGroupProjectionTotalPages :: Maybe Int -- ^ 
  , pageGroupProjectionLast :: Maybe Bool -- ^ 
  , pageGroupProjectionSize :: Maybe Int -- ^ 
  , pageGroupProjectionNumber :: Maybe Int -- ^ 
  , pageGroupProjectionSort :: Maybe Sort -- ^ 
  , pageGroupProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageGroupProjectionFirst :: Maybe Bool -- ^ 
  , pageGroupProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageGroupProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageGroupProjection")
instance ToJSON PageGroupProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageGroupProjection")


-- | Paginated inbox forwarder results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxForwarderDto = PageInboxForwarderDto
  { pageInboxForwarderDtoContent :: Maybe [InboxForwarderDto] -- ^ 
  , pageInboxForwarderDtoPageable :: Maybe PageableObject -- ^ 
  , pageInboxForwarderDtoTotal :: Maybe Integer -- ^ 
  , pageInboxForwarderDtoTotalElements :: Maybe Integer -- ^ 
  , pageInboxForwarderDtoTotalPages :: Maybe Int -- ^ 
  , pageInboxForwarderDtoLast :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoSize :: Maybe Int -- ^ 
  , pageInboxForwarderDtoNumber :: Maybe Int -- ^ 
  , pageInboxForwarderDtoSort :: Maybe Sort -- ^ 
  , pageInboxForwarderDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxForwarderDtoFirst :: Maybe Bool -- ^ 
  , pageInboxForwarderDtoEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxForwarderDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxForwarderDto")
instance ToJSON PageInboxForwarderDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxForwarderDto")


-- | Paginated inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxProjection = PageInboxProjection
  { pageInboxProjectionContent :: Maybe [InboxPreview] -- ^ 
  , pageInboxProjectionPageable :: Maybe PageableObject -- ^ 
  , pageInboxProjectionTotal :: Maybe Integer -- ^ 
  , pageInboxProjectionTotalElements :: Maybe Integer -- ^ 
  , pageInboxProjectionTotalPages :: Maybe Int -- ^ 
  , pageInboxProjectionLast :: Maybe Bool -- ^ 
  , pageInboxProjectionSize :: Maybe Int -- ^ 
  , pageInboxProjectionNumber :: Maybe Int -- ^ 
  , pageInboxProjectionSort :: Maybe Sort -- ^ 
  , pageInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageInboxProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxProjection")
instance ToJSON PageInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxProjection")


-- | Paginated inbox ruleset results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageInboxRulesetDto = PageInboxRulesetDto
  { pageInboxRulesetDtoContent :: Maybe [InboxRulesetDto] -- ^ 
  , pageInboxRulesetDtoPageable :: Maybe PageableObject -- ^ 
  , pageInboxRulesetDtoTotal :: Maybe Integer -- ^ 
  , pageInboxRulesetDtoTotalElements :: Maybe Integer -- ^ 
  , pageInboxRulesetDtoTotalPages :: Maybe Int -- ^ 
  , pageInboxRulesetDtoLast :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoSize :: Maybe Int -- ^ 
  , pageInboxRulesetDtoNumber :: Maybe Int -- ^ 
  , pageInboxRulesetDtoSort :: Maybe Sort -- ^ 
  , pageInboxRulesetDtoNumberOfElements :: Maybe Int -- ^ 
  , pageInboxRulesetDtoFirst :: Maybe Bool -- ^ 
  , pageInboxRulesetDtoEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageInboxRulesetDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageInboxRulesetDto")
instance ToJSON PageInboxRulesetDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageInboxRulesetDto")


-- | Paginated MissedEmail results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageMissedEmailProjection = PageMissedEmailProjection
  { pageMissedEmailProjectionContent :: Maybe [MissedEmailProjection] -- ^ 
  , pageMissedEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageMissedEmailProjectionTotal :: Maybe Integer -- ^ 
  , pageMissedEmailProjectionTotalElements :: Maybe Integer -- ^ 
  , pageMissedEmailProjectionTotalPages :: Maybe Int -- ^ 
  , pageMissedEmailProjectionLast :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionSize :: Maybe Int -- ^ 
  , pageMissedEmailProjectionNumber :: Maybe Int -- ^ 
  , pageMissedEmailProjectionSort :: Maybe Sort -- ^ 
  , pageMissedEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageMissedEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageMissedEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageMissedEmailProjection")
instance ToJSON PageMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageMissedEmailProjection")


-- | Paginated organization inbox results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageOrganizationInboxProjection = PageOrganizationInboxProjection
  { pageOrganizationInboxProjectionContent :: Maybe [OrganizationInboxProjection] -- ^ 
  , pageOrganizationInboxProjectionPageable :: Maybe PageableObject -- ^ 
  , pageOrganizationInboxProjectionTotal :: Maybe Integer -- ^ 
  , pageOrganizationInboxProjectionTotalElements :: Maybe Integer -- ^ 
  , pageOrganizationInboxProjectionTotalPages :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionLast :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionSize :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionNumber :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionSort :: Maybe Sort -- ^ 
  , pageOrganizationInboxProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageOrganizationInboxProjectionFirst :: Maybe Bool -- ^ 
  , pageOrganizationInboxProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageOrganizationInboxProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageOrganizationInboxProjection")
instance ToJSON PageOrganizationInboxProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageOrganizationInboxProjection")


-- | Paginated sent email results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full sent email entity use the projection ID with individual method calls.
data PageSentEmailProjection = PageSentEmailProjection
  { pageSentEmailProjectionContent :: [SentEmailProjection] -- ^ Collection of items
  , pageSentEmailProjectionPageable :: Maybe PageableObject -- ^ 
  , pageSentEmailProjectionTotal :: Maybe Integer -- ^ 
  , pageSentEmailProjectionSize :: Int -- ^ Size of page requested
  , pageSentEmailProjectionNumber :: Int -- ^ Page number starting at 0
  , pageSentEmailProjectionNumberOfElements :: Int -- ^ Number of items returned
  , pageSentEmailProjectionTotalElements :: Integer -- ^ Total number of items available for querying
  , pageSentEmailProjectionTotalPages :: Int -- ^ Total number of pages available
  , pageSentEmailProjectionLast :: Maybe Bool -- ^ 
  , pageSentEmailProjectionSort :: Maybe Sort -- ^ 
  , pageSentEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageSentEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageSentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageSentEmailProjection")
instance ToJSON PageSentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageSentEmailProjection")


-- | Paginated email template results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTemplateProjection = PageTemplateProjection
  { pageTemplateProjectionContent :: Maybe [TemplateProjection] -- ^ 
  , pageTemplateProjectionPageable :: Maybe PageableObject -- ^ 
  , pageTemplateProjectionTotal :: Maybe Integer -- ^ 
  , pageTemplateProjectionTotalElements :: Maybe Integer -- ^ 
  , pageTemplateProjectionTotalPages :: Maybe Int -- ^ 
  , pageTemplateProjectionLast :: Maybe Bool -- ^ 
  , pageTemplateProjectionSize :: Maybe Int -- ^ 
  , pageTemplateProjectionNumber :: Maybe Int -- ^ 
  , pageTemplateProjectionSort :: Maybe Sort -- ^ 
  , pageTemplateProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTemplateProjectionFirst :: Maybe Bool -- ^ 
  , pageTemplateProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageTemplateProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageTemplateProjection")
instance ToJSON PageTemplateProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageTemplateProjection")


-- | Paginated email projection results. EmailProjections and EmailPreviews are essentially the same but have legacy naming issues. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full email entity use the projection ID with individual method calls. For emails there are several methods for fetching message bodies and attachments.
data PageThreadProjection = PageThreadProjection
  { pageThreadProjectionContent :: Maybe [ThreadProjection] -- ^ 
  , pageThreadProjectionPageable :: Maybe PageableObject -- ^ 
  , pageThreadProjectionTotal :: Maybe Integer -- ^ 
  , pageThreadProjectionTotalElements :: Maybe Integer -- ^ 
  , pageThreadProjectionTotalPages :: Maybe Int -- ^ 
  , pageThreadProjectionLast :: Maybe Bool -- ^ 
  , pageThreadProjectionSize :: Maybe Int -- ^ 
  , pageThreadProjectionNumber :: Maybe Int -- ^ 
  , pageThreadProjectionSort :: Maybe Sort -- ^ 
  , pageThreadProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageThreadProjectionFirst :: Maybe Bool -- ^ 
  , pageThreadProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageThreadProjection")
instance ToJSON PageThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageThreadProjection")


-- | Paginated TrackingPixel results. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageTrackingPixelProjection = PageTrackingPixelProjection
  { pageTrackingPixelProjectionContent :: Maybe [TrackingPixelProjection] -- ^ 
  , pageTrackingPixelProjectionPageable :: Maybe PageableObject -- ^ 
  , pageTrackingPixelProjectionTotal :: Maybe Integer -- ^ 
  , pageTrackingPixelProjectionTotalElements :: Maybe Integer -- ^ 
  , pageTrackingPixelProjectionTotalPages :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionLast :: Maybe Bool -- ^ 
  , pageTrackingPixelProjectionSize :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionNumber :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionSort :: Maybe Sort -- ^ 
  , pageTrackingPixelProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageTrackingPixelProjectionFirst :: Maybe Bool -- ^ 
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
  , pageUnknownMissedEmailProjectionTotal :: Maybe Integer -- ^ 
  , pageUnknownMissedEmailProjectionTotalElements :: Maybe Integer -- ^ 
  , pageUnknownMissedEmailProjectionTotalPages :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionLast :: Maybe Bool -- ^ 
  , pageUnknownMissedEmailProjectionSize :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionNumber :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionSort :: Maybe Sort -- ^ 
  , pageUnknownMissedEmailProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageUnknownMissedEmailProjectionFirst :: Maybe Bool -- ^ 
  , pageUnknownMissedEmailProjectionEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageUnknownMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageUnknownMissedEmailProjection")
instance ToJSON PageUnknownMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageUnknownMissedEmailProjection")


-- | Paginated webhook entity. Page index starts at zero. Projection results may omit larger entity fields. For fetching a full entity use the projection ID with individual method calls.
data PageWebhookProjection = PageWebhookProjection
  { pageWebhookProjectionContent :: Maybe [WebhookProjection] -- ^ 
  , pageWebhookProjectionPageable :: Maybe PageableObject -- ^ 
  , pageWebhookProjectionTotal :: Maybe Integer -- ^ 
  , pageWebhookProjectionTotalElements :: Maybe Integer -- ^ 
  , pageWebhookProjectionTotalPages :: Maybe Int -- ^ 
  , pageWebhookProjectionLast :: Maybe Bool -- ^ 
  , pageWebhookProjectionSize :: Maybe Int -- ^ 
  , pageWebhookProjectionNumber :: Maybe Int -- ^ 
  , pageWebhookProjectionSort :: Maybe Sort -- ^ 
  , pageWebhookProjectionNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookProjectionFirst :: Maybe Bool -- ^ 
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
  , pageWebhookResultTotal :: Maybe Integer -- ^ 
  , pageWebhookResultTotalElements :: Maybe Integer -- ^ 
  , pageWebhookResultTotalPages :: Maybe Int -- ^ 
  , pageWebhookResultLast :: Maybe Bool -- ^ 
  , pageWebhookResultSize :: Maybe Int -- ^ 
  , pageWebhookResultNumber :: Maybe Int -- ^ 
  , pageWebhookResultSort :: Maybe Sort -- ^ 
  , pageWebhookResultNumberOfElements :: Maybe Int -- ^ 
  , pageWebhookResultFirst :: Maybe Bool -- ^ 
  , pageWebhookResultEmpty :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageWebhookResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageWebhookResult")
instance ToJSON PageWebhookResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageWebhookResult")


-- | 
data PageableObject = PageableObject
  { pageableObjectOffset :: Maybe Integer -- ^ 
  , pageableObjectSort :: Maybe Sort -- ^ 
  , pageableObjectPageNumber :: Maybe Int -- ^ 
  , pageableObjectPageSize :: Maybe Int -- ^ 
  , pageableObjectPaged :: Maybe Bool -- ^ 
  , pageableObjectUnpaged :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageableObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageableObject")
instance ToJSON PageableObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageableObject")


-- | Content in raw format
data RawEmailJson = RawEmailJson
  { rawEmailJsonContent :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RawEmailJson where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "rawEmailJson")
instance ToJSON RawEmailJson where
  toJSON = genericToJSON (removeFieldLabelPrefix False "rawEmailJson")


-- | 
data Recipient = Recipient
  { recipientRawValue :: Text -- ^ 
  , recipientEmailAddress :: Text -- ^ 
  , recipientName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Recipient where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "recipient")
instance ToJSON Recipient where
  toJSON = genericToJSON (removeFieldLabelPrefix False "recipient")


-- | Options for replying to an alias email using the alias inbox
data ReplyToAliasEmailOptions = ReplyToAliasEmailOptions
  { replyToAliasEmailOptionsBody :: Text -- ^ Body of the reply email you want to send
  , replyToAliasEmailOptionsIsHTML :: Bool -- ^ Is the reply HTML
  , replyToAliasEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToAliasEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToAliasEmailOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variables if using a template
  , replyToAliasEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToAliasEmailOptionsSendStrategy :: Maybe Text -- ^ How an email should be sent based on its recipients
  , replyToAliasEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
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
  , replyToEmailOptionsCharset :: Maybe Text -- ^ The charset that your message should be sent with. Optional. Default is UTF-8
  , replyToEmailOptionsAttachments :: Maybe [Text] -- ^ List of uploaded attachments to send with the reply. Optional.
  , replyToEmailOptionsTemplateVariables :: Maybe (Map.Map String Value) -- ^ Template variables if using a template
  , replyToEmailOptionsTemplate :: Maybe UUID -- ^ Template ID to use instead of body. Will use template variable map to fill defined variable slots.
  , replyToEmailOptionsSendStrategy :: Maybe Text -- ^ How an email should be sent based on its recipients
  , replyToEmailOptionsUseInboxName :: Maybe Bool -- ^ Optionally use inbox name as display name for sender email address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReplyToEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "replyToEmailOptions")
instance ToJSON ReplyToEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "replyToEmailOptions")


-- | Options for the email to be sent
data SendEmailOptions = SendEmailOptions
  { sendEmailOptionsToContacts :: Maybe [UUID] -- ^ Optional list of contact IDs to send email to. Manage your contacts via the API or dashboard. When contacts are used the email is sent to each contact separately so they will not see other recipients.
  , sendEmailOptionsToGroup :: Maybe UUID -- ^ Optional contact group ID to send email to. You can create contacts and contact groups in the API or dashboard and use them for email campaigns. When contact groups are used the email is sent to each contact separately so they will not see other recipients
  , sendEmailOptionsTo :: Maybe [Text] -- ^ List of destination email addresses. Each email address must be RFC 5322 format. Even single recipients must be in array form. Maximum recipients per email depends on your plan. If you need to send many emails try using contacts or contact groups or use a non standard sendStrategy to ensure that spam filters are not triggered (many recipients in one email can affect your spam rating). Be cautious when sending emails that your recipients exist. High bounce rates (meaning a high percentage of emails cannot be delivered because an address does not exist) can result in account freezing.
  , sendEmailOptionsFrom :: Maybe Text -- ^ Optional from address. Email address is RFC 5322 format and may include a display name and email in angle brackets (`my@address.com` or `My inbox <my@address.com>`). If no sender is set the source inbox address will be used for this field. If you set `useInboxName` to `true` the from field will include the inbox name as a display name: `inbox_name <inbox@address.com>`. For this to work use the name field when creating an inbox. Beware of potential spam penalties when setting the from field to an address not used by the inbox. Your emails may get blocked by services if you impersonate another address. To use a custom email addresses use a custom domain. You can create domains with the DomainController. The domain must be verified in the dashboard before it can be used.
  , sendEmailOptionsCc :: Maybe [Text] -- ^ Optional list of cc destination email addresses
  , sendEmailOptionsBcc :: Maybe [Text] -- ^ Optional list of bcc destination email addresses
  , sendEmailOptionsSubject :: Maybe Text -- ^ Optional email subject line
  , sendEmailOptionsReplyTo :: Maybe Text -- ^ Optional replyTo header
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
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendEmailOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sendEmailOptions")
instance ToJSON SendEmailOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sendEmailOptions")


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


-- | Sent email details
data SentEmailDto = SentEmailDto
  { sentEmailDtoId :: UUID -- ^ ID of sent email
  , sentEmailDtoUserId :: UUID -- ^ User ID
  , sentEmailDtoInboxId :: UUID -- ^ Inbox ID email was sent from
  , sentEmailDtoTo :: Maybe [Text] -- ^ Recipients email was sent to
  , sentEmailDtoFrom :: Maybe Text -- ^ 
  , sentEmailDtoReplyTo :: Maybe Text -- ^ 
  , sentEmailDtoCc :: Maybe [Text] -- ^ 
  , sentEmailDtoBcc :: Maybe [Text] -- ^ 
  , sentEmailDtoAttachments :: Maybe [Text] -- ^ Array of IDs of attachments that were sent with this email
  , sentEmailDtoSubject :: Maybe Text -- ^ 
  , sentEmailDtoBodyMD5Hash :: Maybe Text -- ^ MD5 Hash
  , sentEmailDtoBody :: Maybe Text -- ^ 
  , sentEmailDtoCharset :: Maybe Text -- ^ 
  , sentEmailDtoIsHTML :: Maybe Bool -- ^ 
  , sentEmailDtoSentAt :: UTCTime -- ^ 
  , sentEmailDtoPixelIds :: Maybe [UUID] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailDto")
instance ToJSON SentEmailDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailDto")


-- | Collection of items
data SentEmailProjection = SentEmailProjection
  { sentEmailProjectionId :: UUID -- ^ 
  , sentEmailProjectionFrom :: Maybe Text -- ^ 
  , sentEmailProjectionUserId :: UUID -- ^ 
  , sentEmailProjectionSubject :: Maybe Text -- ^ 
  , sentEmailProjectionInboxId :: UUID -- ^ 
  , sentEmailProjectionTo :: [Text] -- ^ 
  , sentEmailProjectionAttachments :: [Text] -- ^ 
  , sentEmailProjectionCreatedAt :: UTCTime -- ^ 
  , sentEmailProjectionBcc :: [Text] -- ^ 
  , sentEmailProjectionCc :: [Text] -- ^ 
  , sentEmailProjectionBodyMD5Hash :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SentEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sentEmailProjection")
instance ToJSON SentEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sentEmailProjection")


-- | Options for setting inbox favourite state
data SetInboxFavouritedOptions = SetInboxFavouritedOptions
  { setInboxFavouritedOptionsState :: Bool -- ^ Is the inbox a favorite. Marking an inbox as a favorite is typically done in the dashboard for quick access or filtering
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SetInboxFavouritedOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "setInboxFavouritedOptions")
instance ToJSON SetInboxFavouritedOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "setInboxFavouritedOptions")


-- | 
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
  { templateDtoId :: UUID -- ^ 
  , templateDtoName :: Text -- ^ 
  , templateDtoVariables :: [TemplateVariable] -- ^ 
  , templateDtoContent :: Text -- ^ 
  , templateDtoCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateDto")
instance ToJSON TemplateDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateDto")


-- | 
data TemplateProjection = TemplateProjection
  { templateProjectionName :: Text -- ^ 
  , templateProjectionId :: UUID -- ^ 
  , templateProjectionCreatedAt :: UTCTime -- ^ 
  , templateProjectionVariables :: [Text] -- ^ 
  , templateProjectionUpdatedAt :: UTCTime -- ^ 
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
  { threadProjectionName :: Maybe Text -- ^ 
  , threadProjectionId :: UUID -- ^ 
  , threadProjectionSubject :: Maybe Text -- ^ 
  , threadProjectionUserId :: UUID -- ^ 
  , threadProjectionInboxId :: UUID -- ^ 
  , threadProjectionTo :: [Text] -- ^ 
  , threadProjectionCreatedAt :: UTCTime -- ^ 
  , threadProjectionBcc :: Maybe [Text] -- ^ 
  , threadProjectionCc :: Maybe [Text] -- ^ 
  , threadProjectionUpdatedAt :: UTCTime -- ^ 
  , threadProjectionAliasId :: UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ThreadProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "threadProjection")
instance ToJSON ThreadProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "threadProjection")


-- | 
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


-- | 
data TrackingPixelProjection = TrackingPixelProjection
  { trackingPixelProjectionName :: Maybe Text -- ^ 
  , trackingPixelProjectionId :: UUID -- ^ 
  , trackingPixelProjectionUserId :: UUID -- ^ 
  , trackingPixelProjectionInboxId :: Maybe UUID -- ^ 
  , trackingPixelProjectionCreatedAt :: UTCTime -- ^ 
  , trackingPixelProjectionSeen :: Bool -- ^ 
  , trackingPixelProjectionRecipient :: Maybe Text -- ^ 
  , trackingPixelProjectionSeenAt :: Maybe UTCTime -- ^ 
  , trackingPixelProjectionSentEmailId :: Maybe UUID -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackingPixelProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trackingPixelProjection")
instance ToJSON TrackingPixelProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trackingPixelProjection")


-- | 
data UnknownMissedEmailProjection = UnknownMissedEmailProjection
  { unknownMissedEmailProjectionId :: UUID -- ^ 
  , unknownMissedEmailProjectionFrom :: Maybe Text -- ^ 
  , unknownMissedEmailProjectionSubject :: Maybe Text -- ^ 
  , unknownMissedEmailProjectionTo :: Maybe [Text] -- ^ 
  , unknownMissedEmailProjectionCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnknownMissedEmailProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unknownMissedEmailProjection")
instance ToJSON UnknownMissedEmailProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unknownMissedEmailProjection")


-- | 
data UnreadCount = UnreadCount
  { unreadCountCount :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnreadCount where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "unreadCount")
instance ToJSON UnreadCount where
  toJSON = genericToJSON (removeFieldLabelPrefix False "unreadCount")


-- | 
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


-- | Options for uploading files for attachments. When sending emails with the API that require attachments first upload each attachment. Then use the returned attachment ID in your &#x60;SendEmailOptions&#x60; when sending an email. This way you can use attachments multiple times once they have been uploaded.
data UploadAttachmentOptions = UploadAttachmentOptions
  { uploadAttachmentOptionsContentType :: Maybe Text -- ^ Optional contentType for file. For instance `application/pdf`
  , uploadAttachmentOptionsFilename :: Maybe Text -- ^ Optional filename to save upload with. Will be the name that is shown in email clients
  , uploadAttachmentOptionsBase64Contents :: Text -- ^ Base64 encoded string of file contents. Typically this means reading the bytes or string content of a file and then converting that to a base64 encoded string. For examples of how to do this see https://www.mailslurp.com/guides/base64-file-uploads/
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UploadAttachmentOptions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "uploadAttachmentOptions")
instance ToJSON UploadAttachmentOptions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "uploadAttachmentOptions")


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


-- | Representation of a webhook for an inbox. The URL specified will be using by MailSlurp whenever an email is received by the attached inbox. A webhook entity should have a URL that points to your server. Your server should accept HTTP/S POST requests and return a success 200. MailSlurp will retry your webhooks if they fail. See https://api.mailslurp.com/schemas/webhook-payload for the payload schema.
data WebhookDto = WebhookDto
  { webhookDtoId :: UUID -- ^ ID of the Webhook
  , webhookDtoUserId :: UUID -- ^ User ID of the Webhook
  , webhookDtoBasicAuth :: Bool -- ^ Does webhook expect basic authentication? If true it means you created this webhook with a username and password. MailSlurp will use these in the URL to authenticate itself.
  , webhookDtoName :: Maybe Text -- ^ Name of the webhook
  , webhookDtoInboxId :: UUID -- ^ The inbox that the Webhook will be triggered by
  , webhookDtoUrl :: Text -- ^ URL of your server that the webhook will be sent to. The schema of the JSON that is sent is described by the payloadJsonSchema.
  , webhookDtoMethod :: Text -- ^ HTTP method that your server endpoint must listen for
  , webhookDtoPayloadJsonSchema :: Text -- ^ Deprecated. Fetch JSON Schema for webhook using the getJsonSchemaForWebhookPayload method
  , webhookDtoCreatedAt :: UTCTime -- ^ When the webhook was created
  , webhookDtoUpdatedAt :: UTCTime -- ^ 
  , webhookDtoEventName :: Maybe Text -- ^ 
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
  , webhookEmailOpenedPayloadInboxId :: UUID -- ^ Id of the inbox that received an email
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
  , webhookEmailReadPayloadInboxId :: UUID -- ^ Id of the inbox that received an email
  , webhookEmailReadPayloadEmailIsRead :: Bool -- ^ Is the email read
  , webhookEmailReadPayloadCreatedAt :: UTCTime -- ^ Date time of event creation
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEmailReadPayload where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookEmailReadPayload")
instance ToJSON WebhookEmailReadPayload where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookEmailReadPayload")


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
  , webhookNewContactPayloadContactId :: UUID -- ^ 
  , webhookNewContactPayloadGroupId :: Maybe UUID -- ^ 
  , webhookNewContactPayloadFirstName :: Maybe Text -- ^ 
  , webhookNewContactPayloadLastName :: Maybe Text -- ^ 
  , webhookNewContactPayloadCompany :: Maybe Text -- ^ 
  , webhookNewContactPayloadPrimaryEmailAddress :: Maybe Text -- ^ 
  , webhookNewContactPayloadEmailAddresses :: [Text] -- ^ 
  , webhookNewContactPayloadTags :: [Text] -- ^ 
  , webhookNewContactPayloadMetaData :: Maybe Value -- ^ 
  , webhookNewContactPayloadOptOut :: Maybe Bool -- ^ 
  , webhookNewContactPayloadCreatedAt :: UTCTime -- ^ 
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
  , webhookNewEmailPayloadInboxId :: UUID -- ^ Id of the inbox that received an email
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


-- | Representation of a webhook
data WebhookProjection = WebhookProjection
  { webhookProjectionName :: Maybe Text -- ^ 
  , webhookProjectionId :: UUID -- ^ 
  , webhookProjectionUrl :: Text -- ^ 
  , webhookProjectionInboxId :: UUID -- ^ 
  , webhookProjectionEventName :: Maybe Text -- ^ 
  , webhookProjectionCreatedAt :: UTCTime -- ^ 
  , webhookProjectionUpdatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookProjection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookProjection")
instance ToJSON WebhookProjection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookProjection")


-- | 
data WebhookRedriveResult = WebhookRedriveResult
  { webhookRedriveResultWebhookResultId :: UUID -- ^ 
  , webhookRedriveResultSuccess :: Bool -- ^ 
  , webhookRedriveResultMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookRedriveResult where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookRedriveResult")
instance ToJSON WebhookRedriveResult where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookRedriveResult")


-- | 
data WebhookResultDto = WebhookResultDto
  { webhookResultDtoId :: Maybe UUID -- ^ 
  , webhookResultDtoUserId :: UUID -- ^ 
  , webhookResultDtoInboxId :: UUID -- ^ 
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
  , webhookResultDtoSeen :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookResultDto where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookResultDto")
instance ToJSON WebhookResultDto where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookResultDto")


-- | 
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


-- | 
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
