unit MistralAI.Conversations.Chunks;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.JSON, System.Rtti, System.Generics.Collections,
  REST.JsonReflect, REST.Json.Types, REST.Json,
  MistralAI.Types, MistralAI.API.Params;

type
  TImageUrl = class
  private
    FUrl: string;
    FDetail: string;
  public
    property Url: string read FUrl write FUrl;
    property Detail: string read FDetail write FDetail;
  end;

  {$REGION 'TContentChunk'}

  /// <summary>
  /// Serves as the base class for content chunk types, encapsulating common functionality shared by all chunk classes.
  /// </summary>
  /// <remarks>
  /// This class defines the <c>Type</c> property, which indicates how a chunk's content should be interpreted
  /// (e.g., as text, an image URL, a document link, etc.). Derived classes inherit this property and may
  /// extend it with additional fields and behavior specific to their chunk type.
  /// </remarks>
  TMessageOutputContentChunksCommon = class
  private
    [JsonReflectAttribute(ctString, rtString, TContentChunkTypeInterceptor)]
    FType: TContentChunkType;
  public
    /// <summary>
    /// Gets or sets the content chunk’s type, determining the format or category of the chunk.
    /// </summary>
    /// <value>
    /// A <see cref="TContentChunkType"/> enumeration value specifying the chunk’s kind (for example,
    /// <c>Text</c>, <c>ImageUrl</c>, <c>ToolFile</c>, etc.).
    /// </value>
    property &Type: TContentChunkType read FType write FType;
  end;

  /// <summary>
  /// Represents a chunk containing plain text within a conversation output.
  /// </summary>
  /// <remarks>
  /// Inherits the <see cref="TMessageOutputContentChunksCommon"/> base and adds
  /// the <c>Text</c> property to hold the actual textual content of the chunk.
  /// </remarks>
  TTextChunk = class(TMessageOutputContentChunksCommon)
  private
    FText: string;
  public
    /// <summary>
    /// Gets or sets the textual content of this chunk.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the text payload for this chunk.
    /// </value>
    property Text: string read FText write FText;
  end;

  /// <summary>
  /// Represents a chunk that includes an image URL, extending the plain-text chunk functionality.
  /// </summary>
  /// <remarks>
  /// Inherits from <see cref="TTextChunk"/> and introduces the <c>ImageUrl</c> property,
  /// which encapsulates a URL (and optional detail) pointing to an image associated with this chunk.
  /// </remarks>
  TImageURLChunk = class(TTextChunk)
  private
    [JsonNameAttribute('image_url')]
    FImageUrl: TImageUrl;
  public
    /// <summary>
    /// Gets or sets the image URL information for this chunk.
    /// </summary>
    /// <value>
    /// A <see cref="TImageUrl"/> instance containing the image’s URL and any descriptive detail.
    /// </value>
    property ImageUrl: TImageUrl read FImageUrl write FImageUrl;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a content chunk that includes metadata for a tool-generated file,
  /// extending image URL capabilities with file-specific information.
  /// </summary>
  /// <remarks>
  /// Inherits from <see cref="TImageURLChunk"/> to combine image URL support with
  /// tool execution context. Adds properties for identifying the tool, file ID,
  /// file name, and file type to facilitate handling of file attachments or outputs.
  /// </remarks>
  TToolFileChunk = class(TImageURLChunk)
  private
    [JsonReflectAttribute(ctString, rtString, TConversationToolInterceptor)]
    FTool: TConversationTool;
    [JsonNameAttribute('file_id')]
    FFileId: string;
    [JsonNameAttribute('file_name')]
    FFileName: string;
    [JsonNameAttribute('file_type')]
    FFileType: string;
  public
    /// <summary>
    /// Gets or sets the tool that produced or is associated with this file chunk.
    /// </summary>
    /// <value>
    /// A <see cref="TConversationTool"/> value identifying the originating tool.
    /// </value>
    property Tool: TConversationTool read FTool write FTool;

    /// <summary>
    /// Gets or sets the unique identifier assigned to the file.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the file’s ID within the tool context.
    /// </value>
    property FileId: string read FFileId write FFileId;

    /// <summary>
    /// Gets or sets the name of the file.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the human-readable file name.
    /// </value>
    property FileName: string read FFileName write FFileName;

    /// <summary>
    /// Gets or sets the type or MIME type of the file.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> describing the file’s format or media type.
    /// </value>
    property FileType: string read FFileType write FFileType;
  end;

  /// <summary>
  /// Represents a content chunk that includes a document link,
  /// extending tool file chunk functionality with document-specific metadata.
  /// </summary>
  /// <remarks>
  /// Inherits from <see cref="TToolFileChunk"/> to combine file and image URL support
  /// with document handling. Adds properties for the document’s URL and its display name,
  /// facilitating the integration of linked documents within conversation outputs.
  /// </remarks>
  TDocumentURLChunk = class(TToolFileChunk)
  private
    [JsonNameAttribute('document_url')]
    FDocumentUrl: string;
    [JsonNameAttribute('document_name')]
    FDocumentName: string;
  public
    /// <summary>
    /// Gets or sets the URL pointing to the document resource.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the fully-qualified URL of the document.
    /// </value>
    property DocumentUrl: string read FDocumentUrl write FDocumentUrl;

    /// <summary>
    /// Gets or sets the display name of the document.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the human-readable name of the document.
    /// </value>
    property DocumentName: string read FDocumentName write FDocumentName;
  end;

  /// <summary>
  /// Represents a content chunk that includes a reference to an external tool resource or metadata link,
  /// extending document URL chunk functionality with additional reference details.
  /// </summary>
  /// <remarks>
  /// Inherits from <see cref="TDocumentURLChunk"/> to combine document linking with tool and file context.
  /// Adds properties for title, URL, source identifier, favicon link, and descriptive text to support rich
  /// reference integration within conversation outputs.
  /// </remarks>
  TToolReferenceChunk = class(TDocumentURLChunk)
  private
    FTitle: string;
    FUrl: string;
    FSource: string;
    FFavicon: string;
    FDescription: string;
  public
    /// <summary>
    /// Gets or sets the title of the referenced resource.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the human-readable title or headline of the reference.
    /// </value>
    property Title: string read FTitle write FTitle;

    /// <summary>
    /// Gets or sets the URL of the referenced resource.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the fully-qualified link to the external resource.
    /// </value>
    property Url: string read FUrl write FUrl;

    /// <summary>
    /// Gets or sets the source identifier for the reference, such as a domain or tool name.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> indicating the origin or provider of the referenced content.
    /// </value>
    property Source: string read FSource write FSource;

    /// <summary>
    /// Gets or sets the favicon URL of the referenced source.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the link to the favicon image used to visually represent the source.
    /// </value>
    property Favicon: string read FFavicon write FFavicon;

    /// <summary>
    /// Gets or sets a brief description of the referenced resource.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> providing contextual or descriptive information about the link.
    /// </value>
    property Description: string read FDescription write FDescription;
  end;

  {$ENDREGION}

  /// <summary>
  /// Represents a concrete, all‑purpose content chunk that aggregates every inherited chunk capability.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TToolReferenceChunk"/>.</para>
  /// <para>Derives from <see cref="TDocumentURLChunk"/>.</para>
  /// <para>Derives from <see cref="TToolFileChunk"/>.</para>
  /// <para>Derives from <see cref="TImageURLChunk"/>.</para>
  /// <para>Derives from <see cref="TTextChunk"/>.</para>
  /// <para>Derives from <see cref="TMessageOutputContentChunksCommon"/>.</para>
  /// <para>Use <c>TContentChunk</c> when you need a single chunk type capable of representing any supported content form.</para>
  /// </remarks>
  TContentChunk = class(TToolReferenceChunk);

  {$REGION 'TConversationChunk'}

  TOutputCommon = class(TJSONFingerprint)
  private
    FObject: string;
    [JsonReflectAttribute(ctString, rtString, TConversatonEventInterceptor)]
    FType: TConversatonEvent;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('completed_at')]
    FCompletedAt: string;
    FId: string;
  public
    /// <summary>
    /// Gets or sets the API object type.
    /// </summary>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the subtype of this entry.
    /// </summary>
    property &Type: TConversatonEvent read FType write FType;

    /// <summary>
    /// Gets or sets the timestamp when the entry was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Gets or sets the timestamp when the entry was completed.
    /// </summary>
    property CompletedAt: string read FCompletedAt write FCompletedAt;

    /// <summary>
    /// Gets or sets the unique identifier of the entry.
    /// </summary>
    property Id: string read FId write FId;
  end;

  /// <summary>
  /// Represents an entry for a tool execution within a conversation, capturing the tool’s identifier and associated metadata.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TOutputCommon"/>.</para>
  /// <para>Defines the <see cref="TToolExecutionEntry.Name"/> property, specifying the name of the tool being executed.</para>
  /// <para>Defines the <see cref="TToolExecutionEntry.Info"/> property, containing detailed information or metadata about the execution.</para>
  /// </remarks>
  TToolExecutionEntry = class(TOutputCommon)
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FInfo: string;
  public
    /// <summary>
    /// Gets or sets the name of the tool involved in this execution entry.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets detailed metadata or execution information for the tool.
    /// </summary>
    property Info: string read FInfo write FInfo;
  end;

  /// <summary>
  /// Represents a function call entry within a conversation log, linking a tool invocation to its supplied arguments.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TToolExecutionEntry"/>.</para>
  /// <para>Defines the <see cref="TFunctionCallEntry.ToolCallId"/> property, which holds the identifier of the invoked tool call.</para>
  /// <para>Defines the <see cref="TFunctionCallEntry.Arguments"/> property, which contains the serialized arguments passed to the function.</para>
  /// </remarks>
  TFunctionCallEntry = class(TToolExecutionEntry)
  private
    [JsonNameAttribute('tool_call_id')]
    FToolCallId: string;
    FArguments: string;
  public
    /// <summary>
    /// Gets or sets the unique identifier of the tool call associated with this function entry.
    /// </summary>
    property ToolCallId: string read FToolCallId write FToolCallId;

    /// <summary>
    /// Gets or sets the arguments provided to the function, typically in serialized form.
    /// </summary>
    property Arguments: string read FArguments write FArguments;
  end;

  /// <summary>
  /// Represents an agent handoff within a conversation, capturing the transition between agents.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TFunctionCallEntry"/>.</para>
  /// <para>Defines the <see cref="TAgentHandoffEntry.PreviousAgentId"/> property, specifying the identifier of the agent handing off.</para>
  /// <para>Defines the <see cref="TAgentHandoffEntry.PreviousAgentName"/> property, specifying the name of the agent handing off.</para>
  /// <para>Defines the <see cref="TAgentHandoffEntry.NextAgentId"/> property, specifying the identifier of the agent taking over.</para>
  /// <para>Defines the <see cref="TAgentHandoffEntry.NextAgentName"/> property, specifying the name of the agent taking over.</para>
  /// </remarks>
  TAgentHandoffEntry = class(TFunctionCallEntry)
  private
    [JsonNameAttribute('previous_agent_id')]
    FPreviousAgentId: string;
    [JsonNameAttribute('previous_agent_name')]
    FPreviousAgentName: string;
    [JsonNameAttribute('next_agent_id')]
    FNextAgentId: string;
    [JsonNameAttribute('next_agent_name')]
    FNextAagentName: string;
  public
    /// <summary>
    /// Gets or sets the identifier of the agent handing off responsibility.
    /// </summary>
    property PreviousAgentId: string read FPreviousAgentId write FPreviousAgentId;

    /// <summary>
    /// Gets or sets the name of the agent handing off responsibility.
    /// </summary>
    property PreviousAgentName: string read FPreviousAgentName write FPreviousAgentName;

    /// <summary>
    /// Gets or sets the identifier of the agent taking over responsibility.
    /// </summary>
    property NextAgentId: string read FNextAgentId write FNextAgentId;

    /// <summary>
    /// Gets or sets the name of the agent taking over responsibility.
    /// </summary>
    property NextAagentName: string read FNextAagentName write FNextAagentName;
  end;

  /// <summary>
  /// Represents a message output entry in a conversation, including agent metadata, the model used, the sender’s role, and the content chunks.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TAgentHandoffEntry"/>.</para>
  /// <para>Derives from <see cref="TFunctionCallEntry"/>.</para>
  /// <para>Derives from <see cref="TToolExecutionEntry"/>.</para>
  /// <para>Derives from <see cref="TOutputCommon"/>.</para>
  /// <para>Derives from <see cref="TJSONFingerprint"/>.</para>
  /// </remarks>
  TMessageOutputEntry = class(TAgentHandoffEntry)
  private
    [JsonNameAttribute('agent_id')]
    FAgentId: string;
    FModel: string;
    [JsonReflectAttribute(ctString, rtString, TMessageRoleInterceptor)]
    FRole: TMessageRole;
    FContent: TArray<TContentChunk>;
  public
    /// <summary>
    /// Gets or sets the identifier of the agent that produced this message entry.
    /// </summary>
    property AgentId: string read FAgentId write FAgentId;

    /// <summary>
    /// Gets or sets the name of the model used to generate this entry.
    /// </summary>
    property Model: string read FModel write FModel;

    /// <summary>
    /// Gets or sets the role of the message author.
    /// </summary>
    property Role: TMessageRole read FRole write FRole;

    /// <summary>
    /// Gets or sets the content chunks associated with this message.
    /// </summary>
    /// <value>An array of <see cref="TContentChunk"/> instances containing the message’s content.</value>
    property Content: TArray<TContentChunk> read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  /// <summary>
  /// Represents a single chunk of conversation output, including all associated metadata and content.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TMessageOutputEntry"/>.</para>
  /// <para>Derives from <see cref="TAgentHandoffEntry"/>.</para>
  /// <para>Derives from <see cref="TFunctionCallEntry"/>.</para>
  /// <para>Derives from <see cref="TToolExecutionEntry"/>.</para>
  /// <para>Derives from <see cref="TOutputCommon"/>.</para>
  /// <para>Derives from <see cref="TJSONFingerprint"/>.</para>
  /// <para>Use <c>TConversationChunk</c> for handling individual output entries within a conversation,
  /// capturing agent transitions, tool execution details, and content chunks.</para>
  /// </remarks>
  TConversationChunk = class(TMessageOutputEntry);

  TConversationUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPromptTokens: Integer;
    [JsonNameAttribute('completion_tokens')]
    FCompletionTokens: Integer;
    [JsonNameAttribute('total_tokens')]
    FTotalTokens: Integer;
    [JsonNameAttribute('connector_tokens')]
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FConnectorTokens: string;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FConnectors: string;
  public
    /// <summary>
    /// Gets or sets the number of tokens consumed by the prompt.
    /// </summary>
    /// <value>An <see cref="Integer"/> representing the prompt token count.</value>
    property PromptTokens: Integer read FPromptTokens write FPromptTokens;

    /// <summary>
    /// Gets or sets the number of tokens consumed by the completion.
    /// </summary>
    /// <value>An <see cref="Integer"/> representing the completion token count.</value>
    property CompletionTokens: Integer read FCompletionTokens write FCompletionTokens;

    /// <summary>
    /// Gets or sets the total number of tokens used (prompt + completion + connectors).
    /// </summary>
    /// <value>An <see cref="Integer"/> representing the total token count.</value>
    property TotalTokens: Integer read FTotalTokens write FTotalTokens;

    /// <summary>
    /// Gets or sets the number of tokens consumed by connector operations.
    /// </summary>
    /// <value>A <see cref="string"/> representing the connector token count.</value>
    property ConnectorTokens: string read FConnectorTokens write FConnectorTokens;

    /// <summary>
    /// Gets or sets the list of connectors involved in the conversation.
    /// </summary>
    /// <value>A <see cref="string"/> containing serialized connector identifiers or details.</value>
    property Connectors: string read FConnectors write FConnectors;
  end;

  TConversation = class(TJSONFingerprint)
  private
    FObject: string;
    [JsonNameAttribute('conversation_id')]
    FConversationId: string;
    FOutputs: TArray<TConversationChunk>;
    FUsage: TConversationUsage;
  public
    /// <summary>
    /// Gets or sets the API object type, typically "conversation".
    /// </summary>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the unique identifier for this conversation.
    /// </summary>
    property ConversationId: string read FConversationId write FConversationId;

    /// <summary>
    /// Gets or sets the collection of output chunks generated during the conversation.
    /// </summary>
    property Outputs: TArray<TConversationChunk> read FOutputs write FOutputs;

    /// <summary>
    /// Gets or sets the usage statistics for the conversation, detailing token counts.
    /// </summary>
    property Usage: TConversationUsage read FUsage write FUsage;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents content retrieved from a conversation, including document URL and associated metadata.
  /// </summary>
  /// <remarks>
  /// <para>Inherits from <see cref="TDocumentURLChunk"/>.</para>
  /// <para>Derives from <see cref="TToolFileChunk"/>.</para>
  /// <para>Derives from <see cref="TImageURLChunk"/>.</para>
  /// <para>Derives from <see cref="TTextChunk"/>.</para>
  /// <para>Derives from <see cref="TMessageOutputContentChunksCommon"/>.</para>
  /// <para>Use <c>TRetrievedContent</c> when you need to represent a retrieved document link with full context of file, image, and text metadata.</para>
  /// </remarks>
  TRetrievedContent = class(TDocumentURLChunk);

  {$REGION 'TEntry'}

  /// <summary>
  /// Represents an input entry in a conversation, extending common output metadata
  /// to include the message role, prefix indicator, and associated retrieved content.
  /// </summary>
  /// <remarks>
  /// Use this class to model the structure and metadata of an incoming message
  /// within the conversation system.
  /// </remarks>
  TMessageInputEntryEx = class(TOutputCommon)
  private
    [JsonReflectAttribute(ctString, rtString, TMessageRoleInterceptor)]
    FRole: TMessageRole;
    FPrefix: Boolean;
    FContent: TArray<TRetrievedContent>;
  public
    /// <summary>
    /// Gets or sets the role of the message sender.
    /// </summary>
    /// <value>
    /// A <see cref="TMessageRole"/> value indicating who authored the message.
    /// </value>
    property Role: TMessageRole read FRole write FRole;

    /// <summary>
    /// Gets or sets a value indicating whether the message is a prefix.
    /// </summary>
    /// <value>
    /// True if the message should be treated as a prefix; otherwise, False.
    /// </value>
    property Prefix: Boolean read FPrefix write FPrefix;

    /// <summary>
    /// Gets or sets the collection of retrieved content items.
    /// </summary>
    /// <value>
    /// An array of <see cref="TRetrievedContent"/> instances.
    /// </value>
    property Content: TArray<TRetrievedContent> read FContent write FContent;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents an output entry in a conversation, extending the input entry
  /// metadata with agent and model information.
  /// </summary>
  /// <remarks>
  /// Use this class to model the structure and metadata of an outgoing message
  /// in the conversation system, capturing which agent produced the output
  /// and which AI model was used to generate it.
  /// </remarks>
  TMessageOutputEntryEx = class(TMessageInputEntryEx)
  private
    [JsonNameAttribute('agent_id')]
    FAgentId: string;
    FModel: string;
  public
    /// <summary>
    /// Gets or sets the identifier of the agent that produced this output.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the agent’s ID.
    /// </value>
    property AgentId: string read FAgentId write FAgentId;

    /// <summary>
    /// Gets or sets the model name used to generate the output.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the model identifier.
    /// </value>
    property Model: string read FModel write FModel;
  end;

  /// <summary>
  /// Represents the result of a function call within a conversation,
  /// extending the output entry with function-specific identifiers and output.
  /// </summary>
  /// <remarks>
  /// Use this class to capture the output from a function invocation,
  /// including the identifier of the tool call and the raw result payload.
  /// </remarks>
  TFunctionResultEntryEx = class(TMessageOutputEntryEx)
  private
    [JsonNameAttribute('tool_call_id')]
    FToolCallId: string;
    FResult: string;
  public
    /// <summary>
    /// Gets or sets the tool call identifier associated with this result.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> holding the unique ID of the function invocation.
    /// </value>
    property ToolCallId: string read FToolCallId write FToolCallId;

    /// <summary>
    /// Gets or sets the result payload returned by the function.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the function’s output, typically serialized JSON or plain text.
    /// </value>
    property Result: string read FResult write FResult;
  end;

  /// <summary>
  /// Represents a function call entry in the conversation, extending the function result entry
  /// with the original call arguments.
  /// </summary>
  /// <remarks>
  /// Use this class to model the details of a function invocation, including the tool call identifier,
  /// the result payload, and the serialized arguments that were passed to the function.
  /// </remarks>
  TFunctionCallEntryEx = class(TFunctionResultEntryEx)
  private
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FArguments: string;
  public
    /// <summary>
    /// Gets or sets the serialized arguments for the function invocation.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the JSON-encoded arguments of the call.
    /// </value>
    property Arguments: string read FArguments write FArguments;
  end;

  /// <summary>
  /// Represents the execution of a tool within a conversation, extending the function call entry
  /// with tool-specific metadata such as the tool’s name, informational summary, and the function invoked.
  /// </summary>
  /// <remarks>
  /// Use this class to capture details about a tool execution event,
  /// including its unique name, descriptive info, and the specific function identifier that was called.
  /// </remarks>
  TToolExecutionEntryEx = class(TFunctionCallEntryEx)
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FInfo: string;
    FFunction: string;
  public
    /// <summary>
    /// Gets or sets the name of the executed tool.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the tool’s name.
    /// </value>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets the informational metadata about the tool execution.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> with summary or details about the execution context.
    /// </value>
    property Info: string read FInfo write FInfo;

    /// <summary>
    /// Gets or sets the function identifier that was invoked on the tool.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the function name or handle.
    /// </value>
    property &Function: string read FFunction write FFunction;
  end;

  /// <summary>
  /// Represents an agent handoff entry in a conversation, extending the tool execution entry
  /// with identifiers for the previous and next agents involved.
  /// </summary>
  /// <remarks>
  /// Use this class to model transitions between agents during a conversation workflow,
  /// capturing both the outgoing agent and the incoming agent metadata.
  /// </remarks>
  TAgentHandoffEntryEx = class(TToolExecutionEntryEx)
  private
    [JsonNameAttribute('previous_agent_id')]
    FPreviousAgentId: string;
    [JsonNameAttribute('previous_agent_name')]
    FPreviousAgentName: string;
    [JsonNameAttribute('next_agent_id')]
    FNextAgentId: string;
    [JsonNameAttribute('next_agent_name')]
    FNextAgentName: string;
  public
    /// <summary>
    /// Gets or sets the ID of the agent that is handing off.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the previous agent’s unique identifier.
    /// </value>
    property PreviousAgentId: string read FPreviousAgentId write FPreviousAgentId;

    /// <summary>
    /// Gets or sets the name of the agent that is handing off.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the previous agent’s display name.
    /// </value>
    property PreviousAgentName: string read FPreviousAgentName write FPreviousAgentName;

    /// <summary>
    /// Gets or sets the ID of the agent that is taking over.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the next agent’s unique identifier.
    /// </value>
    property NextAgentId: string read FNextAgentId write FNextAgentId;

    /// <summary>
    /// Gets or sets the name of the agent that is taking over.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the next agent’s display name.
    /// </value>
    property NextAgentName: string read FNextAgentName write FNextAgentName;
  end;

  {$ENDREGION}

  /// <summary>
  /// Represents a single entry.
  /// combining all conversation metadata, function/tool call information, and agent handoff details.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use this class for each chunk returned by the streaming API when processing a chat completion in real time.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TOutputCommon"/> for base metadata such as object type, creation and completion timestamps, and unique ID.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TMessageInputEntryEx"/> to include the message role, prefix flag, and any retrieved content items.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TMessageOutputEntryEx"/> to capture which agent produced the message and which model was used.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TFunctionResultEntryEx"/> to include the identifier of a function call and its raw result payload.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TFunctionCallEntryEx"/> to record the JSON‑encoded arguments passed to the function invocation.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TToolExecutionEntryEx"/> to capture the executed tool’s name, informational metadata, and invoked function identifier.
  /// </para>
  /// <para>
  /// Inherits from <see cref="TAgentHandoffEntryEx"/> to track agent transitions, including the IDs and names of the previous and next agents.
  /// </para>
  /// </remarks>
  TEntry = class(TAgentHandoffEntryEx);

  /// <summary>
  /// Represents a set of conversation entries retrieved from the API,
  /// including metadata about the conversation and its entries.
  /// </summary>
  /// <remarks>
  /// Use this class to encapsulate the payload returned when fetching
  /// all entries for a given conversation, including the conversation ID
  /// and the list of entries.
  /// </remarks>
  TRetrievedEntries = class(TJSONFingerprint)
  private
    FObject: string;
    [JsonNameAttribute('conversation_id')]
    FConversationId: string;
    FEntries: TArray<TEntry>;
  public
    /// <summary>
    /// Gets or sets the JSON object type for this payload.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> indicating the payload type.
    /// </value>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the conversation identifier.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the conversation ID.
    /// </value>
    property ConversationId: string read FConversationId write FConversationId;

    /// <summary>
    /// Gets or sets the array of entries associated with the conversation.
    /// </summary>
    /// <value>
    /// An array of <see cref="TEntry"/> instances.
    /// </value>
    property Entries: TArray<TEntry> read FEntries write FEntries;

    destructor Destroy; override;
  end;

  {$REGION 'TMessage'}

  /// <summary>
  /// Represents an input message entry in a conversation stream, extending
  /// common output metadata with sender role, prefix flag, and content chunks.
  /// </summary>
  /// <remarks>
  /// Use this class to model the structure and metadata of an incoming message
  /// when retrieving or replaying conversation streams. It captures who sent
  /// the message, whether it serves as a prefix, and the associated content chunks.
  /// </remarks>
  TMessageInputEntryEx1 = class(TOutputCommon)
  private
    [JsonReflectAttribute(ctString, rtString, TMessageRoleInterceptor)]
    FRole: TMessageRole;
    FPrefix: Boolean;
    FContent: TArray<TContentChunk>;
  public
    /// <summary>
    /// Gets or sets the sender role for this message entry.
    /// </summary>
    /// <value>
    /// A <see cref="TMessageRole"/> value indicating who authored the message.
    /// </value>
    property Role: TMessageRole read FRole write FRole;

    /// <summary>
    /// Gets or sets a value indicating whether the message is a prefix.
    /// </summary>
    /// <value>
    /// True if the message should be treated as a prefix; otherwise, False.
    /// </value>
    property Prefix: Boolean read FPrefix write FPrefix;

    /// <summary>
    /// Gets or sets the content chunks for this message entry.
    /// </summary>
    /// <value>
    /// An array of <see cref="TContentChunk"/> instances representing parts of the message.
    /// </value>
    property Content: TArray<TContentChunk> read FContent write FContent;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents an output message entry in a conversation stream, extending the input entry
  /// metadata with agent identifier and model details.
  /// </summary>
  /// <remarks>
  /// Use this class to capture information about an outgoing message when retrieving
  /// or replaying conversation streams, including which agent generated the message
  /// and which AI model produced it.
  /// </remarks>
  TMessageOutputEntryEx1 = class(TMessageInputEntryEx1)
  private
    [JsonNameAttribute('agent_id')]
    FAgentId: string;
    [JsonNameAttribute('model')]
    FModel: string;
  public
    /// <summary>
    /// Gets or sets the agent identifier for this message entry.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> containing the agent’s unique ID.
    /// </value>
    property AgentId: string read FAgentId write FAgentId;

    /// <summary>
    /// Gets or sets the model name used to generate the message entry.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the model identifier.
    /// </value>
    property Model: string read FModel write FModel;
  end;

  {$ENDREGION}

  /// <summary>
  /// Represents a complete chat message entry, combining metadata, sender info, agent details,
  /// and the actual content chunks.
  /// Inherits from <see cref="TMessageOutputEntryEx1"/>, which in turn inherits from
  /// <see cref="TMessageInputEntryEx1"/> and <see cref="TOutputCommon"/>, bringing together:
  /// <para>
  /// - TOutputCommon: common fields like Object, Id, CreatedAt, CompletedAt
  /// </para>
  /// <para>
  /// - TMessageInputEntryEx1: Role, Prefix, Content chunks
  /// </para>
  /// <para>
  /// - TMessageOutputEntryEx1: AgentId, Model
  /// </para>
  /// </summary>
  /// <remarks>
  /// Use this class to deserialize or construct a full message when retrieving or streaming
  /// conversation data. It contains both the raw content pieces and all contextual metadata.
  /// <para>
  /// <see cref="TMessageRole"/> defines the role of the sender (e.g., user, assistant, system).
  /// </para>
  /// <para>
  /// <see cref="TContentChunkType"/> specifies the type of each content chunk (text, image_url, etc.).
  /// </para>
  /// </remarks>
  TMessage = class(TMessageOutputEntryEx1);

  /// <summary>
  /// Represents the set of messages retrieved from a conversation,
  /// including metadata and the message list.
  /// </summary>
  /// <remarks>
  /// Use this class to encapsulate the payload returned when fetching
  /// all messages for a given conversation, providing the JSON object type,
  /// the conversation identifier, and the array of message entries.
  /// </remarks>
  TRetrieveMessages = class(TJSONFingerprint)
  private
    FObject: string;
    [JsonNameAttribute('conversation_id')]
    FConversationId: string;
    FMessages: TArray<TMessage>;
  public
    /// <summary>
    /// Gets or sets the JSON object type for this payload.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> indicating the type of the JSON object.
    /// </value>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the conversation identifier.
    /// </summary>
    /// <value>
    /// A <see cref="string"/> representing the unique conversation ID.
    /// </value>
    property ConversationId: string read FConversationId write FConversationId;

    /// <summary>
    /// Gets or sets the array of messages in the conversation.
    /// </summary>
    /// <value>
    /// An array of <see cref="TMessage"/> instances representing each message entry.
    /// </value>
    property Messages: TArray<TMessage> read FMessages write FMessages;

    destructor Destroy; override;
  end;

implementation

{ TImageURLChunk }

destructor TImageURLChunk.Destroy;
begin
  if Assigned(FImageUrl) then
    FImageUrl.Free;
  inherited;
end;

{ TConversation }

destructor TConversation.Destroy;
begin
  for var Item in Foutputs do
    Item.Free;
  if Assigned(FUsage) then
    FUsage.Free;
  inherited;
end;

{ TMessageOutputEntry }

destructor TMessageOutputEntry.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TMessageInputEntryEx }

destructor TMessageInputEntryEx.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TRetrievedEntries }

destructor TRetrievedEntries.Destroy;
begin
  for var Item  in FEntries do
    Item.Free;
  inherited;
end;

{ TMessageInputEntryEx1 }

destructor TMessageInputEntryEx1.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TRetrieveMessages }

destructor TRetrieveMessages.Destroy;
begin
  for var Item in FMessages do
    Item.Free;
  inherited;
end;

end.
