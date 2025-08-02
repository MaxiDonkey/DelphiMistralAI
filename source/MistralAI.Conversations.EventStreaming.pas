unit MistralAI.Conversations.EventStreaming;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types, MistralAI.Schema,
  MistralAI.Conversations.Params, MistralAI.Conversations.Chunks;

type
  /// <summary>
  /// Base class for streamed conversation chunk events.
  /// Encapsulates the event’s type identifier and the timestamp when it was created.
  /// </summary>
  TChunkEventCommon = class(TJSONFingerprint)
  private
    [JsonReflectAttribute(ctString, rtString, TChunkEventInterceptor)]
    FType: TChunkEvent;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
  public
    /// <summary>
    /// The kind of chunk event (for example, message.output.delta, tool.execution.started, etc.).
    /// </summary>
    property &Type: TChunkEvent read FType write FType;

    /// <summary>
    /// The creation timestamp of the event in ISO 8601 format.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;
  end;

  /// <summary>
  /// Represents an error event in a conversation stream.
  /// Extends TChunkEventCommon to include an error message and numeric code.
  /// </summary>
  TConversationResponseError = class(TChunkEventCommon)
  private
    FMessage: string;
    FCode: Integer;
  public
    /// <summary>
    /// A human‑readable description of the error that occurred.
    /// </summary>
    property Message: string read FMessage write FMessage;

    /// <summary>
    /// A numeric code identifying the specific error type.
    /// </summary>
    property Code: Integer read FCode write FCode;
  end;

  /// <summary>
  /// Fired when a tool begins execution during a conversation stream.
  /// Inherits from TConversationResponseError to include error context if applicable.
  /// </summary>
  TToolExecutionStarted = class(TConversationResponseError)
  private
    [JsonNameAttribute('output_index')]
    FOutputIndex: Integer;
    FId: string;
    FName: string;
  public
    /// <summary>
    /// The zero‑based index of this execution in cases where multiple outputs are produced.
    /// </summary>
    property OutputIndex: Integer read FOutputIndex write FOutputIndex;

    /// <summary>
    /// A unique identifier for this specific tool execution instance.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// The registered name of the tool being executed.
    /// </summary>
    property Name: string read FName write FName;
  end;

  /// <summary>
  /// Fired when a tool finishes execution during a conversation stream.
  /// Inherits from TToolExecutionStarted and adds execution metadata.
  /// </summary>
  TToolExecutionDone = class(TToolExecutionStarted)
  private
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FInfo: string;
  public
    /// <summary>
    /// Additional metadata returned by the tool upon completion,
    /// typically used to convey execution details or results summary.
    /// </summary>
    property Info: string read FInfo write FInfo;
  end;

  /// <summary>
  /// Represents a function call delta event in the conversation stream.
  /// Inherits from TToolExecutionDone to include completion metadata,
  /// and adds the function call identifier and its serialized arguments.
  /// </summary>
  TFunctionCallDelta = class(TToolExecutionDone)
  private
    [JsonNameAttribute('tool_call_id')]
    FToolCallId: string;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FArguments: string;
  public
    /// <summary>
    /// The unique identifier of the tool call instance this delta refers to.
    /// </summary>
    property ToolCallId: string read FToolCallId write FToolCallId;

    /// <summary>
    /// The JSON‐serialized arguments passed to the function at call time.
    /// </summary>
    property Arguments: string read FArguments write FArguments;
  end;

  /// <summary>
  /// Indicates the start of an agent handoff within the conversation stream.
  /// Inherits from TFunctionCallDelta to include function call context and metadata.
  /// </summary>
  TAgentHandoffStarted = class(TFunctionCallDelta)
  private
    [JsonNameAttribute('previous_agent_id')]
    FPreviousAgentId: string;
    [JsonNameAttribute('previous_agent_name')]
    FPreviousAgentName: string;
  public
    /// <summary>
    /// The identifier of the agent handing off control.
    /// </summary>
    property PreviousAgentId: string read FPreviousAgentId write FPreviousAgentId;

    /// <summary>
    /// The name of the agent handing off control.
    /// </summary>
    property PreviousAgentName: string read FPreviousAgentName write FPreviousAgentName;
  end;

  /// <summary>
  /// Indicates the completion of an agent handoff within the conversation stream.
  /// Inherits from TAgentHandoffStarted to include both the previous and next agent context.
  /// </summary>
  TAgentHandoffDone = class(TAgentHandoffStarted)
  private
    [JsonNameAttribute('next_agent_id')]
    FNextAgentId: string;
    [JsonNameAttribute('next_agent_name')]
    FNextAgentName: string;
  public
    /// <summary>
    /// The identifier of the agent taking over control.
    /// </summary>
    property NextAgentId: string read FNextAgentId write FNextAgentId;

    /// <summary>
    /// The name of the agent taking over control.
    /// </summary>
    property NextAgentName: string read FNextAgentName write FNextAgentName;
  end;

  /// <summary>
  /// Signals the start of a conversation response in the event stream.
  /// Inherits from TAgentHandoffDone to include agent handoff context and function call metadata.
  /// </summary>
  TConversationResponseStarted = class(TAgentHandoffDone)
  private
    [JsonNameAttribute('conversation_id')]
    FConversationId: string;
  public
    /// <summary>
    /// The unique identifier for this conversation instance.
    /// </summary>
    property ConversationId: string read FConversationId write FConversationId;
  end;

  /// <summary>
  /// Marks the completion of a conversation response in the event stream.
  /// Inherits from TConversationResponseStarted and includes usage statistics.
  /// </summary>
  TConversationResponseDone = class(TConversationResponseStarted)
  private
    FUsage: TConversationUsage;
  public
    /// <summary>
    /// Token usage details for this conversation response, including prompt, completion, and total counts.
    /// </summary>
    property Usage: TConversationUsage read FUsage write FUsage;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a chunk of streamed message content in a conversation response.
  /// Inherits from TConversationResponseDone to include completion metadata and usage statistics.
  /// </summary>
  TMessageOutputDelta = class(TConversationResponseDone)
  private
    [JsonNameAttribute('content_index')]
    FContentIndex: Integer;
    FModel: string;
    [JsonNameAttribute('agent_id')]
    FAgentId: string;
    [JsonReflectAttribute(ctString, rtString, TMessageRoleInterceptor)]
    FRole: TMessageRole;
    FContent: TArray<TContentChunk>;
  public
    /// <summary>
    /// The zero‑based index indicating the order of this content chunk in the overall response.
    /// </summary>
    property ContentIndex: Integer read FContentIndex write FContentIndex;

    /// <summary>
    /// The identifier of the model that generated this chunk of content.
    /// </summary>
    property Model: string read FModel write FModel;

    /// <summary>
    /// The identifier of the agent (e.g., assistant) producing this content chunk.
    /// </summary>
    property AgentId: string read FAgentId write FAgentId;

    /// <summary>
    /// The role associated with this chunk (such as 'user', 'assistant', or 'system').
    /// </summary>
    property Role: TMessageRole read FRole write FRole;

    /// <summary>
    /// An array of content chunks, each representing a part of the streamed message.
    /// </summary>
    property Content: TArray<TContentChunk> read FContent write FContent;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a single event emitted during streaming reception of a conversation.
  /// Inherits from <see cref="TMessageOutputDelta"/>, which in turn inherits from:
  /// <para>
  /// - <see cref="TConversationResponseDone"/>: adds usage statistics (prompt, completion, total tokens)
  /// </para>
  /// <para>
  /// - <see cref="TConversationResponseStarted"/>: adds the conversation identifier
  /// </para>
  /// <para>
  /// - <see cref="TAgentHandoffDone"/>: adds next agent context
  /// </para>
  /// <para>
  /// - <see cref="TAgentHandoffStarted"/>: adds previous agent context
  /// </para>
  /// <para>
  /// - <see cref="TFunctionCallDelta"/>: adds tool call identifier and arguments
  /// </para>
  /// <para>
  /// - <see cref="TToolExecutionDone"/>: adds tool execution metadata
  /// </para>
  /// <para>
  /// - <see cref="TToolExecutionStarted"/>: adds tool execution id, name, and output index
  /// </para>
  /// <para>
  /// - <see cref="TConversationResponseError"/>: adds error message and code
  /// </para>
  /// <para>
  /// - <see cref="TChunkEventCommon"/>: adds event type and creation timestamp
  /// </para>
  /// </summary>
  /// <remarks>
  /// Use this class to receive and process incremental chunks of a conversation response.
  /// Each event contains the role of the sender, content chunks, agent and model identifiers,
  /// error context if applicable, and token usage metrics. These events are generated in real‑time
  /// as the response is streamed from the server.
  /// <para>
  /// <see cref="TMessageRole"/> defines the role of the sender (e.g., user, assistant, system).
  /// </para>
  /// <para>
  /// <see cref="TContentChunkType"/> specifies the type of each content chunk (text, image_url, etc.).
  /// </para>
  /// <para>
  /// The inheritance chain brings together all contextual metadata needed for full deserialization
  /// or reconstruction of the streaming conversation data.
  /// </para>
  /// </remarks>
  TConversationsEvent = class(TMessageOutputDelta);

implementation

{ TConversationResponseDone }

destructor TConversationResponseDone.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  inherited;
end;

{ TMessageOutputDelta }

destructor TMessageOutputDelta.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

end.
