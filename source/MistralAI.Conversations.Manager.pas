unit MistralAI.Conversations.Manager;

interface

uses
  System.SysUtils, System.Classes, System.Json, REST.JsonReflect, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types;

type
  /// <summary>
  /// Defines URL parameters for specifying the version of an agent when querying or updating.
  /// </summary>
  TAgentVersionParams = class(TUrlParam)
    /// <summary>
    /// Sets the version number for the agent in the URL query.
    /// </summary>
    /// <param name="Value">
    /// The integer version identifier to include in the request.
    /// </param>
    /// <returns>
    /// The <see cref="TAgentVersionParams"/> instance for fluent chaining.
    /// </returns>
    function Version(const Value: Integer): TAgentVersionParams;
  end;

  /// <summary>
  /// Defines URL parameters for fetching a paginated list of conversations,
  /// allowing specification of the page number and the number of items per page.
  /// </summary>
  TConversationsListParams = class(TUrlParam)
    /// <summary>
    /// Sets the page number for the conversation list query.
    /// </summary>
    /// <param name="Value">
    /// The one-based index of the page to retrieve.
    /// </param>
    /// <returns>
    /// The <see cref="TConversationsListParams"/> instance, for fluent chaining.
    /// </returns>
    function Page(const Value: Integer): TConversationsListParams;

    /// <summary>
    /// Sets the number of conversation entries to return per page.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of items to include on each page.
    /// </param>
    /// <returns>
    /// The <see cref="TConversationsListParams"/> instance, for fluent chaining.
    /// </returns>
    function PageSize(const Value: Integer): TConversationsListParams;
  end;

  TConversationsListCommon = class(TJSONFingerprint)
  private
    FName: string;
    FDescription: string;
    FObject: string;
    FId: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
  public
    /// <summary>
    /// Gets or sets the display name of the conversation entry.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets a brief description of the conversation’s content or purpose.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Gets or sets the resource object type for this entry.
    /// </summary>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the unique identifier for this conversation entry.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Gets or sets the creation timestamp of the entry in ISO 8601 format.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Gets or sets the last updated timestamp of the entry in ISO 8601 format.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
  end;

  TAgentConversation = class(TConversationsListCommon)
  private
    [JsonNameAttribute('agent_id')]
    FAgentId: string;
  public
    /// <summary>
    /// Gets or sets the agent’s unique identifier for this conversation.
    /// </summary>
    property AgentId: string read FAgentId write FAgentId;
  end;

  TPredictionObject = class
  private
    FType: string;
    FContent: string;
  public
    /// <summary>
    /// Gets or sets the prediction type (e.g., "content", "schema", etc.).
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Gets or sets the prediction content.
    /// </summary>
    property Content: string read FContent write FContent;
  end;

  TJsonSchemaObject = class
  private
    FName: string;
    FDescription: string;
    FSchema: string;
    FStrict: Boolean;
  public
    /// <summary>
    /// Gets or sets the name of the JSON schema.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets the description of the JSON schema.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Gets or sets the JSON schema content.
    /// </summary>
    property Schema: string read FSchema write FSchema;

    /// <summary>
    /// Gets or sets whether strict schema validation is applied.
    /// </summary>
    property Strict: Boolean read FStrict write FStrict;
  end;

  TCompletionArgs = class
  private
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FStop: string;
    [JsonNameAttribute('presence_penalty')]
    FPresencePenalty: Double;
    [JsonNameAttribute('frequency_penalty')]
    FFrequencyPenalty: Double;
    FTemperature: Double;
    [JsonNameAttribute('top_p')]
    FTopP: Double;
    [JsonNameAttribute('max_tokens')]
    FMaxTokens: Integer;
    [JsonNameAttribute('random_seed')]
    FRandomSeed: Integer;
    FPrediction: TPredictionObject;
    [JsonNameAttribute('json_schema')]
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FJsonSchema: TJsonSchemaObject;
    [JsonNameAttribute('tool_choice')]
    FToolChoice: string;
  public
    /// <summary>
    /// The values that causes the stop
    /// </summary>
    property Stop: string read FStop write FStop;

    /// <summary>
    /// Presence_penalty determines how much the model penalizes the repetition of words or phrases
    /// </summary>
    property PresencePenalty: Double read FPresencePenalty write FPresencePenalty;

    /// <summary>
    /// Frequency_penalty penalizes the repetition of words based on their frequency in the generated text.
    /// </summary>
    property FrequencyPenalty: Double read FFrequencyPenalty write FFrequencyPenalty;

    /// <summary>
    /// The sampling temperature to use for the model's output
    /// </summary>
    property Temperature: Double read FTemperature write FTemperature;

    /// <summary>
    /// The nucleus sampling probability mass for the model (Top-p)
    /// </summary>
    property TopP: Double read FTopP write FTopP;

    /// <summary>
    /// The maximum number of tokens to generate in the completion
    /// </summary>
    property MaxTokens: Integer read FMaxTokens write FMaxTokens;

    /// <summary>
    /// The random seed for deterministic results during sampling
    /// </summary>
    property RandomSeed: Integer read FRandomSeed write FRandomSeed;

    /// <summary>
    /// The users to specify expected results, optimizing response times by leveraging known or predictable content
    /// </summary>
    property Prediction: TPredictionObject read FPrediction write FPrediction;

    /// <summary>
    /// The json schema returned
    /// </summary>
    property JsonSchema: TJsonSchemaObject read FJsonSchema write FJsonSchema;

    /// <summary>
    /// How the model interacts with functions
    /// </summary>
    property ToolChoice: string read FToolChoice write FToolChoice;

    destructor Destroy; override;
  end;

  TToolFunctionObject = class
  private
    FName: string;
    FDescription: string;
    FStrict: Boolean;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FParameters: string;
  public
    /// <summary>
    /// Gets or sets the function’s name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets the function’s description.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Gets or sets a value indicating whether the function requires strict parameter validation.
    /// </summary>
    property Strict: Boolean read FStrict write FStrict;

    /// <summary>
    /// Gets or sets the JSON schema that defines the function’s input parameters.
    /// </summary>
    property Parameters: string read FParameters write FParameters;
  end;

  TToolObject = class
  private
    FType: string;
    [JsonNameAttribute('open_results')]
    FOpenResults: Boolean;
    FFunction: TToolFunctionObject;
    [JsonNameAttribute('library_ids')]
    FLibraryIds: TArray<string>;
  public
    /// <summary>
    /// Gets or sets the tool’s type.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Gets or sets a flag indicating whether the tool should open its results by default.
    /// </summary>
    property OpenResults: Boolean read FOpenResults write FOpenResults;

    /// <summary>
    /// Gets or sets the function metadata associated with this tool.
    /// </summary>
    property &Function: TToolFunctionObject read FFunction write FFunction;

    /// <summary>
    /// Gets or sets the list of library IDs to which this tool has access.
    /// </summary>
    property LibraryIds: TArray<string> read FLibraryIds write FLibraryIds;

    destructor Destroy; override;
  end;

  TModelConversation = class(TAgentConversation)
  private
    FInstructions: string;
    FTools: TArray<TToolObject>;
    [JsonNameAttribute('completion_args')]
    FCompletionArgs: TCompletionArgs;
  public
    /// <summary>
    /// Instruction prompt the model will follow during the conversation.
    /// </summary>
    property Instructions: string read FInstructions write FInstructions;

    /// <summary>
    /// List of tools which are available to the model during the conversation.
    /// </summary>
    property Tools: TArray<TToolObject> read FTools write FTools;

    /// <summary>
    /// Completion arguments that will be used to generate assistant responses. Can be overridden at each message request.
    /// </summary>
    property CompletionArgs: TCompletionArgs read FCompletionArgs write FCompletionArgs;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a single conversation entry in a list of model-driven conversations.
  /// Inherits instructions, tools, completion arguments, and metadata from <see cref="TModelConversation"/>.
  /// </summary>
  TConversationsListItem = class(TModelConversation);

  /// <summary>
  /// Represents a collection of conversation entries, typically returned by the API
  /// when listing available conversations or conversation templates.
  /// </summary>
  TConversationsList = class(TJSONFingerprint)
  private
    FData: TArray<TConversationsListItem>;
  public
    /// <summary>
    /// Array of conversation list items, each containing metadata and settings
    /// for a specific conversation.
    /// </summary>
    property Data: TArray<TConversationsListItem> read FData write FData;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a conversation agent configuration, including its instruction prompt,
  /// available tools, completion parameters, and metadata such as model, ID, version,
  /// and timestamps for creation and updates.
  /// </summary>
  TConversationsAgent = class(TJSONFingerprint)
  private
    FInstructions: string;
    FTools: TArray<TToolObject>;
    [JsonNameAttribute('completion_args')]
    FCompletionArgs: TCompletionArgs;
    FModel: string;
    FName: string;
    FDescription: string;
    FHandoffs: TArray<string>;
    FObject: string;
    FId: string;
    FVersion: Int64;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
  public
    /// <summary>
    /// Gets or sets the instruction prompt the agent will follow during the conversation.
    /// </summary>
    property Instructions: string read FInstructions write FInstructions;

    /// <summary>
    /// Gets or sets the list of tools available to this agent.
    /// </summary>
    property Tools: TArray<TToolObject> read FTools write FTools;

    /// <summary>
    /// Gets or sets the default completion arguments for this agent.
    /// </summary>
    property CompletionArgs: TCompletionArgs read FCompletionArgs write FCompletionArgs;

    /// <summary>
    /// Gets or sets the model identifier used by this agent.
    /// </summary>
    property Model: string read FModel write FModel;

    /// <summary>
    /// Gets or sets the human-readable name of the agent.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets the description of what this agent’s conversation is about.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Gets or sets the list of handoff targets, defining how and when control transfers to other agents.
    /// </summary>
    property Handoffs: TArray<string> read FHandoffs write FHandoffs;

    /// <summary>
    /// Gets the object type, always "agent".
    /// </summary>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// Gets or sets the unique identifier for this agent.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Gets or sets the version number of this agent configuration.
    /// </summary>
    property Version: Int64 read FVersion write FVersion;

    /// <summary>
    /// Gets or sets the creation timestamp of this agent configuration.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Gets or sets the last update timestamp of this agent configuration.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a collection of agent conversation definitions,
  /// typically returned by the API when querying available agent configurations.
  /// </summary>
  TConversationsAgentList = class(TJSONFingerprint)
  private
    FData: TArray<TConversationsAgent>;
  public
    /// <summary>
    /// Gets or sets the array of agent conversation entries.
    /// </summary>
    property Data: TArray<TConversationsAgent> read FData write FData;
    destructor Destroy; override;
  end;

implementation

{ TConversationsListParams }

function TConversationsListParams.Page(
  const Value: Integer): TConversationsListParams;
begin
  Result := TConversationsListParams(Add('page', Value));
end;

function TConversationsListParams.PageSize(
  const Value: Integer): TConversationsListParams;
begin
  Result := TConversationsListParams(Add('page_size', Value));
end;

{ TModelConversation }

destructor TModelConversation.Destroy;
begin
  for var Item in FTools do
    Item.Free;
  if Assigned(FCompletionArgs) then
    FCompletionArgs.Free;
  inherited;
end;

{ TCompletionArgs }

destructor TCompletionArgs.Destroy;
begin
  if Assigned(FPrediction) then
    FPrediction.Free;
  if Assigned(FJsonSchema) then
    FJsonSchema.Free;
  inherited;
end;

{ TConversationsList }

destructor TConversationsList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TToolObject }

destructor TToolObject.Destroy;
begin
  if Assigned(FFunction) then
    FFunction.Free;
  inherited;
end;

{ TConversationsAgent }

destructor TConversationsAgent.Destroy;
begin
  for var Item in FTools do
    Item.Free;
  if Assigned(FCompletionArgs) then
    FCompletionArgs.Free;
  inherited;
end;

{ TConversationsAgentList }

destructor TConversationsAgentList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TAgentVersionParams }

function TAgentVersionParams.Version(const Value: Integer): TAgentVersionParams;
begin
  Result := TAgentVersionParams(Add('version', Value)); 
end;

end.
