unit MistralAI.Conversations.Params;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Json, MistralAI.API.Params,
  MistralAI.API, MistralAI.Types, MistralAI.Schema, MistralAI.Functions.Core;

type
  TImageUrlParams = class(TJSONParam)
    /// <summary>
    /// Sets the URL for this image parameter.
    /// </summary>
    /// <param name="Value">
    /// A string containing the URL or base‑64-encoded image reference.
    /// </param>
    /// <returns>
    /// The same <see cref="TImageUrlParams"/> instance, with its <c>url</c> property updated.
    /// </returns>
    function Url(const Value: string): TImageUrlParams;

    /// <summary>
    /// Sets the detail description for this image parameter.
    /// </summary>
    /// <param name="Value">
    /// An optional string providing additional context or caption for the image.
    /// </param>
    /// <returns>
    /// The same <see cref="TImageUrlParams"/> instance, with its <c>detail</c> property updated.
    /// </returns>
    function Detail(const Value: string): TImageUrlParams;

    /// <summary>
    /// Creates a new <see cref="TImageUrlParams"/> instance.
    /// </summary>
    /// <returns>
    /// A fresh <see cref="TImageUrlParams"/> object with no properties set.
    /// </returns>
    class function New: TImageUrlParams;
  end;

  {$REGION 'TContentParams'}

  TContentParams = class(TJSONParam);

  TTextChunkParams = class(TContentParams)
    /// <summary>
    /// Sets the JSON <c>type</c> field for this text chunk.
    /// </summary>
    /// <param name="Value">
    /// The chunk type string (defaults to "text").
    /// </param>
    /// <returns>
    /// The same <see cref="TTextChunkParams"/> instance, with its <c>type</c> property updated.
    /// </returns>
    function &Type(const Value: string = 'text'): TTextChunkParams;

    /// <summary>
    /// Sets the text content for this chunk.
    /// </summary>
    /// <param name="Value">
    /// The string value to use as the text of the chunk.
    /// </param>
    /// <returns>
    /// The same <see cref="TTextChunkParams"/> instance, with its <c>text</c> property set.
    /// </returns>
    function Text(const Value: string): TTextChunkParams;

    /// <summary>
    /// Creates a new <see cref="TTextChunkParams"/> instance initialized as a text chunk.
    /// </summary>
    /// <returns>
    /// A fresh <see cref="TTextChunkParams"/> object with its <c>type</c> property set to "text".
    /// </returns>
    class function New: TTextChunkParams;
  end;

  TImageURLChunkParams = class(TContentParams)
    /// <summary>
    /// Sets the JSON <c>type</c> field for this image URL chunk.
    /// </summary>
    /// <param name="Value">
    /// The chunk type string (defaults to "image_url").
    /// </param>
    /// <returns>
    /// The same <see cref="TImageURLChunkParams"/> instance, with its <c>type</c> property updated.
    /// </returns>
    function &Type(const Value: string = 'image_url'): TImageURLChunkParams;

    /// <summary>
    /// Sets the image URL parameters for this chunk.
    /// </summary>
    /// <param name="Value">
    /// A <see cref="TImageUrlParams"/> instance specifying the URL and optional detail for the image.
    /// </param>
    /// <returns>
    /// The same <see cref="TImageURLChunkParams"/> instance, with its <c>image_url</c> property set.
    /// </returns>
    function ImageUrl(const Value: TImageUrlParams): TImageURLChunkParams;

    /// <summary>
    /// Creates a new <see cref="TImageURLChunkParams"/> instance initialized as an image URL chunk.
    /// </summary>
    /// <returns>
    /// A fresh <see cref="TImageURLChunkParams"/> object with its <c>type</c> property set to "image_url".
    /// </returns>
    class function New: TImageURLChunkParams;
  end;

  TToolFileChunkParams = class(TContentParams)
    /// <summary>
    /// Sets the JSON <c>type</c> field for this tool file chunk.
    /// </summary>
    /// <param name="Value">
    /// The chunk type string (defaults to "tool_file").
    /// </param>
    /// <returns>
    /// The same <see cref="TToolFileChunkParams"/> instance, with its <c>type</c> property updated.
    /// </returns>
    function &Type(const Value: string = 'tool_file'): TToolFileChunkParams;

    /// <summary>
    /// Specifies which tool processed this file chunk.
    /// </summary>
    /// <param name="Value">
    /// The identifier of the built‑in connector or tool name (e.g., "web_search", "code_interpreter").
    /// </param>
    /// <returns>
    /// The same <see cref="TToolFileChunkParams"/> instance, with its <c>tool</c> property set.
    /// </returns>
    function Tool(const Value: string): TToolFileChunkParams;

    /// <summary>
    /// Sets the unique file identifier for this chunk.
    /// </summary>
    /// <param name="Value">
    /// The file ID assigned by the tool for reference.
    /// </param>
    /// <returns>
    /// The same <see cref="TToolFileChunkParams"/> instance, with its <c>file_id</c> property updated.
    /// </returns>
    function FileId(const Value: string): TToolFileChunkParams;

    /// <summary>
    /// Sets the filename for this chunk.
    /// </summary>
    /// <param name="Value">
    /// The name of the file associated with this tool chunk.
    /// </param>
    /// <returns>
    /// The same <see cref="TToolFileChunkParams"/> instance, with its <c>file_name</c> property updated.
    /// </returns>
    function FileName(const Value: string): TToolFileChunkParams;

    /// <summary>
    /// Sets the file type for this chunk.
    /// </summary>
    /// <param name="Value">
    /// The MIME type or descriptor of the file (e.g., "application/pdf").
    /// </param>
    /// <returns>
    /// The same <see cref="TToolFileChunkParams"/> instance, with its <c>file_type</c> property updated.
    /// </returns>
    function FileType(const Value: string): TToolFileChunkParams;

    /// <summary>
    /// Creates a new <see cref="TToolFileChunkParams"/> instance initialized as a tool file chunk.
    /// </summary>
    /// <returns>
    /// A fresh <see cref="TToolFileChunkParams"/> object with its <c>type</c> property set to "tool_file".
    /// </returns>
    class function New: TToolFileChunkParams;
  end;

  TDocumentUrlChunkParams = class(TContentParams)
    /// <summary>
    /// Sets the JSON <c>type</c> field for this document URL chunk.
    /// </summary>
    /// <param name="Value">
    /// The chunk type string (defaults to "document_url").
    /// </param>
    /// <returns>
    /// The same <see cref="TDocumentUrlChunkParams"/> instance, with its <c>type</c> property updated.
    /// </returns>
    function &Type(const Value: string = 'document_url'): TDocumentUrlChunkParams;

    /// <summary>
    /// Specifies the document URL for this chunk.
    /// </summary>
    /// <param name="Value">
    /// A string containing the URL of the document resource.
    /// </param>
    /// <returns>
    /// The same <see cref="TDocumentUrlChunkParams"/> instance, with its <c>document_url</c> property set.
    /// </returns>
    function DocumentUrl(const Value: string): TDocumentUrlChunkParams;

    /// <summary>
    /// Specifies the document name for this chunk.
    /// </summary>
    /// <param name="Value">
    /// The filename or descriptive name of the document.
    /// </param>
    /// <returns>
    /// The same <see cref="TDocumentUrlChunkParams"/> instance, with its <c>document_name</c> property updated.
    /// </returns>
    function DocumentName(const Value: string): TDocumentUrlChunkParams;

    /// <summary>
    /// Creates a new <see cref="TDocumentUrlChunkParams"/> instance initialized as a document URL chunk.
    /// </summary>
    /// <returns>
    /// A fresh <see cref="TDocumentUrlChunkParams"/> object with its <c>type</c> property set to "document_url".
    /// </returns>
    class function New: TDocumentUrlChunkParams;
  end;

  {$ENDREGION}

  {$REGION 'TEntryParams'}

  TEntryParams = class(TJSONParam)
    /// <summary>
    /// Sets the JSON <object> field for this entry.
    /// </summary>
    /// <param name="Value">
    /// The object type identifier (for example, "entry").
    /// </param>
    /// <returns>
    /// The same <see cref="TEntryParams"/> instance, with its <c>object</c> property updated.
    /// </returns>
    function &Object(const Value: string): TEntryParams;

    /// <summary>
    /// Sets the JSON <type> field for this entry.
    /// </summary>
    /// <param name="Value">
    /// The entry type (for example, "message.input" or "function.result").
    /// </param>
    /// <returns>
    /// The same <see cref="TEntryParams"/> instance, with its <c>type</c> property updated.
    /// </returns>
    function &Type(const Value: string = 'message.input'): TEntryParams;

    /// <summary>
    /// Sets the creation timestamp for this entry.
    /// </summary>
    /// <param name="Value">
    /// An ISO 8601 date‑time string indicating when the entry was created.
    /// </param>
    /// <returns>
    /// The same <see cref="TEntryParams"/> instance, with its <c>created_at</c> property set.
    /// </returns>
    function CreatedAt(const Value: string): TEntryParams;

    /// <summary>
    /// Sets the completion timestamp for this entry.
    /// </summary>
    /// <param name="Value">
    /// An ISO 8601 date‑time string indicating when the entry was completed (or null if not completed).
    /// </param>
    /// <returns>
    /// The same <see cref="TEntryParams"/> instance, with its <c>completed_at</c> property set.
    /// </returns>
    function CompletedAt(const Value: string): TEntryParams;

    /// <summary>
    /// Sets the unique identifier for this entry.
    /// </summary>
    /// <param name="Value">
    /// A string representing the entry’s unique ID.
    /// </param>
    /// <returns>
    /// The same <see cref="TEntryParams"/> instance, with its <c>id</c> property set.
    /// </returns>
    function Id(const Value: string): TEntryParams;

    class function New: TEntryParams;
  end;

  TMessageInputEntryParams = class(TEntryParams)
    /// <summary>
    /// Sets the role for this message input entry.
    /// </summary>
    /// <param name="Value">
    /// A string indicating the sender role (for example, "user" or "assistant").
    /// </param>
    /// <returns>
    /// The same <see cref="TMessageInputEntryParams"/> instance, with its <c>role</c> property updated.
    /// </returns>
    function Role(const Value: string): TMessageInputEntryParams;

    /// <summary>
    /// Specifies the content of this message input entry as a single text string.
    /// </summary>
    /// <param name="Value">
    /// The full text content to include in the entry.
    /// </param>
    /// <returns>
    /// The same <see cref="TMessageInputEntryParams"/> instance, with its <c>content</c> property set to the given text.
    /// </returns>
    function Content(const Value: string): TMessageInputEntryParams; overload;

    /// <summary>
    /// Specifies the content of this message input entry using a sequence of content chunks.
    /// </summary>
    /// <param name="Value">
    /// An array of <see cref="TContentParams"/> instances representing one or more content chunks
    /// (e.g. text, image URL, document reference) to include in the entry.
    /// </param>
    /// <returns>
    /// The same <see cref="TMessageInputEntryParams"/> instance, with its <c>content</c> property populated
    /// by the provided chunk parameters.
    /// </returns>
    function Content(const Value: TArray<TContentParams>): TMessageInputEntryParams; overload;
  end;

  TFunctionResultEntryParams = class(TEntryParams)
    /// <summary>
    /// Sets the identifier of the tool call that produced this function result entry.
    /// </summary>
    /// <param name="Value">
    /// The unique ID of the tool call associated with this result entry.
    /// </param>
    /// <returns>
    /// The same <see cref="TFunctionResultEntryParams"/> instance, with its <c>tool_call_id</c> property updated.
    /// </returns>
    function ToolCallId(const Value: string): TFunctionResultEntryParams;

    /// <summary>
    /// Specifies the output produced by the tool call for this function result entry.
    /// </summary>
    /// <param name="Value">
    /// A string containing the result or return value from the tool execution.
    /// </param>
    /// <returns>
    /// The same <see cref="TFunctionResultEntryParams"/> instance, with its <c>result</c> property set to the provided value.
    /// </returns>
    function Result(const Value: string): TFunctionResultEntryParams;
  end;

  {$ENDREGION}

  TCompletionArgsParams = class(TJSONParam)
    /// <summary>
    /// Stop (string)
    /// Stop generation if this token is detected. Or if one of these tokens is detected when providing an array
    /// </summary>
    /// <param name="Value">
    /// The string that causes the stop
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function Stop(const Value: string): TCompletionArgsParams; overload;

    /// <summary>
    /// Stop Array of Stop (strings) (Stop)
    /// Stop generation if this token is detected. Or if one of these tokens is detected when providing an array
    /// </summary>
    /// <param name="Value">
    /// The array of string that causes the stop
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function Stop(const Value: TArray<string>): TCompletionArgsParams; overload;

    /// <summary>
    /// Presence_penalty determines how much the model penalizes the repetition of words or phrases
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher presence penalty encourages the model to use a wider variety of words and phrases,
    /// making the output more diverse and creative.
    /// </remarks>
    function PresencePenalty(const Value: Double): TCompletionArgsParams;

    /// <summary>
    /// Frequency_penalty penalizes the repetition of words based on their frequency in the generated text.
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher frequency penalty discourages the model from repeating words that have already appeared
    /// frequently in the output, promoting diversity and reducing repetition.
    /// </remarks>
    function FrequencyPenalty(const Value: Double): TCompletionArgsParams;

    /// <summary>
    /// Sets the sampling temperature to use for the model's output.
    /// Higher values like 0.8 make the output more random, while lower values like 0.2 make it more focused and deterministic.
    /// </summary>
    /// <param name="Value">
    /// The temperature value between 0.0 and 1.0. Default is 0.7.
    /// A temperature of 0 makes the model deterministic, while a temperature of 1 allows for maximum creativity.
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function Temperature(const Value: Double): TCompletionArgsParams;

    /// <summary>
    /// Sets the nucleus sampling probability mass for the model (Top-p).
    /// For example, 0.1 means only the tokens comprising the top 10% probability mass are considered.
    /// </summary>
    /// <param name="Value">
    /// The <c>top_p</c> value between 0.0 and 1.0. Default is 1.
    /// Lower values limit the model to consider only the most probable options.
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function TopP(const Value: Double): TCompletionArgsParams;

    /// <summary>
    /// Sets the maximum number of tokens to generate in the completion.
    /// The total token count of your prompt plus <c>max_tokens</c> cannot exceed the model's context length.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of tokens to generate.
    /// Choose an appropriate value based on your prompt length to avoid exceeding the model's limit.
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function MaxTokens(const Value: Integer): TCompletionArgsParams;

    /// <summary>
    /// Sets the random seed for deterministic results during sampling.
    /// </summary>
    /// <param name="Value">An integer value to be used as the seed.</param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Providing a random seed ensures that multiple calls with the same parameters produce the same results, useful for testing or reproducible outcomes.
    /// </remarks>
    function RandomSeed(const Value: Integer): TCompletionArgsParams;

    /// <summary>
    /// Enable users to specify expected results, optimizing response times by leveraging known or
    /// predictable content.
    /// </summary>
    /// <param name="Value">The string prediction content.</param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This approach is especially effective for updating text documents or code files with minimal
    /// changes, reducing latency while maintaining high-quality results.
    /// </remarks>
    function Prediction(const Value: string): TCompletionArgsParams;

    /// <summary>
    /// Specifies the format in which the model should return the response. This can include formats like JSON or plain text.
    /// </summary>
    /// <param name="Value">The <c>TResponseFormatParams</c> value.</param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If not specified, the default value is <c>{ "type": "text" }</c>. When using JSON mode, it's necessary to instruct the model to produce JSON explicitly through the system or user messages.
    /// </remarks>
    function ResponseFormat(const Value: TResponseFormatParams): TCompletionArgsParams; overload;

    /// <summary>
    /// Specifies the format in which the model should return the response. This can include formats like JSON or plain text.
    /// </summary>
    /// <param name="Value">The string value.</param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function ResponseFormat(const Value: string): TCompletionArgsParams; overload;

    /// <summary>
    /// Configures how the model interacts with functions. This can either prevent, allow, or require function calls depending on the setting.
    /// </summary>
    /// <param name="Value">
    /// The <c>TToolChoice</c> setting for function interaction, with a default of "auto".
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If set to <c>none</c>, the model will not call any functions and will generate a message instead. If set to <c>auto</c>, the model can choose between generating a message or calling a function. If set to <c>any</c>, the model is required to call a function.
    /// </remarks>
    function ToolChoice(const Value: TToolChoice): TCompletionArgsParams; overload;

    /// <summary>
    /// Configures how the model interacts when required is on.
    /// </summary>
    /// <param name="Value">
    /// The <c>TToolChoice</c> setting for function interaction, with a default of "auto".
    /// </param>
    /// <returns>
    /// The updated <c>TCompletionArgsParams</c> instance.
    /// </returns>
    function ToolChoice(const Value: string): TCompletionArgsParams; overload;
  end;

  TToolFunctionParams = class(TJSONParam)
    /// <summary>
    /// The name of the response format. Must be a-z, A-Z, 0-9, or contain underscores and dashes, with a maximum length of 64.
    /// </summary>
    function Name(const Value: string): TToolFunctionParams;

    /// <summary>
    /// A description of what the response format is for, used by the model to determine how to respond in the format.
    /// </summary>
    function Description(const Value: string): TToolFunctionParams;

    /// <summary>
    /// Whether to enable strict schema adherence when generating the output. If set to true, the model will always
    /// follow the exact schema defined in the schema field. Only a subset of JSON Schema is supported when strict
    /// is true.
    /// </summary>
    function Strict(const Value: Boolean): TToolFunctionParams;

    /// <summary>
    /// The parameters for the response format, described as a JSON Schema object. Learn how to build JSON schemas here.
    /// </summary>
    function Parameters(const Value: TJSONObject): TToolFunctionParams; overload;

    /// <summary>
    /// The parameters for the response format, described as a JSON Schema object. Learn how to build JSON schemas here.
    /// </summary>
    function Parameters(const Value: TSchemaParams): TToolFunctionParams; overload;
  end;

  TConnectorParams = class(TJSONParam)
    /// <summary>
    /// An object specifying the format that the model must output. Setting to { "type": "json_object" }
    /// enables JSON mode, which guarantees the message the model generates is in JSON. When using JSON
    /// mode you MUST also instruct the model to produce JSON yourself with a system or a user message.
    /// </summary>
    /// <param name="Value">
    /// Enum: "text" "json_object" "json_schema"
    /// </param>
    function &Type(const Value: TToolType): TConnectorParams; overload;

    /// <summary>
    /// An object specifying the format that the model must output. Setting to { "type": "json_object" }
    /// enables JSON mode, which guarantees the message the model generates is in JSON. When using JSON
    /// mode you MUST also instruct the model to produce JSON yourself with a system or a user message.
    /// </summary>
    /// <param name="Value">
    /// Enum: "text" "json_object" "json_schema"
    /// </param>
    function &Type(const Value: string): TConnectorParams; overload;

    /// <summary>
    /// Represents the object function called
    /// </summary>
    function &Function(const Value: TToolFunctionParams): TConnectorParams; overload;

    /// <summary>
    /// Represents the object function called
    /// </summary>
    function &Function(const Value: TJSONValue): TConnectorParams; overload;

    /// <summary>
    /// Ids of the library in which to search.
    /// </summary>
    function LibraryIds(const Value: TArray<string>): TConnectorParams; overload;

    class function New(const Value: TToolType): TConnectorParams; overload;
    class function New(const Value: string): TConnectorParams; overload;
    class function New(const Value: IFunctionCore): TConnectorParams; overload;
    class function New(const Value: TToolFunctionParams): TConnectorParams; overload;
    class function New(const Value: TArray<string>): TConnectorParams; overload;
  end;

  TConnector = record
  public
    class function web_search_premium: TConnectorParams; static;
    class function web_search: TConnectorParams; static;
    class function image_generation: TConnectorParams; static;
    class function code_interpreter: TConnectorParams; static;
    class function &function(const Value: IFunctionCore): TConnectorParams; overload; static;
    class function &function(const Value: TToolFunctionParams): TConnectorParams; overload; static;
    class function document_library(const Value: TArray<string>): TConnectorParams; static;
  end;

  TConversationsCommonParams = class(TJSONParam)
    /// <summary>
    /// Instruction prompt the model will follow during the conversation.
    /// </summary>
    function Instructions(const Value: string): TConversationsCommonParams; overload;

    /// <summary>
    /// Guidelines that the model will adhere to throughout the conversation when using a reasoning module.
    /// </summary>
    function Instructions(const Value: TReasoningInstruction = Default; Arg: string = ''): TConversationsCommonParams; overload;

    /// <summary>
    /// List of tools which are available to the model during the conversation.
    /// </summary>
    function Tools(const Value: TArray<TConnectorParams>): TConversationsCommonParams;

    /// <summary>
    /// Completion arguments that will be used to generate assistant responses.
    /// Can be overridden at each message request.
    /// </summary>
    function CompletionArgs(const Value: TCompletionArgsParams): TConversationsCommonParams;

    /// <summary>
    /// Model used with the agent
    /// </summary>
    function Model(const Value: string): TConversationsCommonParams;

    /// <summary>
    /// Agent name
    /// </summary>
    function Name(const Value: string): TConversationsCommonParams;

    /// <summary>
    /// Agent description
    /// </summary>
    function Description(const Value: string): TConversationsCommonParams;
  end;

  /// <summary>
  /// Defines the set of parameters used to configure and send a conversation request.
  /// </summary>
  /// <remarks>
  /// Use <see cref="Inputs"/> to specify the user input, either as plain text or as a sequence
  /// of <see cref="TEntryParams"/>. Control streaming behavior via <see cref="Stream"/>, and
  /// persist the response with <see cref="Store"/>. You can also configure automatic handoff
  /// execution, restart from a previous entry, or route through a specific agent.
  /// </remarks>
  TConversationsParams = class(TConversationsCommonParams)
    /// <summary>
    /// Specifies the conversation inputs using a plain text string.
    /// </summary>
    /// <param name="Value">
    /// A string containing the input to append to the conversation (for example, a user message or command).
    /// </param>
    /// <returns>
    /// The same <see cref="TConversationsParams"/> instance, with its <c>inputs</c> property set to the provided value.
    /// </returns>
    function Inputs(const Value: string): TConversationsParams; overload;

    /// <summary>
    /// Specifies the conversation inputs using a collection of entry parameters.
    /// </summary>
    /// <param name="Value">
    /// An array of <see cref="TEntryParams"/> instances representing individual input entries
    /// (such as user messages or function results) to append to the conversation.
    /// </param>
    /// <returns>
    /// The same <see cref="TConversationsParams"/> instance, with its <c>inputs</c> property
    /// populated by the provided entry parameters.
    /// </returns>
    function Inputs(const Value: TArray<TEntryParams>): TConversationsParams; overload;

    /// <summary>
    /// Specifies whether to stream back partial progress as server-sent events (SSE).
    /// If <c>true</c>, tokens are sent as they become available.
    /// If <c>false</c>, the server will hold the request open until timeout or completion.
    /// </summary>
    /// <param name="Value">
    /// A boolean value indicating whether to enable streaming. Default is <c>true</c>, meaning streaming is enabled by default.
    /// </param>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function Stream(const Value: Boolean = True): TConversationsParams;

    /// <summary>
    /// Whether to store the generated model response for later retrieval via API.
    /// </summary>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function Store(const Value: Boolean = True): TConversationsParams;

    /// <summary>
    /// Handoffs is the control-transfer mechanism for an action, configurable either to execute automatically
    /// on the server side or to hand execution back to the user in client mode.
    /// </summary>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function HandoffExecution(const Value: THandoffExecutionType): TConversationsParams; overload;

    /// <summary>
    /// Handoffs is the control-transfer mechanism for an action, configurable either to execute automatically
    /// on the server side or to hand execution back to the user in client mode.
    /// </summary>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function HandoffExecution(const Value: string): TConversationsParams; overload;

    /// <summary>
    /// Specify the agent ID to be used for the conversation.
    /// </summary>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function AgentId(const Value: string): TConversationsParams;

    /// <summary>
    /// Specify the identifier from which the conversation will be restarted.
    /// </summary>
    /// <returns>
    /// The updated <c>TConversationsParams</c> instance.
    /// </returns>
    function FromEntryId(const Value: string): TConversationsParams;
  end;

  /// <summary>
  /// Defines the parameters for configuring an agent within a conversation context.
  /// </summary>
  /// <remarks>
  /// Extend <see cref="TConversationsCommonParams"/> to include agent-specific settings.
  /// Use <see cref="Handoffs"/> to declare which handoff mechanisms this agent supports
  /// (for example, server‑side automation or client‑side control transfer).
  /// </remarks>
  TConversationsAgentParams = class(TConversationsCommonParams)
    /// <summary>
    /// Handoffs is the control-transfer mechanism for an action, configurable either to execute automatically
    /// on the server side or to hand execution back to the user in client mode.
    /// </summary>
    function Handoffs(const Value: TArray<string>): TConversationsAgentParams;
  end;

implementation

{ TConversationsParams }

function TConversationsParams.AgentId(const Value: string): TConversationsParams;
begin
  Result := TConversationsParams(Add('agent_id', Value));
end;

function TConversationsParams.FromEntryId(
  const Value: string): TConversationsParams;
begin
  Result := TConversationsParams(Add('from_entry_id', Value));
end;

function TConversationsParams.HandoffExecution(
  const Value: THandoffExecutionType): TConversationsParams;
begin
  Result := TConversationsParams(Add('handoff_execution', Value.ToString));
end;

function TConversationsParams.HandoffExecution(
  const Value: string): TConversationsParams;
begin
  Result := TConversationsParams(Add('handoff_execution', THandoffExecutionType.Create(Value).ToString));
end;

function TConversationsParams.Inputs(const Value: string): TConversationsParams;
begin
  Result := TConversationsParams(Add('inputs', Value));
end;

function TConversationsParams.Inputs(
  const Value: TArray<TEntryParams>): TConversationsParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TConversationsParams(Add('inputs', JSONArray));
end;

function TConversationsParams.Store(const Value: Boolean): TConversationsParams;
begin
  Result := TConversationsParams(Add('store', Value));
end;

function TConversationsParams.Stream(const Value: Boolean): TConversationsParams;
begin
  Result := TConversationsParams(Add('stream', Value));
end;

{ TEntryParams }

function TEntryParams.&Type(const Value: string): TEntryParams;
begin
  Result := TEntryParams(Add('type', Value));
end;

function TEntryParams.CompletedAt(const Value: string): TEntryParams;
begin
  Result := TEntryParams(Add('completed_at', Value));
end;

function TEntryParams.CreatedAt(const Value: string): TEntryParams;
begin
  Result := TEntryParams(Add('created_at', Value));
end;

function TEntryParams.Id(const Value: string): TEntryParams;
begin
  Result := TEntryParams(Add('id', Value));
end;

class function TEntryParams.New: TEntryParams;
begin
  Result := TEntryParams.Create.&Type();
end;

function TEntryParams.&Object(const Value: string): TEntryParams;
begin
  Result := TEntryParams(Add('object', Value));
end;

{ TMessageInputEntryParams }

function TMessageInputEntryParams.Content(
  const Value: string): TMessageInputEntryParams;
begin
  Result := TMessageInputEntryParams(Add('content', Value));
end;

function TMessageInputEntryParams.Content(
  const Value: TArray<TContentParams>): TMessageInputEntryParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TMessageInputEntryParams(Add('content', JSONArray));
end;

function TMessageInputEntryParams.Role(
  const Value: string): TMessageInputEntryParams;
begin
  Result := TMessageInputEntryParams(Add('role', TMessageRole.Create(Value).ToString));
end;

{ TTextChunkParams }

class function TTextChunkParams.New: TTextChunkParams;
begin
  Result := TTextChunkParams.Create.&Type();
end;

function TTextChunkParams.Text(
  const Value: string): TTextChunkParams;
begin
  Result := TTextChunkParams(Add('text', Value));
end;

function TTextChunkParams.&Type(
  const Value: string): TTextChunkParams;
begin
  Result := TTextChunkParams(Add('type', Value));
end;

{ TImageURLChunkParams }

function TImageURLChunkParams.ImageUrl(
  const Value: TImageUrlParams): TImageURLChunkParams;
begin
  Result := TImageURLChunkParams(Add('image_url', Value.Detach));
end;

class function TImageURLChunkParams.New: TImageURLChunkParams;
begin
  Result := TImageURLChunkParams.Create.&Type();
end;

function TImageURLChunkParams.&Type(const Value: string): TImageURLChunkParams;
begin
  Result := TImageURLChunkParams(Add('type', Value));
end;

{ TImageUrlParams }

function TImageUrlParams.Detail(const Value: string): TImageUrlParams;
begin
  Result := TImageUrlParams(Add('detail', Value));
end;

class function TImageUrlParams.New: TImageUrlParams;
begin
  Result := TImageUrlParams.Create;
end;

function TImageUrlParams.Url(const Value: string): TImageUrlParams;
begin
  Result := TImageUrlParams(Add('url', Value));
end;

{ TToolFileChunkParams }

function TToolFileChunkParams.FileId(const Value: string): TToolFileChunkParams;
begin
  Result := TToolFileChunkParams(Add('file_id', Value));
end;

function TToolFileChunkParams.FileName(
  const Value: string): TToolFileChunkParams;
begin
  Result := TToolFileChunkParams(Add('file_name', Value));
end;

function TToolFileChunkParams.FileType(
  const Value: string): TToolFileChunkParams;
begin
  Result := TToolFileChunkParams(Add('file_type', Value));
end;

class function TToolFileChunkParams.New: TToolFileChunkParams;
begin
  Result := TToolFileChunkParams.Create.&Type();
end;

function TToolFileChunkParams.Tool(const Value: string): TToolFileChunkParams;
begin
  Result := TToolFileChunkParams(Add('tool', Value));
end;

function TToolFileChunkParams.&Type(const Value: string): TToolFileChunkParams;
begin
  Result := TToolFileChunkParams(Add('type', Value));
end;

{ TDocumentUrlChunkParams }

function TDocumentUrlChunkParams.DocumentName(
  const Value: string): TDocumentUrlChunkParams;
begin
  Result := TDocumentUrlChunkParams(Add('document_name', Value));
end;

function TDocumentUrlChunkParams.DocumentUrl(
  const Value: string): TDocumentUrlChunkParams;
begin
  Result := TDocumentUrlChunkParams(Add('document_url', Value));
end;

class function TDocumentUrlChunkParams.New: TDocumentUrlChunkParams;
begin
  Result := TDocumentUrlChunkParams.Create.&Type();
end;

function TDocumentUrlChunkParams.&Type(
  const Value: string): TDocumentUrlChunkParams;
begin
  Result := TDocumentUrlChunkParams(Add('type', Value));
end;

{ TFunctionResultEntryParams }

function TFunctionResultEntryParams.Result(
  const Value: string): TFunctionResultEntryParams;
begin
  Result := TFunctionResultEntryParams(Add('result', Value));
end;

function TFunctionResultEntryParams.ToolCallId(
  const Value: string): TFunctionResultEntryParams;
begin
  Result := TFunctionResultEntryParams(Add('tool_call_id', Value));
end;

{ TCompletionArgsParams }

function TCompletionArgsParams.Stop(const Value: string): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('stop', Value));
end;

function TCompletionArgsParams.FrequencyPenalty(
  const Value: Double): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('frequency_penalty', Value));
end;

function TCompletionArgsParams.MaxTokens(
  const Value: Integer): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('max_tokens', Value));
end;

function TCompletionArgsParams.Prediction(
  const Value: string): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('prediction',
    TJSONObject.Create
      .AddPair('type', 'content')
      .AddPair('content', Value)
    ));
end;

function TCompletionArgsParams.PresencePenalty(
  const Value: Double): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('presence_penalty', Value));
end;

function TCompletionArgsParams.RandomSeed(
  const Value: Integer): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('random_seed', Value));
end;

function TCompletionArgsParams.ResponseFormat(
  const Value: string): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('response_format', TResponseFormatParams.Json_Schema(Value)));
end;

function TCompletionArgsParams.ResponseFormat(
  const Value: TResponseFormatParams): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('response_format', Value.Detach));
end;

function TCompletionArgsParams.Stop(
  const Value: TArray<string>): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('stop', Value));
end;

function TCompletionArgsParams.Temperature(
  const Value: Double): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('temperature', Value));
end;

function TCompletionArgsParams.ToolChoice(
  const Value: TToolChoice): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('tool_choice', Value.ToString));
end;

function TCompletionArgsParams.ToolChoice(
  const Value: string): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('tool_choice', TToolChoice.Create(Value).ToString));
end;

function TCompletionArgsParams.TopP(const Value: Double): TCompletionArgsParams;
begin
  Result := TCompletionArgsParams(Add('top_p', Value));
end;

{ TConnectorParams }

function TConnectorParams.&Function(const Value: TToolFunctionParams): TConnectorParams;
begin
  Result := TConnectorParams(Add('function', Value.Detach));
end;

class function TConnectorParams.New(const Value: string): TConnectorParams;
begin
  Result := TConnectorParams.Create.&Type(Value);
end;

function TConnectorParams.&Type(const Value: string): TConnectorParams;
begin
  Result := TConnectorParams(Add('type', TToolType.Create(Value).ToString));
end;

class function TConnectorParams.New(const Value: TToolType): TConnectorParams;
begin
  Result := TConnectorParams.Create.&Type(Value);
end;

function TConnectorParams.&Type(const Value: TToolType): TConnectorParams;
begin
  Result := TConnectorParams(Add('type', Value.ToString));
end;

class function TConnectorParams.New(const Value: IFunctionCore): TConnectorParams;
begin
  Result := TConnectorParams.Create
    .&Type('function')
    .&function(TJSONObject.ParseJSONValue(Value.ToString));
end;

function TConnectorParams.&Function(const Value: TJSONValue): TConnectorParams;
begin
  Result := TConnectorParams(Add('function', Value));
end;

function TConnectorParams.LibraryIds(const Value: TArray<string>): TConnectorParams;
begin
  Result := TConnectorParams(Add('library_ids', Value));
end;

class function TConnectorParams.New(const Value: TArray<string>): TConnectorParams;
begin
  Result := TConnectorParams.Create.&Type('document_library').LibraryIds(Value);
end;

class function TConnectorParams.New(
  const Value: TToolFunctionParams): TConnectorParams;
begin
  Result := TConnectorParams.Create
    .&Type('function')
    .&function(Value);
end;

{ TToolFunctionParams }

function TToolFunctionParams.Description(
  const Value: string): TToolFunctionParams;
begin
  Result := TToolFunctionParams(Add('description', Value));
end;

function TToolFunctionParams.Name(const Value: string): TToolFunctionParams;
begin
  Result := TToolFunctionParams(Add('name', Value));
end;

function TToolFunctionParams.Parameters(
  const Value: TSchemaParams): TToolFunctionParams;
begin
  Result := TToolFunctionParams(Add('parameters', Value.Detach));
end;

function TToolFunctionParams.Parameters(
  const Value: TJSONObject): TToolFunctionParams;
begin
  Result := TToolFunctionParams(Add('parameters', Value));
end;

function TToolFunctionParams.Strict(const Value: Boolean): TToolFunctionParams;
begin
  Result := TToolFunctionParams(Add('strict', Value));
end;

{ TConnector }

class function TConnector.document_library(
  const Value: TArray<string>): TConnectorParams;
begin
  Result := TConnectorParams.New(Value);
end;

class function TConnector.&function(const Value: IFunctionCore): TConnectorParams;
begin
  Result := TConnectorParams.New(Value);
end;

class function TConnector.&function(
  const Value: TToolFunctionParams): TConnectorParams;
begin
  Result := TConnectorParams.New(Value);
end;

class function TConnector.code_interpreter: TConnectorParams;
begin
  Result := TConnectorParams.New('code_interpreter');
end;

class function TConnector.image_generation: TConnectorParams;
begin
  Result := TConnectorParams.New('image_generation');
end;

class function TConnector.web_search: TConnectorParams;
begin
  Result := TConnectorParams.New('web_search');
end;

class function TConnector.web_search_premium: TConnectorParams;
begin
  Result := TConnectorParams.New('web_search_premium');
end;

{ TConversationsCommonParams }

function TConversationsCommonParams.CompletionArgs(
  const Value: TCompletionArgsParams): TConversationsCommonParams;
begin
  Result := TConversationsCommonParams(Add('completion_args', Value.Detach));
end;

function TConversationsCommonParams.Description(
  const Value: string): TConversationsCommonParams;
begin
  Result := TConversationsCommonParams(Add('description', Value));
end;

function TConversationsCommonParams.Instructions(
  const Value: TReasoningInstruction; Arg: string): TConversationsCommonParams;
begin
  case Value of
    Custom:
      Result := TConversationsCommonParams(Add('instructions', Arg));
    else
      Result := TConversationsCommonParams(Add('instructions', Value.ToString));
  end;
end;

function TConversationsCommonParams.Instructions(
  const Value: string): TConversationsCommonParams;
begin
  Result := TConversationsCommonParams(Add('instructions', Value));
end;

function TConversationsCommonParams.Model(
  const Value: string): TConversationsCommonParams;
begin
  Result := TConversationsCommonParams(Add('model', Value));
end;

function TConversationsCommonParams.Name(
  const Value: string): TConversationsCommonParams;
begin
  Result := TConversationsCommonParams(Add('name', Value));
end;

function TConversationsCommonParams.Tools(
  const Value: TArray<TConnectorParams>): TConversationsCommonParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TConversationsCommonParams(Add('tools', JSONArray));
end;

{ TConversationsAgentParams }

function TConversationsAgentParams.Handoffs(const Value: TArray<string>): TConversationsAgentParams;
begin
  Result := TConversationsAgentParams(Add('handoffs', Value));
end;

end.
