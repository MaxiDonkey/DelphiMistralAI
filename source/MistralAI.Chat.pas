unit MistralAI.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, System.NetEncoding, System.Net.Mime, MistralAI.API.Params,
  MistralAI.API, MistralAI.Functions.Core, MistralAI.Functions.Tools,
  MistralAI.Async.Support, MistralAI.Async.Params, MistralAI.Types, MistralAI.Schema,
  MistralAI.Async.Promise, MistralAI.API.Normalizer, MistralAI.Async.Parallel;

type
  /// <summary>
  /// Represents a message payload that includes an image, identified by a URL or a base-64 encoded string.
  /// </summary>
  /// <remarks>
  /// This class is used to encapsulate details of an image message, including its source (URL or base-64 string)
  /// and additional descriptive information. It extends <c>TJSONParam</c> for JSON serialization and integration with APIs.
  /// </remarks>
  /// <example>
  /// This class can be utilized to construct structured image messages for communication in chat systems.
  /// </example>
  TMessageImageURL = class(TJSONParam)
  public
    /// <summary>
    /// Url or base-64 string
    /// </summary>
    function Url(const Value: string): TMessageImageURL;

    /// <summary>
    /// Detail string
    /// </summary>
    function Detail(const Value: string): TMessageImageURL;
  end;

  TDocumentUrlParams = class(TJSONParam)
    function &Type(const Value: string = 'document_url'): TDocumentUrlParams;

    function DocumentUrl(const Value: string): TDocumentUrlParams;

    class function New: TDocumentUrlParams;
  end;

  /// <summary>
  /// Represents the content of a message, which can be either text or an image URL.
  /// </summary>
  /// <remarks>
  /// This class is used to define the structure and type of content within a message,
  /// such as textual data or image references. It extends <c>TJSONParam</c> for
  /// seamless JSON serialization and integration with APIs.
  /// </remarks>
  TMessageContent = class(TJSONParam)
  public
    /// <summary>
    /// Type of the content: text or image_url
    /// </summary>
    function &Type(const Value: TContentType): TMessageContent;

    /// <summary>
    /// Text when type is text
    /// </summary>
    function Text(const Value: string): TMessageContent;

    /// <summary>
    /// Url or base-64 string when type is image_url
    /// </summary>
    function ImageUrl(const Url: string): TMessageContent; overload;

    /// <summary>
    /// Url or base-64 string when type is image_url
    /// </summary>
    function ImageUrl(const Url: string; const Detail: string): TMessageContent; overload;

    function InputAudio(const Url: string): TMessageContent;
  end;

  /// <summary>
  /// Represents the metadata and arguments of a function invoked during a chat interaction.
  /// </summary>
  /// <remarks>
  /// This class is used to define the structure of function calls, including the function's name and its associated arguments.
  /// It is designed to be serialized into JSON format for integration with APIs or other systems requiring structured function data.
  /// </remarks>
  TFunctionCalled = class(TJSONParam)
  public
    /// <summary>
    /// The name of the function called
    /// </summary>
    function Name(const Value: string): TFunctionCalled;

    /// <summary>
    /// The arguments returned by the function called
    /// </summary>
    function Arguments(const Value: string): TFunctionCalled;
  end;

  /// <summary>
  /// Represents a tool call configuration used in chat-based operations.
  /// </summary>
  /// <remarks>
  /// This class is designed to define and manage tool calls within chat contexts, including
  /// their type, identifier, and associated functions. It integrates with <c>TJSONParam</c>
  /// for seamless serialization and API interactions.
  /// </remarks>
  TToolCalls = class(TJSONParam)
  public
    /// <summary>
    /// Tool calls id
    /// </summary>
    function Id(const Value: string): TToolCalls;

    /// <summary>
    /// Tool calls type: Default: "function"
    /// </summary>
    function &Type(const Value: TToolType = TToolType.function): TToolCalls;

    /// <summary>
    /// Represents the object function called
    /// </summary>
    function Func(const Name: string; Arguments: string): TToolCalls;

    /// <summary>
    /// index default 0
    /// </summary>
    function Index(const Value: Integer): TToolCalls;

    /// <summary>
    /// Create a new <c>TtoolCalls</c> instance
    /// </summary>
    class function New(Id, Name, Arguments: string): TToolCalls; overload;

    /// <summary>
    /// Create a new <c>TtoolCalls</c> instance
    /// </summary>
    class function New(Name, Arguments: string): TToolCalls; overload;

    /// <summary>
    /// Create a new <c>TtoolCalls</c> instance
    /// </summary>
    class function New: TToolCalls; overload;
  end;

  /// <summary>
  /// Represents the payload structure for a chat message in the context of a conversation.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the essential elements of a chat message, including its role, content,
  /// and associated metadata. It extends <c>TJSONParam</c>, enabling seamless integration with JSON-based
  /// APIs for chat functionalities. This class is a foundational building block for managing the flow of
  /// conversations between users, systems, and assistants.
  /// </remarks>
  TChatMessagePayload = class(TJSONParam)
  public
    /// <summary>
    /// Gets or sets the role of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property determines who is sending the message. It can be a "user" (representing the end user), an "assistant" (representing an AI or bot),
    /// or "system" (representing system-level messages). This property is essential for contextualizing the content of the message within the chat.
    /// </remarks>
    function Role(const Value: TMessageRole): TChatMessagePayload;

    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant, or the system.
    /// </remarks>
    function Content(const Value: string): TChatMessagePayload; overload;

    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant, or the system.
    /// </remarks>
    function Content(const Value: TArray<string>): TChatMessagePayload; overload;

    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant, or the system.
    /// </remarks>
    function Content(const Kind: TContentType; const Value: string; const Detail: string = ''): TChatMessagePayload; overload;

    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant, or the system.
    /// </remarks>
    function Content(const Value: TJSONArray): TChatMessagePayload; overload;

    function Content(const Value: string; const Url: TDocumentUrlParams): TChatMessagePayload; overload;

    /// <summary>
    /// Set prefix value
    /// </summary>
    function Prefix(const Value: Boolean): TChatMessagePayload;

    /// <summary>
    /// Set a tool_calls with an array of functions
    /// </summary>
    function ToolCalls(const Value: TArray<TToolCalls>): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the assistant.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the assistant is sending.
    /// </param>
    /// <param name="Prefix">
    /// Use prefix or no.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "assistant" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is a convenience for creating assistant messages. Use this method when the assistant needs to respond to the user or system.
    /// </remarks>
    class function Assistant(const Value: string; const Prefix: Boolean = False): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the assistant and includes associated vision sources.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="Url">
    /// An array of strings representing vision sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function Assistant(const Value: string; const Url: TArray<string>; const Prefix: Boolean = False): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the assistant and includes tool calls.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="Func">
    /// An array of strings representing tool calls sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function Assistant(const Value: string; const Func: TArray<TToolCalls>; const Prefix: Boolean = False): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the assistant and includes associated vision sources and tool calls.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="Url">
    /// An array of strings representing vision sources.
    /// </param>
    /// </param>
    /// <param name="Func">
    /// An array of strings representing tool calls sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function Assistant(const Value: string; const Url: TArray<string>;
      const Func: TArray<TToolCalls>; const Prefix: Boolean = False): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the system.
    /// </summary>
    /// <param name="Value">
    /// The content of the system message.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "system" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create system-level messages, which may be used for notifications, warnings, or other system-related interactions.
    /// </remarks>
    class function System(const Value: string): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the system.
    /// </summary>
    /// <param name="Value">
    /// The content of the system message.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "system" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create system-level messages, which may be used for notifications, warnings, or other system-related interactions.
    /// </remarks>
    class function System(const Value: TArray<string>): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the user.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the user is sending.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective, typically representing inputs or queries in the conversation.
    /// </remarks>
    class function User(const Value: string): TChatMessagePayload; overload;

    /// <summary>
    /// Creates a new chat message payload with the role of the user and includes associated vision sources.
    /// </summary>
    /// <param name="Value">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="Url">
    /// An array of strings representing vision sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function User(const Value: string; const Url: TArray<string>): TChatMessagePayload; overload;

    class function User(const Value: string; const Url: string): TChatMessagePayload; overload;

    class function User(const Value: string; const Url: TDocumentUrlParams): TChatMessagePayload; overload;
  end;

  /// <summary>
  /// Represents the payload structure for a chat message in the context of a conversation.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the essential elements of a chat message, including its role, content,
  /// and associated metadata. It extends <c>TJSONParam</c>, enabling seamless integration with JSON-based
  /// APIs for chat functionalities. This class is a foundational building block for managing the flow of
  /// conversations between users, systems, and assistants.
  /// </remarks>
  PayLoad = TChatMessagePayload;

  /// <summary>
  /// The <c>TChatParams</c> class represents the set of parameters used to configure a chat interaction with an AI model.
  /// </summary>
  /// <remarks>
  /// This class allows you to define various settings that control how the model behaves, including which model to use, how many tokens to generate,
  /// what kind of messages to send, and how the model should handle its output. By using this class, you can fine-tune the AI's behavior and response format
  /// based on your application's specific needs.
  /// <para>
  /// It inherits from <c>TJSONParam</c>, which provides methods for handling and serializing the parameters as JSON, allowing seamless integration
  /// with JSON-based APIs.
  /// </para>
  /// <code>
  /// var
  ///   Params: TChatParams;
  /// begin
  ///   Params := TChatParams.Create
  ///     .Model('my_model')
  ///     .MaxTokens(100)
  ///     .Messages([TChatMessagePayload.User('Hello!')])
  ///     .ResponseFormat('json_object')
  ///     .Temperature(0.7)
  ///     .TopP(1)
  ///     .SafePrompt(True);
  /// end;
  /// </code>
  /// This example shows how to instantiate and configure a <c>TChatParams</c> object for interacting with an AI model.
  /// </remarks>
  TChatParams = class(TJSONParam)
    /// <summary>
    /// Specifies the identifier of the model to use.
    /// Currently compatible with "mistral-tiny" or "mistral-large-latest".
    /// </summary>
    /// <param name="Value">
    /// The model ID to be used for the completion.
    /// Ensure that the specified model is supported and correctly spelled.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This parameter is required and determines which model will process the request.
    /// </remarks>
    function Model(const Value: string): TChatParams;

    /// <summary>
    /// Sets the maximum number of tokens to generate in the completion.
    /// The total token count of your prompt plus <c>max_tokens</c> cannot exceed the model's context length.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of tokens to generate.
    /// Choose an appropriate value based on your prompt length to avoid exceeding the model's limit.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function MaxTokens(const Value: Integer): TChatParams;

    /// <summary>
    /// Provides the prompt(s) for the model to generate completions from, structured as a list of messages with roles (user, assistant, system) and content.
    /// </summary>
    /// <param name="Value">An array of <c>TChatMessagePayload</c> representing the messages in the conversation.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The first message should have either a "user" or "system" role to initiate the conversation properly.
    /// </remarks>
    function Messages(const Value: TArray<TChatMessagePayload>): TChatParams;

    /// <summary>
    /// Specifies the format in which the model should return the response. This can include formats like JSON or plain text.
    /// </summary>
    /// <param name="Value">The <c>TResponseFormatParams</c> value.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If not specified, the default value is <c>{ "type": "text" }</c>. When using JSON mode, it's necessary to instruct the model to produce JSON explicitly through the system or user messages.
    /// </remarks>
    function ResponseFormat(const Value: TResponseFormatParams): TChatParams; overload;

    /// <summary>
    /// Specifies the format in which the model should return the response. This can include formats like JSON or plain text.
    /// </summary>
    /// <param name="Value">The string value.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If not specified, the default value is <c>{ "type": "text" }</c>. When using JSON mode, it's necessary to instruct the model to produce JSON explicitly through the system or user messages.
    /// </remarks>
    function ResponseFormat(const Value: string): TChatParams; overload;

    /// <summary>
    /// Specifies whether to stream back partial progress as server-sent events (SSE).
    /// If <c>true</c>, tokens are sent as they become available.
    /// If <c>false</c>, the server will hold the request open until timeout or completion.
    /// </summary>
    /// <param name="Value">
    /// A boolean value indicating whether to enable streaming. Default is <c>true</c>, meaning streaming is enabled by default.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Stream(const Value: Boolean = True): TChatParams;

    /// <summary>
    /// Sets the sampling temperature to use for the model's output.
    /// Higher values like 0.8 make the output more random, while lower values like 0.2 make it more focused and deterministic.
    /// </summary>
    /// <param name="Value">
    /// The temperature value between 0.0 and 1.0. Default is 0.7.
    /// A temperature of 0 makes the model deterministic, while a temperature of 1 allows for maximum creativity.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Temperature(const Value: Single): TChatParams;

    /// <summary>
    /// Sets the nucleus sampling probability mass for the model (Top-p).
    /// For example, 0.1 means only the tokens comprising the top 10% probability mass are considered.
    /// </summary>
    /// <param name="Value">
    /// The <c>top_p</c> value between 0.0 and 1.0. Default is 1.
    /// Lower values limit the model to consider only the most probable options.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function TopP(const Value: Single): TChatParams;

    /// <summary>
    /// Specifies a list of tools that the model can use to generate structured outputs such as JSON inputs for function calls.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TChatMessageTool</c> representing the tools available to the model.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// These tools can include functions that the model can utilize when generating output. For example, they can help the model produce structured data for specific tasks.
    /// </remarks>
    function Tools(const Value: TArray<TChatMessageTool>): TChatParams; overload;

    /// <summary>
    /// Specifies a list of tools that the model can use to generate structured outputs such as JSON inputs for function calls.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>IFunctionCore</c> representing the tools available to the model.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// These tools can include functions that the model can utilize when generating output. For example, they can help the model produce structured data for specific tasks.
    /// </remarks>
    function Tools(const Value: TArray<IFunctionCore>): TChatParams; overload;

    /// <summary>
    /// Configures how the model interacts with functions. This can either prevent, allow, or require function calls depending on the setting.
    /// </summary>
    /// <param name="Value">
    /// The <c>TToolChoice</c> setting for function interaction, with a default of "auto".
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If set to <c>none</c>, the model will not call any functions and will generate a message instead. If set to <c>auto</c>, the model can choose between generating a message or calling a function. If set to <c>any</c>, the model is required to call a function.
    /// </remarks>
    function ToolChoice(const Value: TToolChoice): TChatParams; overload;

    /// <summary>
    /// Configures how the model interacts when required is on.
    /// </summary>
    /// <param name="Value">
    /// The <c>TToolChoice</c> setting for function interaction, with a default of "auto".
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function ToolChoice(const FunctionName: string): TChatParams; overload;

    /// <summary>
    /// Sets the random seed for deterministic results during sampling.
    /// </summary>
    /// <param name="Value">An integer value to be used as the seed.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Providing a random seed ensures that multiple calls with the same parameters produce the same results, useful for testing or reproducible outcomes.
    /// </remarks>
    function RandomSeed(const Value: Integer): TChatParams;

    /// <summary>
    /// Stop (string)
    /// Stop generation if this token is detected. Or if one of these tokens is detected when providing an array
    /// </summary>
    /// <param name="Value">
    /// The string that causes the stop
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Stop(const Value: string): TChatParams; overload;

    /// <summary>
    /// Stop Array of Stop (strings) (Stop)
    /// Stop generation if this token is detected. Or if one of these tokens is detected when providing an array
    /// </summary>
    /// <param name="Value">
    /// The array of string that causes the stop
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Stop(const Value: TArray<string>): TChatParams; overload;

    /// <summary>
    /// Presence_penalty determines how much the model penalizes the repetition of words or phrases
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher presence penalty encourages the model to use a wider variety of words and phrases,
    /// making the output more diverse and creative.
    /// </remarks>
    function PresencePenalty(const Value: Double): TChatParams;

    /// <summary>
    /// Frequency_penalty penalizes the repetition of words based on their frequency in the generated text.
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher frequency penalty discourages the model from repeating words that have already appeared
    /// frequently in the output, promoting diversity and reducing repetition.
    /// </remarks>
    function FrequencyPenalty(const Value: Double): TChatParams;

    /// <summary>
    /// Number of completions to return for each request, input tokens are only billed once.
    /// </summary>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function N(const Value: Integer): TChatParams;

    /// <summary>
    /// Enable users to specify expected results, optimizing response times by leveraging known or
    /// predictable content.
    /// </summary>
    /// <param name="Value">The string prediction content.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This approach is especially effective for updating text documents or code files with minimal
    /// changes, reducing latency while maintaining high-quality results.
    /// </remarks>
    function Prediction(const Value: string): TChatParams;

    /// <summary>
    /// Whether to allow the model to run tool calls in parallel.
    /// </summary>
    /// <param name="Value">If true then enable parallel mode.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Default: true
    /// </remarks>
    function ParallelToolCalls(const Value: Boolean): TChatParams;

    /// <summary>
    /// Allows toggling between the reasoning mode and no system prompt.
    /// When set to reasoning the system prompt for reasoning models will be used.
    /// </summary>
    function PromptMode(const Value: string = 'reasoning'): TChatParams;

    /// <summary>
    /// Determines whether a safety prompt should be injected automatically before the conversation starts.
    /// </summary>
    /// <param name="Value">
    /// A boolean indicating whether to enable the safety prompt. Default is <c>False</c>.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function SafePrompt(const Value: Boolean = False): TChatParams;

    function DocumentImageLimit(const Value: Integer): TChatParams;

    function DocumentPageLimit(const Value: Integer): TChatParams;
  end;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TChatUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TChatUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPrompt_tokens: Int64;
    [JsonNameAttribute('completion_tokens')]
    FCompletion_tokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotal_tokens: Int64;
  public
    /// <summary>
    /// The number of tokens consumed by the prompt in the chat request.
    /// </summary>
    /// <remarks>
    /// This property tracks how many tokens were used for the initial input prompt sent to the model.
    /// It's useful for understanding how much of the token limit is being consumed by the user's query or system instructions.
    /// </remarks>
    property PromptTokens: Int64 read FPrompt_tokens write FPrompt_tokens;

    /// <summary>
    /// The number of tokens consumed by the completion generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>CompletionTokens</c> property tracks how many tokens were used in the response generated by the model.
    /// This helps in determining the token cost of the AI's output relative to the prompt size.
    /// </remarks>
    property CompletionTokens: Int64 read FCompletion_tokens write FCompletion_tokens;

    /// <summary>
    /// The total number of tokens used for the entire chat request, including both the prompt and the completion.
    /// </summary>
    /// <remarks>
    /// The <c>TotalTokens</c> property provides the overall token usage for a single request, which is the sum of the tokens
    /// from the prompt and the completion. This value is critical for tracking token consumption and ensuring it stays within
    /// model limitations or pricing thresholds.
    /// </remarks>
    property TotalTokens: Int64 read FTotal_tokens write FTotal_tokens;
  end;

  TThinking = class
  private
    [JsonReflectAttribute(ctString, rtString, TContentTypeInterceptor)]
    FType: TContentType;
    FText: string;
  public
    property &Type: TContentType read FType write FType;
    property Text: string read FText write FText;
  end;

  TMessageContentCommon = class
  private
    [JsonReflectAttribute(ctString, rtString, TContentTypeInterceptor)]
    FType: TContentType;
  public
    property &Type: TContentType read FType write FType;
  end;

  TMessageContentText = class(TMessageContentCommon)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TMessageContentReference = class(TMessageContentText)
  private
    [JsonNameAttribute('reference_ids')]
    FReferenceIds: TArray<Int64>;
  public
    property ReferenceIds: TArray<Int64> read FReferenceIds write FReferenceIds;
  end;

  TMessageContentDocumentUrl = class(TMessageContentReference)
  private
    [JsonNameAttribute('document_url')]
    FDocumentUrl: string;
    [JsonNameAttribute('document_name')]
    FDocumentName: string;
  public
    property DocumentUrl: string read FDocumentUrl write FDocumentUrl;
    property DocumentName: string read FDocumentName write FDocumentName;
  end;

  TContentImageUrl = class
  private
    FUrl: string;
    FDetail: string;
  public
    property Url: string read FUrl write FUrl;
    property Detail: string read FDetail write FDetail;
  end;

  TMessageContentImageUrl = class(TMessageContentDocumentUrl)
  private
    [JsonNameAttribute('image_url')]
    FImageUrl: TContentImageUrl;
  public
    property ImageUrl: TContentImageUrl read FImageUrl write FImageUrl;
    destructor Destroy; override;
  end;

  TMessageContentThinking = class(TMessageContentImageUrl)
  private
    FThinking: TArray<TThinking>;
  public
    property Thinking: TArray<TThinking> read FThinking write FThinking;
    destructor Destroy; override;
  end;

  TChatContent = class(TMessageContentThinking);

  /// <summary>
  /// Represents a chat message exchanged between participants (user, assistant, or system) in a conversation.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessage</c> class encapsulates the essential information of a message within a chat application, including:
  /// - The role of the sender (user, assistant, or system).
  /// - The content of the message itself.
  /// - Optionally, a list of tool calls that may be required to complete the message response.
  /// This class is fundamental for managing the flow of a conversation, allowing the system to track who said what and what actions need to be taken.
  /// </remarks>
  TChatMessage = class
  private
    FRole: string;
    FContent: TArray<TChatContent>;
    [JsonNameAttribute('tool_calls')]
    FToolsCalls: TArray<TCalledFunction>;
    FPrefix: Boolean;
  public
    /// <summary>
    /// The role of the author of this message, indicating the sender (e.g., user, assistant, or system).
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property identifies the participant responsible for the message. Common values are "user" for messages sent by the user,
    /// "assistant" for responses generated by the AI, or "system" for control messages.
    /// </remarks>
    property Role: string read FRole write FRole;

    /// <summary>
    /// The contents of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property stores the actual message text. This can include user inputs, assistant-generated replies, or system instructions.
    /// </remarks>
    property Content: TArray<TChatContent> read FContent write FContent;

    /// <summary>
    /// A list of tool calls to be executed for query completion.
    /// </summary>
    /// <remarks>
    /// The <c>ToolsCalls</c> property contains a list of functions or tools that need to be invoked to process the current query further.
    /// This is typically used when the assistant needs to call external APIs or perform specific actions before delivering a final response.
    /// </remarks>
    property ToolsCalls: TArray<TCalledFunction> read FToolsCalls write FToolsCalls;

    /// <summary>
    /// Prefix of the content.
    /// </summary>
    /// <remarks>
    property Prefix: Boolean read FPrefix write FPrefix;

    /// <summary>
    /// Destructor to release any resources used by this instance.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a single completion option generated by the AI model during a chat interaction.
  /// </summary>
  /// <remarks>
  /// The <c>TChatChoices</c> class stores the results of the AI model's response to a user prompt. Each instance of this class represents one of potentially
  /// many choices that the model could return. This includes:
  /// - An index identifying the choice.
  /// - A message generated by the model.
  /// - Optional deltas for streamed responses.
  /// - The reason the model stopped generating tokens.
  /// This class is useful when multiple potential responses are generated and evaluated, or when streaming responses incrementally.
  /// </remarks>
  TChatChoices = class
  private
    FIndex: Int64;
    FMessage: TChatMessage;
    [JsonReflectAttribute(ctString, rtString, TFinishReasonInterceptor)]
    FFinish_reason: TFinishReason;
    FDelta: TChatMessage;
  public
    /// <summary>
    /// The index of the choice in the list of possible choices generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>Index</c> property helps identify the position of this particular choice in a set of choices provided by the AI model.
    /// This is useful when multiple options are generated for completion, and each one is referenced by its index.
    /// </remarks>
    property Index: Int64 read FIndex write FIndex;

    /// <summary>
    /// A chat completion message generated by the AI model.
    /// </summary>
    /// <remarks>
    /// The <c>Message</c> property contains the message that the model generated based on the provided prompt or conversation context.
    /// This is typically a complete message representing the AI's response to a user or system message.
    /// </remarks>
    property Message: TChatMessage read FMessage write FMessage;

    /// <summary>
    /// A chat completion delta representing partial responses generated during streaming.
    /// </summary>
    /// <remarks>
    /// The <c>Delta</c> property holds an incremental message (or delta) when the model sends streamed responses.
    /// This allows the model to progressively generate and deliver a response before it is fully completed.
    /// </remarks>
    property Delta: TChatMessage read FDelta write FDelta;

    /// <summary>
    /// The reason the model stopped generating tokens.
    /// </summary>
    /// <remarks>
    /// The <c>FinishReason</c> property indicates why the model ceased generating further tokens. This could be due to reaching a natural stop point
    /// in the conversation, hitting a token limit, or encountering a stop sequence provided by the user.
    /// </remarks>
    property FinishReason: TFinishReason read FFinish_reason write FFinish_reason;

    /// <summary>
    /// Destructor to clean up resources used by the <c>TChatChoices</c> instance.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a chat completion response generated by an AI model, containing the necessary metadata,
  /// the generated choices, and usage statistics.
  /// </summary>
  /// <remarks>
  /// The <c>TChat</c> class encapsulates the results of a chat request made to an AI model.
  /// It contains details such as a unique identifier, the model used, when the completion was created,
  /// the choices generated by the model, and token usage statistics.
  /// This class is crucial for managing the results of AI-driven conversations and understanding the
  /// underlying usage and response characteristics of the AI.
  /// </remarks>
  TChat = class(TJSONFingerprint)
  private
    FId: string;
    FObject: string;
    FCreated: Int64;
    FModel: string;
    FChoices: TArray<TChatChoices>;
    FUsage: TChatUsage;
  public
    /// <summary>
    /// A unique identifier for the chat completion.
    /// </summary>
    /// <remarks>
    /// The <c>Id</c> property stores a unique string that identifies the specific chat completion request.
    /// This is useful for tracking and managing chat sessions or retrieving the results of a particular interaction.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// The object type, which is always "chat.completion".
    /// </summary>
    /// <remarks>
    /// The <c>Object</c> property describes the type of the response. For chat completions, this value is always "chat.completion",
    /// providing a clear indication of the response type when working with multiple object types in a system.
    /// </remarks>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// The Unix timestamp (in seconds) of when the chat completion was created.
    /// </summary>
    /// <remarks>
    /// The <c>Created</c> property contains a Unix timestamp indicating when the AI generated the chat completion. This is
    /// useful for logging, auditing, or ordering chat completions chronologically.
    /// </remarks>
    property Created: Int64 read FCreated write FCreated;

    /// <summary>
    /// The model used for the chat completion.
    /// </summary>
    /// <remarks>
    /// The <c>Model</c> property specifies which AI model was used to generate the chat completion. This can be helpful
    /// when comparing results across different models or tracking which model versions are producing responses.
    /// </remarks>
    property Model: string read FModel write FModel;

    /// <summary>
    /// A list of chat completion choices generated by the model.
    /// </summary>
    /// <remarks>
    /// The <c>Choices</c> property holds an array of <c>TChatChoices</c> objects, which represent the different response options
    /// generated by the AI model. There may be multiple choices if the request asked for more than one completion.
    /// </remarks>
    property Choices: TArray<TChatChoices> read FChoices write FChoices;

    /// <summary>
    /// Usage statistics for the completion request, including token counts for the prompt and completion.
    /// </summary>
    /// <remarks>
    /// The <c>Usage</c> property contains information about the number of tokens consumed during the request, including the
    /// tokens used in the prompt and those generated in the completion. This data is important for monitoring API usage and costs.
    /// </remarks>
    property Usage: TChatUsage read FUsage write FUsage;

    /// <summary>
    /// Destructor to clean up resources used by this <c>TChat</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the memory for the array of choices or usage statistics, are
    /// properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous chat Callbacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = TAsyncCallback<TChat>;

  /// <summary>
  /// Represents a promise-based asynchronous callback for chat completion operations.
  /// </summary>
  /// <remarks>
  /// Alias of <c>TPromiseCallBack&lt;TChat&gt;</c>, this type allows you to await the result
  /// of a chat completion request and handle it as a <see cref="TChat"/> instance.
  /// </remarks>
  TPromiseChat = TPromiseCallback<TChat>;

  /// <summary>
  /// Manages asynchronous streaming chat Callbacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides Callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = TAsyncStreamCallback<TChat>;

  /// <summary>
  /// Represents a promise-based asynchronous callback for streaming chat completion operations.
  /// </summary>
  /// <remarks>
  /// Alias of <c>TPromiseStreamCallBack&lt;TChat&gt;</c>, this type provides a <see cref="TChat"/> stream
  /// that can be awaited, delivering partial <see cref="TChat"/> updates as they arrive.
  /// </remarks>
  TPromiseChatStream = TPromiseStreamCallback<TChat>;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = reference to procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// The <c>TChatRoute</c> class inherits from <c>TMistralAIAPIRoute</c> and provides an interface for managing various interactions with the chat API.
  /// It supports creating chat completion requests in synchronous, asynchronous, and streaming modes, offering mechanisms to handle responses generated by the model.
  /// </summary>
  /// <remarks>
  /// This class facilitates sending messages to a chat model, receiving responses, and managing them, whether synchronously or asynchronously.
  /// The primary methods in the class are:
  /// <para>
  /// - <c>Create</c> : Sends a chat request and waits for a full response.
  /// </para>
  /// <para>
  /// - <c>AsyncCreate</c> : Performs an asynchronous chat completion request with event handling.
  /// </para>
  /// <para>
  /// - <c>CreateStream</c> : Initiates a chat completion request in streaming mode, receiving tokens progressively.
  /// </para>
  /// <para>
  /// - <c>ASyncCreateStream</c> : Performs an asynchronous request in streaming mode with event handling.
  /// </para>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including Callbacks for processing responses or errors.
  /// </remarks>
  TChatRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Asynchronously sends a chat completion request and returns a promise that resolves with the resulting <see cref="TChat"/> object.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure in which you configure the <see cref="TChatParams"/> for the request, such as model selection, messages, and other options.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that provides asynchronous Callbacks wrapped in a <see cref="TPromiseChat"/>, allowing you to handle success, error, or cancellation events.
    /// If omitted, the promise will only resolve or reject based on the API response.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TChat}"/> that resolves to the completed <see cref="TChat"/> result when the request succeeds, or rejects with an exception on error.
    /// </returns>
    /// <remarks>
    /// This method wraps the standard <see cref="AsyncCreate"/> callback pattern in a promise, simplifying integration with code that favors
    /// promise-based asynchronous flows. Use <c>await</c> on the returned promise to retrieve the <see cref="TChat"/> result directly.
    /// </remarks>
    function AsyncAwaitCreate(const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseChat> = nil): TPromise<TChat>;

    /// <summary>
    /// Asynchronously sends a streaming chat completion request and returns a promise that resolves with the complete response text once streaming finishes.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure in which you configure the <see cref="TChatParams"/> for the request, such as model selection, messages, and streaming options.
    /// </param>
    /// <param name="Callbacks">
    /// A function that provides asynchronous Callbacks wrapped in a <see cref="TPromiseChatStream"/>, allowing you to handle progress updates, success, errors, and cancellation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{string}"/> that resolves to the full concatenated response text when the stream completes, or rejects with an exception on error.
    /// </returns>
    /// <remarks>
    /// This method wraps the standard <see cref="AsyncCreateStream"/> callback pattern in a promise-based interface. Partial responses received via the provided Callbacks
    /// are accumulated internally, and the returned promise yields the entire response once streaming is done.
    /// </remarks>
    function AsyncAwaitCreateStream(const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseChatStream>): TPromise<string>;

    /// <summary>
    /// Initiates parallel processing of multiple chat prompts and returns a promise for the bundled results.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the bundle parameters (model, prompts, reasoning effort, etc.) before dispatch.
    /// </param>
    /// <param name="CallBacks">
    /// An optional function that provides a <see cref="TPromiseBundleList"/> record for hooking into lifecycle events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TBundleList&gt;"/> that resolves with a <see cref="TBundleList"/> containing all responses
    /// once every parallel task completes, or rejects on the first error.
    /// </returns>
    /// <remarks>
    /// This method wraps the callback-based <see cref="CreateParallel"/> in a promise interface, enabling async/await usage
    /// for parallel prompt execution. If <paramref name="CallBacks"/> is omitted, only the promise resolution is available.
    /// </remarks>
    function AsyncAwaitCreateParallel(const ParamProc: TProc<TBundleParams>;
      const CallBacks: TFunc<TPromiseBundleList> = nil): TPromise<TBundleList>;

    /// <summary>
    /// Creates a completion for the chat message using the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, setting token limits, etc.
    /// </param>
    /// <returns>
    /// Returns a <c>TChat</c> object that contains the chat response, including the choices generated by the model.
    /// </returns>
    /// <exception cref="MistralAIExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The <c>Create</c> method sends a chat completion request and waits for the full response. The returned <c>TChat</c> object contains the model's generated response, including multiple choices if available.
    ///
    /// Example usage:
    /// <code>
    ///   MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///   var Chat := MistralAI.Chat.Create(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     for var Choice in Chat.Choices do
    ///       WriteLn(Choice.Message.Content);
    ///   finally
    ///     Chat.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Create(const ParamProc: TProc<TChatParams>): TChat;

    /// <summary>
    /// Creates a chat message completion with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, and adjusting other settings like token limits or temperature.
    /// </param>
    /// <param name="Event">
    /// A callback of type <c>TChatEvent</c> that is triggered with each chunk of data received during the streaming process. It includes the current state of the <c>TChat</c> object, a flag indicating if the stream is done, and a boolean to handle cancellation.
    /// </param>
    /// <returns>
    /// Returns <c>True</c> if the streaming process started successfully, <c>False</c> otherwise.
    /// </returns>
    /// <remarks>
    /// This method initiates a chat request in streaming mode, where the response is delivered incrementally in real-time.
    /// The <c>Event</c> callback will be invoked multiple times as tokens are received.
    /// When the response is complete, the <c>IsDone</c> flag will be set to <c>True</c>, and the <c>Chat</c> object will be <c>nil</c>.
    /// The streaming process can be interrupted by setting the <c>Cancel</c> flag to <c>True</c> within the event.
    ///
    /// Example usage:
    /// <code>
    ///   MistralAI.Chat.CreateStream(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end,
    ///
    ///     procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       // Handle displaying
    ///     end
    ///   );
    /// </code>
    /// </remarks>
    function CreateStream(const ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;

    /// <summary>
    /// Initiates parallel processing of chat prompts by creating multiple chat completions
    /// asynchronously, with results stored in a bundle and provided back to the callback function.
    /// This method allows for parallel processing of multiple prompts in an efficient manner,
    /// handling errors and successes for each chat completion.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure delegate that configures the parameters for the bundle. It is responsible
    /// for providing the necessary settings (such as model and reasoning effort) for the chat completions.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns an instance of TAsynBuffer, which manages the lifecycle of the
    /// asynchronous operation. The callbacks include handlers for start, error, and success events.
    /// </param>
    /// <remarks>
    /// The method allows for efficient parallel processing of multiple prompts by delegating
    /// individual tasks to separate threads. It each task's result is properly bundled and communicated back to the caller.
    /// If an error occurs, the error handling callback will be triggered, and the rest of the tasks
    /// will continue processing. The success callback is triggered once all tasks are completed.
    /// </remarks>
    procedure CreateParallel(const ParamProc: TProc<TBundleParams>;
      const CallBacks: TFunc<TAsynBundleList>);

    /// <summary>
    /// Create an asynchronous completion for chat message
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the chat request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided Callbacks.
    /// <code>
    /// MistralAI.Chat.AsyncCreate(
    ///   procedure (Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///   function: TAsynChat
    ///   begin
    ///     Result.Sender := Memo1;  // Instance passed to callback parameter
    ///
    ///     Result.OnStart := nil;   // If nil then; Can be omitted
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Chat: TChat)
    ///     begin
    ///       var M := Sender as TMemo; // Because Result.Sender = Memo1
    ///       // Handle success operation
    ///     end;
    ///
    ///     Result.OnError := procedure (Sender: TObject; Value: string)
    ///     begin
    ///       // Handle error message
    ///     end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreate(const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TAsynChat>);

    /// <summary>
    /// Creates an asynchronous streaming chat completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsynChatStream</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// MistralAI.Chat.AsyncCreateStream(
    ///   procedure(Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///     Params.Stream;
    ///   end,
    ///
    ///   function: TAsynChatStream
    ///   begin
    ///     Result.Sender := Memo1; // Instance passed to callback parameter
    ///     Result.OnProgress :=
    ///         procedure (Sender: TObject; Chat: TChat)
    ///         begin
    ///           // Handle progressive updates to the chat response
    ///         end;
    ///     Result.OnSuccess :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Handle success when the operation completes
    ///         end;
    ///     Result.OnError :=
    ///         procedure (Sender: TObject; Value: string)
    ///         begin
    ///           // Handle error message
    ///         end;
    ///     Result.OnDoCancel :=
    ///         function: Boolean
    ///         begin
    ///           Result := CheckBox1.Checked; // Click on checkbox to cancel
    ///         end;
    ///     Result.OnCancellation :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Processing when process has been canceled
    ///         end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreateStream(const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TAsynChatStream>);
  end;

implementation

uses
  system.StrUtils, Rest.Json, System.Rtti, MistralAI.NetEncoding.Base64;

{ TChatParams }

function TChatParams.DocumentImageLimit(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('document_image_limit', Value));
end;

function TChatParams.DocumentPageLimit(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('document_page_limit', Value));
end;

function TChatParams.FrequencyPenalty(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('frequency_penalty', Value));
end;

function TChatParams.MaxTokens(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_tokens', Value));
end;

function TChatParams.Messages(const Value: TArray<TChatMessagePayload>): TChatParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TChatParams(Add('messages', JSONArray));
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.N(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('n', Value));
end;

function TChatParams.ParallelToolCalls(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('parallel_tool_calls', Value));
end;

function TChatParams.Prediction(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('prediction',
    TJSONObject.Create
      .AddPair('type', 'content')
      .AddPair('content', Value)
    ));
end;

function TChatParams.PresencePenalty(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('presence_penalty', Value));
end;

function TChatParams.PromptMode(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('prompt_mode', Value));
end;

function TChatParams.RandomSeed(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('random_seed', Value));
end;

function TChatParams.ResponseFormat(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('response_format', TResponseFormatParams.Json_Schema(Value)));
end;

function TChatParams.ResponseFormat(
  const Value: TResponseFormatParams): TChatParams;
begin
  Result := TChatParams(Add('response_format', Value.Detach));
end;

function TChatParams.SafePrompt(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('safe_prompt', Value));
end;

function TChatParams.Stop(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('stop', Value));
end;

function TChatParams.Stop(const Value: TArray<string>): TChatParams;
begin
  Result := TChatParams(Add('stop', Value));
end;

function TChatParams.Stream(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('stream', Value));
end;

function TChatParams.Temperature(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('temperature', Value));
end;

function TChatParams.ToolChoice(const Value: TToolChoice): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', Value.ToString));
end;

function TChatParams.ToolChoice(const FunctionName: string): TChatParams;
begin
  var Tool := TJSONParam.Create
        .Add('type', 'function')
        .Add('function', TJSONObject.Create
          .AddPair('Name', FunctionName));
  Result := TChatParams(Add('tool_choice', Tool.Detach));
end;

function TChatParams.Tools(const Value: TArray<IFunctionCore>): TChatParams;
var
  Arr: TArray<TChatMessageTool>;
begin
  for var Item in Value do
    Arr := Arr + [TChatMessageTool.Add(Item)];
  Result := Tools(Arr);
end;

function TChatParams.Tools(const Value: TArray<TChatMessageTool>): TChatParams;
var
  Item: TChatMessageTool;
  Items: TJSONArray;
begin
  Items := TJSONArray.Create;
  try
    for Item in Value do
      begin
        Items.Add(Item.ToJson);
      end;
    Result := TChatParams(Add('tools', Items));
  except
    on E: Exception do
      begin
        Items.Free;
        raise;
      end;
  end;
end;

function TChatParams.TopP(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_p', Value));
end;

{ TChatChoices }

destructor TChatChoices.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TChat }

destructor TChat.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  for var Item in FChoices do
    Item.Free;
  inherited;
end;

{ TChatRoute }

function TChatRoute.AsyncAwaitCreate(const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseChat>): TPromise<TChat>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TChat>(
    procedure(const CallbackParams: TFunc<TAsynChat>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TChatRoute.AsyncAwaitCreateStream(const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseChatStream>): TPromise<string>;
begin
  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    var
      Buffer: string;
    begin
      AsyncCreateStream(ParamProc,
        function : TAsynChatStream
        begin
          Result.Sender := Callbacks.Sender;

          Result.OnStart := Callbacks.OnStart;

          Result.OnProgress :=
            procedure (Sender: TObject; Event: TChat)
            begin
              if Assigned(Callbacks.OnProgress) then
                Callbacks.OnProgress(Sender, Event);
              Buffer := Buffer + Event.Choices[0].Delta.Content[0].Text;
            end;

          Result.OnSuccess :=
            procedure (Sender: TObject)
            begin
              Resolve(Buffer);
            end;

          Result.OnError :=
            procedure (Sender: TObject; Error: string)
            begin
              if Assigned(Callbacks.OnError) then
                Error := Callbacks.OnError(Sender, Error);
              Reject(Exception.Create(Error));
            end;

          Result.OnDoCancel :=
            function : Boolean
            begin
              if Assigned(Callbacks.OnDoCancel) then
                Result := Callbacks.OnDoCancel()
              else
                Result := False;
            end;

          Result.OnCancellation :=
            procedure (Sender: TObject)
            begin
              var Error := 'aborted';
              if Assigned(Callbacks.OnCancellation) then
                Error := Callbacks.OnCancellation(Sender);
              Reject(Exception.Create(Error));
            end;
        end);
    end);
end;

function TChatRoute.AsyncAwaitCreateParallel(const ParamProc: TProc<TBundleParams>;
  const CallBacks: TFunc<TPromiseBundleList>): TPromise<TBundleList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBundleList>(
    procedure(const CallBackParams: TFunc<TAsynBundleList>)
    begin
      CreateParallel(ParamProc, CallBackParams);
    end,
    CallBacks);
end;

procedure TChatRoute.AsyncCreate(
  const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TAsynChat>);
begin
  with TAsyncCallBackExec<TAsynChat, TChat>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TChatRoute.AsyncCreateStream(
  const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TAsynChatStream>);
begin
  var CallbackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(Callbacks);

  var Sender := CallbackParams.Param.Sender;
  var OnStart := CallbackParams.Param.OnStart;
  var OnSuccess := CallbackParams.Param.OnSuccess;
  var OnProgress := CallbackParams.Param.OnProgress;
  var OnError := CallbackParams.Param.OnError;
  var OnCancellation := CallbackParams.Param.OnCancellation;
  var OnDoCancel := CallbackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
          procedure()
          begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStream(ParamProc,
                procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  if not IsDone and Assigned(Chat) then
                    begin
                      var LocalChat := Chat;
                      Chat := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChat);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
                          end;
                        end)
                     else
                       LocalChat.Free;
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnSuccess(Sender);
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);
  Task.Start;
end;

function TChatRoute.Create(const ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('chat/completions', ParamProc,
    ['choices', '*', 'message', 'content']);
end;

procedure TChatRoute.CreateParallel(const ParamProc: TProc<TBundleParams>;
  const CallBacks: TFunc<TAsynBundleList>);
var
  Tasks: TArray<ITask>;
  BundleParams: TBundleParams;
begin
  BundleParams := TBundleParams.Create;
  try
    if not Assigned(ParamProc) then
      raise Exception.Create('The lambda can''t be null');

    ParamProc(BundleParams);
    var Bundle := TBundleList.Create;
    var Ranking := 0;
    var ErrorExists := False;
    var Prompts := BundleParams.GetPrompt;
    var Counter := Length(Prompts);

    if Assigned(CallBacks.OnStart) then
      CallBacks.OnStart(CallBacks.Sender);

    SetLength(Tasks, Length(Prompts));
    for var index := 0 to Pred(Length(Prompts)) do
      begin
        Tasks[index] := TTask.Run(
          procedure
          begin
            var Buffer := Bundle.Add(index + 1);
            Buffer.Prompt := Prompts[index];
            try
              var Chat := Create(
                procedure (Params: TChatParams)
                begin
                  {--- Set the model for the process }
                  Params.Model(BundleParams.GetModel);

                  {--- Set the current prompt and developer message }
                  Params.Messages([
                    Payload.System(BundleParams.GetSystem),
                    Payload.User(Buffer.Prompt)
                  ]);
                end);
              Inc(Ranking);
              Buffer.FinishIndex := Ranking;
              Buffer.Response := Chat.Choices[0].Message.Content[0].Text;
              Buffer.Chat := Chat;
            except
              on E: Exception do
                begin
                  {--- Catch the exception }
                  var Error := AcquireExceptionObject;
                  ErrorExists := True;
                  try
                    var ErrorMsg := (Error as Exception).Message;
                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(CallBacks.OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        CallBacks.OnError(CallBacks.Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);

        if ErrorExists then
          Continue;

        {--- TTask.WaitForAll is not used due to a memory leak in TLightweightEvent/TCompleteEventsWrapper.
             See report RSP-12462 and RSP-25999. }
        TTaskHelper.ContinueWith(Tasks[Index],
          procedure
          begin
            Dec(Counter);
            if Counter = 0 then
              begin
                try
                  if not ErrorExists and Assigned(CallBacks.OnSuccess) then
                    CallBacks.OnSuccess(CallBacks.Sender, Bundle);
                finally
                  Bundle.Free;
                end;
              end;
          end);
        {--- Need a delay, otherwise the process runs only with the first task. }
        Sleep(30);
      end;
  finally
    BundleParams.Free;
  end;
end;

function TChatRoute.CreateStream(const ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos, LineFeed: Integer;
  ResponseBuffer, CurrentLine, Data, NewBuffer: string;
  Chunk: TChat;
  IsDone: Boolean;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TChatParams>('chat/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
      begin
        try
          ResponseBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
        while LineFeed >= 0 do
          begin
            CurrentLine := ResponseBuffer.Substring(RetPos, LineFeed - RetPos).Trim([' ', #13, #10]);
            Inc(RetPos, LineFeed - RetPos + 1);

            if CurrentLine.IsEmpty then
              begin
                LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
                Continue;
              end;

            if CurrentLine.StartsWith('data: ') then
              Data := CurrentLine.Substring(6).Trim
            else
              Data := CurrentLine;

            IsDone := SameText(Data, '[DONE]');

            Chunk := nil;
            if not IsDone then
              try
                Chunk := TApiDeserializer.Parse<TChat>(
                  TJSONNormalizer.Normalize(Data, ['choices', '*', 'delta', 'content']));
              except
                Chunk := nil;
              end;

            if Assigned(Event) then
              try
                Event(Chunk, IsDone, AAbort);
              finally
                Chunk.Free;
              end;

            if IsDone then
              Break;

            LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
          end;

        if RetPos > 0 then
          begin
            NewBuffer := ResponseBuffer.Substring(RetPos);
            Response.Size := 0;
            if NewBuffer <> '' then
              Response.WriteString(NewBuffer);
            RetPos := 0;
          end;
      end);
  finally
    Response.Free;
  end;
end;

{ TChatMessage }

destructor TChatMessage.Destroy;
begin
  for var Tool in FToolsCalls do
    Tool.Free;
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TChatMessagePayload }

class function TChatMessagePayload.Assistant(const Value: string; const Prefix: Boolean): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.assistant).Content(Value).Prefix(Prefix);
end;

class function TChatMessagePayload.Assistant(const Value: string;
  const Url: TArray<string>; const Prefix: Boolean): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  JSONArray.Add(TMessageContent.Create.&Type(TContentType.text).Text(Value).Detach);
  for var Item in Url do
    JSONArray.Add(TMessageContent.Create.&Type(TContentType.image_url).ImageUrl(ImageUrlCheck(Item)).Detach);
  Result := TChatMessagePayload.Create.Role(TMessageRole.assistant).Content(JSONArray).Prefix(Prefix);
end;

class function TChatMessagePayload.Assistant(const Value: string;
  const Func: TArray<TToolCalls>; const Prefix: Boolean): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.assistant).Content(Value).ToolCalls(Func).Prefix(Prefix);
end;

function TChatMessagePayload.Content(const Value: string): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('content', Value));
end;

function TChatMessagePayload.Content(const Kind: TContentType; const Value,
  Detail: string): TChatMessagePayload;
begin
  var MessageContent := TMessageContent.Create.&Type(Kind);
  case Kind of
    TContentType.text:
      MessageContent := MessageContent.Text(Value);
    TContentType.image_url:
      MessageContent := MessageContent.ImageUrl(Value, Detail);
    TContentType.input_audio:
      MessageContent := MessageContent.InputAudio(Value);
  end;
  Result := TChatMessagePayload(Add('content', MessageContent.Detach));
end;

function TChatMessagePayload.Content(const Value: TJSONArray): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('content', Value));
end;

function TChatMessagePayload.Content(
  const Value: TArray<string>): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Item));
  Result := TChatMessagePayload(Add('content', JSONArray));
end;

function TChatMessagePayload.Content(const Value: string;
  const Url: TDocumentUrlParams): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  JSONArray
    .Add(
      TJSONObject.Create
        .AddPair('type', 'text')
        .AddPair('text', Value))
    .Add(Url.Detach);

  Result := TChatMessagePayload(Add('content', JSONArray));
end;

function TChatMessagePayload.Prefix(const Value: Boolean): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('prefix', Value));
end;

function TChatMessagePayload.Role(const Value: TMessageRole): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('role', Value.ToString));
end;

class function TChatMessagePayload.System(
  const Value: TArray<string>): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.system).Content(Value);
end;

class function TChatMessagePayload.System(const Value: string): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.system).Content(Value);
end;

function TChatMessagePayload.ToolCalls(
  const Value: TArray<TToolCalls>): TChatMessagePayload;
begin
  if Length(Value) = 0 then
    Exit(Self);

  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TChatMessagePayload(Add('tool_calls', JSONArray));
end;

class function TChatMessagePayload.User(const Value: string;
  const Url: TDocumentUrlParams): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(Value, Url);
end;

class function TChatMessagePayload.User(const Value,
  Url: string): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  JSONArray.Add(TMessageContent.Create.&Type(TContentType.input_audio).InputAudio(AudioUrlCheck(Url)).Detach);
  JSONArray.Add(TMessageContent.Create.&Type(TContentType.text).Text(Value).Detach);
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(JSONArray);
end;

class function TChatMessagePayload.User(const Value: string;
  const Url: TArray<string>): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  JSONArray.Add(TMessageContent.Create.&Type(TContentType.text).Text(Value).Detach);
  for var Item in Url do
    JSONArray.Add(TMessageContent.Create.&Type(TContentType.image_url).ImageUrl(ImageUrlCheck(Item)).Detach);
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(JSONArray);
end;

class function TChatMessagePayload.User(const Value: string): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(Value);
end;

class function TChatMessagePayload.Assistant(const Value: string;
  const Url: TArray<string>;
  const Func: TArray<TToolCalls>;
  const Prefix: Boolean = False): TChatMessagePayload;
begin
  Result := User(Value, Url);
  Result := Result.ToolCalls(Func).Prefix(Prefix);
end;

{ TMessageContent }

function TMessageContent.ImageUrl(const Url: string): TMessageContent;
begin
  Result := TMessageContent(Add('image_url', Url));
end;

function TMessageContent.ImageUrl(const Url, Detail: string): TMessageContent;
begin
  var Value := TMessageImageURL.Create.Url(Url);
  if not Detail.IsEmpty then
    Value := Value.Detail(Detail);
  Result := TMessageContent(Add('image_url', Value.Detach));
end;

function TMessageContent.InputAudio(const Url: string): TMessageContent;
begin
  Result := TMessageContent(Add('input_audio', Url));
end;

function TMessageContent.Text(const Value: string): TMessageContent;
begin
  Result := TMessageContent(Add('text', Value));
end;

function TMessageContent.&Type(const Value: TContentType): TMessageContent;
begin
  Result := TMessageContent(Add('type', Value.ToString));
end;

{ TMessageImageURL }

function TMessageImageURL.Detail(const Value: string): TMessageImageURL;
begin
  Result := TMessageImageURL(Add('detail', Value));
end;

function TMessageImageURL.Url(const Value: string): TMessageImageURL;
begin
  Result := TMessageImageURL(Add('url', Value));
end;

{ TToolCalls }

function TToolCalls.&Type(const Value: TToolType): TToolCalls;
begin
  Result := TToolCalls(Add('type', Value.ToString));
end;

function TToolCalls.Func(const Name: string; Arguments: string): TToolCalls;
begin
  var Value := TFunctionCalled.Create.Name(Name).Arguments(Arguments);
  Result := TToolCalls(Add('function', Value.Detach));
end;

function TToolCalls.Id(const Value: string): TToolCalls;
begin
  Result := TToolCalls(Add('id', Value));
end;

function TToolCalls.Index(const Value: Integer): TToolCalls;
begin
  Result := TToolCalls(Add('index', Value));
end;

class function TToolCalls.New(Name, Arguments: string): TToolCalls;
begin
  Result := New.Func(Name, Arguments);
end;

class function TToolCalls.New(Id, Name, Arguments: string): TToolCalls;
begin
  Result := New.Id(Id).Func(Name, Arguments);
end;

class function TToolCalls.New: TToolCalls;
begin
  Result := TToolCalls.Create.&Type();
end;

{ TFunctionCalled }

function TFunctionCalled.Arguments(const Value: string): TFunctionCalled;
begin
  Result := TFunctionCalled(Add('arguments', Value));
end;

function TFunctionCalled.Name(const Value: string): TFunctionCalled;
begin
  Result := TFunctionCalled(Add('name', Value));
end;

{ TMessageContentImageUrl }

destructor TMessageContentImageUrl.Destroy;
begin
  if Assigned(FImageUrl) then
    FImageUrl.Free;
  inherited;
end;

{ TMessageContentThinking }

destructor TMessageContentThinking.Destroy;
begin
  for var Item in FThinking do
    Item.Free;
  inherited;
end;

{ TDocumentUrlParams }

function TDocumentUrlParams.DocumentUrl(
  const Value: string): TDocumentUrlParams;
begin
  Result := TDocumentUrlParams(Add('document_url', Value));
end;

class function TDocumentUrlParams.New: TDocumentUrlParams;
begin
  Result := TDocumentUrlParams.Create.&Type();
end;

function TDocumentUrlParams.&Type(const Value: string): TDocumentUrlParams;
begin
  Result := TDocumentUrlParams(Add('type', Value));
end;

end.
