unit MistralAI.Chat;

{-------------------------------------------------------------------------------

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, System.NetEncoding, System.Net.Mime, MistralAI.API.Params,
  MistralAI.API, MistralAI.Functions.Core, MistralAI.Functions.Tools,
  MistralAI.Vision.Params, MistralAI.Async.Support, MistralAI.Params.Core;

type
  /// <summary>
  /// Type of message role
  /// </summary>
  TMessageRole = (
    /// <summary>
    /// System message
    /// </summary>
    system,
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    assistant,
    /// <summary>
    /// Function message
    /// </summary>
    tool);

  /// <summary>
  /// Helper record for the <c>TMessageRole</c> enumeration, providing utility methods for converting
  /// between <c>TMessageRole</c> values and their string representations.
  /// </summary>
  TMessageRoleHelper = record helper for TMessageRole
    /// <summary>
    /// Converts the current <c>TMessageRole</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TMessageRole</c> value.
    /// </returns>
    /// <remarks>
    /// <code>
    /// var Role: TMessageRole;
    /// begin
    ///   Role := TMessageRole.system;
    ///   ShowMessage(Role.ToString);  // Outputs "system"
    /// end;
    /// </code>
    /// </remarks>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TMessageRole</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TMessageRole</c>.
    /// </param>
    /// <returns>
    /// The <c>TMessageRole</c> enumeration value that corresponds to the provided string.
    /// </returns>
    /// <remarks>
    /// If the input string does not match any valid <c>TMessageRole</c> value, an exception will be raised.
    /// <code>
    /// var Role: TMessageRole;
    /// begin
    ///   Role := TMessageRoleHelper.FromString('user');  // Returns TMessageRole.user
    /// end;
    /// </code>
    /// </remarks>
    class function FromString(const Value: string): TMessageRole; static;
  end;

  /// <summary>
  /// Represents the different reasons why the processing of a request can terminate.
  /// </summary>
  TFinishReason = (
    /// <summary>
    /// API returned complete model output
    /// </summary>
    stop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    length_limit,
    /// <summary>
    /// model_length
    /// </summary>
    model_length,
    /// <summary>
    /// An error was encountered while processing the request
    /// </summary>
    error,
    /// <summary>
    /// A function must be invoked before further processing of the request
    /// </summary>
    tool_calls);

  /// <summary>
  /// Helper record for the <c>TFinishReason</c> enumeration, providing utility methods for conversion between string representations and <c>TFinishReason</c> values.
  /// </summary>
  TFinishReasonHelper = record helper for TFinishReason
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TFinishReason</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TFinishReason</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TFinishReason</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TFinishReason</c> values.
    /// </remarks>
    class function Create(const Value: string): TFinishReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TFinishReason</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TFinishReason</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TFinishReason</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TFinishReason</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFinishReason</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TFinishReason</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TFinishReason</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TFinishReason</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TFinishReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// The <c>TChatMessagePayload</c> record represents a chat message payload, which includes both the role of the message sender and the content of the message.
  /// This type is used to distinguish between different participants in the conversation (e.g., user, assistant, or system) and manage the flow of messages accordingly.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessagePayload</c> record is essential for managing conversations in a chat application, allowing the differentiation between user inputs, assistant responses,
  /// and system messages. Each message has a role (defining the participant type) and a content field (the actual message being conveyed).
  /// This record provides several helper methods to create messages with predefined roles for easier message handling.
  /// </remarks>
  TChatMessagePayload = record
  private
    FRole: TMessageRole;
    FContent: string;
    FVisionSources: TArray<TVisionSource>;
  public
    /// <summary>
    /// Gets or sets the role of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property determines who is sending the message. It can be a "user" (representing the end user), an "assistant" (representing an AI or bot),
    /// or "system" (representing system-level messages). This property is essential for contextualizing the content of the message within the chat.
    /// </remarks>
    property Role: TMessageRole read FRole write FRole;
    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant, or the system.
    /// </remarks>
    property Content: string read FContent write FContent;
    /// <summary>
    /// Represents an array of <c>TVisionSource</c> objects that contain image sources for the vision system.
    /// Each <c>TVisionSource</c> object stores the image either as a URL or a Base64-encoded string.
    /// </summary>
    /// <value>
    /// A dynamic array (<c>TArray</c>) of <c>TVisionSource</c> objects. This array holds the image sources used in the vision system.
    /// </value>
    /// <remarks>
    /// The <c>TVisionSource</c> objects in this array can be used to provide images to the system. Each source is verified as a secure HTTPS URL or a Base64-encoded image.
    /// </remarks>
    property VisionSources: TArray<TVisionSource> read FVisionSources write FVisionSources;
    /// <summary>
    /// Creates a new chat message payload with the role of the assistant.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the assistant is sending.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "assistant" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is a convenience for creating assistant messages. Use this method when the assistant needs to respond to the user or system.
    /// </remarks>
    class function Assistant(const Content: string): TChatMessagePayload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the system.
    /// </summary>
    /// <param name="Content">
    /// The content of the system message.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "system" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create system-level messages, which may be used for notifications, warnings, or other system-related interactions.
    /// </remarks>
    class function System(const Content: string): TChatMessagePayload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the user.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective, typically representing inputs or queries in the conversation.
    /// </remarks>
    class function User(const Content: string): TChatMessagePayload; overload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the user and includes associated vision sources.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="VisionSrc">
    /// An array of strings representing vision sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function User(const Content: string;
      const VisionSrc: TArray<string>): TChatMessagePayload; overload; static;
  end;

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
    function MaxTokens(const Value: Integer = 16): TChatParams;
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
    /// <param name="Value">The desired response format, with the default being <c>"json_object"</c>.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If not specified, the default value is <c>{ "type": "text" }</c>. When using JSON mode, it's necessary to instruct the model to produce JSON explicitly through the system or user messages.
    /// </remarks>
    function ResponseFormat(const Value: string = 'json_object'): TChatParams;
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
    function Temperature(const Value: Single = 0.7): TChatParams;
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
    function TopP(const Value: Single = 1): TChatParams;
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
    function Tools(const Value: TArray<TChatMessageTool>): TChatParams;
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
    function ToolChoice(const Value: TToolChoice = auto): TChatParams;
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
    /// Constructor to initialize the <c>TChatParams</c> object with default values.
    /// </summary>
    constructor Create; override;
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
    [JsonNameAttribute('role')]
    FRole: string;
    [JsonNameAttribute('content')]
    FContent: string;
    [JsonNameAttribute('tool_calls')]
    FToolsCalls: TArray<TCalledFunction>;
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
    property Content: string read FContent write FContent;
    /// <summary>
    /// A list of tool calls to be executed for query completion.
    /// </summary>
    /// <remarks>
    /// The <c>ToolsCalls</c> property contains a list of functions or tools that need to be invoked to process the current query further.
    /// This is typically used when the assistant needs to call external APIs or perform specific actions before delivering a final response.
    /// </remarks>
    property ToolsCalls: TArray<TCalledFunction> read FToolsCalls write FToolsCalls;
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
    [JsonNameAttribute('index')]
    FIndex: Int64;
    [JsonNameAttribute('message')]
    FMessage: TChatMessage;
    [JsonReflectAttribute(ctString, rtString, TFinishReasonInterceptor)]
    FFinish_reason: TFinishReason;
    [JsonNameAttribute('delta')]
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
  TChat = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('model')]
    FModel: string;
    [JsonNameAttribute('choices')]
    FChoices: TArray<TChatChoices>;
    [JsonNameAttribute('usage')]
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
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = TAsyncCallBack<TChat>;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = TAsyncStreamCallBack<TChat>;

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
  /// <list type="bullet">
  /// <item>
  /// <term><c>Create</c></term>
  /// <description>Sends a chat request and waits for a full response.</description>
  /// </item>
  /// <item>
  /// <term><c>AsyncCreate</c></term>
  /// <description>Performs an asynchronous chat completion request with event handling.</description>
  /// </item>
  /// <item>
  /// <term><c>CreateStream</c></term>
  /// <description>Initiates a chat completion request in streaming mode, receiving tokens progressively.</description>
  /// </item>
  /// <item>
  /// <term><c>ASyncCreateStream</c></term>
  /// <description>Performs an asynchronous request in streaming mode with event handling.</description>
  /// </item>
  /// </list>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including callbacks for processing responses or errors.
  /// </remarks>
  TChatRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Create an asynchronous completion for chat message
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the chat request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided callBacks.
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
    procedure AsyncCreate(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChat>);
    /// <summary>
    /// Creates an asynchronous streaming chat completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
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
    procedure AsyncCreateStream(ParamProc: TProc<TChatParams>;
      CallBacks: TFunc<TAsynChatStream>);
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
    function Create(ParamProc: TProc<TChatParams>): TChat;
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
    function CreateStream(ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
  end;

implementation

uses
  system.StrUtils, Rest.Json, System.Rtti;

{ TMessageRoleHelper }

class function TMessageRoleHelper.FromString(const Value: string): TMessageRole;
begin
  case IndexStr(AnsiLowerCase(Value), ['system', 'user', 'assistant', 'tool']) of
    0 :
      Exit(system);
    1 :
      Exit(user);
    2 :
      Exit(assistant);
    3 :
      Exit(tool);
  end;
  Result := user;
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    system:
      Exit('system');
    user:
      Exit('user');
    assistant:
      Exit('assistant');
    tool:
      Exit('tool');
  end;
end;

{ TChatParams }

constructor TChatParams.Create;
begin
  inherited;
  Model('mistral-tiny');
end;

function TChatParams.MaxTokens(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_tokens', Value));
end;

function TChatParams.Messages(
  const Value: TArray<TChatMessagePayload>): TChatParams;
var
  JSON: TJSONObject;
begin
  var Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        var SubItems: TJSONArray := nil;

        if Length(Item.FVisionSources) > 0 then
          {--- Processing with images }
          begin
            SubItems := TJSONArray.Create;

            JSON := TJSONObject.Create;
            {"type": "text", "text": "What’s in this image?"}
            JSON.AddPair('type', 'text');
            JSON.AddPair('text', Item.Content);
            SubItems.Add(JSON);

            for var Source in Item.VisionSources do
              begin
                JSON := TJSONObject.Create;
                {"type": "image_url", "image_url": "Url or Image content to base64 string"}
                JSON.AddPair('type', 'image_url');
                JSON.AddPair('image_url', Source.Data);
                SubItems.Add(JSON);
              end;
          end;

        JSON := TJSONObject.Create;
        {--- Add role}
        JSON.AddPair('role', Item.Role.ToString);
        {--- Add content}
        if Length(Item.FVisionSources) > 0 then
          JSON.AddPair('content', SubItems) else
          JSON.AddPair('content', Item.Content);
        Items.Add(JSON);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TChatParams(Add('messages', Items));
  Exit;
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.RandomSeed(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('random_seed', Value));
end;

function TChatParams.ResponseFormat(const Value: string): TChatParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('type', Value);
  Result := TChatParams(Add('response_format', JSON));
end;

function TChatParams.SafePrompt(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('safe_prompt', Value));
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

{ TChatMessagePayload }

class function TChatMessagePayload.Assistant(
  const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.assistant;
  Result.FContent := Content;
end;

class function TChatMessagePayload.System(const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.system;
  Result.FContent := Content;
end;

class function TChatMessagePayload.User(const Content: string;
  const VisionSrc: TArray<string>): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
  for var Item in VisionSrc do
    Result.VisionSources := Result.VisionSources + [TVisionSource.Create(Item)];         
end;

class function TChatMessagePayload.User(
  const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
end;

{ TFinishReasonHelper }

class function TFinishReasonHelper.Create(const Value: string): TFinishReason;
begin
  case IndexStr(AnsiLowerCase(Value), ['stop', 'length', 'model_length', 'error', 'tool_calls']) of
    0 :
      Exit(stop);
    1 :
      Exit(length_limit);
    2 :
      Exit(model_length);
    3 :
      Exit(error);
    4 :
      Exit(tool_calls);
  end;
  Result := stop;
end;

function TFinishReasonHelper.ToString: string;
begin
  case Self of
    stop:
      Exit('stop');
    length_limit:
      Exit('length');
    model_length:
      Exit('model_length');
    error:
      Exit('error');
    tool_calls:
      Exit('tool_calls');
  end;
end;

{ TFinishReasonInterceptor }

function TFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFinishReason>.ToString;
end;

procedure TFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFinishReason.Create(Arg)));
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

procedure TChatRoute.AsyncCreate(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChat>);
begin
  with TAsyncCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
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

procedure TChatRoute.AsyncCreateStream(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChatStream>);
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

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
                        end);
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

function TChatRoute.Create(ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('chat/completions', ParamProc);
end;

function TChatRoute.CreateStream(ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TChatParams>('chat/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Chat: TChat;
        TextBuffer: string;
        Line: string;
        Ret: Integer;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        repeat
          Ret := TextBuffer.IndexOf(#10, RetPos);
          if Ret < 0 then
            Continue;
          Line := TextBuffer.Substring(RetPos, Ret - RetPos);
          RetPos := Ret + 1;

          if Line.IsEmpty or Line.StartsWith(#10) then
            Continue;
          Chat := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Chat := TJson.JsonToObject<TChat>(Data);
          except
            Chat := nil;
          end;

          try
            Event(Chat, IsDone, AAbort);
          finally
            Chat.Free;
          end;
        until Ret < 0;

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
  inherited;
end;

end.
